library(mlr3data)
library(rpart)
library(skimr)
library(corrplot)
library(tidyverse)
library(rpart.plot)
library(caret)
library(C50)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(paradox)
library(data.table)
library(mlr3extralearners)


package = "mlr3data"
data("ilpd", package = "mlr3data")


#-------EDA A MANO------------
summary(ilpd)
sapply(ilpd,class)
sapply(ilpd,function(x) sum(is.na(x))/length(x))
par(mfrow = c(2,2))
plot(diseased~., data = ilpd)

numericos=sapply(ilpd, is.numeric)
mat_cor=cor(ilpd[, numericos])
corrplot(mat_cor, method='ellipse',type='upper')
skimr::skim(ilpd)

#create_report(ilpd, y = "diseased")

#----------VISUAL(no tiene sentido hacer esto con un T/F cambiar o quitar)------------
library(ggplot2)
head(ilpd)

for (var in numericos) {
  p <- ggplot(ilpd, aes_string(x = var, y = "diseased")) +
    geom_point(color = "steelblue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = paste("diseased vs", var),
         x = var, y = "diseased") +
    theme_minimal()
  print(p)
}

#-------------Limpieza(atipicos y correlacion)----------------
filas_eliminar <- c(576, 549, 520, 262, 280)
ilpd_clean <- ilpd[-filas_eliminar, ]         # quitar filas
ilpd_clean$direct_bilirubin <- NULL           # quitar columna

head(ilpd_clean)

#----------train/test----------------
set.seed(1)
ind_train <- sample(seq_len(nrow(ilpd_clean)),
                    size = round(nrow(ilpd_clean) * 3/4),
                    replace = FALSE)
ind_test <- setdiff(seq_len(nrow(ilpd_clean)), ind_train)

# Usa estos índices en R base:
ilpd_train <- ilpd_clean[ind_train, ]
ilpd_test  <- ilpd_clean[ind_test, ]

#------------arbol rpart-------------
set.seed(1)
rpart_model <- rpart(diseased ~ ., data = ilpd_train, method = "class")
rpart.plot(rpart_model)
# El número que sale en las hojas del modelo el la proporción de no enfermos que hay en ese nodo 

rpart_test <- predict(rpart_model, ilpd_test, type = "class")

conf_matrix <- table(Predicho = rpart_test, Real = ilpd_test$diseased)
confusionMatrix(rpart_test, ilpd_test[["diseased"]])

train_pred <- predict(rpart_model, ilpd_train, type = "class")
train_acc <- mean(train_pred == ilpd_train$diseased)

test_pred <- predict(rpart_model, ilpd_test, type = "class")
test_acc <- mean(test_pred == ilpd_test$diseased)

cat("Accuracy entrenamiento:", round(train_acc, 3), "\n")
cat("Accuracy test:", round(test_acc, 3), "\n")

#--------------hiperparametros rpart-------------------
cp_values <- c(0.001, 0.005, 0.01, 0.02)
minsplit_values <- c(10, 20, 30, 40)
maxdepth_values <- c(3, 5, 7, 10)

results <- data.frame(
  cp = numeric(),
  minsplit = numeric(),
  maxdepth = numeric(),
  acc_train = numeric(),
  acc_test = numeric(),
  overfit_penalty = numeric(),
  score = numeric()
)

for (cp in cp_values) {
  for (minsplit in minsplit_values) {
    for (maxdepth in maxdepth_values) {
      model <- rpart(
        diseased ~ .,
        data = ilpd_train,
        method = "class",
        control = rpart.control(cp = cp, minsplit = minsplit, maxdepth = maxdepth)
      )
      pred_train <- predict(model, ilpd_train, type = "class")
      acc_train <- mean(pred_train == ilpd_train$diseased)
      
      pred_test <- predict(model, ilpd_test, type = "class")
      acc_test <- mean(pred_test == ilpd_test$diseased)
      
      overfit_penalty <- abs(acc_train - acc_test)
      score <- acc_test - 0.5 * overfit_penalty
      
      results <- rbind(
        results,
        data.frame(cp, minsplit, maxdepth, acc_train, acc_test, overfit_penalty, score)
      )
      
      cat("Evaluado: cp =", cp, ", minsplit =", minsplit, ", maxdepth =", maxdepth, "\n")
    }
  }
}

best <- results %>% arrange(desc(score)) %>% slice(1)
cat("\n🟢 Mejor combinación encontrada:\n")
print(best)

#--------------entrenamiento y reevaluación-------------

best_model <- rpart(
  diseased ~ .,
  data = ilpd_train,
  method = "class",
  control = rpart.control(
    cp = best$cp,
    minsplit = best$minsplit,
    maxdepth = best$maxdepth
  )
)
rpart.plot(best_model, type = 2, extra = 104, fallen.leaves = TRUE)

pred_train <- predict(best_model, ilpd_train, type = "class")
pred_test <- predict(best_model, ilpd_test, type = "class")

acc_train <- mean(pred_train == ilpd_train$diseased)
acc_test <- mean(pred_test == ilpd_test$diseased)

cat("\n✅ Accuracy entrenamiento:", round(acc_train, 3))
cat("\n✅ Accuracy test:", round(acc_test, 3))
cat("\n⚖️ Diferencia train-test:", round(abs(acc_train - acc_test), 3))

best_cp <- best$cp
best_minsplit <- best$minsplit
best_maxdepth <- best$maxdepth

#----------rpart final-----------------
final_model <- rpart(
  diseased ~ .,
  data = ilpd_clean,
  method = "class",
  control = rpart.control(
    cp = best_cp,
    minsplit = best_minsplit,
    maxdepth = best_maxdepth
  )
)

rpart.plot(final_model, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Árbol de Clasificación Final (entrenado con todos los datos)")

pred_final <- predict(final_model, ilpd_clean, type = "class")
acc_final <- mean(pred_final == ilpd_clean$diseased)

cat("\n✅ Accuracy global (entrenado con todos los datos):", round(acc_final, 3))

#-------------------C5.0--------------
# Modelo 5.0

# Definimos variables predictoras y objetivo
x_train <- ilpd_train[, !(names(ilpd_train) %in% "diseased")]
y_train <- ilpd_train$diseased

x_test <- ilpd_test[, !(names(ilpd_test) %in% "diseased")]
y_test <- ilpd_test$diseased

#----------hiperparametros C5.0----------
# Grid de hiperparámetros
trials_values <- c(1, 5, 10, 15, 20)
winnow_values <- c(TRUE, FALSE)
CF_values <- c(0.1, 0.25, 0.4, 0.5)

results <- data.frame(trials = numeric(),
                      winnow = logical(),
                      CF = numeric(),
                      acc_train = numeric(),
                      acc_test = numeric(),
                      score = numeric())

# Bucle de búsqueda
for (t in trials_values) {
  for (w in winnow_values) {
    for (cf in CF_values) {
      model <- C5.0(x = x_train, y = y_train,
                    trials = t,
                    control = C5.0Control(winnow = w, CF = cf))
      pred_train <- predict(model, x_train)
      pred_test <- predict(model, x_test)
      
      acc_train <- mean(pred_train == y_train)
      acc_test <- mean(pred_test == y_test)
      
      overfit_penalty <- abs(acc_train - acc_test)
      score <- acc_test - 0.5 * overfit_penalty
      
      results <- rbind(results, data.frame(
        trials = t,
        winnow = w,
        CF = cf,
        acc_train = acc_train,
        acc_test = acc_test,
        score = score
      ))
    }
  }
}

best_params <- results %>% arrange(desc(score)) %>% head(1)
best_params

#-------------C5.0 final-------------
x_clean <- ilpd_clean[, !(names(ilpd_clean) %in% "diseased")]
y_clean <- ilpd_clean$diseased
best_model <- C5.0(
  x = x_clean, y = y_clean,
  trials = best_params$trials,
  control = C5.0Control(winnow = best_params$winnow, CF = best_params$CF)
)

summary(best_model)

pred_final_c5 <- predict(best_model, x_clean)
conf_matrix <- confusionMatrix(pred_final_c5, y_clean)
conf_matrix

# ========================================================
#---------rpart y C5.0 misma muestra MLR3------------

#cambiar los datos al formato necesario
num_cols <- c("age","total_bilirubin","alkaline_phosphatase","alanine_transaminase","aspartate_transaminase","total_protein","albumin","albumin_globulin_ratio")
cat_cols <- c("gender")
target   <- "diseased"
ilpd_clean <- ilpd_clean |>
  dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ as.numeric(.x)),dplyr::across(dplyr::all_of(cat_cols), ~ as.factor(.x)),!!target := factor(.data[[target]], levels = c("no","yes")))

#Task con TODOS los datos
task_all <- TaskClassif$new(id = "ilpd_clean_task", backend = ilpd_clean, target = "diseased")
task_train <- task_all$clone(deep = TRUE); task_train$filter(ind_train)
task_test  <- task_all$clone(deep = TRUE); task_test$filter(ind_test)

#-------------rpart------------------

learner_cart <- lrn("classif.rpart", predict_type = "response")
set.seed(1)
learner_cart$train(task_train)

pred_cart_test <- learner_cart$predict(task_test)
acc_cart       <- pred_cart_test$score(msr("classif.acc"))
cat("Accuracy test:",acc_cart, "\n")

pred_cart_dt <- as.data.table(pred_cart_test)
print(with(pred_cart_dt, table(Predicho = response, Real = truth)))

#visualizar el árbol entrenado en TRAIN
rpart.plot::rpart.plot(learner_cart$model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Árbol RPART mlr3")

#---------hiperparametros rpart-------------
cp_values       <- c(0.001, 0.005, 0.01, 0.02)
minsplit_values <- c(10, 20, 30, 40)
maxdepth_values <- c(3, 5, 7, 10)

res_cart <- list()
k <- 1
for (cp in cp_values) {
  for (ms in minsplit_values) {
    for (md in maxdepth_values) {
      lr <- lrn("classif.rpart", predict_type = "response"); lr$param_set$values <- list(cp = cp, minsplit = ms, maxdepth = md)
      lr$train(task_train)
      pred_tr <- lr$predict(task_train); pred_te <- lr$predict(task_test)
      acc_tr <- pred_tr$score(msr("classif.acc")); acc_te <- pred_te$score(msr("classif.acc")); overfit <- abs(acc_tr - acc_te); score <- acc_te - 0.5 * overfit
      res_cart[[k]] <- data.table(cp = cp, minsplit = ms, maxdepth = md, acc_train = acc_tr, acc_test = acc_te, overfit_penalty = overfit, score = score, model = list(lr))
      k <- k + 1
    }
  }
}

res_cart_dt <- rbindlist(res_cart)[order(-score)]
best_cart <- res_cart_dt[1]
cat("RPART Mejor combinación:"); print(best_cart[, .(cp, minsplit, maxdepth, acc_train = round(acc_train,3), acc_test = round(acc_test,3), gap = round(overfit_penalty,3), score = round(score,3))])

best_cart_learner <- best_cart$model[[1]]

# Predicciones y matriz de confusión en TEST:
pred_best_cart <- best_cart_learner$predict(task_test)
pred_best_cart_dt <- as.data.table(pred_best_cart)
print(with(pred_best_cart_dt, table(Predicho = response, Real = truth)))

#visualizar árbol con hiperparámetros óptimos
try({
  rpart.plot::rpart.plot(best_cart_learner$model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Árbol CART (mlr3 - mejor configuración)")}, silent = TRUE)

#---------C5.0---------------------
learner_c50 <- lrn("classif.C50", predict_type = "response")
set.seed(1)
learner_c50$train(task_train)

pred_c50_test <- learner_c50$predict(task_test)
acc_c50       <- pred_c50_test$score(msr("classif.acc"))
cat("\n[C5.0 baseline - mlr3] Accuracy test:", round(acc_c50, 3), "\n")

pred_c50_dt <- data.table::as.data.table(pred_c50_test)
print(head(pred_c50_dt[, .(row_id = row_ids, truth, response)], 5))
print(with(pred_c50_dt, table(Predicho = response, Real = truth)))

#-----------hiperparametros C5.0----------------
trials_values <- c(1, 5, 10, 15, 20)
winnow_values <- c(TRUE, FALSE)
CF_values     <- c(0.1, 0.25, 0.4, 0.5)

res_c50 <- list(); k <- 1
for (t in trials_values) {
  for (w in winnow_values) {
    for (cf in CF_values) {
      lr <- lrn("classif.C50", predict_type = "response", trials = t, winnow = w, CF = cf)
      lr$train(task_train); pred_tr <- lr$predict(task_train); pred_te <- lr$predict(task_test)
      acc_tr <- pred_tr$score(msr("classif.acc")); acc_te <- pred_te$score(msr("classif.acc")); overfit <- abs(acc_tr - acc_te); score <- acc_te - 0.5 * overfit
      res_c50[[k]] <- data.table::data.table(trials = t, winnow = w, CF = cf, acc_train = acc_tr, acc_test = acc_te, overfit_penalty = overfit, score = score, model = list(lr))
      k <- k + 1
    }
  }
}

res_c50_dt <- data.table::rbindlist(res_c50)[order(-score)]
best_c50 <- res_c50_dt[1]
cat("C5.0 Mejor combinación:"); print(best_c50[, .(trials, winnow, CF, acc_train = round(acc_train,3), acc_test = round(acc_test,3), gap = round(overfit_penalty,3), score = round(score,3))])

best_c50_learner <- best_c50$model[[1]]
pred_best_c50 <- best_c50_learner$predict(task_test)
pred_best_c50_dt <- data.table::as.data.table(pred_best_c50)

print(with(pred_best_c50_dt, table(Predicho = response, Real = truth)))


#-----------resumen mlr------------------------
# RPART base
acc_cart_bl <- acc_cart
cm_cart_bl  <- with(pred_cart_dt, table(Predicho = response, Real = truth))
cat("\n==== CART (rpart) — Baseline ====\n"); cat("Accuracy TEST:", round(acc_cart_bl,4), "\n"); cat("Matriz de confusión (TEST):\n"); print(cm_cart_bl); cat("Primeras 5 predicciones (TEST):\n"); print(head(pred_cart_dt[, .(row_id = row_ids, truth, response)], 5))

# RPART mejorado
acc_cart_tuned_tr <- best_cart$acc_train; acc_cart_tuned_te <- pred_best_cart$score(msr("classif.acc")); gap_cart_tuned <- abs(acc_cart_tuned_tr - acc_cart_tuned_te); score_cart_tuned <- best_cart$score; cm_cart_tuned <- with(pred_best_cart_dt, table(Predicho = response, Real = truth))
cat("\n==== CART (rpart) — Tuned ====\n"); cat("Mejores hiperparámetros -> cp:", best_cart$cp, " minsplit:", best_cart$minsplit, " maxdepth:", best_cart$maxdepth, "\n"); cat("Accuracy TRAIN:", round(acc_cart_tuned_tr,4), " | Accuracy TEST:", round(acc_cart_tuned_te,4), " | Gap:", round(gap_cart_tuned,4), " | Score:", round(score_cart_tuned,4), "\n"); cat("Matriz de confusión (TEST):\n"); print(cm_cart_tuned); cat("Primeras 5 predicciones (TEST):\n"); print(head(pred_best_cart_dt[, .(row_id = row_ids, truth, response)], 5))

# C5.0 base
acc_c50_bl <- acc_c50; cm_c50_bl <- with(pred_c50_dt, table(Predicho = response, Real = truth))
cat("\n==== C5.0 — Baseline ====\n"); cat("Accuracy TEST:", round(acc_c50_bl,4), "\n"); cat("Matriz de confusión (TEST):\n"); print(cm_c50_bl); cat("Primeras 5 predicciones (TEST):\n"); print(head(pred_c50_dt[, .(row_id = row_ids, truth, response)], 5))

# C5.0 mejorado
acc_c50_tuned_tr <- best_c50$acc_train; acc_c50_tuned_te <- pred_best_c50$score(msr("classif.acc")); gap_c50_tuned <- abs(acc_c50_tuned_tr - acc_c50_tuned_te); score_c50_tuned <- best_c50$score; cm_c50_tuned <- with(pred_best_c50_dt, table(Predicho = response, Real = truth))
cat("\n==== C5.0 — Tuned ====\n"); cat("Mejores hiperparámetros -> trials:", best_c50$trials, " winnow:", as.logical(best_c50$winnow), " CF:", best_c50$CF, "\n"); cat("Accuracy TRAIN:", round(acc_c50_tuned_tr,4), " | Accuracy TEST:", round(acc_c50_tuned_te,4), " | Gap:", round(gap_c50_tuned,4), " | Score:", round(score_c50_tuned,4), "\n"); cat("Matriz de confusión (TEST):\n"); print(cm_c50_tuned); cat("Primeras 5 predicciones (TEST):\n"); print(head(pred_best_c50_dt[, .(row_id = row_ids, truth, response)], 5)); cat("\n###########################################################################\n")







