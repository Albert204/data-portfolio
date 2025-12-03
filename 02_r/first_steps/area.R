AreaC = function(r){
  area = c(pi*r^2, 2*pi*r)
  return(area)
}
AreaC(10)

ej = c(2,-4, 5, -7, 0, 9)
PosNeg = function(x){
  pos = 0
  neg = 0
  for (i in x){
    if (x[i] >= 0){
      pos = pos+1
    }else{
      neg = neg+1
    }
  }
  Y = c(pos,neg)
  return (Y)
  print("neg = " + neg)
  print("pos = " + pos)
}
PosNeg(ej)

print("numero" + ej)
