# find equilibrium when stock = recruits

equilibrium_BH <- function(alpha = NULL, beta = NULL){
  
  a = alpha
  b = beta
  
  int1 <- ((a-1) - sqrt((a - 1)^2 - 4*a*b))/ (2 * b)
  int2 <- ((a-1) + sqrt((a - 1)^2 - 4*a*b))/ (2 * b)
  
  return(c(int1, int2))
  
}