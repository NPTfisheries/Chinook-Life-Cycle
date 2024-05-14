

predict_Ricker <- function(stock = NULL,
                       alpha = NULL,
                       beta = NULL,
                       capacity = NULL){

  S = stock
  a = alpha
  b = beta
  k = capacity
  
  stopifnot(!is.null(S),
            !is.null(a),
            (is.null(b) && !is.null(k)) || (!is.null(b) && is.null(k))
  )
  
  if (is.null(b)) {
    y = a * S * exp(-S*(a/(k*exp(1))))
  } else if (is.null(k)) {
    y = a*S*exp(-b*S)
  }
  
  return(y)
}