

predict_BH <- function(stock = NULL,
                   alpha = NULL,
                   beta = NULL,
                   capacity = NULL) {
  
  S = stock
  a = alpha
  b = beta
  k = capacity
  
  stopifnot(!is.null(S),
            !is.null(a),
            (is.null(b) && !is.null(k)) || (!is.null(b) && is.null(k))
            )
  
  if (is.null(b)) {
    y = (a * S) / (1 + (a * (S / k)))
  } else if (is.null(k)) {
    y = (a * S) / (1 + (b * S))
  }
  
  return(y)
}
