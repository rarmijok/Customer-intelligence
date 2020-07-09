between <- function(x, a, b, left.closed = TRUE, right.closed = TRUE) {
  if (a > b) set <- c(b, a) else set <- c(a, b)
  
  isCompact   <-  left.closed &  right.closed
  isLeftOpen  <- !left.closed &  right.closed
  isRightOpen <-  left.closed & !right.closed
  isOpen      <- !left.closed & !right.closed
  
  if (isCompact)   return(set[1] <= x & x <= set[2])
  if (isLeftOpen)  return(set[1] <  x & x <= set[2])
  if (isRightOpen) return(set[1] <= x & x <  set[2])
  if (isOpen)      return(set[1] <  x & x <  set[2])
}


