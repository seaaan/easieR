#' Nicely display p-values at a specified number of digits.
#'
#' P-values will be displayed in one of two ways: 
#' \itemize{
#'    \item Like 0.01 if the rounded p-value is large enough to fit in 
#'          \code{digits} with at least one non-zero digit. 
#'    \item In scientific notation (e.g. 1.23e-03) otherwise. The displayed
#'          value will have one more digit than in \code{digits} to account
#'          for the digit before the decimal place.
#' }
#' 
#' @param p A numeric vector of p-values, all between 0 and 1.
#' @param digits The number of digits after the decimal point to display.
#'
#' @return A character vector of formatted p-values. 
#' @export
#'
#' @examples
#' format_p(p = c(0.01, 0.001, 0.009, 0.5), digits = 2)
format_p <- function(p, digits) {
   if(any(p < 0 || p > 1)) {
      stop("All p-values must be between 0 and 1.")
   }
   
   # calculate the minimum p that can fit into digits
   min_p <- 10^(-digits)
   
   ifelse(round(p, digits) < min_p, 
      # p is too small:
      #  display with scientific notation (need 1 extra digit for the number
      #  before the decimal point)
      format(p, scientific = TRUE, digits = digits + 1), 
      
      # p fits:
      #  display rounded
      as.character(round(p, digits))) 
}

