# Helper function for \code{stop}. 
#
# @param ... One or more character vectors. 
# @param call. Logical indicating if the call should become part of the error
# message. 
#
# Calls \code{stop} with the message made from pasting together \code{...}
# without any spaces.  
stop_h <- function(..., call. = FALSE) {
   stop(paste0(...), call. = call.)
}

# Helper function for \code{warning}. 
#
# @param ... One or more character vectors. 
# @param call. Logical indicating if the call should become part of the error
# message. 
#
# Calls \code{warning} with the message made from pasting together \code{...}
# without any spaces.  
warning_h <- function(..., call. = FALSE) {
   warning(paste0(...), call. = call.)
}

validate_single_element <- function(x, name) {
   if (length(x) != 1) {
      stop_h("Vector '", name, "' must contain exactly one ", 
         "element, but contains ", length(x), ".")
   }
}

validate_equal_length <- function(x, y, name_x, name_y) {
   if (length(x) != length(y)) {
      stop_h("Vectors '", name_x, "' and '", name_y, "' must ",
         "contain equal numbers of elements.")
   }
}