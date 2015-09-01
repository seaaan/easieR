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