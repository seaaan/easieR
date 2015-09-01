#' Collapse a character vector to a list suitable for an English sentence.
#'
#' @param vector The character vector to collapse (2 or more elements).
#' @param conjunction The word to use as a conjunction (no spaces).
#' @param oxford_comma Logical indicating whether the oxford comma should be 
#' used.
#' @param terminal_period Logical indicating whether a period should be 
#' appended. 
#'
#' @return A single element character vector containing all the elements of 
#' \code{vector} separated by commas and conjoined by conjunction. 
#' 
#' @export
#' 
#' @examples 
#' collapse_to_sentence(c("eats", "shoots", "leaves"), oxford_comma = FALSE)
#' collapse_to_sentence(c("my way", "the highway"), conjunction = "or")
#' collapse_to_sentence(1:10, conjunction = "or", terminal_period = TRUE)
collapse_to_sentence <- function(vector, conjunction = "and", 
      oxford_comma = TRUE, terminal_period = FALSE) {
   
   terminal_period <- ifelse(terminal_period, ".", "")
   oxford_comma <- ifelse(oxford_comma, ", ", " ")
   
   if (length(vector) <= 1) {
      stop_h("Vector '", deparse(substitute(vector)), 
            "' must contain more than one element, but contains ", 
            length(vector), ".")
   } else if (length(vector) == 2) {
      result <- paste0(vector, collapse = paste0(" ", conjunction, " "))
   } else {
      result <- paste0(vector[1:(length(vector) - 1)], collapse = ", ")
      result <- paste0(result, oxford_comma, conjunction, " ", 
         vector[length(vector)])
   }
         
   paste0(result, terminal_period)
}

#' Collapse a character vector to a slash-delimited string.
#'
#' @param vector The character vector to collapse (2 or more elements).
#' @param forward Logical indicating whether a forward slash (TRUE) should be 
#' used or a doubled-backslash (FALSE)
#' @param terminal_slash Logical indicating whether the string should be 
#' terminated by a slash.
#'
#' @return A single element character vector containing all the elements of 
#' \code{vector} separated by slashes. 
#' 
#' @export
#' 
#' @examples 
#' collapse_slash(c("www.github.com", "seaaan", "easieR"))
#' collapse_slash(c("~", "Documents", "Important", 
#'    "passwords-in-plain-text.txt"), terminal_slash = FALSE)
#' collapse_slash(c("backslashes", "are", "doubled"), forward = FALSE)
collapse_slash <- function(vector, forward = TRUE, terminal_slash = TRUE) {
   slash <- ifelse(forward, "/", "\\")
   terminal_slash <- ifelse(terminal_slash, slash, "")
   
   if (length(vector) <= 1) {
      stop_h("Vector '", deparse(substitute(vector)), 
         "' must contain more than one element, but contains ", 
         length(vector), ".")
   } else {
      result <- paste0(vector, collapse = slash)
   }
   
   paste0(result, terminal_slash)
}