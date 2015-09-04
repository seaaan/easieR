#' Replace NA values with \code{replacement}.
#'
#' @param vector Vector to replace NA values in.
#' @param replacement Value to replace NAs with (must be a single element).
#' @param warn Logical indicating whether to issue a warning message if no 
#' element is replaced.
#'
#' @return \code{Vector} with all NA elements replaced by \code{replacement}.
#' 
#' Note that factors will be releveled with the replacement values and levels
#' no longer present in the factor may be lost.
#'
#' @export
#'
#' @examples
#' replace_na(c("a", "b", NA), "look at me")
replace_na <- function(vector, replacement, warn = TRUE) {
   validate_single_element(replacement, deparse(substitute(replacement)))
   f <- function(x) which(is.na(x))
   return(replace_internal(vector, replacement, warn, f, "NAs"))
}

# replace_null <- function(vector, replacement, warn = TRUE) {
#    f <- function(x) which(is.null(x))
#    return(replace_internal(vector, replacement, warn, f, "null values"))
# }

#' Replace values identical to \code{value} with \code{replacement}.
#'
#' @param vector Vector to replace matches in.
#' @param value Value to be replaced (must be a single element).
#' @param replacement Value to replace matches with (must be a single element).
#' @param warn Logical indicating whether to issue a warning message if no 
#' element is replaced.
#'
#' @return \code{Vector} with all instances of \code{value} replaced by 
#' \code{replacement}.
#' 
#' Note that factors will be releveled with the replacement values and levels
#' no longer present in the factor may be lost.
#'
#' @export
#'
#' @examples
#' replace_value(c("Lions", "Tigers", "Antelopes"), value = "Antelopes", 
#'    replacement = "Bears")
replace_value <- function(vector, value, replacement, warn = TRUE) {
   validate_single_element(replacement, deparse(substitute(replacement)))
   validate_single_element(value, deparse(substitute(value)))
   f <- function(x) which(x == value)
   return(replace_internal(vector, replacement, warn, f, "matching values"))
}

#' Replace elements matching a regular expression with \code{replacement}.
#'
#' @param vector Vector to replace matches in.
#' @param regex A regular expression (must be a single element).
#' @param replacement Value to replace matches with (must be a single element).
#' @param warn Logical indicating whether to issue a warning message if no 
#' element is replaced.
#'
#' @return \code{Vector} with all elements matching \code{regex} replaced by 
#' \code{replacement}.
#'
#' Note that factors will be releveled with the replacement values and levels
#' no longer present in the factor may be lost.
#'
#' @export
#' 
#' @examples
#' replace_regex(c("Farmworker", "Programmer", "Farmhand", "Pole vaulter", 
#'       "Farmer", "Dancer"), regex = "^Farm.", replacement = "Farmer")
replace_regex <- function(vector, regex, replacement, warn = TRUE) {
   validate_single_element(replacement, deparse(substitute(replacement)))
   validate_single_element(regex, deparse(substitute(regex)))
   f <- function(x) which(grepl(pattern = regex, x = x))
   return(replace_internal(vector, replacement, warn, f, "matching values"))
}

# Helper function for \code{replace_*} functions.
#
# @param vector Vector to make replacements in.
# @param replacement Value to replace with (assumed to be a single element).
# @param warn Logical indicating whether to issue a warning message if no
# element is replaced.
# @param f Function to determine which elements of vector to replace.
# @param description Single-element character vector describing the type of 
# match. 
#
# @return \code{Vector} with matches replaced.
replace_internal <- function(vector, replacement, warn, f, description) {
   indices <- f(vector)
   
   if(length(indices) == 0) {
      if(warn) {
         warning_h("Vector contains no ", description, 
            ", so nothing will be replaced.")
      }
      return(vector)
   } else {
      isFactor <- is.factor(vector)
      if (isFactor) vector <- as.character(vector)
      result <- replace(vector, indices, replacement)
      if (isFactor) result <- factor(result)
   }
   
   result
}

#' Replace multiple unique elements with multiple unique replacements.
#'
#' @param vector Vector to make replacements in.
#' @param values Values to be replaced (vector of same length as 
#' \code{replacements}).
#' @param replacements Values to replace matches with (vector of same length as
#' \code{values}).
#' @param warn Logical indicating whether to issue a warning message if no 
#' element is replaced.
#'
#' @return Vector with elements in \code{values} replaced with the 
#' corresponding elements from \code{replacements}.
#' 
#' Every element in \code{values} will be searched for in \code{vector}. If it is
#' found, it will be replaced in \code{vector} with the corresponding element of
#' \code{replacements}. That is, if the 3rd element of \code{values} is found 
#' anywhere in \code{vector}, it will be replaced by the 3rd element of 
#' \code{replacements}.
#' 
#' Note that factors will be releveled with the replacement values and levels
#' no longer present in the factor may be lost.
#' 
#' @export
#'
#' @examples
#' replace_values(
#'    vector = c("WA", "WY", "OR", "NY", "NY", "WA", "FL", "OR"), 
#'    values = c("FL", "NY", "OR", "WA", "WY"),
#'    replacements = c("Florida", "New York", "Oregon", "Washington", "Wyoming")) 
#' replace_values(
#'    vector = c("M", "F", "F", "M", "F", "M", "F", "M", "M", "M", "F", "F"),
#'    values = c("F", "M"), 
#'    replacements = c("Female", "Male"))
replace_values <- function(vector, values, replacements, warn = TRUE) {
   validate_equal_length(replacements, values, 
         deparse(substitute(replacements)), deparse(substitute(values)))
   
   names(replacements) <- values
   
   if(!(any(vector %in% values))) {
      if(warn) {
         warning_h("Vector '", deparse(substitute(vector)), "' contains no ", 
               "values from '", deparse(substitute(values)), 
               "', so nothing will be replaced.")
      }
      return(vector)
   } else {
      indices <- which(vector %in% values)
      toReplace <- vector[indices]
      isFactor <- is.factor(vector)
      if (isFactor) vector <- as.character(vector)
      vector[indices] <- replacements[as.character(toReplace)]
      if (isFactor) vector <- factor(vector)
   }
   vector
}