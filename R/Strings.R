#' Head and Tail functions for Strings.
#' 
#' @description C++ implemetations of convenience functions for strings. All functions but \code{strTake} operate with O(1) memory.
#' 
#' @section Note:
#' While originally also implemented for performance reasons, the built-in \code{substr} function is usually faster due to the Call-overhead. At least for R 3.x onward the built-in functions should be preferred when performance is an issue. See the vignette for details.
#' 
#' @details A few convenient list operations, tailored to 8-bit-character strings. 
#' @usage 
#' strHead(s)
#' strHeadLower(s)
#' strTail(s)
#' strTake(s, n)
#' strDrop(s, n)
#' @param s a string object
#' @param n the number of characters to take or drop
#' @return A character vector.

#' @name Strings
NULL

#' @name strHead
#' @rdname Strings
#' @description \code{strHead} returns the first character of a string
#' @examples
#' strHead("Gnu")
#' @export strHead
NULL

#' @name strHeadLower
#' @rdname Strings
#' @description \code{strHeadLower} returns the first character of a string coerced to lower case
#' @examples
#' strHeadLower("Gnu")
#' @export strHeadLower
NULL

#' @name strTail
#' @rdname Strings
#' @description \code{strTail} returns all but the first character of a string
#' @examples
#' strTail("Gnu")
#' @export strTail
NULL

#' @name strTake
#' @rdname Strings
#' @description \code{strTake} returns the first \code{n} characters of a string
#' @examples
#' strTake("Gnus and Gnats",3)
#' @export strTake
NULL

#' @name strDrop
#' @rdname Strings
#' @description \code{strDrop} returns all but the first \code{n} characters of a string
#' @examples
#' strDrop("Gnus and Gnats",9)
#' @export strDrop
NULL
