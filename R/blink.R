#' Your data in a blink of an eye
#' @description High level function, wrapping [myop], [eyes] and [insight]. 
#'  Checks if data is in long format. If not, myop will be applied. 
#'  eyes and insight will be applied on the result. 
#' @name blink
#' @param x data frame
#' @details
#' @examples
#' @export

blink <- function(x) {  

if(!inherits(x, "myop")){
  x <- myop(x)
}
   count <- eyes(x)
   
   names(x)[names(x) %in% c("VA", "IOP", "age")]
   #insight(x)

}
