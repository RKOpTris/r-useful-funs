stringIn <- function(x, pattern, negate = F){
  stopifnot(is.character(x) | is.character(pattern))
  if(!negate){
    x[str_detect(x, pattern)]
  } else {
    x[!str_detect(x, pattern)]
  }
}
