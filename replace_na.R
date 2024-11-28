replace_na <- function(x, replacement = 0){
  x[is.na(x)] <- replacement
  x
}