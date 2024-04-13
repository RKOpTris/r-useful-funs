#defuse (atomise) an element into a vector

defuse <- function(x, as_class = NULL){
  y <- stringr::str_split(x, "") %>% unlist()
  if(is.null(as_class)){
    as_class <- class(x)
  }
  eval(parse(text = paste0("as.", as_class, "(y)")))
}

# defuse("abcde")
# defuse(12345)
# defuse(12345, as_class = "character")
# defuse(pi, "character")
# defuse(pi)