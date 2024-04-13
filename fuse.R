#fuse (collapse) elements of a vector together

fuse <- function(x, as_class = NULL){
  y <- paste0(x, collapse = "")
  if(is.null(as_class)){
    as_class <- class(x)
  }
  eval(parse(text = paste0("as.", as_class, "(y)")))
}

# letters[1:5] %>% fuse()
# 1:5 %>% fuse()
# 1:5 %>% fuse(as_class = "character")