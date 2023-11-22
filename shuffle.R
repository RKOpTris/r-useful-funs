# to shuffle data

shuffle <- function(x, seed = NULL){
  if(is.vector(x)){
    x[sample(length(x), length(x))]
  } else if(is.factor(x)){
    levs <- levels(x)
    x <- as.character(x)
    x <- x[sample(length(x), length(x))]
    factor(x, levels = levs)
  } else if(is.data.frame(x) | is.matrix(x)){
    x[sample(nrow(x), nrow(x)), ]
  } else {
    stop("Not a supported class")
  }
}

# iris$Species

# shuffle 1 column
# shuffle(iris$Species)

# shuffle many columns (in this case the numeric data)
# lapply(iris[1:4], shuffle) %>% call.do(cbind) %>% data.frame() %>% mutate(Species = iris$Species)
