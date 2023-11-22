list_to_objects <- function(x, suffix = NULL, envir = parent.env(environment())){
  stopifnot(is.list(x) | is.environment(envir))
  if(is.null(names(x))){
    names(x) <- paste0("X", 1:length(x))
  } 
  list_names <- names(x)
  if(!is.null(suffix)){
    object_names <- paste0(suffix, "_", list_names)
  } else {
    object_names <- list_names
  }
  for(i in 1:length(x)){
    assign(object_names[i], x[[list_names[i]]], envir = envir)
    message(object_names[i], " created")
  }
}

# list(c(1, 2), c(3, 4)) %>% list_to_objects()