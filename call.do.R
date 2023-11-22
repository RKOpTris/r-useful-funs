# useful for use with pipes
call.do <- function(args, what){
  do.call(what, args)
}

# iris_list <- iris %>% split(iris$Species)
# iris_list %>% call.do(rbind)
# iris_list |> call.do(rbind)
