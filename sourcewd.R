sourcewd <- function(){
  path <- rstudioapi::getActiveDocumentContext()$path  
  newpath <- str_split(path, "/") %>% unlist()
  newpath <- newpath[-length(newpath)]
  setwd(paste(newpath, collapse = "/"))
}
