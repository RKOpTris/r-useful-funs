str_anyof <- function(vector_to_search, search_for, case_sensitive = F){
  pattern <- paste(search_for, collapse = "|")
  if(!case_sensitive){
    pattern <- paste0("(?i)", pattern)
  }
  str_detect(vector_to_search, pattern)
}
