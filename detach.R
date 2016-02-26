#####################################################
#                                                   #
#           R function to unload packages           #
#     e.g. unload igraph so we can use statnet      #
#                                                   #
#####################################################


detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}