#####################################################
#                                                   #
#       Function to reverse tie direction           #
#           provided by Sam Steingold               #
#                                                   #
#####################################################

graph.reverse <- function (graph) {
  if (!is.directed(graph))
    return(graph)
  e <- get.data.frame(graph, what="edges")
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
}