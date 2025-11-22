library(R6)
library(rlang)   # Needed only if you use env()

LinkedList <- R6::R6Class(
  "LinkedList",
  public = list(
    head = NULL,
    size = 0,
    
    initialize = function() {
      self$head <- NULL
      self$size <- 0
    }
  )
)

create_node <- function(data = NULL, next_node = NULL) {
  env(data = data, next = next_node)
}

add_first <- function(list_obj, data) {
  new_node <- create_node(data, list_obj$head)
  list_obj$head <- new_node
  list_obj$size <- list_obj$size + 1
  invisible(list_obj)
}
