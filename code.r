makeCacheMatrix <- function(initial_matrix = matrix(sample(1:100, 9), 3, 3)) {
  stored_solve <- NULL
  set_matrix <- function(new_matrix) {
    initial_matrix <<- new_matrix
    stored_solve <<- NULL
  }
  get_matrix <- function() initial_matrix
  set_solve <- function(solve) stored_solve <<- solve
  get_solve <- function() stored_solve
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_solve = set_solve,
       get_solve = get_solve)
}

cacheSolve <- function(matrix_cache, ...) {
  stored_solve <- matrix_cache$get_solve()
  if (!is.null(stored_solve)) {
    message("Getting the inverse matrix from cache.")
    return(stored_solve)
  }
  input_matrix <- matrix_cache$get_matrix()
  inverse_matrix <- solve(input_matrix, ...)
  matrix_cache$set_solve(inverse_matrix)
  inverse_matrix
}
