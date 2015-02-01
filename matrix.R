getRowsWhere <- function(matrix, columnName, columnValue) {
	return ( matrix[ matrix[, columnName] == columnValue,] )
}

getRowCount <- function(matrix) {
	return ( nrow(matrix) )
}

getColumnCount <- function(matrix) {
	return ( ncol(matrix) )
}