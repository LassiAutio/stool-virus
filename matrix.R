GetRowsWhere <- function(matrix, columnName, columnValue) {
	return ( matrix[ matrix[, columnName] == columnValue,] )
}

GetRowCount <- function(matrix) {
	return ( nrow(matrix) )
}

GetColumnCount <- function(matrix) {
	return ( ncol(matrix) )
}