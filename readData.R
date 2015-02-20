source("matrix.R")

# usage: GetPercentForVirus(virusName="crassph")
GetPercentForVirus <- function(virusName, year=0) {

	dataTable=getDataTable()

	values = c()
	for (month in 1:12) {

		zero = getValuesCountEqualZero(virusName, year, month, dataTable)
		greaterThanZero = getValuesCountGreaterThanZero(virusName, year, month, dataTable)
		sumOfValues = zero + greaterThanZero

		values = c(values, (greaterThanZero / sumOfValues) )
	}

	return (values)
}

getDataTable <- function() {
	dataFilename = "all_stool_samples_remapped.txt"
	dataTable = read.table(dataFilename, sep="\t", header=TRUE)

	return(dataTable)
}

getRowsByYear <- function(year, dataTable=getDataTable() ) {

	rowsByYear = GetRowsWhere(dataTable, columnName="s_year", columnValue=year)

	return (rowsByYear)
}

getRowsByMonth <- function(month, dataTable=getDataTable() ) {

	rowsByMonth = GetRowsWhere(dataTable, columnName="s_month", columnValue=month)

	return (rowsByMonth)
}

getValuesForVirus <- function(virusName, year=0, month, dataTable=getDataTable() ) {

	dataTableFiltered = getRowsByMonth(month, dataTable)

	if (year != 0) {
		dataTableFiltered = getRowsByYear(year, dataTableFiltered)
	}

	values = dataTableFiltered[virusName]

	return (values)
}

getValuesCountGreaterThanZero <- function(virusName, year=0, month, dataTable=getDataTable()) {

	valuesForVirus = getValuesForVirus(virusName, year, month, dataTable)

	return ( sum( valuesForVirus > 0) )
}

getValuesCountEqualZero <- function(virusName, year=0, month, dataTable=getDataTable()) {

	valuesForVirus = getValuesForVirus(virusName, year, month, dataTable)

	return ( sum( valuesForVirus == 0) )
}