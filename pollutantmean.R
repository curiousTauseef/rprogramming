loadFiles <- function(directory, id) {
	      files <- paste0(directory, '/', sprintf("%03d", id), '.csv');
	      tables <- lapply(files, read.csv);
	      merged <- do.call(rbind, tables);
}

pollutantmean <- function(directory, pollutant, id=1:332) {
	      merged <- loadFiles(directory, id);
	      mean <- mean(merged[,pollutant], na.rm=TRUE);
}