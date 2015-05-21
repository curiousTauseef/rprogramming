library('data.table')

loadFiles <- function(directory, id) {
	      files <- paste0(directory, '/', sprintf("%03d", id), '.csv');
	      tables <- lapply(files, read.csv);
	      merged <- do.call(rbind, tables);
}

complete <- function(directory, id=1:332) {
	 merged <- loadFiles(directory, id);
         modified <- as.data.table(data.frame(merged["ID"], complete.cases(merged)));
         complete <- as.data.frame(modified[, lapply(.SD,sum), by="ID"]);
	 colnames(complete) <- c('id', 'nobs');
	 complete;
}
