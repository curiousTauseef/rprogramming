library(plyr)

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

corr <- function(directory, treshold=0, id=1:332) {
    ids = numeric()
    for (i in id) {
        if (complete(directory, i)$nobs > treshold) {
            ids <- append(ids, i);
        }
    }
    if (length(ids) == 0) {
        return (numeric())
    }
    data <- loadFiles(directory, ids);
    cor <- ddply(data[complete.cases(data),],"ID",function(x) cor(x$nitrate,x$sulfate))$V1;
}
