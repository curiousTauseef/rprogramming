rankall <- function(outcome, num = 'best') {
    map <- list('heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23);
    hospitalNameIndex = 2;
    conditionIndex <- map[[outcome]];

    hospitals <- read.csv('outcome-of-care-measures.csv', colClasses = 'character');
    if (!outcome %in% names(map)) {
        stop('invalid outcome')
    }
    hospitals[,conditionIndex] <- as.numeric(hospitals[,conditionIndex]);
    hospitals <- hospitals[complete.cases(hospitals[, conditionIndex]),];
    hospitals <- hospitals[order(hospitals[, conditionIndex], hospitals[, hospitalNameIndex]),];
    length(hospitals$State);

    if (num == 'best') {
        hospital <- aggregate(hospitals[,c("Hospital.Name")], by = list(hospitals$State), function(x) x[1]);
        names(hospital) <- c('state', 'hospital');
        hospital[,1:2] <- hospital[,2:1];
    }
    else if (num == 'worst') {
        hospital <- aggregate(hospitals[,c("Hospital.Name")], by = list(hospitals$State), function(x) x[length(x)]);
        names(hospital) <- c('state', 'hospital');
        hospital[,1:2] <- hospital[,2:1];
    }
    else {
        hospital <- aggregate(hospitals[,c("Hospital.Name")], by = list(hospitals$State), function(x) x[num]);
        names(hospital) <- c('state', 'hospital');
        hospital[,1:2] <- hospital[,2:1];
    }
}
