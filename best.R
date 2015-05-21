best <- function(state, outcome) {
    map <- list('heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23);
    hospitalNameIndex = 2;
    conditionIndex <- map[[outcome]];

    hospitals <- read.csv('outcome-of-care-measures.csv', colClasses = 'character');
    hospitals[,conditionIndex] <- as.numeric(hospitals[,conditionIndex]);

    if (!outcome %in% names(map)) {
        stop('invalid outcome')
    }
    else if (!state %in% unique(hospitals$State)) {
        stop('invalid state')
    }

    hospitals <- subset(hospitals, State == state);
    hospitals <- hospitals[complete.cases(hospitals[, conditionIndex]),];
    hospitals <- hospitals[order(hospitals[, conditionIndex], hospitals[, hospitalNameIndex]),];
    print(hospitals[,c(2, 23)]);
    best <- hospitals$Hospital.Name[1];
}
