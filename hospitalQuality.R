best <- function(state, outcome) {
    map <- list('heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23);
    hospitalNameIndex = 2;
    conditionIndex <- map[[outcome]];

    hospitals <- read.csv('outcome-of-care-measures.csv', colClasses = 'character');
    hospitals <- subset(hospitals, State == state);
    hospitals <- hospitals[order(hospitals[, conditionIndex], hospitals[, hospitalNameIndex]),];
    print(hospitals[1:100, c(2, conditionIndex)]);
    best <- hospitals$Hospital.Name[1];
}
