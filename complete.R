complete <- function(directory, id = 1:332){
    completeCount <- 0
    nobs <- NULL
    files <- list.files(directory)
    for(x in id){
        workingDirectory <- read.csv(paste(directory,"/",files[x], sep=""))
        sulfateSet <- workingDirectory[["sulfate"]]
        nitrateSet <- workingDirectory[["nitrate"]]
        for(y in 1:length(sulfateSet)){
            if(is.na(sulfateSet[y]) == FALSE && is.na(nitrateSet[y]) == FALSE){
                completeCount <- completeCount + 1
            }
        }
        nobs <- append(nobs, completeCount)
        completeCount <- 0
    }
    return(data.frame(id,nobs))
}
