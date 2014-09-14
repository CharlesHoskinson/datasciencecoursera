corr <- function(directory, threshold = 0){
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
    nobSet <- complete(directory)[["nobs"]]
    counter <- 1
    corrVector <- NULL
    files <- list.files(directory)
    maxNob <- max(nobSet, na.rm = TRUE)
    if(threshold > maxNob){ 
        return(numeric(0))}
    for(x in nobSet){
        if(x > threshold){
            workingFile <- read.csv(paste(directory,"/",files[counter], sep=""))
            sulSet <- workingFile[["sulfate"]]
            nitSet <- workingFile[["nitrate"]]
            counter <- counter + 1
            corrVector <- append(corrVector, cor(sulSet,nitSet, use="complete.obs" ))
    }   else { 
            counter <- counter + 1
        }
   
}
return(corrVector)
}