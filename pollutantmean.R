pollutantmean <- function(directory,pollutant, id = 1:332 ){
    pMean <- NULL #Initialize the pMean
    files <- list.files(directory) #Grab list of files in the folder
    for( x in id){
        tempG <- read.csv(paste(directory,"/",files[x],sep="")) #Get CSV
        tempP <- tempG[[pollutant]] #Gets the Specific dataset
        filter <- is.na(tempP) #Filter to removing Nas
        tempF <- tempP[!filter] #Remove the Nas from the dataset
        pMean <- append(pMean,tempF) #Creates a single global dataset
    }
    return(mean(pMean)) #Returns the mean
}