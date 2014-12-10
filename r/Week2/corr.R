corr <- function (directory = "specdata", threshold = 0)
{
# create correct paths to files   
        id <- 1:332             
        my_files_names <- sapply(id,idToChar)
        my_full_file_name <- paste(directory,"/",my_files_names,".csv", sep = "")
        
        # combine all monitors data in 'allmonitors'        
        complete_all <- NULL
        corvect <- NULL
        for (i in seq(along=id)){
                monitor <- loadmonitor(my_full_file_name[i])
#               complete_Cases <- data.frame(id[i],sum(complete.cases(monitor)))
                monitorcomplete <- complete.cases(monitor)                
                if (sum(monitorcomplete)>threshold){
#                         monitorcomplete <- complete.cases(monitor)
                        monitor <- monitor[monitorcomplete,]
#                         complete_all <- rbind(complete_all,monitor)        
                        corn <- cor(monitor$sulfate,monitor$nitrate)
                        corvect <- c(corvect,corn)
                }
        }
        corvect
}

idToChar <- function (id){
        id_char <- if (id/100 >= 1) as.character(id)
        else if(id/100 >= 0.1) paste("0",id,sep = "")
        else paste("00",id, sep="")
        id_char
}

loadmonitor <- function(my_full_file_name){
        ds <- read.csv(my_full_file_name, header = TRUE)
}
