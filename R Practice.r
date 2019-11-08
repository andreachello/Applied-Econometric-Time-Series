
# The purpose of this script is to define a funtcion that returns the median weight of a given day.
# Parameters:
"

- Directory: The folder containing the files that through the funct will have full path
- Day: That will be the row of the matching sequence
"

weightMedian <- function(directory, day) {
  
  
  files_list <- list.files(directory, full.names = TRUE)
  dat <- data.frame() #Initialise the empty dataframe
  
  for (i in 1:length(files_list)) {
    
    #Loop de loop
    dat <- rbind(dat, read.csv(files_list[i]))
    
  }
  
  dat_subset <- subset(dat, dat$Day == day)
  median(dat_subset[,"Weight"], na.rm = TRUE)
}
