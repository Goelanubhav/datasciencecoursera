corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  #if(grep("specdata", directory) == 1) {
   # directory <- ("./specdata/")
  #}
  # get the complete table
  complete_table <<- complete("C:/SCM/Coursera/DST-1/specdata/", 1:332)
  nobs <- complete_table$nobs
  # find the valid ids
  ids <- complete_table$id[nobs > threshold]
  # get the length of ids vector
  id_len <<- length(ids)
  corr_vector <<- rep(0, id_len)
  # find all files in the specdata folder
  all_files <<- as.character( list.files(directory) )
  #file_paths <- paste(directory, all_files, sep="")
  j <- 1
  for(i in ids) {
    current_file <<- read.csv(all_files[i], header=T, sep=",")
    corr_vector[j] <<- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    j <- j + 1
  }
  result <- corr_vector
  return(result)   
}




cr <- corr("C:/SCM/Coursera/DST-1/specdata/", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("C:/SCM/Coursera/DST-1/specdata/", 2000)                
n <- length(cr)                
cr <- corr("C:/SCM/Coursera/DST-1/specdata/", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
