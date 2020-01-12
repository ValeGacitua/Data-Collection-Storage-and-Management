# flashiness
flash <- function(temp) {
  n <- length(temp) # read out length of input data
  t_fl <<- c() # create return vector; <<- defines global variable
  for(i in 2:n) t_fl[i] <- abs(temp[i]-temp[i-1]) # calculate RB-index
  flashy <- sum(t_fl, na.rm = TRUE)/(n-1)
  return(flashy)
}

t_flsh <- flash(temp = df_temp$th) # average of the hourly flashiness