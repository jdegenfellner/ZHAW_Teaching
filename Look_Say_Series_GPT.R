look_and_say <- function(n) {
  series <- character(n)
  series[1] <- "1"
  
  for (i in 2:n) {
    current <- series[i-1]
    new_sequence <- character(0)
    count <- 1
    
    for (j in 2:(nchar(current) + 1)) {
      if (j <= nchar(current) && substr(current, j, j) == substr(current, j-1, j-1)) {
        count <- count + 1
      } else {
        new_sequence <- c(new_sequence, paste0(count, substr(current, j-1, j-1)))
        count <- 1
      }
    }
    
    series[i] <- paste(new_sequence, collapse = "")
  }
  
  return(series)
}

n <- 10  # Change this to the desired number of elements
look_and_say_series <- look_and_say(n)
print(look_and_say_series)
