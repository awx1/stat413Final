deleteData <- function(data, response, percentLeft) {
  
  resp <- data[response]
  data <- select(data, -all_of(response))
  
  rowNum <- nrow(data)
  colNum <- ncol(data)
  items = rowNum * colNum
  itemsToDel = as.integer((1 - percentLeft) * items)
  
  dim_data <- dim(data)
  
  all_possible <- expand.grid(1:dim_data[1], 1:dim_data[2])
  
  sample_NA <- sample(1:dim(all_possible)[1], itemsToDel)
  
  for (samp in sample_NA) {
    delrowNum <- all_possible[samp, ][[1]]
    delcolNum <- all_possible[samp, ][[2]]
    data[[delrowNum, delcolNum]] <- NA
  }
  
  return(cbind(data, resp))
}


Wage_NA <- deleteData(Wage, "logwage", 0.95)
