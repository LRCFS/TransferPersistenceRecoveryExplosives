# this is to set the range for the keyword figure
# range <- as.numeric(max(SubsetKeywordNarrowRangeGraph$x, na.rm = TRUE))

if (range>400){
  DatasetRange <- c("0","1-2","3-5","6-10","11-25","26-50","51-75","76-100", "101-200","200-400",">400")
  BreakRange <- list(-1,0,2,5,10,25,50,75,100,200,400)
} else{
  if (range>100){
    DatasetRange <- c("0","1-2","3-5","6-10","11-20","21-30","31-40","41-50","51-75","76-100",">100")
    BreakRange <- list(-1,0,2,5,10,20,30,40,50,75,100)
  } else{
  if (range>50){
    DatasetRange <- c("0","1-2","3-5","6-9","10-14","15-19","20-24","25-29","30-39","40-49",">49")
    BreakRange <- list(-1,0,2,5,9,14,19,24,29,39,49)
  } else{
  if (range>40){
    DatasetRange <- c("0","1-2","3-5","6-8","8-10","11-15","16-20","21-25","25-30","31-40",">40")
    BreakRange <- list(-1,0,2,5,7,10,15,20,24,30,40)
  } else{
  if (range>30){
    DatasetRange <- c("0","1","2","3-5","6-8","8-10","11-15","16-20","21-25","25-30",">30")
    BreakRange <- list(-1,0,1,2,5,7,10,15,20,24,30)
  } else{
  if (range>30){
    DatasetRange <- c("0","1","2","3","4","5","6-7","8-10","11-15","15-20",">20")
    BreakRange <- list(-1,0,1,2,3,4,5,7,10,14,10)
  } else{
  if (range>10){
    DatasetRange <- c("0","1","2","3","4","5","6","7","8","9",">9")
    BreakRange <- list(-1,0,1,2,3,4,5,6,7,8,9)
  } else{
  DatasetRange <- c("0","1","2","3","4",">5")
  BreakRange <- list(-1,0,1,2,3,4)
  }}}}}}}