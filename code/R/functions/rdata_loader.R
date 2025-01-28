#RData loader 
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}