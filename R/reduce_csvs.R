rbind.fill.csv <- function(mypath){
    require("plyr")
    filenames  <- list.files(path=mypath, full.names=TRUE)
    datalist  <- lapply(filenames, function(x){read.csv(file=x,header=T, stringsAsFactors=F)})
    Reduce(function(x,y) {rbind.fill(x,y)}, datalist)
}

output <- merge.csv('~/Desktop/documentary')
write.csv(output, "documentaries.csv", row.names=F)
getwd()