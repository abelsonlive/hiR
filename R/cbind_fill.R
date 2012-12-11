#' Like rbind.fill in plyr but with cbind.
#'
#' @param ... data.frames to combine.
#'
#' @export
#'

cbind_fill<-function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}