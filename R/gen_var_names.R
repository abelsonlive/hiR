#' Automatically generate variable names for count subsets.
#'
#' Say you were building a dataset and wanted to automatically
#' generate variable names by some pattern.
#' For instance, you might want to do this with population counts
#' within 100 census tracts by race
#' IE:
#'
#'   tracts <- paste("c", rep(1:100), sep="")
#'   race - c("black", "white", "hispanic")
#'
#' In this case you would want to generate 300 unique variable names
#' This function will generate these variable names automatically when provided with:
#'   1. the "roots" - in the example above, the unique census tracts
#'   2. the "vars" - in the example above, the unique races
#'
#' @param roots A set of names that serve as the root variable
#' @param vars A set of names that represent the subsets of each root variable
#' @param delim Character to separate roots and vars by. Defeaults to "_"
#'
#' @export
#'
#' @examples
#' tracts <- paste("ct", rep(1:100), sep="")
#' race <- c("black", "white", "hispanic")
#' library("hiR")
#' gen_var_names(roots=tracts, vars=race)
gen_var_names <- function(roots, vars, delim="_"){
    n.roots <- length(roots)
    n.vars <- length(vars)
    tot.vars <- n.roots*n.vars
    instances <- seq(0,tot.vars, by=n.vars) + 1
    varnames <- character(tot.vars)

        for (i in instances){
            range <- (i+(n.vars-1))
            j <- range/n.vars
            varnames[i:range]<- paste(roots[j], delim, vars[1:n.vars], sep="")
        }
        # clean up
        varnames <- varnames[-c((length(varnames)-(n.vars-1)):length(varnames))]
        return(varnames)
}
