##hiR by ![Alt text](http://dl.dropbox.com/u/6535582/HI_Files/hiR/imgs/hidatalab.jpg)##
_[Harmony Institute's](http://www.harmony-institute.org/) toolkit for [R](http://www.cran.r-project.org/)_
####Install:####

	if(!require("Rstem")){
        	install.packages("Rstem", repos="http://www.omegahat.org/R", type="source")
        	library("Rstem")
    	}
	library("devtools")
	install_github("hiR", "hinstitute")
	library("hiR")
	help(package="hiR")

[documentation](http://github.com/hinstitute/hiR/blob/master/docs/hiR-manual.pdf?raw=true) | [@hinstitute](http://www.twitter.com/hinstitute)
