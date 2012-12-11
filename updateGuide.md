##GUIDE FOR UPDATING THIS (AND ANY OTHER) R PACKAGE##

If you'd like to add a function to this R package,
just follow these simple steps:

1. OPTIONAL: If you want to create a new package from scratch, start here:

    ```
    path_to_package_dir <- "myNewPacakage"
    library("devtools")
    create(path_to_package_dir)
    ```

2. Write your functions based on this template:

    ```
    #' Short description of function
    #'
    #' More detailed description of function
    #'
    #' @param x Enter a description of this parameter x's requirements
    #' @param y Enter a description of this parameter y's requirements
    #'
    #' @return
    #' An explanation of the values the function returns
    #'
    #' @export
    #'
    #' @example
    #' # a simple example
    #' x <- 2
    #' y <- 2
    #' library("hiR")
    #' add(x, y)
    #'
    # insert your function here
    add <- function(x, y) {
        x + y
    }
    ```

3. Save your functions in the "R" subdirectory with a ".R" file extension, e.g. "add.R"

4. Run the following command the pacakge's parent directory to build your NAMESPACE, DESCRIPTION, and manual files.

    ```
    library("roxygen2")
    path_to_package_dir <- "hiR"
    roxygenise(path_to_package_dir)
    ```

5. Open the "DESCRIPTION" file and make sure it follows this template:
    ```
    Package: hiR
    Title: Harmony Institute's toolkit for R
    Description: Various helper tools for R maintained by HI data lab, since
        December 2012
    Version: 0.1
    Author: Brian Abelson <brian@harmony-institute.org>
    Maintainer: Brian Abelson <brian@harmony-institute.org>
    License: MIT
    Depends:
        R (>= 2.15.1),
        package1,
        package2,
    Suggests:
        package3,
        package4,
    LazyLoad: yes
    Collate:
        'funcion1.R'
        'funtion2.R'
    ```

6. Navigate to the pacakge's parent directory and run this in the command line:

    ```
    $R CMD check hiR
    ```

7. OPTIONAL: build the package and push to cran

    ```
    $RCMD build hiR
    $ftp ftp://cran.r-projects.org/incoming/ hiR_0.1.targz
    ```
8. OR: locate the "hiR.Rcheck" folder in the package's parent directory.
* Open this folder and move hiR-manual.pdf and hiR-ex.pdf into the "inst" folder.
* Overwite if they exist already.

9. Now push these updates to git, e.g.

    ```
    $cd hiR
    $cd git add .
    $git commit -m"my commit"
    $git push
    ```

10. Now reinstall the package using devtools:

    ```
    library("devtools")
    install_github("hiR", "hinstitute")
    library("hiR")
    help(package="hiR")
    ```

#fin!









