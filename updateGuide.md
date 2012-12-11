##10 STEP GUIDE TO CREATING / UPDATING THIS (AND ANY OTHER) R PACKAGE##

[h/t @ hadley](http://scholarship.rice.edu/bitstream/handle/1911/36084/r-packages.key.pdf?sequence=2)

If you'd like to add a function to this R package,
just follow these 10 simple steps:

_OPTIONAL: If you want to create a new package from scratch, start here:_
    ```
    p <- "rpckg"
    library("devtools")
    create(p)
    ```
1. Carefully format your functions acoording to this template ([more info](https://github.com/hadley/devtools/wiki/docs-function)):

    ```
    #' A short description of the function
    #'
    #' A more detailed description of the function
    #'
    #' @param x A description of parameter x's requirements
    #' @param y A description of parameter y's requirements
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
    #' library("rpckg")
    #' add(x, y)
    # insert your function below
    add <- function(x, y) {
        x + y
    }
    ```

2. Save each of your functions in the "R" subdirectory with a ``.R`` file extension, e.g. ``add.R``
3. Run the following commands in the pacakge's parent directory to build your ``NAMESPACE``, ``DESCRIPTION``, and man files.

    ```
    library("roxygen2")
    roxygenise(p)
    ```

4. Open the ``DESCRIPTION`` file and make sure it follows this template. Be careful to include all dependencies.

    ```
    Package: rpckg
    Title: A package that does someting in R
    Description: Explanation of what the package does broadly
    Version: 0.1
    Authors: name1 <name1@domain.come>, name2 <name2@domain.come>
    Maintainer: name3 <name3@domain.come>
    License: MIT
    Depends:
        R (>= 2.15.1)
        # your required packages would go here, separated by commas every line
    Suggests:
        # your suggested packages would go here in the same format. these are packages the library rarely uses or only uses for examples.
    LazyLoad: yes
    Collate:
        'add.R'
    ```

5. Make sure the functions in your ``NAMESAKE`` file matche the functions under ``Collate:`` within your ``DESCRPTION`` file, e.g:
    ```
    export(add.R)
    ```
6. Navigate to the pacakge's parent directory and run this in the command line. Make note of any error messages.
    ```
    $R CMD check rpckg
    ```
7. OPTIONAL: now build the package and push to cran

    ```
    $R CMD build rpckg
    $ftp -u ftp://cran.r-projects.org/incoming/ rpckg_0.1.targz
    ```

8. OR: locate the "hiR.Rcheck" folder in the package's parent directory, open this folder, move hiR-manual.pdf and hiR-ex.pdf into the "inst" folder, and overwite if they exist there already.
9. Now push these updates to git, e.g:

    ```
    $cd rpckg
    $cd git add .
    $git commit -m "my commit"
    $git push
    ```

10. Finally, reinstall the package using devtools::install_github:

    ```
    library("devtools")
    install_github("rpckg", "hinstitute")
    library("rpckg")
    help(package="rpckg")
    ```

_WARNING: if you install the package twice in the same R Session the manual will break. Simply restart R to fix this issue_

