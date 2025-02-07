#### A history of what i runned in the console to build the package


<<<<<<< HEAD
## What document to ignore in the package (this one because it is just to keep track of what ive done)
=======
## What document to ignore in the package (this one beacause it is just to keep track of what ive done)
>>>>>>> 66e447b856e70a8d82a3bf1bb1e8575f9cab916e
usethis::use_build_ignore("devtools_history.R")


## What package we load as necessary in our package (used in our functions)
usethis::use_package("dplyr")
usethis::use_package("plyr")
usethis::use_package("magrittr")
usethis::use_package("stats")
usethis::use_package("graphics")


## Generate documentations for each function (based on the roxygen skeleton created on top of each function)
devtools::document()
