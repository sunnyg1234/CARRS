# Set up workspace {{{ ====

source("../code/1_libraries.R")
source("../code/2a_intake-similar-columns.R")
source("../code/2b_intake-different-columns.R")
source("../code/2c_intake-merge.R")
source("../code/3_tidy.R")

# }}}


x <- names(cohort2_data)

grep("hdl", x, perl = TRUE, value = TRUE)
str_extract("hdl", x)
