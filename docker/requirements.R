required_standard_packages <- c(
  "languageserver",
  "here",
  "janitor",
  "openxlsx",
  "tidyhydat",
  "zoo",
  "pacman",
  "magrittr",
  "dplyr",
  "duckdb",
  "plumber",
  "doparallel",
  "foreach",
  "plotly",
  "hydat",
  "doFuture",
  "doParallel",
  "furrr"
)

install.packages(required_standard_packages)
library(tidyhydat)
tryCatch(
  {
    download_hydat(dl_hydat_here = "/work", ask = FALSE)
  },
  error = function(e) {
    message(e$message)
    message("This is expected: the hydat database will be moved when the container is started.")
  }
)
