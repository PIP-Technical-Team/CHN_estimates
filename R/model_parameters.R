library(data.table)
library(wbpip)
library(purrr)
library(glue)
library(ggplot2)

source("R/functions.R")
source("R/utils.R")
source("R/list_group_data.R")

# regression -----
gd <-
  # Year level
  map(vctrs,
      ~{
        reporting_levels <- names(.x)
        # reporting level
        lb <-
          map(.x,
            ~{
              # poverty line level
              welfare    <- .x$welfare
              population <- .x$population

              la <-
                map(povlines,
                  ~{
                    get_gd_select_lorenz(welfare,
                                         population,
                                         povline = .x,
                                         complete = TRUE)
                  })
              names(la) <- povlines
              la
            })
        names(lb) <- reporting_levels
        lb
      })
names(gd) <- versions

