library(data.table)
library(wbpip)
library(purrr)
library(glue)
library(ggplot2)

source("R/functions.R")
source("R/utils.R")
source("R/list_group_data.R")



lt_welfare <-
  get_elements(vctrs, "welfare") |> # get coefficients
  remove_null() |> # remove null or NA
  flatten_recurse()  # flatten the nested list

lt_population <-
  get_elements(vctrs, "population") |> # get coefficients
  remove_null() |> # remove null or NA
  flatten_recurse()  # flatten the nested list



lt_synth_dist <-
  map2(.x = lt_welfare,
       .y = lt_population,
       .f = \(.x, .y) {
         wbpip:::sd_create_synth_vector(
           welfare    = .x,
           population = .y,
           mean       = 1,
           pop        = 1,
           nobs =     1e+5
         )
       })

save_dir <-
  tdirp |>
  fs::path("synth_CHN_distr") |>
  fs::dir_create()

pattern <- "([[:alpha:]]+_[[:digit:]]+)(_[[:alpha:]]+_)([[:alpha:]]+)"
purrr::iwalk(lt_synth_dist,
             \(x, idx) {
               nfile <- gsub(pattern, "\\1_\\3", idx)
               haven::write_dta(x,
                                path = fs::path(save_dir,
                                                idx,
                                                ext = "dta"))
             })


