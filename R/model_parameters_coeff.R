
source("R/model_parameters.R")
## get Coeff ------

lt_coefs <-
  get_elements(gd, "coef") |> # get coefficients
  remove_null() |> # remove null or NA
  flatten_recurse()  # flatten the nested list

dt_coefs <-
  lt_coefs |>
  # convert atomic named vectors to data.tables
  map(\(x){
    as.list(x) |>
      as.data.table()
  }) |>
  # Append
  rbindlist()


vars <-
  c(
    "country_code",
    "year",
    "welfare_type",
    "reporting_level",
    "poverty_line",
    "lorenz_type"
  )

dt_coefs[, id := names(lt_coefs)
  ][,
    (vars) := tstrsplit(id,
                        split = "_",
                        keep = 1:length(vars))
  ][, id := NULL
  ]


data.table::setorderv(dt_coefs, vars)
data.table::setcolorder(dt_coefs, vars)

### Save -----
haven::write_dta(dt_coefs, "data/coeff.dta")

### plot =====

dt_coefs[lorenz_type == "lb" &
           poverty_line == 2.15 &
           year %in% c(2000, 2010, 2015, 2019, 2020)
] |>
  melt(id.vars = c("year",
                   "welfare_type",
                   "reporting_level"),
       measure.vars = c("A", "B", "C"),
       variable.name = "coef") |>
  ggplot(aes(x = year, y = value, group = coef)) +
  geom_line(aes(colour = coef)) +
  theme_minimal() +
  facet_wrap(vars(reporting_level), nrow = 2)


