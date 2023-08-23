library(data.table)
library(wbpip)
library(purrr)
library(glue)
library(ggplot2)

source("R/functions.R")
source("R/utils.R")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             Initial parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nq       <- 100
lorenz   <- NULL
popshare <- seq(from = 1/nq, to = 1, by = 1/nq)
version  <- "20230328_2017_01_02_PROD"
version  <- "20230626_2017_01_02_TEST"
# ppp_year <- py <- 2011
ppp_year <- py <- version |>
  gsub("(.*)_([0-9]{4})(_.*)", "\\2", x = _) |>
  as.numeric()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux <- pipfun::pip_merge_aux(c("cpi", "ppp", "gdm"))

pfw <- pipload::pip_load_aux("pfw")
pfw <- pfw[country_code == "CHN"]

aux_id <- attr(aux, "id")
pfw_id <- pipfun::aux_ids("pfw")$pfw

byv <- intersect(aux_id, pfw_id)

# filter according to pfw
dt <- merge(aux, pfw, byv)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# deflate mean   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dt[, mean_ppp :=  {
  x <- deflate_welfare_mean(survey_mean_lcu,
                            ppp,
                            cpi)
  x <- x/(365/12)
}
][,
  id := paste(country_code, surveyid_year, welfare_type, sep = "_")
]

mean_ppp <-
  dt[!is.na(mean_ppp)
  ][,  # keep no na data
    # keep important variables
    c("id", "mean_ppp", "data_level")
  ] |>
  # create list of means and reporting level by id
  split(by = "id",
        keep.by = FALSE) |>
  # convert data.table into vectors of means with reporting levels as names
  map(~{
    y        <- .x[, mean_ppp]
    names(y) <- .x[, data_level]
    attr(y,"label") <- NULL
    y
  })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population and welfare vctrs for Group data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fpf <- pfw[, .(country_code,
               year,
               surveyid_year,
               reporting_year,
               survey_year,
               pop_domain,
               welfare_type)]

fpf[, `:=`(
  id = paste(country_code, surveyid_year, welfare_type, sep = "_"),
  version = version
)
]

# fpf <- fpf[1:5]
# lf <- as.list(fpf)
pl <- split(fpf, by = "id")

vctrs <- map(pl, gd_pop_wlf)
names(vctrs) <- fpf[, id]

## Model parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## init parameters -----
resp <- vctrs
vctrs <- resp

# vctrs <- vctrs[1]
povlines   <- seq(from = 1,
                  to   = 4,
                  by   = .1)
# povlines <- 2.15
versions <- names(vctrs)

## regression -----
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

