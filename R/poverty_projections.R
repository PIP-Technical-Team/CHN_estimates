library(data.table)
library(ggplot2)
library(collapse)
# remotes::install_github("PIP-technical-team/pipapi@dev")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Set up   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_pipeline <-  "//w1wbgencifs01/pip/pip_ingestion_pipeline/pc_data/output-tfs-sync/ITSES-POVERTYSCORE-DATA"
lkups <- pipapi::create_versioned_lkups(data_pipeline)
v1 <- "20230626_2017_01_02_TEST"
df   <-
  lapply(c(2.15, 3.65, 6.85),
         \(.x) {
           pipapi::pip (country = "CHN",
                        fill_gaps = FALSE,
                        povline = .x,
                        lkup = lkups$versions_paths[[v1]])
         }) |>
  rbindlist()

haven::write_dta(df, fs::path(tdirp, "chn_proj.dta"))
