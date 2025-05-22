# ---------------------------------------------------------------------------- #
# Description: Cruncher et génération de bilan qualité -----------------------
# ---------------------------------------------------------------------------- #


## Chargement packages ---------------------------------------------------------

library("dplyr")
library("rjwsacruncher")
library("JDCruncheR")


## Options et paramètres -------------------------------------------------------

cruncher_bin_path <- file.path("C:", "Software",
                               "jwsacruncher-standalone-3.5.0-windows-x86_64",
                               "jwsacruncher-3.5.0", "bin")
cruncher_bin_path


options(
  cruncher_bin_directory = cruncher_bin_path,
  is_cruncher_v3 = TRUE,
  default_matrix_item = c(
    "span.start", "span.end", "span.n",
    "arima", "arima.mean", "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq",
    "m-statistics.m7", "m-statistics.q", "m-statistics.q-m2",
    "diagnostics.out-of-sample.mean:2", "diagnostics.out-of-sample.mse:2",
    "regression.nout",
    "residuals.kurtosis:3", "residuals.skewness:3", "residuals.lb2:3",
    "diagnostics.seas-sa-qs:2", "diagnostics.seas-sa-qs", 
    "diagnostics.seas-sa-f:2", 
    "diagnostics.seas-i-qs:2", "diagnostics.seas-i-qs",
    "diagnostics.seas-i-f:2",
    "diagnostics.td-sa-last:2", "diagnostics.td-i-last:2",
    "residuals.lb:3",
    "residuals.dh:3",
    "residuals.doornikhansen:3"
  )
)

getOption("default_matrix_item")

getOption("default_tsmatrix_series")
options(default_tsmatrix_series = c("s", "s_f", "sa", "sa_f"))

# attention: faire une copie avant de cruncher (ex: copie to repo cruncher)
# cruncher : mise à jour du workspace
getwd()
cruncher_and_param(
  workspace = "Workspaces/Cruncher/ws3.xml",
  rename_multi_documents = TRUE,
  delete_existing_file = TRUE,
  policy = "complete",
  csv_layout = "vtable",
  log_file = "Workspaces/Cruncher/cruncher.log")


## generation BQ 

demetra_ref_path <- "Workspaces/Cruncher/ws3/Output/SAProcessing-1/demetra_m.csv"

BQ_test <- demetra_ref_path |>
  extract_QR() |>
  compute_score(n_contrib_score = 3L) 

class(BQ_test)
str(BQ_test)

## Lecture pondérations --------------------------------------------------------

POND_NAF4 <- read.csv(file.path("Donnees", "Ponderations_2024.csv"),
                      encoding = "UTF-8", dec = ",")
colnames(POND_NAF4)
colnames(POND_NAF4)[1]<-"series"
colnames(POND_NAF4)


# Formule score 

# score_pond <- c(
#     qs_residual_sa_on_sa = 30L,
#     f_residual_sa_on_sa = 30L,
#     qs_residual_sa_on_i = 20L,
#     f_residual_sa_on_i = 20L,
#     f_residual_td_on_sa = 30L,
#     f_residual_td_on_i = 20L,
#     oos_mean = 15L,
#     residuals_homoskedasticity = 5L,
#     residuals_skewness = 5L,
#     m7 = 5L,
#     q_m2 = 5L
# )

BQ_ref <- demetra_ref_path |>
    extract_QR() |>
    compute_score(n_contrib_score = 3L) |>  # score_pond= formule
    add_indicator(POND_NAF4) |> 
  weighted_score("ponderation")

head(BQ_ref$values)

## Extraction score ------------------------------------------------------------

scores_ref<- extract_score(BQ_ref, weighted_score = FALSE)

scores_ref_w<- extract_score(BQ_ref, weighted_score = TRUE)



## Export ----------------------------------------------------------------------

export_xlsx(
    x = BQ_ref,
    file = file.path("BQ", "BQ_ref.xlsx"))

