# ---------------------------------------------------------------------------- #
# Description: Cruncher et génération de bilan qualité -----------------------
# ---------------------------------------------------------------------------- #


## Chargement packages ---------------------------------------------------------

library("dplyr")
library("rjwsacruncher")
library("JDCruncheR")
library("rjd3workspace")

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

getwd()


# attention: faire une copie avant de cruncher (copie to repo cruncher)
# mettre à jour les chemins vers les données brutes 



ws_ref_xml_path <- file.path("Workspaces","Cruncher","workspace_ref", "industrie.xml")
ws_ref_xml_path
ws_ref_path <- tools::file_path_sans_ext(ws_ref_xml_path)
ws_ref_path
demetra_ref_path <- file.path(ws_ref_path, "Output", "industrie",
                              "demetra_m.csv")
demetra_ref_path

ws_auto_xml_path <- file.path("Workspaces","Cruncher", "workspace_automatique", "industrie.xml")
ws_auto_xml_path
ws_auto_path <- tools::file_path_sans_ext(ws_auto_xml_path)
ws_auto_path 
demetra_auto_path <- file.path(ws_auto_path, "Output", "industrie",
                               "demetra_m.csv")
demetra_auto_path


# Chemin pour rendre les ws  ref et auto crunchable 

donnees <- file.path("Donnees", "IPI_nace4.csv")|>
  normalizePath()

donnees

# update path 
file <- ws_ref_xml_path
ws_ref <- jws_open(file)
ws_sap_count(ws_ref)
txt_update_path(
  jws = ws_ref,
  new_path = donnees,
  idx_sap = 1)

save_workspace(ws_ref, ws_ref_xml_path, replace=TRUE)

file <- ws_auto_xml_path
ws_auto <- jws_open(file)
txt_update_path(
  jws = ws_auto,
  new_path = donnees,
  idx_sap = 1L
)

read_workspace(ws_auto,compute=TRUE)

save_workspace(ws_auto, ws_auto_xml_path, replace=TRUE)



## Lecture pondérations --------------------------------------------------------

POND_NAF4 <- read.csv(file.path("Donnees", "Ponderations_2024.csv"),
                      encoding = "UTF-8", dec = ",")
colnames(POND_NAF4)
colnames(POND_NAF4)[1]<-"series"
colnames(POND_NAF4)


## Appel du cruncher -----------------------------------------------------------
# WS ref
cruncher_and_param(
    workspace = ws_ref_xml_path,
    rename_multi_documents = TRUE,
    delete_existing_file = TRUE,
    policy = "lastoutliers",
    csv_layout = "vtable",
    log_file = file.path("Workspaces","Cruncher","workspace_ref", "output_cruncher.log")
)


# WS automatique
cruncher_and_param(
    workspace = ws_auto_xml_path,
    rename_multi_documents = TRUE,
    delete_existing_file = TRUE,
    policy = "complete",
    csv_layout = "vtable",
    log_file = file.path("Workspaces","Cruncher","workspace_automatique", "output_cruncher.log")
)


## Generation BQ ---------------------------------------------------------------

score_pond <- c(
    qs_residual_sa_on_sa = 30L,
    f_residual_sa_on_sa = 30L,
    qs_residual_sa_on_i = 20L,
    f_residual_sa_on_i = 20L,
    f_residual_td_on_sa = 30L,
    f_residual_td_on_i = 20L,
    oos_mean = 15L,
    residuals_homoskedasticity = 5L,
    residuals_skewness = 5L,
    m7 = 120L, ###############
    q_m2 = 2L
)

BQ_ref <- demetra_ref_path |>
    extract_QR() |>
    compute_score(n_contrib_score = 3L, score_pond = score_pond) |>
    add_indicator(POND_NAF4)|>
    weighted_score("ponderation")

BQ_auto <- demetra_auto_path |>
    extract_QR() |>
    compute_score(n_contrib_score = 3L, score_pond = score_pond) |>
    add_indicator(POND_NAF4) |>
    weighted_score("ponderation")



## Extraction score ------------------------------------------------------------

scores_ref <- extract_score(BQ_ref, weighted_score = FALSE) |>
    rename(score_ref = score)
scores_auto <- extract_score(BQ_auto, weighted_score = FALSE) |>
    rename(score_auto = score)

scores <- merge(scores_ref, scores_auto, by = "series", all = TRUE)

cat(
    "Il y a",
    sum(scores[["score_auto"]] < scores[["score_ref"]], na.rm = TRUE),
    "séries mieux ajustées par le WS automatique que par le WS ref.\n",
    "Il y a",
    sum(scores[["score_ref"]] < scores[["score_auto"]], na.rm = TRUE),
    "séries mieux ajustées par le WS ref que par le WS automatique.\n"
)


## Export ----------------------------------------------------------------------

write.table(
    x = scores,
    file = file.path("BQ", "score_ref_auto.csv"),
    quote = FALSE,
    sep = ";",
    row.names = FALSE,
    dec = ".",
    na = ""
)

export_xlsx(
    x = BQ_ref,
    file = file.path("BQ", "BQ_ref.xlsx")
)
export_xlsx(
    x = BQ_auto,
    file = file.path("BQ", "BQ_auto.xlsx")
)

