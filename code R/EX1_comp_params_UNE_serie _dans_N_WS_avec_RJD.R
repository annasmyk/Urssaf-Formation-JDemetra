
################################################################################
##############    Récupérer une série dans plusieurs workspaces    #############
################################################################################

# Présentation -----------------------------------------------------------------

# But : Comparer les composantes d'UNE série dans plusieurs workspaces
# --> voir l'impact de différents choix de paramètres
# --> par exemple : changement outliers, filtres....


# Plan du programme :
#   1) Initialisation de paramètres généraux et import des packages
#   2) Préciser les chemins des WS et les variables "user-defined" (pas présentes dans l'output par défaut)
#   3) Extraire toutes les infos concernant la série (à expertiser) de chaque WS et stocker dans une liste
#   4) Merger les listes
#   5) Ecrire les données dans des fichiers excel
#   6) Création de graphiques


# en sortie de RJDemetra les séries sont des objets TS: on transforme tout en data frames
#   avec colonne date
# on aurait pu tout laisser en TS 


### Export presse papier 

# Initialisation des paramètres ------------------------------------------------

options(stringAsFactors = FALSE)

# Import des packages ----------------------------------------------------------

# Si RJDemetra n'est pas installé sur votre poste, 
# faire tourner la ligne suivante :

# install.packages("RJDemetra", type = "source", 
#                  INSTALL_opts = "--no-multiarch" )

library("RJDemetra")

library("magrittr")
library("dplyr")
library("xlsx")
library("ggplot2")
library("zoo")


# Personnalisation des chemins et des séries -----------------------------------

# Série à expertiser (même nom que dans les workspaces)
serie_a_exp <- "RF1073"

# W1: Workspace auquel on compare (automatique par exemple) 
ch_1 <- "./WS/industrie_old.xml" # series de l'ipi 

# Workspace modifié post expertise manuelle 
ch_2 <- "./WS/industrie.xml"

# Création d'une table pour les chemins et noms des workspaces
# name : méthode de refreshment policy
# path : liste chemins ws
# id : liste noms WS permettant d'identifier les WS
#   --> pour suffixer les variables et identifier le WS
# Mettre les noms / id dans le même ordre que les ws

summary_ws <- data.frame(
    num_ws = 1:2,
    name = c("auto", "expertise"),
    path = c(ch_1,ch_2),
    id = c("old", "new")
)

summary_ws


# Paramètres de X13 ------------------------------------------------------------

# Ici on définit un exemple de "user-defined" output  (séries pas présentes dans l'output par défaut)
# On ajoute :
#   - "cal" + "full residuals" = residus du modele REG-Arima
#   - "d10" = coefficient saisonnier hors effet calendrier 
#       (d10a pour la prevision)
#   - "b1" = ylin, 
#   - "c20" = poids finaux X11, 
#   - "d1" brute corrigée finale dans X11
#   - et les series prévues ("y_f", "s_f", "d10a", ...)

# Liste choix series et diagnostics possibles
user_defined_variables("X13-ARIMA") 

# Indices des variables choisies
num_out <- c(2, 4, 6, 8, 10, 28, 35, 36, 67, 134, 173, 174, 183, 184)
v_out <- user_defined_variables("X13-ARIMA")[num_out]
v_out


# Fonctions auxiliaires --------------------------------------------------------

# Calcul d'un taux de croissance
tx_cr <- function(v) {
    if ("ts" %in% class(v)) {
        w <- (v - stats::lag(v, -1)) / stats::lag(v, -1) * 100
    } else {
        w <- (v - dplyr::lag(v)) / dplyr::lag(v) * 100
    }
    return(w)
}


# Calcul d'un glissement annuel
tx_12 <- function(v) {
    w <- (v - lag(v, 12)) / lag(v, 12) * 100
    return(w)
}


# Recherches dans les WS -------------------------------------------------------

# Récupération des composantes de la SERIE à expertiser dans chacun des WS de la liste

list_ws <- list()

for (i in summary_ws$num_ws) {
    
    print(paste("WS n°", i))
    # chemins WS
    ch <- summary_ws[i, "path"]
    print(paste("Le chemin est ", ch))
    
    
    ## Chargement du WorkSpace -------------------------------------------------
    
    ws <- RJDemetra::load_workspace(ch)
    RJDemetra::compute(ws)
    serie_ipi <- get_object(ws, 1)
    series <- get_all_objects(serie_ipi)
    # On choisit une série (ici serie_a_exp)
    sa <- get_object(serie_ipi, which(names(series) == serie_a_exp))
    model_sa <- get_model(sa, ws, userdefined = v_out)
    
    
    ## Séries principales ------------------------------------------------------
    
    print(paste("Séries principales du WorkSpace", summary_ws[i, "name"]))
    # Récupération des séries principales sous forme de ts
    main_series_ts <- model_sa$final$series
    head(main_series_ts)
    
    # Passage en data.frame
    main_series_df <- cbind(date = zoo::as.Date(time(main_series_ts)),
                            as.data.frame(main_series_ts))
    head(main_series_df)
    
    # afficher la dernière obs realisée
    last_obs <- tail(main_series_df$date, n = 1)
    print(paste("Dernier point (non prevu) du ws :", last_obs))
    
    
    ## Série du preprocessing --------------------------------------------------
    
    print(paste("Séries du preprocessing du WorkSpace", summary_ws[i, "name"]))
    pre_processing_ts <- model_sa$regarima$model$effects
    pre_processing_df <- cbind(date = zoo::as.Date(time(pre_processing_ts)),
                               as.data.frame(pre_processing_ts))
    head(pre_processing_df)
    
    # On enleve effet de Paques et fêtes mobiles
    pre_processing_df <- subset(pre_processing_df, select = -c(ee, omhe))
    head(pre_processing_df)
    
    
    ## Output user defined -----------------------------------------------------
    
    print(paste("Séries user-defined du WorkSpace", summary_ws[i, "name"]))
    # Changement des noms de colonne
    names(model_sa$user_defined) <- names(model_sa$user_defined) %>%
        gsub(pattern = "preprocessing.model.", replacement = "") %>%
        gsub(pattern = "decomposition.", replacement = "") %>%
        gsub(pattern = "fullresiduals", replacement = "full_res")
    
    # Sélection des séries de prévision : series "_f" dans la liste + d10a
    index_prev <- unique(c(grep("_f", names(model_sa$user_defined)),
                           grep("d10a", names(model_sa$user_defined))))
    
    print("Index des séries de prévision :")
    print(index_prev)
    
    # Récupération et aggrégation des séries
    prev_series_ts <- do.call(cbind, model_sa$user_defined[index_prev])
    
    # Changement en data.frame
    prev_series_df <- cbind(date = zoo::as.Date(time(prev_series_ts)),
                            as.data.frame(prev_series_ts))
    print("Séries de prévision :")
    print(prev_series_df)
    
    # Afficher la derniere obs realisée
    last_obs_prev <- tail(prev_series_df$date, n = 1)
    print(paste("Dernier point prevu du ws :", last_obs_prev))
    
    
    ## Ajustement schéma modèle ------------------------------------------------
    
    # AJUSTEMENT y_lin : prendre l exponenetielle en cas de modele multiplicatif
    if (model_sa$regarima$model$spec_rslt$`Log transformation`) {
        # Schéma multiplicatif
        pre_processing_df$y_lin <- exp(pre_processing_df$y_lin)
        prev_series_df$y_lin_f <- exp(prev_series_df$y_lin_f)
    }
    # Schéma additif --> ne rien faire
    
    head(pre_processing_df)
    print(prev_series_df)
    
    
    ## Séries complémentaires --------------------------------------------------
    
    print(paste("Séries complémentaires du WorkSpace", summary_ws[i, "name"]))
    # Ce sont les séries NON STOCKEES précédemment dans la liste :
    # cal, fullresiduals = fr, b1, c20, d1, d10
    index_comp <- setdiff(seq_len(length(model_sa$user_defined)), index_prev)
    print("Index des séries de complémentaire :")
    print(index_comp)
    
    comp_series_ts <- do.call(cbind, model_sa$user_defined[index_comp])
    comp_series_df <- cbind(date = zoo::as.Date(time(comp_series_ts)),
                            as.data.frame(comp_series_ts))
    print("Début de série :")
    head(comp_series_df)
    print("Fin de série :")
    tail(comp_series_df)
    
    
    ## Réunion des tables ------------------------------------------------------
    
    # suffixe = id du ws avec _
    suffixe <- paste0("_", summary_ws[i, "id"])
    print(suffixe)
    
    # merge REALISEES df = main + pre proc + comp
    print(paste("Séries réalisées du WorkSpace", summary_ws[i, "name"]))
    realises_df <- main_series_df %>%
        merge(comp_series_df, by = "date", all = TRUE) %>%
        merge(pre_processing_df, by = "date", all = TRUE)
    colnames(realises_df) <- c(
        "date", paste0(colnames(realises_df)[-1], suffixe))
    print("Merge séries main + comp + prepro :")
    head(realises_df)
    
    # Réunion des séries réalisées et des prévisions associées
    
    # Séries réalisées
    temp_main_series <- main_series_df[, c("date", "y", "sa",
                                           "t", "s", "i")] %>%
        merge(comp_series_df[, c("date", "cal")], by = "date",
              all.x = TRUE, all.y = FALSE) %>%
        merge(pre_processing_df[, c("date", "y_lin")], by = "date",
              all.x = TRUE, all.y = FALSE)
    # Prévisions des séries
    temp_prev_series <- prev_series_df[, c("date", 
                                           "y_f", "sa_f", "t_f", "s_f",
                                           "i_f", "cal_f", "y_lin_f")]
    colnames(temp_prev_series) <- colnames(temp_main_series)
    extended_series_df <- rbind(
        temp_main_series,
        temp_prev_series
    )
    
    # Calcul des taux de croissance / évolution des séries y et sa
    ev_extended_series <- apply(
        X = subset(extended_series_df, select = c(y, sa)),
        MARGIN = 2, FUN = tx_cr)
    colnames(ev_extended_series) <- paste0("ev_", colnames(ev_extended_series))
    # Regroupement des séries
    extended_series_df <- cbind(extended_series_df, ev_extended_series)
    
    colnames(extended_series_df) <- c(
        "date", paste0(colnames(extended_series_df)[-1], suffixe))
    print("Réunion des séries réalisées et des prévisions associées :")
    head(extended_series_df)
    tail(extended_series_df)
    
    # Séries prévues
    
    colnames(prev_series_df) <- c(
        "date", paste0(colnames(prev_series_df)[-1], suffixe))
    
    ## Outliers ----------------------------------------------------------------
    
    # Outliers par WorkSpace
    outliers_df <- as.data.frame(model_sa$regarima$regression.coefficients)
    # Regresseurs cjo
    regs_cjo_ts <- model_sa$regarima$specification$regression$userdef$variables$serie
    regs_cjo_df <- cbind(date = zoo::as.Date(time(regs_cjo_ts)), 
                         as.data.frame(regs_cjo_ts))
    
    
    # Réunions des différents WS -----------------------------------------------
    
    # pour chaque WS de la boucle, on récupère les tables :
    #   - realises_df
    #   - prev_series_df
    #   - extended_series_df
    #   - outliers_df
    #   - regs_cjo_df
    
    objets_ws <- list(rea = realises_df, 
                      prev = prev_series_df, 
                      extended = extended_series_df, 
                      outliers = outliers_df, 
                      regs_cjo = regs_cjo_df, 
                      modele = model_sa)
    
    list_ws[[summary_ws[i, "id"]]] <- objets_ws
}

### Fin Boucle lecteure stockage par WS

# ici regarder list_ws$old 
#list_ws$new$rea

# Regroupement des tables ------------------------------------------------------
## pourrait être paramétré en fonction des suffixes utilisés au début 



# Les séries réalisées
realises_ws_df <- list_ws$old$rea %>% 
    merge(y = list_ws$new$rea, by = "date", all = TRUE)


# Les séries de forecast (prévision)
prevision_ws_df <- list_ws$old$prev %>% 
    merge(list_ws$new$prev, by = "date", all = TRUE) 

# Les séries étendues (réalisées puis forecast)
extended_ws_df <- list_ws$old$extended %>% 
    merge(list_ws$new$extended, by = "date", all = TRUE)

tail(extended_series_df)

# L'ensemble des séries réalisées et forecast (mais sans etre prolongées)
total_ws_df <- merge(x = realises_ws_df, y = prevision_ws_df, 
                     by = "date", all = TRUE)
head(total_ws_df)
tail(total_ws_df)

# Export des données sous Excel ------------------------------------------------

# Créé le dossier output
if (!file.exists("output")) {
    dir.create(path = "./output/")
}

#### Export rapide : dernières valeurs et graphiques 

# 1 les N denieres annees 
df_s<-extended_ws_df[extended_ws_df$date>="2020-01-01",]
## ordonner variables (confort visuel)
df_v <- df_s |> select(date,y_old,sa_new, sa_old, s_new,s_old, y_lin_old,y_lin_old,
                    ev_y_old,ev_sa_old,ev_sa_new)

df_v1<-df_v[,-1] #on enleve 1ere ligne redondante + format numerique 
# transposée (confort visuel ?)
t_df_v<-as.data.frame(t(df_v1))
str(t_df_v)
# on renomme les colonnes 
colnames(t_df_v)<-df_v$date
View(t_df_v)

ch_out1 <-paste0("./output/",serie_a_exp,"_old_vs_new.xlsx")
ch_out1
write.xlsx(t_df_v,ch_out1)

## graphiques 

## Graphique des taux de croissance --------------------------------------------

data_to_plot <- extended_ws_df[, c("date", paste0("ev_sa_", summary_ws$id))] %>%
    subset(date >= "2020-01-01")
tail(data_to_plot)

data_to_plot <- data_to_plot %>% 
    tidyr::pivot_longer(col = !date, names_to = "WS", 
                        values_to = "ev", names_prefix = "ev_") %>%
    dplyr::group_by(WS) %>% 
    subset(!is.na(ev))

ggplot(data_to_plot, aes(x = date, y = ev, colour = WS)) +
    ggtitle(paste("Série", serie_a_exp,": taux de croissance")) +
    geom_line(size = 1) +
    scale_x_date(name = "Année", 
                 date_breaks = "1 year", 
                 date_labels = "%Y") +
    scale_y_continuous(name = serie_a_exp)

#####################################################################


# 
# 
# ################# autres exemples d'exports 
# 
# ## Export : un fichier par WS ------------------------------------------
# 
# for (i in summary_ws$num_ws) {
#     file_path <- paste0("./output/", summary_ws$name[i], "_", 
#                         serie_a_exp, ".xlsx")
#     write.xlsx(x = list_ws[[i]]$rea, file = file_path, 
#                sheetName = "Séries réalisées", row.names = FALSE)
#     write.xlsx(x = list_ws[[i]]$prev, file = file_path, 
#                sheetName = "Séries prévues", row.names = FALSE)
#     write.xlsx(x = list_ws[[i]]$extended, file = file_path, 
#                sheetName = "Séries réalisées étendues", row.names = FALSE)
#     if (nrow(list_ws[[i]]$outliers) > 0) {
#         write.xlsx(x = list_ws[[i]]$outliers, file = file_path, 
#                    sheetName = "Outliers", row.names = FALSE)
#     }
#     write.xlsx(x = list_ws[[i]]$regs_cjo, file = file_path, 
#                sheetName = "Régresseurs cjo", row.names = FALSE)
# }
# 
# ## Deuxième export : un onglet par type de données -----------
# 
# ### Séries réalisées -----------------------------------------------------------
# 
# write.xlsx(x = realises_ws_df, 
#            file = paste0("./output/Comparaison_ws_", serie_a_exp, ".xlsx"), 
#            sheetName = "Séries réalisées", row.names = FALSE)
# 
# ### Séries prévues -------------------------------------------------------------
# 
# write.xlsx(x = prevision_ws_df, 
#            file = paste0("./output/Comparaison_ws_", serie_a_exp, ".xlsx"), 
#            sheetName = "Séries prévues", row.names = FALSE)
# 
# ### Séries réalisées prolongées ---------------------------------------------
# 
# write.xlsx(x = extended_ws_df, 
#            file = paste0("./output/Comparaison_ws_", serie_a_exp, ".xlsx"), 
#            sheetName = "Séries réalisées étendues", row.names = FALSE)
# 
# 
# # Visualiser des séries ---------------------------------------------------------
# 
# ## Graphique des séries ajustées -----------------------------------------------
# 
# data_to_plot <- extended_ws_df[, c("date", paste0("sa_", summary_ws$id))] %>% 
#     subset(date >= "2019-01-01")
# tail(data_to_plot)
# 
# data_to_plot <- data_to_plot %>% 
#     tidyr::pivot_longer(col = !date, names_to = "WS", 
#                         values_to = "sa", names_prefix = "sa_") %>% 
#     dplyr::group_by(WS) %>% 
#     subset(!is.na(sa))
# 
# ggplot(data_to_plot, aes(x = date, y = sa, colour = WS)) +
#     ggtitle(paste("Série", serie_a_exp)) +
#     geom_line(size = 1) +
#     scale_x_date(name = "Année", 
#                  date_breaks = "1 year", 
#                  date_labels = "%Y") +
#     scale_y_continuous(name = serie_a_exp)
# 
# ## Graphique des taux de croissance --------------------------------------------
# 
# data_to_plot <- extended_ws_df[, c("date", paste0("ev_sa_", summary_ws$id))] %>%
#     subset(date >= "2020-01-01")
# tail(data_to_plot)
# 
# data_to_plot <- data_to_plot %>% 
#     tidyr::pivot_longer(col = !date, names_to = "WS", 
#                         values_to = "ev", names_prefix = "ev_") %>%
#     dplyr::group_by(WS) %>% 
#     subset(!is.na(ev))
# 
# ggplot(data_to_plot, aes(x = date, y = ev, colour = WS)) +
#     ggtitle(paste("Série", serie_a_exp)) +
#     geom_line(size = 1) +
#     scale_x_date(name = "Année", 
#                  date_breaks = "1 year", 
#                  date_labels = "%Y") +
#     scale_y_continuous(name = serie_a_exp)
# 
# 
# ## Graphique des prévisions ----------------------------------------------------
# # en pointillés 
# # séries sa et sa_f
# 
# data_to_plot <- total_ws_df[, c("date", paste0("sa_", summary_ws$id), 
#                                 paste0("sa_f_", summary_ws$id))] %>% 
#     subset(date >= "2020-01-01")
# 
# head(data_to_plot)
# tail(data_to_plot)
# 
# data_to_plot <- data_to_plot %>% 
#     tidyr::pivot_longer(col = !date, names_to = "WS", 
#                         values_to = "sa", names_prefix = "sa_") %>% 
#     tidyr::separate(col = WS, sep = "_", 
#                     into = c("serie", "WS"), fill = "left") %>% 
#     dplyr::mutate( 
#         serie = dplyr::case_when(
#             serie == "f" ~ "Prévue", 
#             TRUE ~ "Réalisé")
#     ) %>%  
#     subset(!is.na(sa))
# 
# ggplot(data_to_plot, aes(x = date, y = sa, colour = WS, linetype = serie)) +
#     ggtitle(paste("Série", serie_a_exp)) +
#     geom_line(size = 1) +
#     scale_linetype_manual(values = c("dashed", "solid")) +
#     scale_x_date(name = "Année", 
#                  date_breaks = "1 year", 
#                  date_labels = "%Y") +
#     scale_y_continuous(name = serie_a_exp)
# 
# 
# ## Graphique du pré-ajustement -------------------------------------------------
# 
# ### Graphique des y linéarisées ------------------------------------------------
# 
# data_to_plot <- extended_ws_df[, c("date", paste0("y_lin_", summary_ws$id))] %>%
#     subset(date >= "2020-01-01")
# tail(data_to_plot)
# 
# data_to_plot <- data_to_plot %>% 
#     tidyr::pivot_longer(col = !date, names_to = "WS", 
#                         values_to = "lin", names_prefix = "y_lin_") %>% 
#     dplyr::group_by(WS) %>% 
#     subset(!is.na(lin))
# 
# ggplot(data_to_plot, aes(x = date, y = lin, colour = WS)) +
#     ggtitle(paste("Série", serie_a_exp)) +
#     geom_line(size = 1) + 
#     geom_vline(xintercept = c(as.Date("2021-09-01"), as.Date("2022-01-01")), 
#                linetype = "dashed", colour = c("blue", "red")) + 
#     scale_x_date(name = "Année", 
#                  date_breaks = "1 year", 
#                  date_labels = "%Y") +
#     scale_y_continuous(name = serie_a_exp)
# 
