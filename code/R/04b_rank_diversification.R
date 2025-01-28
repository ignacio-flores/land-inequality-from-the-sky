source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)
source("code/R/functions/compute_diversification_ratio.R")

#open original reference 
# library(foreign)
# gadm <- read.dbf(file.path(pathgadm, "gadm36_FRA_4.dbf")) %>% clean_names() %>%
#   dplyr::select(gid_4, name_4) %>% distinct() %>% 
#   group_by(gid_4) %>% mutate(n = row_number(), nmax = max(n))

# liste agreste-cadastre matched crops 
codc <- "georeferenced_data/cadastre/FR/metadata"
exclude_na <- c("4.13.2")
match_all <- read_csv(file = file.path(codc, "cadastre_to_agreste_match.csv"), show_col_types = F) %>% 
  filter(!(n306_mod %in% exclude_na))
match_cad_agr <- match_all %>% select(code_culture, n306_mod, n306_lib)

# Bind all agreste yearly production data 
filelist <- list.files("data/FDS_DEVELOPPE_2", pattern = ".csv", full.names = T)
alldata <- do.call(rbind, lapply(filelist, function(file) {
  read_delim(file, delim = ";", show_col_types = FALSE) %>% 
    janitor::clean_names()
}))
nat_subset <- alldata %>%
  filter(n027_lib != "Production (volume)",
         n027_lib != "Superficie développée", 
         frdom != "METRO", # region != "..." & dep == "..."
         frdom != "DOM") # %>% mutate(yearid = paste0(annref, "-", region))

#subset agreste data to matched crops and reshape 
db_abr <- match_all %>% select(n306_mod) %>% distinct() %>% 
  left_join(nat_subset, by = "n306_mod") %>% # multiple = "all" 
  select(n306_mod, valeur, annref)  %>% #yearid
  pivot_wider(names_from = n306_mod, values_from = valeur ) %>%
  filter (!is.na(annref)) %>% select(-annref)

# Get correlation and covariance matrices 
corr_matrixsna <- cor(na.omit(db_abr))
#corrplot(corr_matrixsna, method="color", order = "alphabet", t1.cex = 6)
cov_matrix <- cov(na.omit(db_abr))
print(cov_matrix)

# Get full cadastral data 
cadastre <- "data/FR/cadastre/temp/all_yearly_pa28.csv"
cadastre %<>% read_delim(show_col_types = FALSE) %>% 
  filter(type != "canton" & type != "farms") 
dcropvars <- grep("dcro", colnames(cadastre), value=TRUE)
cadcrop_n <- grep("crop", colnames(cadastre), value = TRUE)
cadcrop_cod <- grep("dcro", colnames(cadastre), value = TRUE)
cadastre <- cadastre %>% 
  select(type, gid_4, name_4, year, all_of(cadcrop_n), all_of(cadcrop_cod)) %>% 
  arrange(type, gid_4, year)

#harmonize cadastre nomenclature
codc_all <- read_delim(file = file.path(codc, "Codification_cultures_principales.csv"), 
            delim = ";", show_col_types = F, locale = locale(encoding = "Windows-1252")) %>% 
            clean_names() %>%
            mutate(code_culture = if_else(str_detect(code_culture, "[A-Z]+[0-9]"), 
                   str_replace_all(code_culture, "([A-Z]+)[0-9]", "\\1_"), code_culture),
                   libelle_culture = str_replace_all(libelle_culture, "20[0-9]{2}", "20XX")) 
# Function to standardize "libelle_culture" based on "code_culture" prefixes
standardize_libelle <- function(codc_all, code_prefix, new_description) {
  codc_all <- codc_all %>%
    mutate(libelle_culture = 
             if_else(
               str_starts(code_culture, code_prefix), 
               new_description, 
               libelle_culture
             )
          )
  return(codc_all)
}

# List of code prefixes and their new standardized descriptions
code_descriptions <- list(
  "FF_" = "Féverole fourragère pour la récolte 20XX",
  "JO_" = "Jachère officielle pour la récolte 20XX",
  "LH_" = "Luzerne hybride pour la récolte 20XX",
  "LO_" = "Luzerne ordinaire pour la récolte 20XX",
  "LP_" = "Lupin pour la récolte 20XX",
  "LU_" = "Luzerne pour la récolte 20XX",
  "MC_" = "Maïs consommation pour la récolte 20XX",
  "ME_" = "Maïs ensilage pour la récolte 20XX",
  "MH_" = "Maïs hybride pour la récolte 20XX",
  "MI_" = "Maïs industrie pour la récolte 20XX",
  "ML_" = "Mélilot pour la récolte 20XX",
  "PH_" = "Phacélie pour la récolte 20XX",
  "PP_" = "Pois protéagineux pour la récolte 20XX",
  "SA_" = "Sarrasin pour la récolte 20XX",
  "SE_" = "Seigle pour la récolte 20XX",
  "TR_" = "Trèfle pour la récolte 20XX",
  "VE_" = "Vesce pour la récolte 20XX"
)
# Apply the standardization for each code prefix
for (code_prefix in names(code_descriptions)) {
  codc_all <- standardize_libelle(codc_all, 
    code_prefix, code_descriptions[[code_prefix]]
  )
}
codc_all <- codc_all %>% 
  distinct(code_culture, libelle_culture, .keep_all = TRUE)

#run main function
tic("Computing diversification ratios: ")
  div_ratios <- map_dfr(1:nrow(cadastre), 
    ~compute_diversification_ratio(
      n = .x,
      data = cadastre, 
      piv = cadcrop_cod
    )
  )  
toc()

write.csv(div_ratios, file = file.path(codc, "diversification_ratios.csv"))  


