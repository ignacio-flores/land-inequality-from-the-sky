#remotes::install_github("d3treeR/d3treeR")
#Setup 
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)
library(treemap)
library(d3treeR)

#load function 
source("code/R/functions/study_crop_composition.R")
selyear = 2021

#store cadastre labels in memory  
lbls <- file.path(pathcada, "metadata", "all_labels.xlsx")
lbls %<>% readxl::read_excel() %>% clean_names() %>%
  rename(lab_group = `libelle_groupe_culture`, 
         lab_crop  = `libelle_culture`,
         cod_crop  = `code_culture`) %>%
  dplyr::select(cod_crop, lab_crop, lab_group) 

#ignore year of plantation   
lbls %<>% 
  mutate(cod_crop = gsub("[[:digit:]]", "_", cod_crop),
          lab_crop = gsub("\\d{4}", "XXXX", lab_crop)) %>%
  distinct(.keep_all = TRUE)

#combine some categories 

#summarize microdata 
notfood <- c(11, 17, 18, 19)
natcomp <- 
  study_crop_composition(
    debug = FALSE , 
    gid4  = NULL, # c("FRA.1.1.1.2_1", "FRA.1.1.1.1_1")
    cropdrop = notfood, 
    t = selyear
  ) 

#label values 
natcomp %<>% 
  full_join(lbls, by="cod_crop") %>% 
  group_by(code_group) %>% 
  mutate(sumpct = sum(pct_farmland)) %>% 
  arrange(-sumpct, -pct_farmland) %>% 
  ungroup() %>%
  mutate(
     cumulative = cumsum(pct_farmland),
     lab_group2 = paste0(lab_group, " (", round(sumpct, 1), "%)"),
     lab_crop = tolower(gsub("\\s*[(-].*", "", lab_crop)),
     lab_crop2 = paste0(lab_crop, " (", round(pct_farmland, 1), "%)")
  )

#export data 
write_csv(natcomp, 
  file = paste0(pathmatch, "crop_composition", selyear, ".csv")
)

#count matches in agreste 
cad_to_agre <- read_csv(
  file.path(pathcada, "metadata", "cadastre_to_agreste_match.csv"),
  show_col_types = FALSE) %>% 
  rename(lab_crop = `original`, 
         lab_agre = `similar`)
cad_to_agre
natcomp %<>% full_join(cad_to_agre, by="lab_crop")


# basic treemap
p <- treemap(
   natcomp,
   index=c("lab_group2","lab_crop2", "lab_crop2"),
   vSize="pct_farmland",
   type="index",
   palette = "Set2",
   bg.labels=255,
   align.labels=list(
     c("left", "top"), 
     c("right", "bottom")
   )  
)
p

# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2(p,  rootname = "Grouped crops" )
inter


