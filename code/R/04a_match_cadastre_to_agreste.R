#https://agreste.agriculture.gouv.fr/agreste-saiku/?plugin=true&query=query/open/SAANR_DEVELOPPE_2#query/open/SAANR_DEVELOPPE_2
#metadata: https://agreste.agriculture.gouv.fr/agreste-web/methodon/S-SAA/methodon/
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)

#load functions 
source("code/R/functions/sum_agreste_y.R")
source("code/R/functions/clean_strings.R")
source("code/R/functions/match_strings.R")

#list raw files 
mdir <- "data/FDS_DEVELOPPE_2"
codc <- "georeferenced_data/cadastre/FR/metadata"
flist <- list.files(mdir, full.names = TRUE, pattern =".csv")

#bring cadastral info 
cod_cadas <- readxl::read_xlsx(paste0(codc, "/all_labels.xlsx")) %>%
  clean_names() %>% mutate(libelle_culture = tolower(libelle_culture))

#clean agreste data and put in panel 
big_panel <- map_dfr(1:length(flist), ~sum_agreste_y(t = .x)) 
big_panel %<>% mutate(n306_lib = tolower(n306_lib))

#get unique combinations 
uniq_comb_cadast <- cod_cadas %>% select(code_culture, libelle_culture, libelle_groupe_culture, code_groupe_culture)
uniq_comb_agrest <- big_panel %>% select(n306_lib, n306_mod) %>% distinct()
  
#list levels of aggregation (not used anymore)
for(x in 0:5) {
  temp <- filter(big_panel, level == x)
  lgt <- unique(temp$n306_lib)
  print(paste0("..............Level " , x, ", length: ", length(lgt), " ............."))
  print(head(unique(temp$n306_lib), n=10))
}

#match categories
list_pan <- unique(big_panel$n306_lib) 
list_cad <- unique(cod_cadas$libelle_culture) 
try <- match_strings(list1 = list_cad, list2 = list_pan, nr=5)
try %<>% arrange(distance, original)

#separate approved
approved <- filter(try, distance==0 | distance==1) 
head(approved)

gp <- filter(try, distance==6) 
head(gp, n=10)

col <- c("épinard", "lavande / lavandin", "oignon / échalote", 
  "poivron / piment", "pomme de terre féculière", "	blé dur d’hiver",
  "canne à sucre - autre", "lupin doux d’hiver", "triticale d’hiver",
  "poivron / piment", "canne à sucre - fermage", "radis fourrager")
try %<>% arrange(similar)
approved <-  bind_rows(approved, filter(try, original %in% col)) %>% 
  arrange(original, distance)
approved %<>%
  distinct(.keep_all = TRUE) %>%
  group_by(original) %>% mutate(rank = row_number())

approved <- approved[approved$original!="canne à sucre - autre" | (approved$original=="canne à sucre - autre" & approved$similar=="canne à sucre") & approved$rank==1,]
approved <- approved[approved$original!="canne à sucre - fermage" | (approved$original=="canne à sucre - fermage" & approved$similar=="canne à sucre") & approved$rank==1,]
approved <- approved[approved$original!="épinard" | (approved$original=="épinard" & approved$similar=="epinards") & approved$rank==1,]
approved <- approved[approved$original!="lavande / lavandin" | (approved$original=="lavande / lavandin" & approved$similar=="lavande et lavandin") & approved$rank==1,]
approved <- approved[approved$original!="lupin doux d’hiver" | (approved$original=="lupin doux d’hiver" & approved$similar=="lupin doux") & approved$rank==1,]
approved <- approved[approved$original!="oignon / échalote" | (approved$original=="oignon / échalote" & approved$similar=="oignon et échalote") & approved$rank==1,]
approved <- approved[approved$original!="poivron / piment" | (approved$original=="poivron / piment" & approved$similar=="poivron, piment") & approved$rank==1,]
approved <- approved[approved$original!="pomme de terre féculière" | (approved$original=="pomme de terre féculière" & approved$similar=="pommes de terre de féculerie") & approved$rank==1,]
approved <- approved[approved$original!="radis fourrager" | (approved$original=="radis fourrager" & approved$similar=="radis") & approved$rank==7,]
approved <- approved[approved$original!="triticale d’hiver" | (approved$original=="triticale d’hiver" & approved$similar=="triticale") & approved$rank==1,]
head(approved, n=10)

#add more matches manually 
newobs_1 <- c("blé tendre d’hiver", "blé tendre d'hiver et épeautre")
newobs_2 <- c("maïs", "maïs grain")
newobs_3 <- c("maïs ensilage", "maïs grain")
newobs_4 <- c("orge d'hiver", "orge d'hiver et escourgeon")
newobs_5 <- c("colza d’hiver", "colza grain d'hiver")
newobs_6 <- c("colza de printemps", "colza grain de printemps et navette")
newobs_7 <- c("triticale d’hiver", "triticale")
newobs_8 <- c("sorgho", "sorgho grain")
newobs_9 <- c("seigle d’hiver", "seigle et méteil")

# list nweobs_X objects 
newobs_names <- ls(pattern = "^newobs_[0-9]{1,2}$") 
newobs_list <- lapply(newobs_names, get)
# create the data frame with two columns
manual <- data.frame(original = character(), similar = character(), stringsAsFactors = FALSE)
for (i in seq_along(newobs_list)) {
  manual <- rbind(manual,
    data.frame(
      original = newobs_list[[i]][1], 
      similar = newobs_list[[i]][2])
    )
}

#put everything together 
approved %<>% dplyr::select(original, similar) %>% bind_rows(manual) %>% 
  rename(n306_lib = `similar`, libelle_culture = `original`) %>% 
  left_join(uniq_comb_agrest, by = "n306_lib") %>% 
  left_join(uniq_comb_cadast, by = "libelle_culture")

#save 
write_csv(approved, file = file.path(codc, "cadastre_to_agreste_match.csv"))


# det <- "national"
# lvl = 0 
# sub_panel <- filter(big_panel, detail == det & level == lvl) %>% 
#   select(-c(detail, level)) %>% arrange(n306_mod, year) %>% 
#   mutate(n306_lib = paste0(n306_mod, ": ", n306_lib))
# 
# #graph productivity 
# ggplot(sub_panel, aes(x=year, y=pvity, group = n306_mod)) + 
#   geom_line(aes(color=n306_mod)) 
# unique(sub_panel$n306_lib) #run to see labels
# 
# #production as % of total  
# sub_panel %<>% group_by(year, dep) %>%
#   mutate(pct_prod = production / sum(production))
# ggplot(sub_panel, aes(x=year, y=pct_prod, group = n306_mod)) + 
#   geom_area(aes(fill=n306_mod)) 
# unique(sub_panel$n306_lib) #run to see labels 
# 
# #estimate averages and standard deviations 
