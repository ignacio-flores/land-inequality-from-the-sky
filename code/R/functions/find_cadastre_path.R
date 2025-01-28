find_cadastre_path <- function(t){
  if (t < 2019) {
    nam <- "PARCELLES_GRAPHIQUES"
    pcl <- file.path(pathcada, t, "PARCELLES_GRAPHIQUES.shp")
  }  
  if (t >= 2019) {
    nam <- "parcelle_graphique"
    if (t >= 2021) {
      nam <- "parcelles_graphiques"
    }
    pcl <- file.path(pathcada, t, "PARCELLES_GRAPHIQUES.gpkg")
  }
  return(list(nam, pcl))
}