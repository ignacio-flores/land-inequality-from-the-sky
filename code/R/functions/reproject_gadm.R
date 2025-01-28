# gadm reprojection 
filegadm <- c("gadm36_FRA_0", "gadm36_FRA_4")
names(filegadm) <- c("gadm0", "gadm4")
gadm <- mapply(
  function(f,p){ 
    x <- sf::read_sf(dsn = p, layer = f)
    return(sf::st_transform(x, crs = sf::st_crs(2154)))
  },
  filegadm, 
  rep(pathgadm, length(filegadm)), 
  USE.NAMES = T
)
