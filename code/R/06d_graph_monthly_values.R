#setwd("~/Dropbox/land_ineq_degradation/")
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(magrittr)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(biscale)
library(janitor)
library(sf)
library(cowplot)
library(gganimate)
library(animation)
library(haven)

#paths 
savepath <- "figures/"

#bring monthly panel 
df <- "data/FR/monthly_panel.dta" %>% 
  read_dta(.name_repair = "universal") %>%
  dplyr::select(name_4, adm_can, year, month, temp_canton, 
  temp_farmland, gpp_canton, gpp_farmland) %>% 
  arrange(month) %>% mutate(month = month.abb[month])

#make a toy version?
# df$drop <- runif(nrow(df), 0, 100)
# df <- subset(df, drop > 99)

#graph seasonal temp averages
color <- colorRampPalette(c("darkorchid3", "darkorange1"))(21)
df$season <- 1
df$season[df$month == 'Mar' | df$month == 'Apr' | df$month == 'May'] <-2
df$season[df$month == 'Jun' | df$month == 'Jul' | df$month == 'Aug'] <-3
df$season[df$month == 'Sep' | df$month == 'Oct' | df$month == 'Nov'] <-4
df$yrseason <- paste(df$year, df$season)
tempyr <- df %>% group_by(yrseason) %>% 
  summarize(
    temp_canton = mean(temp_canton), 
    season = first(season), 
    year = first(year)
  )
tempyr$season[tempyr$season == 1] <- "Winter"
tempyr$season[tempyr$season == 2] <- "Spring"
tempyr$season[tempyr$season == 3] <- "Summer"
tempyr$season[tempyr$season == 4] <- "Fall"
xlabels <- sort(unique(df$yrseason))
xlabels[c(2:20, 22:40, 42:60, 62:80, 82:84)] <- ""
xlabels %<>% substr(1,4)
tempyrgg2 <- ggplot(tempyr, 
  aes(x = yrseason, y = temp_canton, color = factor(season))) +
  geom_point() + scale_x_discrete(labels = xlabels) +
  labs(title = "Mean temperatures [ºC] in France", x = "", y = "", color = "") 
ggsave(paste0(
  savepath, "temperatures/seasonal_temps_2000-2020.png"), 
  plot = tempyrgg2
)

#plot temperature static 
temp <- ggplot(df, 
  aes(x = temp_canton, y = month, group = month, fill = ..x..)) +
  geom_rect(aes(xmin=-20, xmax=0, ymin="Dec", ymax="Jan"), fill="#E0E0E0") +
  geom_rect(aes(xmin=25, xmax=40, ymin="Dec", ymax="Jan"), fill="#E0E0E0") +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_y_discrete(limits = month.abb) +
  scale_fill_viridis(name = "Temp. [C]", option = "magma") +
  labs(title = 'Average temperatures, France 2000-2020') +
  xlab("Temp. [C]") +
  ylab("") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) 
ggsave(paste0(savepath, "temperatures/monthly_temps_2000-2020.png"), 
  plot = temp)

#plot temperature dynamic 
temp_anim <- temp + transition_time(year) +
  labs(title = "Average temperatures in France, 2000-2020") + 
   geom_text(aes(x = 32.5, y = "Feb", label = as.factor(year)) , col = "gray", size = 12) 
animate(temp_anim, rewind = FALSE)
anim_save("monthly_temps_2000-2020.mp4", animation = last_animation(), 
  path = "figures/temperatures/")

#ploy gpp static 
df %<>% mutate(gpp_canton = gpp_canton * 10)
gpp <- ggplot(df, aes(x = gpp_canton, y = month, group = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_y_discrete(limits = month.abb) +
  scale_fill_viridis(name = "GPP", option = "magma") +
  labs(title = 'GPP in France, 2000-2020', x = "Gpp (C.kg m2)", y = "") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
gpp
ggsave(paste0(savepath, "productivity/monthly_gpp_2000-2020.png"), 
  plot = gpp)

#plot gpp dynamic 
 temp_anim <- gpp + transition_time(year) +
   labs(title = "Average Gross Productivity in France, 2000-2020") + 
   geom_text(aes(x = 32.5, y = "Feb", label = as.factor(year)) , col = "gray", size = 12) 
 animate(temp_anim, rewind = FALSE)
 anim_save("monthly_gpp_2000-2020.mp4", animation = last_animation(), 
   path = "figures/productivity/")