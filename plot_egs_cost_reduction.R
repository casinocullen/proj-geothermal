#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>----
# Plot: EGS learning curve ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>----
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(readxl)
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Read inputs ----
x.learning.raw = read_xlsx("./source/Geothermal Cost Reduction Scenarios.xlsx", sheet = "Cost Reduction Scenarios")

x.learning = x.learning.raw %>% 
  select(year, `ATB 2023 Advanced`, `ATB 2023 Conservative`, `ATB 2023 Moderate`, 
         `van der Zwaan (2019)`, `Clauser (2018)`, `Schulz (2023)`) %>% 
  gather(key = 'study', value = 'rate', -year) %>% 
  mutate(study = case_when(study == 'ATB 2023 Conservative' ~ 'Conservative',
                           study == 'ATB 2023 Moderate' ~ 'Moderate',
                           study == 'ATB 2023 Advanced' ~ 'Advanced',
                           TRUE ~ study
  ), 
  highlight = case_when(study == 'Conservative' ~ T,
                        study == 'Moderate' ~ T,
                        study == 'Advanced' ~ T,
                        TRUE ~ F
  ), 
  study = factor(study, level = c('Advanced', 'Conservative', 'Moderate', 
                                  'van der Zwaan (2019)', 'Clauser (2018)', 'Schulz (2023)')), 
  rate_revese = 1 - rate,
  label = if_else(year == max(year), 
                  as.character(study), 
                  NA_character_)) %>% 
  filter(year >= 2022)

colourCount = length(unique(x.learning$study))

# Plot: learning curves ----
ggplot(x.learning, aes(x=year, y=rate)) +
  geom_line(aes(color=study, linetype = study), linewidth = 1.4) + 
  geom_text_repel(data = . %>% filter(!is.na(label)), 
                  aes(label = paste0("  ", label), 
                      color = study), 
                  fontface = 'bold', 
                  size = 5,
                  force = 0.2, 
                  force_pull = 100,
                  hjust = 0,
                  direction="y",
                  na.rm = TRUE, 
                  xlim = c(2022, 2068), 
                  ylim = c(-0.01, 1))  + 
  scale_y_continuous(name = 'Cost Relative to 2023', 
                     labels = percent_format(), 
                     breaks = c(0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9, 1)) + 
  scale_color_manual(name = '', values = getPalette(colourCount)) + 
  scale_linetype_manual(name = '', values = c(1,1,1,2,2,2,2))+
  scale_size_manual(values = c(5,5.5)) + 
  scale_x_continuous(name = '', 
                     breaks = c(2023 ,2030,2035,2040,2045, 2050), 
                     limits = c(2023, 2062)) + 
  theme_minimal() + 
  theme(legend.position = 'none', 
        panel.background = element_rect(fill = 'white', color = 'white'), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = 'bold', size = 16), 
        axis.text = element_text(face = 'bold', size = 15)
  )


ggsave(filename = paste0("./output/plot_1.png"), height = 6, width = 8.5, dpi = 800) 

