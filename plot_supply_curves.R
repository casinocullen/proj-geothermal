#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>----
# Plot: EGS supply curves ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>----

# Rebuild geothermal supply curves
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(readxl)
library(sf)
library(data.table)
library(cowplot)
library(viridis)
library(ggrepel)
library(RColorBrewer)
library(stringr)

# Previous supply curve used in GeoVision ----
# Read Geovision BAU
x.geovision.bau =readxl::read_xlsx("./source/GeoVision ReEDS Geothermal Supply Curve Inputs.xlsx", 
                                  sheet = 'BAU + IRT', range = "A2:H790") %>% 
  filter(`Geothermal Resource Type` == 'Deep EGS') %>% 
  arrange(`Capital Costs`) %>% 
  group_modify(~ add_row(.x,.before=0)) %>% 
  mutate(Cap = replace_na(CAPACITY, 0), 
         Cost = lead(`Capital Costs`), 
         Cap_sum_GWe = cumsum(Cap)/1000,
         Cost_k = Cost/1000, 
         scenario = 'GeoVision BAU',
         year = '2022'
  ) %>% 
  fill(Cost_k, .direction = "down") %>% 
  ungroup()

# Read Geovision TI
x.geovision.ti =readxl::read_xlsx("./source/GeoVision ReEDS Geothermal Supply Curve Inputs.xlsx", 
                                  sheet = 'Improved Tech 2030', range = "A2:H828") %>% 
  filter(`Geothermal Resource Type` == 'Deep EGS') %>% 
  arrange(`Capital Costs`) %>% 
  group_modify(~ add_row(.x,.before=0)) %>% 
  mutate(Cap = replace_na(CAPACITY, 0), 
         Cost = lead(`Capital Costs`), 
         Cap_sum_GWe = cumsum(Cap)/1000,
         Cost_k = Cost/1000, 
         scenario = 'GeoVision TI',
         year = '2030'
  ) %>% 
  fill(Cost_k, .direction = "down") %>% 
  ungroup()


# New supply curve in this study ----
x.rsc.deep.pregion = read.csv('./source/egs_supply_curve_by_BAs.csv')

# EGS Supply Curve in 2023
x.rsc.deep.pregion.p1.all = x.rsc.deep.pregion %>% 
  arrange(dollar_per_kw) %>% 
  group_modify(~ add_row(.x,.before=0)) %>% 
  mutate(Cap = replace_na(geo_rsc_mw, 0), 
         Cost = lead(dollar_per_kw), 
         Cap_sum_GWe = cumsum(Cap)/1000,
         Cost_k = Cost/1000, 
         scenario = 'This study (2023)',
         tech = 'deep-egs'
  ) %>% 
  fill(Cost_k, .direction = "down") %>% 
  ungroup()


# EGS 2035 Supply Curves
x.rsc.deep.pregion.p1.all.2035.moderate = x.rsc.deep.pregion.p1.all %>% 
  mutate(Cost_k = Cost_k * 0.5759, 
         Cost = Cost * 0.5759, 
         scenario = 'This study (2035 Moderate)',
         year = '2035 (Moderate)')

x.rsc.deep.pregion.p1.all.2035.advance = x.rsc.deep.pregion.p1.all %>% 
  mutate(Cost_k = Cost_k * 0.2879, 
         Cost = Cost * 0.2879, 
         scenario = 'This study (2035 Advanced)',
         year = '2035 (Advanced)')

# Combine supply curves
x.geo.rsc.p1.all = x.geovision.bau %>% 
  rbind(x.geovision.ti) %>% 
  bind_rows(x.rsc.deep.pregion.p1.all) %>% 
  bind_rows(x.rsc.deep.pregion.p1.all.2035.advance) %>%
  bind_rows(x.rsc.deep.pregion.p1.all.2035.moderate) %>%
  arrange(scenario, Cost_k) %>% 
  mutate(scenario = factor(scenario, levels = c('GeoVision BAU', 'GeoVision TI', 'This study (2023)',  'This study (2035 Moderate)', 'This study (2035 Advanced)')))
 

# Plot: stepchart ----
getPalette = colorRampPalette(brewer.pal(6, "Set2"))

pstep = ggplot(data = x.geo.rsc.p1.all, aes(x=Cap_sum_GWe,y=Cost)) + 
  geom_step(aes(color = scenario), 
            direction = 'hv', linejoin = 'round', size = 1) + 
  scale_x_continuous(breaks = pretty_breaks(8), 
                     labels = number_format(big.mark = ','), 
                     name = "Capacity (GWe)") + 
  scale_y_continuous(breaks = pretty_breaks(10), 
                     labels = number_format(big.mark = ','), 
                     name = "CAPEX ($/kW)") + 
  scale_color_manual(name = '', values = getPalette(6)) + 
  guides(colour = guide_legend(nrow = 2, direction = 'horizontal', byrow = T)) + 
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        legend.box = 'horizontal',
        legend.direction = "horizontal", 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', color = 'white'), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        axis.title = element_text(face = 'bold', size = 15), 
        axis.text = element_text(face = 'bold', size = 12), 
        legend.text = element_text(face = 'bold', size = 15), 
        plot.margin=unit(c(0,-0.5,0,0.2), "cm")
  )
