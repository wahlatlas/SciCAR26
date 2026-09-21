setwd("~/Documents/R/SciCAR2026")

# Zur Visualisierung (optional)
library(tidyverse)
library(ggauto)
library(patchwork)
library(ggtext)

## Darum Geht es
library(restatis)


# Code: 46241-0011
# Unfallbeteiligte, Hauptverursacher des Unfalls: 
# Deutschland, Jahre, Geschlecht, Altersgruppen, Art der Verkehrsbeteiligung, Unfallkategorie, Ortslage
# https://genesis.destatis.de/datenbank/online/table/46241-0011/

vars46241 <- gen_var2stat(database = "genesis", code="46241")

print(vars46241[[1]], n=100)

gen_val2var(database = "genesis", code="VERVB1") # Verkehrsbeteiligung
gen_val2var(database = "genesis", code="VERUK1") # Unfallkategorie


df <- gen_table(database="genesis", name="46241-0011",
                #classifyingvariable1="VERVB1",
                #classifyingkey1="BETEILART08,BETEILART12,BETEILART13",
                classifyingvariable2 = "VERUK1",
                classifyingkey2 = "UNF-PERS",
                timeslices = 1,
                language="de"
                ) |> 
  janitor::clean_names()

df$value <- as.numeric(df$value)
df$time <- as.numeric(df$time)

# VISUALISIERUNG

selection <- df |> 
  filter(is.na(x6_variable_attribute_code),
         !is.na(x5_variable_attribute_code),
         x3_variable_attribute_code!="ALTNN",
         !is.na(x3_variable_attribute_code),
         x2_variable_attribute_code %in% c("GESM","GESW")
         ) |> 
        arrange(x2_variable_attribute_code, desc(x3_variable_attribute_code))


# factor levels for correct appearance order in plot

selection$x3_variable_attribute_label <- factor(selection$x3_variable_attribute_label, 
                                                levels = unique(selection$x3_variable_attribute_label))

selection$x2_variable_attribute_label <- factor(selection$x2_variable_attribute_label,
                                                levels = unique(selection$x2_variable_attribute_label))

plot_fuss_betlgt <- selection %>%
  filter(x4_variable_attribute_code=="BETEILART10",
         value_variable_code=="VER024") %>%  # Parties involved in the accident   
  ggauto(x2_variable_attribute_label,x3_variable_attribute_label,value,
         title=.$x4_variable_attribute_label[1], # subtitle = "by age and sex",
         base_family = "Statis Sans", base_size=9) +
  guides(fill = guide_colorbar(barwidth = 8, barheight=.5)) +
  theme(legend.ticks = element_blank())

plot_bike_betlgt <- selection %>%
  filter(x4_variable_attribute_code=="BETEILART08",
         value_variable_code=="VER024") %>%  # Parties involved in the accident   
  ggauto(x2_variable_attribute_label,x3_variable_attribute_label,value,
         title=.$x4_variable_attribute_label[1], # subtitle = "by age and sex",
         base_family = "Statis Sans", base_size=9) +
  guides(fill = guide_colorbar(barwidth = 8, barheight=.5)) +
  theme(legend.ticks = element_blank())

plot_scootr_betlgt <- selection %>%
  filter(x4_variable_attribute_code=="BETEILART12",
         value_variable_code=="VER024") %>%  # Parties involved in the accident   
  ggauto(x2_variable_attribute_label,x3_variable_attribute_label,value,
         title=.$x4_variable_attribute_label[1], # subtitle = "by age and sex",
         base_family = "Statis Sans", base_size=9) +
  guides(fill = guide_colorbar(barwidth = 8, barheight=.5)) +
  theme(legend.ticks = element_blank())

plot_e_betlgt <-   selection %>%
  filter(x4_variable_attribute_code=="BETEILART13",
         value_variable_code=="VER024") %>%   
  ggauto(x2_variable_attribute_label,x3_variable_attribute_label,value,
         title=.$x4_variable_attribute_label[1], # subtitle = "by age and sex", 
         base_family = "Statis Sans", base_size=9) +
  guides(fill = guide_colorbar(barwidth = 8, barheight=.5)) +
  theme(legend.ticks = element_blank())

plot_motor_betlgt <-   selection %>%
  filter(x4_variable_attribute_code=="BETEILART02",
         value_variable_code=="VER024") %>%   
  ggauto(x2_variable_attribute_label,x3_variable_attribute_label,value,
         title=.$x4_variable_attribute_label[1], # subtitle = "by age and sex", 
         base_family = "Statis Sans", base_size=9) +
  guides(fill = guide_colorbar(barwidth = 8, barheight=.5)) +
  theme(legend.ticks = element_blank())


# used in title
latestDate <- max(df$time)

p <- (plot_fuss_betlgt|plot_bike_betlgt|plot_scootr_betlgt|plot_e_betlgt|plot_motor_betlgt) +
  plot_annotation(title = paste(selection$x5_variable_attribute_label[1], selection$x1_variable_label[1], latestDate),
                  subtitle = "Unfallbeteiligte nach Alter und Geschlecht",
                  caption = "Genesis Online Tabelle 46241-0011") &
  theme(plot.title = element_markdown(hjust = 0, size=12, color = "#484848",
                                      family="Statis Sans Bold", 
                                      #lineheight = 1.4,
                                      margin = unit(c(0,0,6,0), "pt")),
        plot.subtitle = element_markdown(hjust = 0, size=12, color = "#484848",
                                      family="Statis Sans", 
                                      #lineheight = 1.4,
                                      margin = unit(c(0,0,16,0), "pt")),
        plot.caption = element_markdown(size = 9, lineheight = 1.4,
                                        family="Statis Sans",
                                        color = "#484848", hjust = 0,
                                        margin = margin(9,0,0,0)))

ggsave("saved_results/00_bio_ebike_scooter.png", plot = p,
       width = 3840, height = 2160, units = "px") 
