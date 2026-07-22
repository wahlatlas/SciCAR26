# Zur Visualisierung (optional)
library(tidyverse)
library(ggauto)
library(patchwork)
library(ggtext)

## Darum Geht es
library(restatis)

#install.packages("restatis")
#packageVersion("restatis")

 
# kein Produkt des Stat. Bundesamt
# in aktiver Entwicklung auf Github/CRAN
# https://github.com/CorrelAid/restatis


#######################
# {restatis} einrichten
#######################

gen_auth_save(database="genesis")
usethis::edit_r_environ()



# Code: 46241-0011
# Unfallbeteiligte, Hauptverursacher des Unfalls: 
# Deutschland, Jahre, Geschlecht, Altersgruppen, Art der Verkehrsbeteiligung, Unfallkategorie, Ortslage
# https://genesis.destatis.de/datenbank/online/table/46241-0011/

test <- gen_table(database="genesis", name="46241-0011", startyear = 2024)

# Mehrdimensionale ffcsv Tabelle, 
# Größe reduzieren, an Fragestellung anpassen
# Abruf mit ausgewählten Merkmalen
# Merkmale anhand der Weboberfläche recherchieren

# Alternative direkt in restatis

vars46241 <- gen_var2stat(database = "genesis", code="46241")

print(vars46241[[1]], n=100)

gen_val2var(database = "genesis", code="VERVB1") # Verkehrsbeteiligung
gen_val2var(database = "genesis", code="VERUK1") # Unfallkategorie


df <- gen_table(database="genesis", name="46241-0011",
                classifyingvariable1="VERVB1",
                classifyingkey1="BETEILART08,BETEILART13",
                classifyingvariable2 = "VERUK1",
                classifyingkey2 = "UNF-PERS",
                startyear = 2025,
                #timeslices = 1,
                language="de"
                ) |> 
  janitor::clean_names()

df$value <- as.numeric(df$value)
df$time <- as.numeric(df$time)


# Fahrradunfälle auf Autobahnen?

test <- df |> 
  filter(x6_variable_attribute_label=="auf Autobahnen",
         x5_variable_attribute_label!="Insgesamt",
         x4_variable_attribute_label!="Insgesamt",
         x3_variable_attribute_label!="Insgesamt",
         x2_variable_attribute_label!="Insgesamt",
         #value_variable_label=="Unfallbeteiligte",
         value>1) |> 
  select(x2_variable_attribute_label,
         x3_variable_attribute_label,
         x4_variable_attribute_label,
         x5_variable_attribute_label,
         x6_variable_attribute_label,
         value_variable_label,
         value, value_unit)

# https://www.borkenerzeitung.de/welt/in-ausland/panorama/Rentner-mit-E-Bike-auf-der-Autobahn-622577.html



#################
# VISUALISIERUNG

# filter by label, better readability
#selection <- df |> 
#  filter(x6_variable_attribute_label=="Total",
#         x5_variable_attribute_label!="Total",
#         x3_variable_attribute_label!="age unknown",
#         x3_variable_attribute_label!="Total",
#         x2_variable_attribute_label!="Total")  

# filter by code, language agnostic
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

plot_bio_betlgt <- selection %>%
  filter(x4_variable_attribute_code=="BETEILART08",
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


# used in title
latestDate <- max(df$time)

(plot_bio_betlgt|plot_e_betlgt) +
  plot_annotation(title = paste(selection$x5_variable_attribute_label[1], selection$x1_variable_label[1], latestDate),
                  caption = "Genesis Online Tabelle 46241-0011") &
  theme(plot.title = element_markdown(hjust = 0, size=12, color = "#484848",
                                      lineheight = 1.4,
                                      margin = unit(c(0,0,16,0), "pt")),
        plot.caption = element_markdown(size = 9, lineheight = 1.4,
                                        color = "#484848", hjust = 0,
                                        margin = margin(9,0,0,0)))
 