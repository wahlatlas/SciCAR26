# Visualisierung
library(tidyverse)
library(ggtext)

# Datenabruf Zensusdatenbank
library(restatis)
gen_logincheck(database = "zensus")


# Wohnfläche Mieter vs Eigentümer in Großstädten ?


########################################
# Gemeindeschlüssel der Städte > 500k Ew
########################################

df1000A0001 <- gen_table(database="zensus", "1000A-0001", language = "de")

staedte500k <- df1000A0001 |> 
            filter(value_variable_label=="Personen") |> 
            mutate(EWZ_2022 = as.numeric(value)) |> 
            filter(EWZ_2022 > 500000) |> 
            rename(AGS=`1_variable_attribute_code`, Name=`1_variable_attribute_label`) |> 
            select(AGS,Name,EWZ_2022)
  
ags500k <- paste(staedte500k$AGS, collapse = ",")

# Code: 4000W-2001
# Wohnungen: Art der Wohnungsnutzung (vom Eigentümer bewohnt | vermietet | leer stehend | FeWo) - 
# Fläche der Wohnung (10 m²-Intervalle) 
# https://ergebnisse.zensus2022.de/datenbank/online/table/4000W-2001

df4000W2001 <- gen_table(database="zensus", "4000W-2001", language = "de",
                         classifyingvariable1 = "WHGNZ2", classifyingkey1 = "NUTZ-EIGEN,NUTZ-MIETE,NUTZ-LEER",
                         regionalvariable = "GEOGM4", regionalkey = ags500k) |> 
                         rename(AGS=`1_variable_attribute_code`, Name=`1_variable_attribute_label`) |> 
                         mutate(value=as.numeric(value))
  
# Anspielen der Einwohnerzahlen zum Sortieren
df4000W2001 <- staedte500k |>
  select(AGS,EWZ_2022) |> 
  inner_join(df4000W2001, by="AGS")




################
# Visualisierung
################

format_string <- function(x) {
  x <- sub(" m²","",x)
  # An Leerzeichen umbrechen (Leerzeichen -> Zeilenumbruch)
  x <- sub(" - ", "\n–\n", x)
  x <- sub("Unter 30","Unter\n30qm",x)
  x <- sub(" und mehr","+\nqm",x)
  return(x)
}

# useful for selective labeling
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

wohnfl_sort <- c(
  "Unter 30 m²",
  "30 - 39 m²",
  "40 - 49 m²",
  "50 - 59 m²",
  "60 - 69 m²",
  "70 - 79 m²",
  "80 - 89 m²",
  "90 - 99 m²",
  "100 - 109 m²",
  "110 - 119 m²",
  "120 - 129 m²",
  "130 - 139 m²",  
  "140 - 149 m²",
  "150 - 159 m²",
  "160 - 169 m²",
  "170 - 179 m²",
  "180 m² und mehr" 
)

df4000W2001$`3_variable_attribute_label` <- factor(df4000W2001$`3_variable_attribute_label`, 
                                              levels = wohnfl_sort)

df4000W2001 |>
  group_by(Name) |> 
  mutate(sort_value = EWZ_2022)  |> 
  ungroup() |> 
  filter(
    `3_variable_attribute_label`!="Insgesamt",
    `2_variable_attribute_label`!="Insgesamt",
  ) |> 
  mutate(Name = fct_reorder(Name, sort_value, .desc = TRUE)) |> 
  ggplot(aes(x=`3_variable_attribute_label`, y=value/1000, 
             fill = `2_variable_attribute_label`)) +
  facet_wrap(~ Name, scales = "free_y",
             labeller = labeller(Name = function(x) sub(",.*", "", x))) +
  geom_col(position = position_dodge(width=.9), width=.75) +
  scale_x_discrete(breaks=every_nth(2), labels=function(x){format_string(x)}) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values = c("Zu Wohnzwecken vermietet (auch mietfrei)" = "#e75e70", 
                               "Von Eigentümer/-in bewohnt" = "#5294e6",
                               "Leer stehend" = "#929292"),
                    labels = c(
                      "Zu Wohnzwecken vermietet (auch mietfrei)" = "Zu Wohnzwecken vermietet\n(auch mietfrei)",
                      "Von Eigentümer/-in bewohnt" = "Von Eigentümer/-in\nbewohnt"
                    )) +
  labs(title = "Wohnfläche 2022 nach Art der Wohnungsnutzung in Städten ab 500 Tsd Ew",
       subtitle = "Anzahl Wohnungen in 1.000",
       x=NULL, y=NULL, caption="Zensusdatenbank Tabelle 4000W-2001") +
  theme_minimal(base_size = 12) +
  theme(
    plot.caption = element_markdown(hjust=1),
    strip.text = element_text(face = "bold", hjust=0, size=9),
    panel.spacing = unit(5, "mm"),
    text = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.position = c(1,0),
    legend.justification = c(1, 0),
    legend.key.spacing.y = unit(1, "mm")
    )

ggsave("02_wohnen.pdf", width = 298, height = 210, units = "mm")


