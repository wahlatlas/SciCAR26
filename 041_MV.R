library(restatis)
library(tidyverse)
library(janitor)
library(scales)

# Wahlergebnisse MV für Gemeinden, csv, utf-8
# https://www.laiv-mv.de/Wahlen/Landtagswahlen/2021/Ergebnisse/

ltw_mv2021endg <- read_csv2("041_MV_LTW_2021_l_gemeinden.csv",
                            locale = locale(encoding = "Windows-1252"),
                            skip = 5) |> 
                    filter(Ausgabe=="P", # Prozent
                           `Erst-/Zweitstimme`==2) |> 
                    mutate(ARS=as.character(Gemeinde))
  
bev_regio_mv2011 <- gen_table(database = "regio", name="12411-01-01-5-B",
                              regionalvariable = "GEMEIN",
                              regionalkey = "13*",
                              startyear = 2011, endyear = 2011,
                              language = "de") |> 
                    clean_names()

bev_regio_mv2025 <- gen_table(database = "regio", name="12411-01-01-5-B",
                              regionalvariable = "GEMEIN",
                              regionalkey = "13*",
                              startyear = 2025, endyear = 2025,
                              language = "de") |> 
                    clean_names()


bev_regio_mv2011insg <- bev_regio_mv2011 |> 
        filter(x2_variable_attribute_label=="Insgesamt") |> 
        mutate(value2011=as.integer(value))

bev_regio_mv2025insg <- bev_regio_mv2025 |> 
        filter(x2_variable_attribute_label=="Insgesamt") |> 
        mutate(value2025=as.integer(value))

df <- left_join(bev_regio_mv2025insg,bev_regio_mv2011insg,
                by="x1_variable_attribute_code")

df$bev25pc11 <- (df$value2025 - df$value2011) / df$value2011 *100

df <- df |> 
  select(x1_variable_attribute_code,bev25pc11,value2025) |> 
  right_join(ltw_mv2021endg, by = c("x1_variable_attribute_code"="ARS"))


ggplot(df, aes(x=bev25pc11, y=AfD, 
               size = value2025)) +
  geom_point(color = "#009EE0") +
  geom_text(
    data = subset(df, value2025 >1e5),
    aes(label = sub(",.*", "", Gemeindename)),
    nudge_y = 0,
    size = 8,
    size.unit = "pt"
  ) +
  scale_x_continuous(limits = c(-50,50)) +
  scale_y_continuous(limits = c(2.5,45)) +
  scale_size_continuous(
    name = "Bevölkerung 2025",
    range = c(1, 7),
    labels = label_number(
      big.mark = ".",
      decimal.mark = ","
    ),
    breaks = c(20000,80000,200000)
  ) +
  labs(title="Landtagswahl MV 2021 nach Gemeinden (endgültig)",
       x="Bevölkerungsveränderung seit 2011 in %",
       y="Zweit-\nstimmen-\nanteil\nAfD (%)") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.title.position = "top",
        axis.title.y = element_text(angle=0,hjust=0))
