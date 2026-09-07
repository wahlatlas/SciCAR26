library(restatis)
library(tidyverse)
library(janitor)
library(scales)

#######################################################################
#
# Nachbau der Spiegel Grafik "5. Sachsen-Anhalt ist ideales AfD-Umfeld"
# https://www.spiegel.de/politik/deutschland/landtagswahl-in-sachsen-anhalt-wie-die-afd-die-wahl-gewann-der-grafik-ueberblick-a-bc49a0e2-7c15-4f3a-b885-4b4e12164f93
#
#######################################################################

# Vorläufige Ergebnisse für Gemeinden, csv, utf-8
# https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/downloads.html

ltwst26vorl <- read_csv2("Ergebnisse_Gemeinden_LT_2026.csv") |> 
  filter(is.na(Wahllokal)) |> # Brief- und Urnenwahl zusammen
  mutate(ARS=as.character(Schlüsselnummer))

ltwst26vorl$afd_zweit_proz <- ltwst26vorl$F02.AfD / ltwst26vorl$F.Gültige.Zweitstimmen *100

bev_gem_st_2011_2025 <- gen_table(database = "st", name="12411-0001",
                            startyear = 2011, endyear = 2025,
                            language = "de") |> 
                        clean_names()

bev_gem_st_2011_2025 <- bev_gem_st_2011_2025 |> 
                          filter(x1_variable_code=="GEMEIN",
                                 time %in% c(2011,2025)) |> 
                          mutate(value=as.integer(value))

bev_gem_st_2011_2025_wide <- pivot_wider(bev_gem_st_2011_2025,
                                         id_cols = x1_variable_attribute_code,
                                         names_from = c(time,x2_variable_attribute_label),
                                         values_from = value)

bev_gem_st_2011_2025_wide$bev2025proz2011 <- (bev_gem_st_2011_2025_wide$`2025_Insgesamt`
                                             -bev_gem_st_2011_2025_wide$`2011_Insgesamt`) /
                                              bev_gem_st_2011_2025_wide$`2011_Insgesamt` *100

df <- ltwst26vorl |> 
  select(ARS,Name,afd_zweit_proz) |> 
  left_join(bev_gem_st_2011_2025_wide, by = c("ARS"="x1_variable_attribute_code"))


ggplot(df, aes(x=bev2025proz2011, y=afd_zweit_proz, 
               size = `2025_Insgesamt`)) +
  geom_point(color = "#009EE0") +
  geom_text(
    data = subset(df, `2025_Insgesamt` >2e5),
    aes(label = sub(",.*", "", Name)),
    nudge_y = 2,
    size = 9,
    size.unit = "pt"
  ) +
  scale_x_continuous(limits = c(-28,10)) +
  scale_size_continuous(
    name = "Bevölkerung 2025",
    range = c(1, 7),
    labels = label_number(
      big.mark = ".",
      decimal.mark = ","
    ),
    breaks = c(20000,80000,200000)
  ) +
  labs(title="Landtagswahl Sachsen-Anhalt 2026 nach Gemeinden (vorläufig)",
       x="Bevölkerungsveränderung seit 2011 in %",
       y="Zweit-\nstimmen-\nanteil\nAfD (%)") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.justification = "left",
        legend.title.position = "top",
        axis.title.y = element_text(angle=0,hjust=0))
