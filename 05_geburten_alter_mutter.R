library(tidyverse)
library(janitor)
library(ggtext)
library(restatis)
options(restatis.use_cache = TRUE)


# Mehr Erstgeburten nach 40 als Teenagerschwangerschaften?
# Seit wann?

df <- gen_table("12612-0005", database = "genesis", 
                startyear = 2000, language = "de") |> 
                clean_names() |> 
                mutate(value = as.integer(value),
                       time = as.Date(paste(time,1,1, sep="-"))
                )

df <-  df|> 
        #nur erstes Kind
        filter(x2_variable_attribute_label == "erstes Kind")

df <- df |> mutate(
  age_group = case_when(
    x3_variable_attribute_code %in% c("ALT000B15", paste0("ALT0", 15:19)) ~ "Teenager",
    x3_variable_attribute_code %in% c(paste0("ALT0", 41:49), "ALT050UM") ~ "Älter 40",
    TRUE ~ "middle"
    )
)

df_groups <- df |> 
  group_by(time, age_group) |> 
  summarise(Erstgeburten = sum(value, na.rm = TRUE))

df_groups |>
  filter(age_group != "middle") |> 
  ggplot(aes(x=time, y=Erstgeburten/1000, color = age_group)) +
  geom_line(linewidth = 2, lineend = "round") +
  scale_color_manual(values = c(
    "Teenager" = "#e75e70",
    "Älter 40" = "#5294e6"
  )) +
  scale_x_date() +
  scale_y_continuous(limits = c(4,14), breaks = c(4,6,8,10,12,14)) +
  labs(title = "Geburten des ersten Kindes nach Alter der Mutter",
       subtitle = "in 1.000",
       caption = "Genesis Tabelle 12612-0005",
       x=NULL, y=NULL, color=NULL) +
  theme_minimal(base_size = 9) +
  theme(plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size=9,
                                  margin = margin(t = 6, b=3)),
        plot.subtitle = element_markdown(margin = margin(b=6)),
        axis.text = element_text(size=7))
