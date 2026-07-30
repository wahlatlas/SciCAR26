library(dplyr)
library(purrr)
library(DT)
options(tibble.print_max = 100)

library(restatis)

#packageVersion("restatis")
options(restatis.use_cache = FALSE)

#gen_auth_save(database="st")
#usethis::edit_r_environ()

# Check ob alle Datenbanken erreichbar sind
gen_logincheck(database = "all")

# convenience tools
flatten_tibbles <- function(x) {
  if (inherits(x, "data.frame")) {
    list(x)
  } else if (is.list(x)) {
    unlist(lapply(x, flatten_tibbles), recursive = FALSE)
  } else {
    list()
  }
}


#########################
#       SUCHE
#########################


searchresults_raw <- gen_find(database = "genesis", term = "industrie", category = "tables") 

searchresults_raw <- gen_find(database = c("nrw","regio","bildung"), term = "miete", category = "tables") 

searchresults_raw <- gen_find(database = "all", term = "abschluss", category = "tables") 


# Suche im Katalog 
# (Tabellenüberschriften)

catalogue_results_raw <- gen_catalogue(database = "genesis", category = "tables", 
                                       code = "6*", error.ignore = TRUE)

# Suche über alle EVAS Kategorien

catevas <- function(evas1){
  
  bind_rows(flatten_tibbles(
    gen_catalogue(
      database = "genesis", category = "tables", 
      code = paste0(evas1,"*"), pagelength = 1250, 
      error.ignore = TRUE)))
  }

df <- bind_rows(map(1:9, catevas))

df |> 
  filter(grepl("wasser(?!g)", Content, 
               perl = TRUE, 
               ignore.case = TRUE))


saveRDS(df, "03_tabellennamen_genesis.rds")
df <- readRDS("03_tabellennamen_genesis.rds")