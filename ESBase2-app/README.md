# ESBase 2.0

En enkel app för att demonstrera sökning, export och visa poster i ESBase fisk-katalog. Se till att nedanstående paket är installerade, öppna `app.R` i RStudio och kör med *Run app*. I `app_sqlite.R` finns en uppdaterad version som läser direkt från en SQLite-databas (som dock inte finns i repot då den kan innehålla känsliga uppgifter). För att istället läsa från ESBase/MySQL, byt anropet `DBI:dBConnect` till motsvarande för [`RMariaDB`](https://rmariadb.r-dbi.org/).

```
install.packages(c("shiny", "shinydashboard", "DT", "leaflet", "tidyverse"))
```
För demosyfte har data sparats i form av ett tabellutdrag skapat av

```
accession <- dbGetQuery(con, "SELECT * FROM accession")
fish <- dbGetQuery(con, "SELECT * FROM fish")
locality <- dbGetQuery(con, "SELECT * FROM locality")
project <- dbGetQuery(con, "SELECT * FROM project")
species <- dbGetQuery(con, "SELECT * FROM species")
person <- dbGetQuery(con, "SELECT * FROM person")
age <- dbGetQuery(con, "SELECT * FROM agerecord_row")


fish_tab <- accession %>% 
  inner_join(fish, by = c("id" = "accession_id"), suffix = c("", ".fish")) %>% 
  left_join(locality, by = c("locality_id" = "id"), suffix = c("", ".locality")) %>% 
  left_join(project, by = c("project_id" = "id"), suffix = c("", ".project")) %>% 
  left_join(species, by = c("species_id" = "id"), suffix = c("", ".species")) %>% 
  left_join(person, by = c("created_by" = "id")) %>% 
  left_join(age, by = c("id" = "accession_id"), suffix = c("", ".age")) %>% 
  mutate(accnr = paste0("C20", str_sub(id, 3, 4), "/", str_sub(id, 5)), .before = 1) %>% 
  mutate(created_by = paste(firstname, lastname)) %>% 
  select(accnr, sort(tidyselect::peek_vars())) %>% 
  select(id, accnr,  species = swe_name, project = name.project, 
         date = arrival_date, locality = name, longitude, latitude, age = age_start, gonadweight, 
         liverweight, totallength, bodylength, created_by)

write_csv(fish_tab, "fish.csv")
```



