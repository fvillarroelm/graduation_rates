#' this script generates the descriptive table 02 of the paper.
rm(list = ls())
if(!require("pacman")){install.packages("pacman")}

# load libraries
pacman::p_load(tidyverse, data.table, janitor, here, haven, magrittr, openxlsx, broom)

# load data
data <- read_dta(here("data", "proc", "working_dataset_cohorte4m_2010.dta"))

data_2 <- data %>% filter(entra_ES == 1)

data_2 <-
data_2 %>% mutate(area_conocimiento_cat = as_factor(area_conocimiento_cat),
                  dependencia_cat = as_factor(dependencia_cat))

# row names
row_names <- data_2 %>% count(area_conocimiento_cat) %>% 
                        select(area_conocimiento_cat)

titulados_y_no_titulados_3a <-
data_2 %>% filter(duracion_total_anios <= 3) %>%
    tabyl(area_conocimiento_cat, dependencia_cat) %>%
    adorn_totals(c("row", "col")) %>%
    filter(area_conocimiento_cat != "Sin Área definida") %>%
    select(-area_conocimiento_cat)

titulados_3a <- 
data_2 %>% filter(titulado == 1 & duracion_total_anios <= 3) %>%
    tabyl(area_conocimiento_cat, dependencia_cat) %>%
    adorn_totals(c("row", "col")) %>%
    filter(area_conocimiento_cat != "Sin Área definida") %>% 
    select(-area_conocimiento_cat)
            
table_titulados_3a <- round(titulados_3a / titulados_y_no_titulados_3a, 3) %>% 
                        bind_cols(row_names) %>%
                        relocate(area_conocimiento_cat)

# it is missing the area_conocimiento term. I need to add it to this df.
diff_pp_mun_3a <-
map_df(.x = data_2 %$% levels(area_conocimiento_cat) %>% extract(. != "Sin Área definida"),
    .f = ~{data_2 %>% filter(area_conocimiento_cat == .x) %>%
            lm (titulado ~ dependencia_cat, .) %>% 
            tidy() %>%
            tail(1)})
