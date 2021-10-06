#' this script generates the descriptive table 02 of the paper.
rm(list = ls())
if(!require("pacman")){install.packages("pacman")}

# load libraries
pacman::p_load(tidyverse, data.table, janitor, here, haven, magrittr, openxlsx, broom)

# load data
data <- read_dta(here("data", "proc", "working_dataset_cohorte4m_2010.dta"))

data_2 <-
    data %>% filter(entra_ES == 1 & area_conocimiento_cat != 10) %>%
            mutate(area_conocimiento_cat = as_factor(area_conocimiento_cat) %>% droplevels(),
                   dependencia_cat = as_factor(dependencia_cat))

data_3y <-
data_2 %>% filter(duracion_total_anios <= 3)

data_4y <-
data_2 %>% filter(duracion_total_anios >= 4)

# table function ----

table <- function(data){

    # row names
    row_names <-
        data %>% tabyl(area_conocimiento_cat, dependencia_cat) %>%
        adorn_totals(c("row", "col")) %>%
        pull(area_conocimiento_cat)
    
    
    # denominator: graduates + non-graduates
    grads_and_nongrads <-
        data %>% tabyl(area_conocimiento_cat, dependencia_cat) %>%
        adorn_totals(c("row", "col")) %>%
        select(-area_conocimiento_cat)
    
    # numerator: graduates
    grads <- 
        data %>% filter(titulado == 1) %>%
        tabyl(area_conocimiento_cat, dependencia_cat) %>%
        adorn_totals(c("row", "col")) %>% 
        select(-area_conocimiento_cat)
    
    # numerator/denominator (graduation rate by knowledge area)
    table_grads <- round(grads / grads_and_nongrads, 3) %>% 
        mutate(area_conocimiento_cat = row_names) %>%
        relocate(area_conocimiento_cat)
    
    # diff pp - mun for each program.
    diff_pp_mun <-
        map_df(.x = data %$% levels(area_conocimiento_cat),
               .f = ~{data %>% filter(area_conocimiento_cat == .x) %>%
                       lm (titulado ~ dependencia_cat, .) %>% 
                       tidy() %>%
                       tail(1) %>% # diff between pp and m
                       mutate(p.value = round(p.value, 3),
                              dif_pp_m = round(estimate, 3),
                              area_conocimiento_cat = .x) %>%
                       select(area_conocimiento_cat, dif_pp_m, p.value)}) %>%
        
        bind_rows(
            # total row!
            data %>% lm (titulado ~ dependencia_cat, .) %>% 
                tidy() %>%
                tail(1) %>% # diff between pp and m
                mutate(p.value = round(p.value, 3),
                       dif_pp_m = round(estimate, 3),
                       area_conocimiento_cat = "Total") %>%
                select(area_conocimiento_cat, dif_pp_m, p.value))
    
    
    
    # final table
    final_table <- table_grads %>% left_join(diff_pp_mun, by="area_conocimiento_cat") %>%
        mutate(dif_pp_m = case_when(p.value > 0.05 & p.value <= 0.1 ~ paste0(dif_pp_m,"*"),
                                    p.value > 0.01 & p.value <= 0.05 ~ paste0(dif_pp_m, "**"),
                                    p.value <= 0.01 ~ paste0(dif_pp_m,"***"),
                                    TRUE ~ as.character(dif_pp_m))) %>%
        select(-p.value)
    
    return(final_table)

}



# tables ----

table_3y <- table(data_3y)

table_4y <- table(data_4y)