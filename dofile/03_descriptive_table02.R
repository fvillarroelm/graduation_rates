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

# 3-year programs table ----
# problem of map within another map
tables <-
map(.x = 3:4,
    .f = ~{
        # row names
        row_names <-
            data_2 %>% filter(duracion_total_anios <= .x) %>%
            tabyl(area_conocimiento_cat, dependencia_cat) %>%
            adorn_totals(c("row", "col")) %>%
            filter(area_conocimiento_cat != "Sin Área definida") %>%
            pull(area_conocimiento_cat)
        
        # denominator: graduates + non-graduates
        grads_and_nongrads_3y <-
            data_2 %>% filter(duracion_total_anios <= .x) %>%
            tabyl(area_conocimiento_cat, dependencia_cat) %>%
            adorn_totals(c("row", "col")) %>%
            filter(area_conocimiento_cat != "Sin Área definida") %>%
            select(-area_conocimiento_cat)
        
        # numerator: graduates
        grads_3y <- 
            data_2 %>% filter(titulado == 1 & duracion_total_anios <= .x) %>%
            tabyl(area_conocimiento_cat, dependencia_cat) %>%
            adorn_totals(c("row", "col")) %>%
            filter(area_conocimiento_cat != "Sin Área definida") %>% 
            select(-area_conocimiento_cat)
        
        # numerator/denominator (graduation rate by knowledge area)
        table_grads_3y <- round(grads_3y / grads_and_nongrads_3y, 3) %>% 
            mutate(area_conocimiento_cat = row_names) %>%
            relocate(area_conocimiento_cat)
        
        # diff pp - mun for <3 year programs.
        diff_pp_mun_3y <-
            map_df(.x = data_2 %$% levels(area_conocimiento_cat) %>% extract(. != "Sin Área definida"),
                   .f = ~{data_2 %>% filter(duracion_total_anios <= .x & area_conocimiento_cat == .x) %>%
                           lm (titulado ~ dependencia_cat, .) %>% 
                           tidy() %>%
                           tail(1) %>% # diff between pp and m
                           mutate(p.value = round(p.value, 3),
                                  dif_pp_m = round(estimate, 3),
                                  area_conocimiento_cat = .x) %>%
                           select(area_conocimiento_cat, dif_pp_m, p.value)})%>%
            
            bind_rows(
                # total row!
                data_2 %>% filter(duracion_total_anios <= .x) %>%
                    lm (titulado ~ dependencia_cat, .) %>% 
                    tidy() %>%
                    tail(1) %>% # diff between pp and m
                    mutate(p.value = round(p.value, 3),
                           dif_pp_m = round(estimate, 3),
                           area_conocimiento_cat = "Total") %>%
                    select(area_conocimiento_cat, dif_pp_m, p.value))
        
        
        
        # final table
        final_3y_table <- table_grads_3y %>% left_join(diff_pp_mun_3y, by="area_conocimiento_cat") %>%
            mutate(dif_pp_m = case_when(p.value > 0.05 & p.value <= 0.1 ~ paste0(dif_pp_m,"*"),
                                        p.value > 0.01 & p.value <= 0.05 ~ paste0(dif_pp_m, "**"),
                                        p.value <= 0.01 ~ paste0(dif_pp_m,"***"),
                                        TRUE ~ as.character(dif_pp_m))) %>%
            select(-p.value)
    }
)
tables[[1]] 
tables[[2]]

# row names
row_names <-
data_2 %>% filter(duracion_total_anios <= 3) %>%
    tabyl(area_conocimiento_cat, dependencia_cat) %>%
    adorn_totals(c("row", "col")) %>%
    filter(area_conocimiento_cat != "Sin Área definida") %>%
    pull(area_conocimiento_cat)

# denominator: graduates + non-graduates
grads_and_nongrads_3y <-
data_2 %>% filter(duracion_total_anios <= 3) %>%
    tabyl(area_conocimiento_cat, dependencia_cat) %>%
    adorn_totals(c("row", "col")) %>%
    filter(area_conocimiento_cat != "Sin Área definida") %>%
    select(-area_conocimiento_cat)

# numerator: graduates
grads_3y <- 
data_2 %>% filter(titulado == 1 & duracion_total_anios <= 3) %>%
    tabyl(area_conocimiento_cat, dependencia_cat) %>%
    adorn_totals(c("row", "col")) %>%
    filter(area_conocimiento_cat != "Sin Área definida") %>% 
    select(-area_conocimiento_cat)

# numerator/denominator (graduation rate by knowledge area)
table_grads_3y <- round(grads_3y / grads_and_nongrads_3y, 3) %>% 
                        mutate(area_conocimiento_cat = row_names) %>%
                        relocate(area_conocimiento_cat)

# diff pp - mun for <3 year programs.
diff_pp_mun_3y <-
map_df(.x = data_2 %$% levels(area_conocimiento_cat) %>% extract(. != "Sin Área definida"),
    .f = ~{data_2 %>% filter(duracion_total_anios <= 3 & area_conocimiento_cat == .x) %>%
            lm (titulado ~ dependencia_cat, .) %>% 
            tidy() %>%
            tail(1) %>% # diff between pp and m
            mutate(p.value = round(p.value, 3),
                   dif_pp_m = round(estimate, 3),
                   area_conocimiento_cat = .x) %>%
            select(area_conocimiento_cat, dif_pp_m, p.value)})%>%
    
    bind_rows(
                                # total row!
            data_2 %>% filter(duracion_total_anios <= 3) %>%
                lm (titulado ~ dependencia_cat, .) %>% 
                tidy() %>%
                tail(1) %>% # diff between pp and m
                mutate(p.value = round(p.value, 3),
                       dif_pp_m = round(estimate, 3),
                       area_conocimiento_cat = "Total") %>%
                select(area_conocimiento_cat, dif_pp_m, p.value))



# final table
final_3y_table <- table_grads_3y %>% left_join(diff_pp_mun_3y, by="area_conocimiento_cat") %>%
                    mutate(dif_pp_m = case_when(p.value > 0.05 & p.value <= 0.1 ~ paste0(dif_pp_m,"*"),
                                                p.value > 0.01 & p.value <= 0.05 ~ paste0(dif_pp_m, "**"),
                                                p.value <= 0.01 ~ paste0(dif_pp_m,"***"),
                                                TRUE ~ as.character(dif_pp_m))) %>%
                    select(-p.value)
