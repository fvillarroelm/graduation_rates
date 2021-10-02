if(!require("pacman")){install.packages("pacman")}

# load libraries
pacman::p_load(tidyverse, data.table, janitor, here, haven, magrittr)

# load data
data <- read_dta(here("data", "proc", "working_dataset_cohorte4m_2010.dta"))

data_2 <- data %>% filter(entra_ES == 1)

# table
# graduation rates (outcomes) ----

graduation_section <-
data_2 %>% select(starts_with("titulado"), duracion_total_anios) %>%
    filter(duracion_total_anios <= 3) %>% 
    select(-duracion_total_anios) %>%
    pivot_longer(everything()) %>%
    mutate(name = paste0(name,"_3a")) %>%
    group_by(name) %>%
    summarise_all(list(mean = mean, sum = sum)) %>%
    
    bind_rows(

data_2 %>% select(starts_with("titulado"), duracion_total_anios) %>%
    filter(duracion_total_anios > 3) %>% 
    select(-duracion_total_anios) %>%
    pivot_longer(everything()) %>%
    mutate(name = paste0(name,"_4a")) %>%
    group_by(name) %>%
    summarise_all(list(mean = mean, sum = sum)) %>% 
    rbind(NA, NA))


# demographic ----
n_obs <- data_2 %>% tally() %>% pull()

# set value labels as values
data_2 <- data_2 %>% mutate(dependencia_cat = as_factor(dependencia_cat),
                            q_nse = as_factor(q_nse))

demographic_section <- 
data_2 %>% group_by(dependencia_cat) %>% 
    summarise(mean = n() / n_obs,
              sum = n()) %>% 
    ungroup() %>%
    rename(name = dependencia_cat) %>% # alert: original table has a different name order.
    rbind(NA) %>%
    
    bind_rows(

data_2 %>% group_by(q_nse) %>%
    summarise(mean = n() / n_obs,
              sum = n()) %>%
    ungroup() %>%
    rename(name = q_nse)) %>%
    rbind(NA) %>%
    
    bind_rows(

data_2 %>% select(d_mujer_alu, d_estudia_otra_region) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise_all(list(mean = mean, sum = sum)) %>%
    rbind(NA, NA))

# institutional ----

# set value labels as values
data_2 <- data_2 %>% mutate(rango_acreditacion_cat = as_factor(rango_acreditacion_cat),
                            area_conocimiento_cat = as_factor(area_conocimiento_cat))


institutional_section <- 
data_2 %>% group_by(tipo_inst_3) %>% 
    summarise(mean = n() / n_obs,
              sum = n()) %>%
    rename(name = tipo_inst_3) %>%
    ungroup() %>%
    
    bind_rows(

data_2 %>% select(d_sede_RM) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise_all(list(mean = mean, sum = sum)) %>%
    rbind(NA)) %>%
    
    bind_rows(

data_2 %>% group_by(rango_acreditacion_cat) %>% 
    summarise(mean = n() / n_obs,
              sum = n()) %>%
    ungroup() %>%
    rename(name = rango_acreditacion_cat) %>%
    rbind(NA)) %>%
    
    bind_rows(

data_2 %>% group_by(area_conocimiento_cat) %>% 
    summarise(mean = n() / n_obs,
              sum = n()) %>%
    ungroup() %>%
    rename(name = area_conocimiento_cat) %>%
    rbind(NA))

institutional_section

# # academic ----
# # I will keep this pending.
# 
# # these are not working.
# data_2 %>% select(starts_with("ptje"), nem) %>%
#     summarise(ptje_simce_lect = mean(ptje_lect2m_alu),
#               ptje_simce_mat = mean(ptje_mate2m_alu),
#               nem = mean(nem))
# 
# # Need to impute the values of these tests in Stata first..
# data_2 %$% sum(is.na((ptje_lect2m_alu)))
# 

#' asddas
#' asdadsdsaadsdsa
#' asdsadss
#' dsadsadsaa '#
