#' this script generates the descriptive table 01 of the paper.
rm(list = ls())
if(!require("pacman")){install.packages("pacman")}

# load libraries
pacman::p_load(tidyverse, data.table, janitor, here, haven, magrittr, openxlsx)

# load data
data <- read_dta(here("data", "proc", "working_dataset_cohorte4m_2010.dta"))

data_2 <- data %>% filter(entra_ES == 1 & 
                          area_conocimiento_cat != 10 & 
                          is.na(ptje_lect2m_alu) == 0 &
                          is.na(ptje_mate2m_alu) == 0
                          )

# table
# graduation rates (outcomes) ----

graduation_section <-
data_2 %>% select(starts_with("titulado"), duracion_total_anios) %>%
    filter(duracion_total_anios <= 3) %>% 
    select(-duracion_total_anios) %>%
    pivot_longer(everything()) %>%
    mutate(name = paste0(name,"_3a")) %>%
    group_by(name) %>%
    summarise_all(.funs = list(mean = mean, sum = ~sum(!is.na(.))), na.rm = T) %>%
    
    bind_rows(

data_2 %>% select(starts_with("titulado"), duracion_total_anios) %>%
    filter(duracion_total_anios > 3) %>% 
    select(-duracion_total_anios) %>%
    pivot_longer(everything()) %>%
    mutate(name = paste0(name,"_4a")) %>%
    group_by(name) %>%
    summarise_all(.funs = list(mean = mean, sum = ~sum(!is.na(.))), na.rm = T) %>%
    rbind(NA, NA)) %>%
    
    mutate(mean = round(mean, 3))


# demographic ----
n_obs_dem <- data_2 %>% filter(is.na(dependencia_cat) == 0) %>% nrow(.)
n_obs_nse <- data_2 %>% filter(is.na(q_nse) == 0) %>% nrow(.)

# set value labels as values
data_2 <- data_2 %>% mutate(dependencia_cat = as_factor(dependencia_cat),
                            q_nse = as_factor(q_nse))

demographic_section <- 
data_2 %>% group_by(dependencia_cat) %>% 
    summarise(mean = n() / n_obs_dem,
              sum = n_obs_dem) %>% 
    ungroup() %>%
    rename(name = dependencia_cat) %>% # alert: original table has a different name order.
    rbind(NA) %>%
    
    bind_rows(

data_2 %>% group_by(q_nse) %>%
    summarise(mean = n() / n_obs_nse,
              sum = n_obs_nse) %>%
    ungroup() %>%
    rename(name = q_nse)) %>%
    rbind(NA) %>%
    
    bind_rows(

data_2 %>% select(d_mujer_alu, d_estudia_otra_region) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise_all(.funs = list(mean = mean, sum = ~sum(!is.na(.))), na.rm = T) %>%
    rbind(NA, NA))%>%
    
    mutate(mean = round(mean, 3))

# institutional ----
n_obs_inst <- data_2 %>% filter(is.na(tipo_inst_3) == 0) %>% nrow(.)
n_obs_acr <- data_2 %>% filter(is.na(rango_acreditacion_cat) == 0) %>% nrow(.)
n_obs_area <- data_2 %>% filter(is.na(area_conocimiento_cat) == 0) %>% nrow(.)

# set value labels as values
data_2 <- data_2 %>% mutate(rango_acreditacion_cat = as_factor(rango_acreditacion_cat),
                            area_conocimiento_cat = as_factor(area_conocimiento_cat))


institutional_section <- 
data_2 %>% group_by(tipo_inst_3) %>% 
    summarise(mean = n() / n_obs_inst,
              sum = n_obs_inst) %>%
    rename(name = tipo_inst_3) %>%
    ungroup() %>%
    
    bind_rows(

data_2 %>% select(d_sede_RM) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise_all(.funs = list(mean = mean, sum = ~sum(!is.na(.))), na.rm = T) %>%
    rbind(NA)) %>%
    
    bind_rows(

data_2 %>% group_by(rango_acreditacion_cat) %>% 
    summarise(mean = n() / n_obs_acr,
              sum = n_obs_acr) %>%
    ungroup() %>%
    rename(name = rango_acreditacion_cat) %>%
    rbind(NA)) %>%
    
    bind_rows(

data_2 %>% group_by(area_conocimiento_cat) %>% 
    summarise(mean = n() / n_obs_area,
              sum = n_obs_area) %>%
    ungroup() %>%
    rename(name = area_conocimiento_cat) %>%
    filter(!str_detect(name, "definida$")) %>%
    rbind(NA)) %>%
    
    mutate(mean = round(mean, 3))

# academic ----

# 1381 NAs.
#data_2 %$% sum(is.na((ptje_lect2m_alu)))
#data_2 %$% sum(is.na((ptje_mate2m_alu)))

academic_section <- 
data_2 %>% select(starts_with("ptje"), nem) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise_all(.funs = list(mean = mean, sum = ~sum(!is.na(.))), na.rm = T) %>%
    mutate(mean = round(mean,1)) %>%
    rbind(NA)



# join all sections ----
table <- 
graduation_section %>% bind_rows(demographic_section) %>%
                        bind_rows(institutional_section) %>%
                        bind_rows(academic_section)


# overwrite results over existing table

## Load existing file
excel_file <- loadWorkbook(here("results", "tables", "descriptive","03_descriptive_table01.xlsx"))

## Pull all data from sheet 1
excel_table <- read.xlsx(excel_file, sheet=1)

## join data
table <- table %>% add_row(mean = NA, .before = 1) # this is just for format!
excel_table <- excel_table %>% mutate("Valor.(%)" = table %>% pull(mean),
                                        "N" = table %>% pull(sum))

## Put the data back into the workbook
writeData(excel_file, sheet=1, excel_table)

## Save to disk
saveWorkbook(excel_file, 
             here("results", "tables", "descriptive","03_descriptive_table01.xlsx"), 
             overwrite = TRUE)
