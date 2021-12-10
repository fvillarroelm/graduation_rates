#' this script generates the descriptive table 04 of the paper.
rm(list = ls()); gc()
if(!require("pacman")){install.packages("pacman")}

# load libraries
pacman::p_load(tidyverse, data.table, janitor, here, haven, magrittr, openxlsx, broom, fastDummies)

# load functions
source(here("dofile", "00_aux_functions.R"))

# load and proc data
data <- read_dta(here("data", "proc", "working_dataset_cohorte4m_2010.dta")) %>% 
        filter(entra_ES == 1 & 
               area_conocimiento_cat != 10 & 
               is.na(ptje_lect2m_alu) == 0 &
               is.na(ptje_mate2m_alu) == 0
         ) %>% select(-matches("cat")) %>%
    # mutate(dependencia_cat = dependencia_cat %>% as_factor(),
    #        rango_acreditacion_cat = rango_acreditacion_cat %>% as_factor(),
    #        area_conocimiento_cat = area_conocimiento_cat %>% as_factor())
        mutate(q_ptje_lect = ptje_lect2m_alu %>% ntile(5),
               q_ptje_mate = ptje_mate2m_alu %>% ntile(5),
               nem_interval = nem %>% cut(breaks = sprintf("%0.1f", c(4, seq(4.5, 6.5, 0.5), 7)), include.lowest = T),
               d_estudia_misma_region = ifelse(d_estudia_otra_region == 1, 0, 1),
               d_hombre_alu = ifelse(d_mujer_alu == 1, 0, 1),
               d_sede_no_RM = ifelse(d_sede_RM == 1, 0, 1)) %>%
        group_by(q_ptje_lect) %>%
        mutate(min_max_q_ptje_lect = paste0(min(ptje_lect2m_alu),"-", max(ptje_lect2m_alu))) %>%
        ungroup() %>%
        group_by(q_ptje_mate) %>%
        mutate(min_max_q_ptje_mate = paste0(min(ptje_mate2m_alu), "-", max(ptje_mate2m_alu)))

#data %>% tabyl(min_max_q_ptje_lect)
#data %>% tabyl(min_max_q_ptje_mate)

# on-time grad rates, <3-year program ----

on_time_3y_program <- data %>% filter(titulado_oportuno == 1 & duracion_total_anios <= 3)

all_3y_program <- data %>% filter(duracion_total_anios <= 3)

table_on_time_3y_program <- table_grad_rates(on_time_3y_program, all_3y_program)


# on-time grad rates, >4-year program ----

on_time_4y_program <- data %>% filter(titulado_oportuno == 1 & duracion_total_anios >= 4)

all_4y_program <- data %>% filter(duracion_total_anios >= 4)

table_on_time_4y_program <- table_grad_rates(on_time_4y_program, all_4y_program)


# total grad rates, <3-year program ----

total_3y_program <- data %>% filter(titulado == 1 & duracion_total_anios <= 3)

table_total_3y_program <- table_grad_rates(total_3y_program, all_3y_program)


# total grad rates, >4-year program ----

total_4y_program <- data %>% filter(titulado == 1 & duracion_total_anios >= 4)

table_total_4y_program <- table_grad_rates(total_4y_program, all_4y_program)


# final (non-formatted) table
final_table_grad_rates <- 
    table_on_time_3y_program %>%
    bind_cols(table_on_time_4y_program %>% select(-name)) %>%
    bind_cols(table_total_3y_program %>% select(-name)) %>%
    bind_cols(table_total_4y_program %>% select(-name))


# export on-time grad rates ----

# load existing file
excel_file <- loadWorkbook(here("results", "tables", "descriptive", "03_descriptive_table04_grad-rates-over-control-vars.xlsx"))

# pull all data from sheet
excel_table <- read.xlsx(excel_file, sheet = "table", startRow = 3) %>% head(-1)

# edit names
remove <- c("Sociodemográficas", 
            "Tipo de establecimiento", 
            "Quintil de Ingresos", 
            "Otras", 
            "Institucionales", 
            "Tipo de Institución", 
            "Tipo de acreditación institucional",
            "Área del Conocimiento",
            "Académicas",
            "SIMCE LENG",
            "SIMCE MAT",
            "NEM")
final_table_grad_rates <- 
    final_table_grad_rates %>% mutate(name = excel_table %>% filter(!name %in% remove) %>% pull(name))

# join
excel_table <-
excel_table %>% select(name) %>% left_join(y = final_table_grad_rates, by = "name")

# put the data back into the workbook
writeData(excel_file, sheet = "table", excel_table, startRow = 3)

# save to disk
saveWorkbook(excel_file, 
             here("results", "tables", "descriptive", "03_descriptive_table04_grad-rates-over-control-vars.xlsx"), 
             overwrite = TRUE)
