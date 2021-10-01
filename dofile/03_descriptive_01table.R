if(!require("pacman")){install.packages("pacman")}

# load libraries
pacman::p_load(tidyverse, data.table, janitor, here, haven, magrittr)

# load data
data <- read_dta(here("data", "proc", "working_dataset_cohorte4m_2010.dta"))

data_2 <- data %>% filter(entra_ES == 1)

# table
# graduation rates (outcomes) ----
graduation <-
data_2 %>% filter(duracion_total_anios <= 3) %>% 
        summarise(tasa_titulacion_3a_pct = mean(titulado) * 100,
                  tasa_titulacion_3a_n = sum(titulado),
                  tasa_titulacion_op_3a_pct = mean(titulado_oportuno) * 100,
                  tasa_titulacion_op_3a_n = sum(titulado_oportuno)) %>%
    bind_cols(

data_2 %>% filter(duracion_total_anios > 3) %>%
        summarise(tasa_titulacion_4a_pct = mean(titulado) * 100,
                  tasa_titulacion_4a_n = sum(titulado),
                  tasa_titulacion_op_4a_pct = mean(titulado_oportuno) * 100,
                  tasa_titulacion_op_4a_n = sum(titulado_oportuno)))

data_2 %>% filter(duracion_total_anios > 3) %>% pull(titulado) %>% prop.table()
graduation_section <-
graduation %>% select(matches("pct$")) %>% 
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "pct") %>%
    bind_cols(
        
graduation %>% select(matches("n$")) %>%
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "n") %>% select(-"variable")) %>%
    arrange(variable)


# dat %>%
#     summarise_all(list(mean = mean,sum  = sum)) %>%
#     tidyr::pivot_longer(cols = everything(),
#                         names_sep = "_",
#                         names_to  = c("variable", ".value"))

# A tibble: 5 x 3
#  variable  mean   sum
#  <chr>    <dbl> <int>
#1 V1        10.5   210
#2 V2        30.5   610
#3 V3        50.5  1010
#4 V4        70.5  1410
#5 V5        90.5  1810





# demographic ----
n_obs <- data_2 %>% tally() %>% pull()

# set value labels as values
data_2 <- data_2 %>% mutate(dependencia_cat = as_factor(dependencia_cat),
                            q_nse = as_factor(q_nse))

data_2 %>% group_by(dependencia_cat) %>% 
    summarise(pct = n() / n_obs * 100,
              n = n()) %>% 
    ungroup() %>%
    rename(variable = dependencia_cat) %>%
    
    bind_rows(

data_2 %>% group_by(q_nse) %>%
    summarise(pct = n() / n_obs * 100,
              n = n()) %>%
    ungroup() %>%
    rename(variable = q_nse))

data_2 %>% summarise(mujer_pct = sum(d_mujer_alu) / n_obs * 100,
                     mujer_n = sum(d_mujer_alu),
                     otra_region_pct = sum(d_estudia_otra_region) / n_obs * 100,
                     otra_region_n = sum(d_estudia_otra_region))

# institutional ----

data_2 %>% group_by(tipo_inst_3) %>% 
    summarise(tipo_inst_pct = n() / n_obs * 100,
              tipo_inst_n = n()) %>%
    ungroup()

data_2 %>% summarise(sede_RM_pct = sum(d_sede_RM) / n_obs * 100,
                     sede_RM_n = n())


data_2 %>% group_by(rango_acreditacion_cat) %>% 
    summarise(tipo_acreditacion_pct = n() / n_obs * 100,
              tipo_acreditacion_n = n()) %>%
    ungroup()

data_2 %>% group_by(area_conocimiento_cat) %>% 
    summarise(area_conocimiento_pct = n() / n_obs * 100,
              tipo_acreditacion_n = n()) %>%
    ungroup()

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
