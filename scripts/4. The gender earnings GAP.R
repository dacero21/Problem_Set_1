# 4. The gender earnings GAP ----------------------------------------------

## 4.1 libraries and environment -------------------------------------------

rm(list = ls())

library(dplyr)

## 4.2 Data frame and cleaning ------------------------------------------

df_geih = read_csv('stores/df_definitivo.csv')

# 4.a. Unconditional wage gap ---------------------------------------------

df_regresion = df_geih %>% 
  filter(!is.na(ingtot),
         !is.na(sex),
         age >= 18,
         ingtot > 0) %>% 
  mutate(log_salario = log(ingtot)) %>% 
  select(log_salario,sex)
  

lm(log_salario ~ !sex,data = df_regresion)


