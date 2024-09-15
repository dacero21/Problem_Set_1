# 4. The gender earnings GAP ----------------------------------------------

## 4.1 libraries and environment -------------------------------------------

rm(list = ls())

library(dplyr)
library(readr)

## 4.2 Data frame and cleaning ------------------------------------------

df_geih = read_csv('stores/df_definitivo.csv')
# df_geih = read_csv('stores/df_prepared-2.csv')

df_clean = df_geih %>% 
  filter(!is.na(ingtot),
         !is.na(sex),
         age >= 18,
         dsi == 0)


# 4.a. Unconditional wage gap ---------------------------------------------
 
df_regresion = df_clean %>% 
  filter(ingtot > 0) %>% 
  mutate(log_salario = log(ingtot)) %>% 
  select(ingtot,log_salario,sex,age,oficio,microEmpresa,maxEducLevel)
  

lm(log_salario ~ !sex,data = df_regresion)


# 4.b. equal pay for equal work? ------------------------------------------

## 4.b.i FWL  --------------------------------------------------------------

normal_regression = lm(log_salario,data = df_regresion)

# 4.b.ii FWL and bootstrap ------------------------------------------------










