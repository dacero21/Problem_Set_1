# 4. The gender earnings GAP ----------------------------------------------

## 4.1 libraries and environment -------------------------------------------

rm(list = ls())

library(dplyr)
library(readr)
library(stargazer)
library(boot)

## 4.2 Data frame and cleaning ------------------------------------------

df_geih = read_csv('stores/df_definitivo.csv')
# df_geih = read_csv('stores/df_prepared-2.csv')

df_clean = df_geih %>% 
  filter(!is.na(y_salary_m_hu),
         !is.na(sex),
         age >= 18,
         dsi == 0)


# 4.a. Unconditional wage gap ---------------------------------------------
 
df_regresion = df_clean %>% 
  filter(y_salary_m_hu > 0) %>% 
  mutate(log_salario = log(y_salary_m_hu),
         oficio = as.factor(oficio),
         maxEducLevel = as.factor(maxEducLevel),
         sex = ifelse(sex == 1,0,1),
         age2 = age*age) %>% 
  select(y_salary_m_hu,log_salario,sex,age,age2,oficio,microEmpresa,maxEducLevel) %>% 
  tidyr::drop_na()
  

reg_gender_gap = lm(log_salario ~ sex,data = df_regresion)


# 4.b. equal pay for equal work? ------------------------------------------

## 4.b.i FWL  --------------------------------------------------------------

normal_regression = lm(log_salario ~ sex + oficio + maxEducLevel + microEmpresa + age + age2,data = df_regresion)

df_residuals = df_regresion %>% 
  mutate(sexResid = lm(sex ~ oficio + maxEducLevel + microEmpresa + age + age2,
                       data = df_regresion)$residuals,
         wageResid = lm(log_salario ~ oficio + maxEducLevel + microEmpresa + age + age2,
                        data = df_regresion)$residuals)

FWL_regression = lm(wageResid ~ sexResid,df_residuals)

stargazer(normal_regression,FWL_regression,type="text",digits=7) # with stargazer we can visualize the coefficients next to each other

## 4.b.ii FWL and bootstrap ------------------------------------------------

conditional_wage_gap <- function(data,index){
  
  df_regresion = data %>% 
    filter(row_number() %in% index) 
  
  df_residuals = df_regresion %>% 
    mutate(sexResid = lm(sex ~ oficio + maxEducLevel + microEmpresa + age + age2,
                         data = df_regresion)$residuals,
           wageResid = lm(log_salario ~ oficio + maxEducLevel + microEmpresa + age ,
                          data = df_regresion)$residuals)
  
  coef(lm(wageResid ~ sexResid,df_residuals))[2] #returns the second coefficient of the linear regression
  
}

# conditional_wage_gap(df_regresion,1:nrow(df_regresion))

set.seed(1516)

bootstrap_wage_gap = boot(df_regresion, conditional_wage_gap, R = 1000)


# 4.c. Age -wage by gender ------------------------------------------------

ggplot(df_regresion,aes(x = age,y = log_salario,colour = as.factor(sex))) + 
  geom_point() + 
  stat_smooth(method = 'lm',formula = log_salario ~ age ,size = 1)  
  scale_colour_manual(values = c('black','blue')) + 
  labs(title = 'Age-wage curve by gender',
       x = 'Age',
       y = 'Log wage',
       subtitle = 'Estamation vs real') + 
  theme_classic()







