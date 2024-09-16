rm(list = ls())

library(dplyr)
library(caret)
library(skimr)
library(rio)
library(readr)
library(tidyr)


# Datos -------------------------------------------------------------------

df_geih  <- read_csv('stores/df_definitivo.csv')
# df_geih = read_csv('stores/df_prepared-2.csv')

df_clean = df_geih %>% 
  filter(!is.na(y_salary_m_hu),
         !is.na(sex),
         age >= 18,
         dsi == 0)

df_regresion = df_clean %>% 
  filter(y_salary_m_hu > 0) %>% 
  mutate(log_salario = log(y_salary_m_hu),
         oficio = as.factor(oficio),
         maxEducLevel = as.factor(maxEducLevel),
         sex = ifelse(sex == 1,0,1),
         age2 = age*age) 

# División de los datos ---------------------------------------------------

df_regresion = df_clean %>% 
  filter(y_salary_m_hu > 0) %>% 
  mutate(log_salario = log(y_salary_m_hu),
         oficio = as.factor(oficio),
         maxEducLevel = as.factor(maxEducLevel),
         sex = ifelse(sex == 1,0,1),
         age2 = age*age) %>% 
  select(y_salary_m_hu,log_salario,sex,age,age2,oficio,microEmpresa,maxEducLevel, 
         totalHoursWorked) %>% 
  drop_na() 
  

df_regresion <- df_regresion %>% 
  mutate(sexResid = lm(sex ~ oficio + maxEducLevel + microEmpresa + age + age2,
                       data = df_regresion)$residuals,
         wageResid = lm(log_salario ~ oficio + maxEducLevel + microEmpresa + age + age2,
                        data = df_regresion)$residuals) 


set.seed(123)
inTrain <- createDataPartition(
  y = df_regresion$log_salario,
  p = .70,
  list = FALSE
)

data_train <- df_regresion %>% 
  filter(row_number() %in% inTrain)

data_testing  <- df_regresion %>% 
  filter(!row_number() %in% inTrain)



# Regresiones  ------------------------------------------------------------

lista_modelos <- list(lm(log_salario ~ age + age^2, data = data_train), #Modelo del punto 3
                   lm(log_salario ~ sex, data = data_train), #Modelo del punto 3 sencillo 
                   lm(wageResid ~ sexResid, data = data_train), #Modelo con FWÑ
                   lm(log_salario ~ sex + oficio + maxEducLevel + microEmpresa + age + age2,data = data_train),
                   lm(log_salario ~ age + age^2 + sex + maxEducLevel + totalHoursWorked, data = data_train ), 
                   lm(log_salario ~ age + age^2 + sex +  maxEducLevel + totalHoursWorked + oficio, data = data_train), 
                   lm(log_salario ~ age + age^2 + sex + age*sex + age^2*sex +  maxEducLevel + totalHoursWorked + oficio, data = data_train),
                   lm(log_salario ~ sex + poly(age,3,raw=TRUE):sex  +
                        maxEducLevel + poly(age,3,raw=TRUE):maxEducLevel + oficio*sex + oficio + poly(totalHoursWorked, 2, raw = TRUE):sex, data = data_train),
                   lm(log_salario ~ sex + poly(age,3,raw=TRUE):sex  +
                       poly(age,3,raw=TRUE):poly(maxEducLevel,3,raw=TRUE) + poly(maxEducLevel,3,raw=TRUE):sex + oficio*sex + oficio + poly(totalHoursWorked, 2, raw = TRUE):sex, data = data_train), 
                   lm(log_salario ~ sex + poly(age,3,raw=TRUE):sex  +
                        poly(age,3,raw=TRUE):poly(maxEducLevel,3,raw=TRUE) + poly(maxEducLevel,3,raw=TRUE):sex  +  oficio:totalHoursWorked + oficio:maxEducLevel + poly(totalHoursWorked, 2, raw = TRUE):sex, data = data_train))



#Se validan los MSE fuera de muestra

lista_mse <- sapply(lista_modelos, function(modelo) {
  predicciones <- predict(modelo, newdata = data_testing)
  mean((data_testing$log_salario - predicciones)^2)
})   

indice_mejor_modelo <- which.min(lista_mse)
mejor_modelo <- lista_modelos[[indice_mejor_modelo]]
predicciones_mejor_modelo <- predict(mejor_modelo, newdata = data_testing)
residuos <- data_testing$log_salario - predicciones_mejor_modelo

ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_histogram(binwidth = 0.1, fill = "navy", color = "navy", alpha = 0.7) +
  labs(title = "Distribución de los errores (residuos)",
       x = "Residuos",
       y = "Frecuencia") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# LOOCV -------------------------------------------------------------------

#Modelos con dos menores MSE
indice_mejor_modelo <- which.min(lista_mse)
mejor_modelo <- lista_modelos[[indice_mejor_modelo]]


ctrl <- trainControl(
  method = "LOOCV")

modelo1c <- train(mejor_modelo$terms,
                  data = df_regresion,
                  method = 'lm', 
                  trControl= ctrl)

score1c<-RMSE(modelo1c$pred$pred, df_regresion$log_salario)


indice_segundo_mejor_modelo <- order(lista_mse)[2]
segundo_mejor_modelo <- lista_modelos[[indice_segundo_mejor_modelo]]


modelo2c <- train(segundo_mejor_modelo$terms,
                  data = df_regresion,
                  method = 'lm', 
                  trControl= ctrl)
modelo2c

score2c<-RMSE(modelo2c$pred$pred, df_regresion$log_salario)

