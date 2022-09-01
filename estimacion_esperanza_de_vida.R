library(plm)
library(car)
library(readr)
library(knitr)
library(dplyr)
library(lmtest)
library(GGally)
library(tseries)
library(ggplot2)
library(tidyverse)
library(gridExtra)

datos <- read_csv("C:/Users/Abelino/Downloads/datos5.csv")
attach(datos)

datos <- datos %>% group_by(Country) %>%
  mutate(años = sum(Year)) %>%
  filter(años == 32120)


datos2014 <- datos %>% group_by(Country) %>%
  filter(Year==2014)

datos2000 <- datos %>% group_by(Country) %>%
  filter(Year==2000)


reg2014 <- lm(Life_expectancy_~Status, data=datos2014)
summary(reg2014)

reg2000 <- lm(Life_expectancy_~Status, data=datos2000)
summary(reg2000)

datos2014ende <- datos2014 %>% group_by(Country) %>%
  filter(Status == 0)

datos2014de  <- datos2014 %>% group_by(Country) %>%
  filter(Status == 1)


datos2000ende <- datos2000 %>% group_by(Country) %>%
  filter(Status == 0)

datos2000de  <- datos2000 %>% group_by(Country) %>%
  filter(Status == 1)


borrar <- c("Country","_HIV/AIDS","Diphtheria_","años","Year","Status","Alcohol","Measles_","under-five_deaths_","Population","_thinness__1-19_years","_thinness_5-9_years")
datos1 <- datos2014[, !(names(datos2014) %in% borrar)]
datos2 <- datos2000[, !(names(datos2000) %in% borrar)]

datos2014ende <- datos2014ende[, !(names(datos2014ende) %in% borrar)]
datos2000ende <- datos2000ende[, !(names(datos2000ende) %in% borrar)]
datos2014de <- datos2014de[, !(names(datos2014de) %in% borrar)]
datos2000de <- datos2000de[, !(names(datos2000de) %in% borrar)]

ggpairs(datos1, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggpairs(datos2, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


##Regresiones países 2014 
reg2014ende <- lm(Life_expectancy_~infant_deaths+percentage_expenditure+
                    Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
                    `_BMI_`+Income_composition_of_resources, data=datos2014ende)

summary(reg2014ende)


reg2014de <- lm(Life_expectancy_~infant_deaths+percentage_expenditure+
                    Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
                    `_BMI_`+Income_composition_of_resources, data=datos2014de)

summary(reg2014de)


## Regresiones países 2000
reg2000ende <- lm(Life_expectancy_~infant_deaths+percentage_expenditure+
                    Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
                    `_BMI_`+Income_composition_of_resources, data=datos2000ende)

summary(reg2000ende)

reg2000de <- lm(Life_expectancy_~infant_deaths+percentage_expenditure+
                  Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
                  `_BMI_`+Income_composition_of_resources, data=datos2000de)

summary(reg2000de)

##Intervalo de confianza 2014
confint(lm(Life_expectancy_~infant_deaths+percentage_expenditure+
             Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
             `_BMI_`+Income_composition_of_resources, data=datos2014ende))

confint(lm(Life_expectancy_~infant_deaths+percentage_expenditure+
             Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
             `_BMI_`+Income_composition_of_resources, data=datos2014de))

##Intervalo de confianza 2000
confint(lm(Life_expectancy_~infant_deaths+percentage_expenditure+
             Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
             `_BMI_`+Income_composition_of_resources, data=datos2000ende))

confint(lm(Life_expectancy_~infant_deaths+percentage_expenditure+
             Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
             `_BMI_`+Income_composition_of_resources, data=datos2000de))

##Minimos cuadrados generalizados 
#2014 en desarrollo 
wg <- 1/lm(abs(reg2014ende$residuals)~reg2014ende$fitted.values)$fitted.values^2
wls_model <- lm(Life_expectancy_~infant_deaths+percentage_expenditure+
                  Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
                  `_BMI_`+Income_composition_of_resources, data=datos2014ende, weights=wg) 
summary(wls_model)

#2000 en desarrollo
wt <- 1/lm(abs(reg2000ende$residuals)~reg2000ende$fitted.values)$fitted.values^2
wlt_model <- lm(Life_expectancy_~infant_deaths+percentage_expenditure+
                  Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
                  `_BMI_`+Income_composition_of_resources, data=datos2000ende, weights=wt) 
summary(wlt_model)



##Homocedasticidad datos 2014
bptest(reg2014ende)
bptest(reg2014de)
##Homocedasticidad datos 2000
bptest(reg2000ende)
bptest(reg2000de)

##Homocedasticidad MCG
#2014
bptest(wls_model)
#2010
bptest(wlt_model)

##No autocorrelacion
#datos 2014
dwt(wls_model, alternative = "two.sided")
dwt(reg2014de, alternative = "two.sided")
#datos 2000
dwt(wlt_model, alternative = "two.sided")
dwt(reg2000de, alternative = "two.sided")

##Normalidad
#datos 2014
jarque.bera.test(wls_model$residuals)
jarque.bera.test(reg2014de$residuals)
#datos 2000
jarque.bera.test(wlt_model$residuals)
jarque.bera.test(reg2000de$residuals)


##Intervalos de confianza para MCO
#2014 en desarrollo
confint(lm(Life_expectancy_~infant_deaths+percentage_expenditure+
             Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
             `_BMI_`+Income_composition_of_resources, data=datos2014ende, weights=wg))
#2000 en desarrollo
confint(lm(Life_expectancy_~infant_deaths+percentage_expenditure+
             Hepatitis_B+Polio+Total_expenditure+Adult_Mortality+
             `_BMI_`+Income_composition_of_resources, data=datos2000ende, weights=wt))


##Datos 2014
plot1 <- ggplot(data = datos2014ende, aes(infant_deaths, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos2014ende, aes(percentage_expenditure, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos2014ende, aes(Hepatitis_B, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos2014ende, aes(Polio, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = datos2014ende, aes(Total_expenditure, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot6 <- ggplot(data = datos2014ende, aes(Diphtheria_, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot7 <- ggplot(data = datos2014ende, aes(Adult_Mortality, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot8 <- ggplot(data = datos2014ende, aes(`_BMI_`, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot9 <- ggplot(data = datos2014ende, aes(Income_composition_of_resources, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4,plot5,plot6,plot7,plot8,plot9)

plot10 <- ggplot(data = datos2014ende, aes(reg2014ende$fitted.values, reg2014ende$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot10)



bptest(reg2014ende)

vif(reg2014ende)
