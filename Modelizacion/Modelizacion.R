if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

if (!require("gap")){
  install.packages("gap")
  library(gap)
}

if (!require("xlsx")){
  install.packages("xlsx")
  library(xlsx)
}

###### Cargar datos 

setwd("~/Desktop/TFM/Modelizacion")
      
datos=read.xlsx("/home/dsc/Desktop/TFM/DATA/Dataset_limpio1.xlsx", sheetIndex = 1)  

str(datos) 
head(datos)
summary(datos)
#Aunque hay ciertos outliers en general tiene bastante sentido a primera vista

###### regresión lineal

modeloInd1=lm(BMI ~ Weight, data = datos)
summary(modeloInd1)

#He realizado un pequeño modelo para comprobar BMI con peso y estadísticamente tiene sentido.
#Verlo graficamente
ggplot(datos, aes(x = Weight, y = BMI)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)


#vamos a compararlo ahora con la altura

modeloInd2=lm(BMI ~ Height, data = datos)
summary(modeloInd2)

ggplot(datos, aes(x = Height, y = BMI)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

#Si lo comparamos como el peso veremos que el peso es mucho mas indicativo que la altura.

#Un tercer modelo con una variable que no"deberia" tener mucha relación

modeloInd3=lm(BMI ~ breakfast, data = datos)
summary(modeloInd3)

ggplot(datos, aes(x = Height, y = BMI)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)


## Representación de todos los modelos sobre BMI

modeloGlobal=lm(BMI ~ ., data = datos)
summary(modeloGlobal)

#Las variables estadisticamente mas altas son con las que se calcula el BMI de una persona

#Por curiosidad vamos hacerlo con una variable de comida

modeloGlobal2=lm(Meat ~ ., data = datos)
summary(modeloGlobal2)
# En efecto , tienen relación con el resto variables de la alimentacion.

#Tambien voy hacerlo con una varible de datos que se obtienen en analisis o pruebas medias.

modeloGlobal3=lm(LDL ~ ., data = datos)
summary(modeloGlobal3)
#Estadísticamente tiene relación con sus variables de caracter similar.


##### Comparación de modelos

anova(modeloInd1,modeloGlobal)

##### Modelo con variables mas importantes

modeloMultiple=lm(BMI ~ WC+HIP+WHR, data = datos)
summary(modeloMultiple)

#Vovlemos a comparar modelos
anova(modeloInd1,modeloMultiple) 
anova(modeloGlobal, modeloMultiple)
# Es mas recomendable el modelomultiple y mejoramos los anteriores

######Analizar el modelo

modeloFinal=lm(BMI ~ WC+HIP+WHR, data = datos)
summary(modeloFinal)
plot(modeloFinal$residuals)
hist(modeloFinal$residuals)
qqnorm(modeloFinal$residuals); qqline(modeloFinal$residuals,col=2)
confint(modeloFinal,level=0.95)

cor(modeloFinal$residuals,datos$WC)
cor(modeloFinal$residuals,datos$HIP)
cor(modeloFinal$residuals,datos$WHR)

boxplot(modeloFinal$residuals~datos$WHR)
aggregate(modeloFinal$residuals~datos$WHR,FUN=mean)

shapiro.test(modeloFinal$residual)

anova(modeloFinal,modeloGlobal) # Vemos que hay diferencia entre modelos

##### modelo con WHR

modeloWHR=lm(WHR ~ ., data = datos)
summary(modeloWHR)

modeloBMI=lm(BMI ~ ., data = datos)
summary(modeloBMI)
#Dos variables que mas relacion tiene son con lo que se obtiene WHR


##### Comrpobar con stepwise cual es el modelo que me recomienda que es mas util.

ModelAutoBackward=step(modeloBMI,direction="backward",trace=1)
summary(ModelAutoBackward)

anova(ModelAutoBackward, modeloBMI)

# nos recomienda dos modelos con las variables sin apenas quitar variables
#BMI ~ Height + Weight + WC + HIP + SBP + TG + HDL + Fruit + beverage + breakfast

