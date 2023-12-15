# The original data set it was upload in https://datos.gob.ar/series/api/series/?ids=364.3_INDUSTRIARIA__9
# In this practice: linear regression, multilinear regression, logistic regression


setwd("C:/Users/marie/Documents/Mariela/data_science")
gn_produccion <- read.csv("produccion_gas_sin_indice_tiempo.csv", header=TRUE, sep=",", dec=".")

head(gn_produccion)
tail(gn_produccion)

dim(gn_produccion) # [1] 324   11
names(gn_produccion) # 

summary(gn_produccion)
# boxplots
boxplot(gn_produccion,
        ylab="m³",
        main="Boxplot total",
        col=rainbow(12),
        yaxp = c(0, 4500000, 2))
boxplot(gn_produccion[,1], ylab="m³", main="Boxplot producción de gas natural", col=rainbow(12))
boxplot(gn_produccion[,2:11],
        ylab="m³",
        main="Boxplot distribuidoras de gas natural",
        col=rainbow(12),
        yaxp = c(0, 900000, 2))

hist(gn_produccion$produccion_gas_natural, col=gray.colors(9), main="Histograma produccion de gas natural", xlab="m³")
# densidad de probabilidad
hist(gn_produccion$produccion_gas_natural, prob = TRUE)
# densidad de probabilidad en forma de curva
plot(density(gn_produccion$produccion_gas_natural))

pairs(gn_produccion, pch=".")

# regresion lineal
plot(produccion_gas_natural ~ metrogas, data = gn_produccion,
     xlab = "Metrogas",
     ylab = "Producción gas natural",
     main = "Produccion gas natural ~ Metrogas",
     pch = 20,
     cex = 2,
     col = "gray")

# modelo lineal
model_lm = lm(produccion_gas_natural ~ metrogas, data = gn_produccion)
model_lm

#fit
abline(model_lm, col = "darkorange")

#residuos
model_lm$residuals

#sumario fit
summary(model_lm)

 #grafico 1 residuals vs fitted
plot(model_lm ,which = 1)
#grafico 2 Normal Q-Q
plot(model_lm ,which = 2)
# grafico 3 scale-location
plot(model_lm ,which = 3)
# grafico 4 residuals vs leverage 
plot(model_lm ,which = 4)
# grafico 5 distancia de Cook
plot(model_lm ,which = 5)

# análisis de los residuos
plot(fitted(model_lm), model_lm$residuals)
abline(0,0)

confint(model_lm, level = 0.99)

# regresion lineal multiple
model_lmm = lm(produccion_gas_natural ~ metrogas + distrib_gas_del_centro_ecogas, data = gn_produccion)
coef(model_lmm)
summary(model_lmm)
plot(model_lmm)

model2_lmm = lm(produccion_gas_natural ~ gasnea + camuzzi_gas_pampeana + metrogas + distrib_gas_cuyana_ecogas, data = gn_produccion)
coef(model2_lmm)
summary(model2_lmm)
plot(model2_lmm)

# https://book.stat420.org/analysis-of-variance.html
#grafico
rango_metrogas <- range(gn_produccion$metrogas)
nuevos_valores_metrogas <- seq(from = rango_metrogas[1], to = rango_metrogas[2],
                               length.out = 50)
rango_distrib_gas_del_centro_ecogas <- range(gn_produccion$distrib_gas_del_centro_ecogas)
nuevos_valores_distrib_gas_del_centro_ecogas <- seq(from = rango_distrib_gas_del_centro_ecogas[1], to = rango_distrib_gas_del_centro_ecogas[2],
                                                    length.out = 50)
predicciones <- outer(X = nuevos_valores_metrogas, Y = nuevos_valores_distrib_gas_del_centro_ecogas,
                      FUN = function(metrogas, distrib_gas_del_centro_ecogas) {
                        predict(object = model_lmm,
                                newdata = data.frame(metrogas, distrib_gas_del_centro_ecogas))
                      })
superficie <- persp(x = nuevos_valores_metrogas, y = nuevos_valores_distrib_gas_del_centro_ecogas,
                    z = predicciones,
                    theta = 20, phi = 5,
                    col = "lightblue", shade = 0.1,
                    zlim = range(2000000,6000000),
                    xlab = "Metrogas", ylab = "Ecogas", zlab = "Produccion gas natural",
                    ticktype = "detailed",
                    main = "Predición produccion de gas natural ~ Metrogas y Ecogas"
)
observaciones <- trans3d(gn_produccion$metrogas, gn_produccion$distrib_gas_del_centro_ecogas, gn_produccion$produccion_gas_natural,
                         superficie)
error <- trans3d(gn_produccion$metrogas, gn_produccion$distrib_gas_del_centro_ecogas, fitted(model_lmm), superficie)
points(observaciones, col = "red", pch = 16)
segments(observaciones$x, observaciones$y, error$x, error$y)


# clase 6 multicolinealidad
library(corrplot)
M = cor(gn_produccion)
corrplot.mixed(M, order = "AOE")

# calcular y analizar VIF
library(car)
vif_values <- vif(model2_lmm)
barplot(vif_values , main = "VIF Values", horiz = TRUE, col = "steelblue", xpd = TRUE)
abline(v = 5, lwd = 3, lty = 2)

reg = lm(produccion_gas_natural ~ poly(distrib_gas_del_centro_ecogas,2), data=gn_produccion)
reg
plot(produccion_gas_natural ~ distrib_gas_del_centro_ecogas, data = gn_produccion)
pred = predict(reg)

newdata = data.frame(hp = seq(min(gn_produccion$distrib_gas_del_centro_ecogas),
                             + max(gn_produccion$distrib_gas_del_centro_ecogas), length.out = 100))
pred = predict(reg)
plot(produccion_gas_natural ~ distrib_gas_del_centro_ecogas, data = gn_produccion)
with.default(newdata, points(x = gn_produccion$distrib_gas_del_centro_ecogas, y = pred, col="red"))

#grafico 1 residuals vs fitted
plot(reg ,which = 1)
#grafico 2 Normal Q-Q
plot(reg ,which = 2)
# grafico 3 scale-location
plot(reg ,which = 3)
# grafico 4 residuals vs leverage 
plot(reg ,which = 4)
# grafico 5 distancia de Cook
plot(reg ,which = 5)

#regresion polinomica
gn2_produccion <- read.csv("produccion_gas_con_indice_tiempo.csv", header=TRUE, sep=",", dec=".")
head(gn2_produccion)
fit_glm = glm(indice_tiempo ~ produccion_gas_natural, data = gn2_produccion, family = binomial)
fit_lm = lm(indice_tiempo ~ produccion_gas_natural, data = gn2_produccion)
#Ahora graficamos la data
rho <- function(produccion_gas_natural) predict(fit_glm, data.frame(produccion_gas_natural), type = "response")
plot(indice_tiempo ~ produccion_gas_natural, data = gn2_produccion,
     pch = 20, ylab = "Estimated Probability",
     main = "Ordinary vs Logistic Regression")
grid()
curve(rho, add = TRUE, col = "dodgerblue", lty = 2)

legend("topleft", c("Ordinary", "Logistic", "Data"), lty = c(1, 2, 0),
       pch = c(NA, NA, 20), lwd = 2, col = c("darkorange", "dodgerblue", "black"))
abline(fit_lm, col = "darkorange")

# Regularizacion - Clase 6.1 7.1  pag 60
library(glmnet)
index <- sample(nrow(gn_produccion),nrow(gn_produccion)*0.80) #80-20 split
gn_produccion.train <- gn_produccion[index,]
gn_produccion.test <- gn_produccion[-index,]
X.train<- as.matrix(gn_produccion.train)
Y.train<- gn_produccion[index, "produccion_gas_natural"]
cv.lasso<- cv.glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1, nfolds = 10)
plot(cv.lasso)