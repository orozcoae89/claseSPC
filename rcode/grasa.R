rm(list = ls())

library(agricolae)
grasa <- c(2.7, 3.4, 3.5, 4.0, 3.1, 3.3, 3.5, 3.3, 3.2, 3.4, 2.6, 3.1, 
3.4, 2.7, 3.3, 3.6, 2.9, 2.8, 3.0, 3.6, 3.5, 2.8, 3.1, 2.8, 
2.2, 3.4, 3.3, 2.5, 3.4, 2.7, 2.9, 3.6, 3.3, 2.7, 3.7, 3.3, 
3.2, 3.1, 2.9, 2.7, 3.3, 3.6, 3.3, 3.1, 3.1, 3.4, 3.0, 3.5,
3.4, 3.0, 2.9, 3.2, 3.2, 3.0, 3.3, 3.9, 3.3, 3.0, 3.0, 3.5, 
2.9, 3.5, 3.1, 3.5, 3.0, 3.1, 2.9, 3.1, 3.1, 2.9, 2.9, 3.4,
3.4, 3.1, 3.2, 3.3, 3.2, 3.3, 3.0, 3.2, 3.5, 3.4, 3.8, 3.2,
2.9, 3.0, 3.2, 3.2, 3.3, 3.8)

n <- length(grasa)
est_des <- c(mean = mean(grasa), median = median(grasa),
             stat.dev = sd(grasa), IQr = IQR(grasa),
             MIN = min(grasa),MAX = max(grasa)
)
est_des
aux <- hist(grasa, plot = FALSE)
tabla_agrupada <- table.freq(aux)
tabla_agrupada

## estimaciones para datos agrupados
media_agrupados <- sum(tabla_agrupada$Main*tabla_agrupada$Frequency)/n
nn <- ceiling(n/2)
ID <- min(which(tabla_agrupada$CF > nn))
amp <- tabla_agrupada[ID,][2]-tabla_agrupada[ID,][1]
mediana_agrupados <- tabla_agrupada[ID,][1]+
  ((n/2-tabla_agrupada[ID-1,][6])/(tabla_agrupada[ID,][4]))*amp
IDm <- which(tabla_agrupada$Frequency==max(tabla_agrupada$Frequenc))
Da <- tabla_agrupada[IDm,][4]-tabla_agrupada[IDm-1,][4]
Db <- tabla_agrupada[IDm,][4]-tabla_agrupada[IDm+1,][4]
moda_agrupados <- tabla_agrupada[IDm,][1]+((Da)/(Da+Db))*amp
c(media=media_agrupados,mediana=as.numeric(mediana_agrupados),
  moda=as.numeric(moda_agrupados))

hist(grasa, 
     freq = FALSE, # parámetro de densidad
     xlab = "% de grasa",
     ylab = "Densidad", 
     main = "% de grasa de la leche (Densidad)",
     xlim = c(1.5, 5)
) 
lines(density(grasa), col = "red", lwd=3) # traza densidad
abline(v=3, lwd=3, col="blue", lty="dashed")
