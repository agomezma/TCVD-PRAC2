
# Los dos ficheros deben estar en el mismo directorio donde se ejecuta el código en R

# Carga del fichero de vinos blancos y resumen de datos
cat("VINOS BLANCOS \n")
cat("Fichero de datos: winequality-white.csv  \n")
whitewines = read.csv("winequality-white.csv", header = TRUE, sep=";")
cat("Número de registros, número de campos \n")
dim(whitewines)
cat("Tipos de los campos \n")
sapply(whitewines, function(x) class(x))
cat("Primeras filas \n")
head(whitewines)
cat("Resumen \n")
summary(whitewines)
cat("\n\n")

# Carga del fichero de vinos tintos y resumen de dato
cat("VINOS TINTOS \n")
cat("Fichero de datos: winequality-red.csv  \n")
redwines = read.csv("winequality-red.csv", header = TRUE, sep=";")
cat("Número de registros, número de campos \n")
dim(redwines)
cat("Tipos de los campos \n")
sapply(redwines, function(x) class(x))
cat("Primeras filas  \n")
head(redwines)
cat("Resumen \n")
summary(redwines)
cat("\n\n")


# Se añade el campo color (colour) a los dos dataframes: 0 para los vinos blancos, 1 para los tintos

cat("VINOS BLANCOS \n")
whitewines = cbind(colour = rep(0L,nrow(whitewines)), whitewines)
cat("Número de registros, número de campos \n")
dim(whitewines)
cat("Nombres de los campos \n")
names (whitewines)
cat("Tipos de los campos \n")
sapply(whitewines, function(x) class(x))

cat("VINOS TINTOS \n")
redwines = cbind(colour = rep(1L,nrow(redwines)), redwines)

cat("Número de registros, número de campos \n")
dim(redwines)
cat("Nombres de los campos \n")
names (redwines)
cat("Nombres de los campos \n")
names (redwines)

# Integrar en un dataframe los dos anterioress
wines = rbind(whitewines, redwines) 

# Resumen del nuevo conjunto
cat("VINOS \n")
cat("Número de registros, número de campos \n")
dim(wines)
cat("Nombres de los campos \n")
names(wines)
cat("Tipos de los campos \n")
sapply(wines, function(x) class(x))

cat("Primeras filas  \n")
head(wines)
cat("Últimas filas  \n")
tail(wines)
cat("Resumen  \n")
summary(wines)



### Duplicated da los duplicados, no la primera aparición, y por eso no los veo en repetidos
cat("Numero de registros originales: ", nrow(wines), "\n")
sinrep = wines[!duplicated(wines),]
cat("Numero de registro sin repetir: ", nrow(sinrep), "\n")

repetidos = wines[duplicated(wines),]

cat("Numero de registro repetidos: ", nrow(repetidos), "\n")

x = nrow(repetidos)+nrow(sinrep) 

cat("Total (igual al número de registros originales): ", x, "\n")

cat("Porcentaje de repetidos: ", nrow(repetidos)/nrow(wines), "\n")

# Se añade un campo id
       
wines = cbind(id=rep(1:nrow(wines)), wines)
# Resumen del nuevo conjunto
cat("VINOS \n")
cat("Número de registros, número de campos \n")
dim(wines)
cat("Nombres de los campos \n")
names(wines)
cat("Tipos de los campos \n")
sapply(wines, function(x) class(x))

cat("Primeras filas  \n")
head(wines)
cat("Últimas filas  \n")
tail(wines)
cat("Resumen  \n")
summary(wines)

       
# para simplificar cuando tengamos que usar solo vinos blancos o tintos, definimos los data frame siguientes:
white_wines = wines[wines$colour==0,]
red_wines = wines[,][wines$colour==1,]


# Número de valores nulos en cada variable:
sapply(wines, function(x) sum(is.na(x)))

# Código
# Para que no salgan demasiado grandes, se divide el tamaño normal de un gráfico en una matriz de 3 por 3
# Representamos en una fila los histogramas del conjunto completo, solo de blancos y solo de tintos,
# y en la fila siguiente, el boxpolot correspondiente 
par(mfrow=c(3,3))
for (i in seq(3, 14)) {
    hist(wines[,i], xlab="", main=names(wines[i]))
    hist(white_wines[,i], xlab="", main=paste(names(wines[i]),"- white"))
    hist(red_wines[,i], xlab="", main=paste(names(wines[i]),"- red"))
    boxplot(wines[,i])
    boxplot(white_wines[,i])    
    boxplot(red_wines[,i]) 
}


cat ("Número de valores atípicos: \n")
for(i in seq(3, 13)) {
    cat("  Variable:",names(wines[i]), "\n")
    cat("     En total  : ", length(boxplot.stats(wines[,i])$out), "\n")
    cat("     En blancos: ", length(boxplot.stats(white_wines[,i])$out), "\n")
    cat("     En tintos : ", length(boxplot.stats(red_wines[,i])$out), "\n")
}

par(mfrow=c(3,3))
for (i in seq(3, 13)) {
   
    boxplot(wines[,i]~wines$quality, xlab = "Quality", ylab=names(wines[i]))
    boxplot(white_wines[,i]~white_wines$quality, xlab = "Quality - white", ylab=names(wines[i]))
    boxplot(red_wines[,i]~red_wines$quality, xlab = "Quality - red", ylab=names(wines[i]))

} 


# Exportación del conjunto de datos a un .csv

write.table(wines, file = "winequality.csv",col.names=TRUE, row.names=FALSE, sep=",")
vinos_tmp = read.csv("winequality.csv", header = TRUE)
cat("Número de registros, número de campos \n")
dim(vinos_tmp)
summary(vinos_tmp)

cat("Todos los vinos \n")
summary(wines[,3:14])
cat("Desviación típica:\n")
for (i in 3:14){ 
   cat(names(wines[i]),"  ", sd(wines[,i]), "\n")
}
cat("\n\n")
cat("Frecuencias absolutas por calidad:\n")
table(wines$quality)
bpt = barplot(table(wines$quality), main = "Distribución de vinos por calidad", xlab = "Quality")
text(bpt, table(wines$quality)+50, format(table(wines$quality)))

cat("Vinos blancos \n")
summary(white_wines[,3:14])
cat("Desviación típica:\n")
for (i in 3:14){ 
   cat(names(white_wines[i]),"  ", sd(white_wines[,i]), "\n")
}
cat("\n\n")
cat("Frecuencias absolutas por calidad:\n")
table(white_wines$quality)
bpt = barplot(table(white_wines$quality), col = "lightyellow", 
              main = "Distribución de vinos blancos por calidad", xlab = "Quality")
text(bpt, table(white_wines$quality)+50, format(table(white_wines$quality)))

cat("Vinos tintos \n")
summary(red_wines[,3:14])
cat("Desviación típica:\n")
for (i in 3:14){ 
   cat(names(red_wines[i]),"  ", sd(red_wines[,i]), "\n")
}
cat("\n\n")
cat("Frecuencias absolutas por calidad:\n")
table(red_wines$quality)
bpt = barplot(table(red_wines$quality), col = "red4", 
              main = "Distribución de vinos tintos por calidad", xlab = "Quality")
text(bpt, table(red_wines$quality)+30, format(table(red_wines$quality)))

cat("Frecuencias absolutas por calidad y tipo de vino (0: blanco, 1: tinto):\n")
table(wines$quality, wines$colour)

barplot(table(wines$quality, wines$colour), col = terrain.colors(7), beside = FALSE, main = "Número de vinos por calidad y color", names.arg = c("Blancos", "Tintos"), legend = seq(3,9))


par(mfrow=c(2,2))
for (i in 3:13) {
    qqnorm(white_wines[,i],main = paste(colnames(white_wines)[i], " - white"))
    qqline(white_wines[,i],col="red")
    qqnorm(red_wines[,i],main = paste(colnames(red_wines)[i], " - red"))
    qqline(red_wines[,i],col="red")
}


alfa = 0.05

for (i in 3:14) {
    cat("Variable: ", colnames(wines)[i],"\n")
    test = shapiro.test(white_wines[,i])
    cat("   Vinos blancos: \t")
    cat("   W: ", test[["statistic"]], "\t")
    cat("   p-value: ", test[["p.value"]], "\t")
    if (test[["p.value"]] < alfa)
        cat("      Los datos no siguen una normal \n")
    else
        cat("\n")
    test = shapiro.test(red_wines[,i])
    cat("   Vinos tintos: \t")
    cat("   W: ", test[["statistic"]], "\t")
    cat("   p-value: ", test[["p.value"]], "\t")
    if (test[["p.value"]] < alfa)
        cat("      Los datos no siguen una normal \n")
    else
        cat("*** \n")

}

library("nortest")

for (i in 3:14) {
    cat("Variable: ", colnames(wines)[i],"\n")
    test = lillie.test(white_wines[,i])
    cat("   Vinos blancos: \t")
    cat("   D: ", test[["statistic"]], "\t")
    cat("   p-value: ", test[["p.value"]], "\t")
    if (test[["p.value"]] < alfa)
        cat("      Los datos no siguen una normal \n")
    else
        cat("*** \n")
    test = lillie.test(red_wines[,i])
    cat("   Vinos tintos: \t")
    cat("   D: ", test[["statistic"]], "\t")
    cat("   p-value: ", test[["p.value"]], "\t")
    if (test[["p.value"]] < alfa)
        cat("      Los datos no siguen una normal \n")
    else
        cat("*** \n")

}



for (i in 3:14) {
    cat("Variable: ", colnames(wines)[i],"\n")
    test = fligner.test(list(white_wines[,i], red_wines[,i]))
    cat("   med chi-2: ", test[["statistic"]], "\t")
    cat("   p-value: ", test[["p.value"]], "\t")
    if (test[["p.value"]] < alfa)
        cat("      No homogeneidad de las varianzas \n")
    else
        cat("*** \n")
}

colores = ifelse(wines$colour==0, "yellow", "red4")

plot(wines[3:14],col = colores)


# vinos blancos

plot(white_wines[3:14], col = "yellow")




# vinos tintos

plot(red_wines[3:14], col = "red4")

library(corrgram)
corrgram(wines[3:14], order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Vinos")

corrgram(white_wines[3:14], order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Vinos blancos")

corrgram(red_wines[3:14], order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Vinos tintos")

cat("Conjunto completo de vinos \n")
for (i in 3:13) {
    for (j in (i+1):14) {

        test = cor.test(wines[,i], wines[,j], method="spearman", exact = FALSE)
      
        if (test[["p.value"]] >= alfa) {
            cat(names(wines[i]), " - ", names(wines[j]) )
            print(test)
            cat("     ****** \n")
        }
        
    }
}


Según los resultados del test,  la correlación de las siguientes variables es significativa:
volatile.acidity  -  alcohol
citric.acid  -  alcohol
residual.sugar  -  quality
free.sulfur.dioxide  -  density
density  -  pH
sulphates  -  alcohol

Sin embargo, los valores de rho son bastante bajos en todos los casos, muy alejados de los valores -1 o 1.

Vamos a ver los resultados con los conjuntos de datos separados (blancos y tintos).


cat("Conjunto de vinos blancos \n")
for (i in 3:13) {
    for (j in (i+1):14) {

        test = cor.test(white_wines[,i], white_wines[,j], method="spearman", exact = FALSE)
      
        if (test[["p.value"]] >= alfa) {                       
            print(test)
            cat(names(white_wines[i]), " - ", names(white_wines[j]) , "\n")
 
        }
        
    }
}


for (i in 3:13) {
    for (j in (i+1):14) {

        test = cor.test(red_wines[,i], red_wines[,j], method="spearman", exact = FALSE)
      
        if (test[["p.value"]] >= alfa) {
            cat(names(red_wines[i]), " - ", names(red_wines[j]), "\n" )
            print(test)
  
        }
        
    }
}


fit = lm(red_wines$quality ~ red_wines$density + red_wines$alcohol + red_wines$pH 
         + red_wines$volatile.acidity, data=redwines)
summary(fit) 




fit = lm(white_wines$quality ~ white_wines$density + white_wines$alcohol + white_wines$pH 
         + white_wines$volatile.acidity, data=whitewines)
summary(fit)


fit = lm(wines$quality ~ wines$density + wines$alcohol + wines$pH + wines$volatile.acidity, data=wines)
summary(fit) 



fit = lm(redwines$quality ~ redwines$residual.sugar + redwines$pH, data = redwines)
summary(fit)  
