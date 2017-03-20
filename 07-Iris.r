### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 7: Große Datensätze: iris ##########################################

# 7.1 Datensatz
# 7.2 Subsets (Teildatensätze, indizieren)
# 7.3 Graphiken
# 7.4 EDA - Maßzahlen, Boxplots
# 7.5 Histogramme, QQ-Plots


# 7.1 Datensatz ----------------------------------------------------------------
# Integrierter Datensatz mit Messungen von Kronblättern (Petalen;oben; oft groß)
# und Kelchblättern (Sepalen; unten; meist kleiner) verschiedener Iris Spezies
iris                              # Sepalen bei Schwertlilien größer als Petalen
class(iris)
str(iris)
summary(iris)
pairs(iris, col=iris$Species)

library(psych)
pairs.panels(iris[,1:4])



# 7.2 Subsets (Teildatensätze, indizieren) -------------------------------------
# Anzeige + Verarbeitung ausgewählter Daten (Siehe Kapitel 3)
iris$Petal.Length                  # Spalte "Petal.Length" als Vektor
iris["Petal.Length"]               # Spalte "Petal.Length" als Spalte
iris[1:50,"Petal.Length"]          # Zeilen 1 bis 50 der Spalte "Petal.Length" 
iris[iris$Species == "setosa",]    # Datensatz für die Species Iris setosa
iris$Petal.Length[iris$Species=="virginica"] # Petal.Length von Iris virginica



# 7.3 Graphiken ----------------------------------------------------------------
# Petal.Length (y) gegen Petal.Width (x)
plot(iris$Petal.Width, iris$Petal.Length, main = "Iris Petal-Dimensionen", 
xlab = "Breite der Petalen [cm]", ylab = "Länge der Petalen [cm]",
col = ifelse(iris$Species=="setosa", "red", "blue" ))

# allen drei Arten verschiedene Farben zuordnen
plot(iris$Petal.Width, iris$Petal.Length, col=iris$Species) 
plot(sort(iris$Petal.Width), sort(iris$Petal.Length), col=iris$Species)

# allen drei Arten verschiedene ausgewählte Farben zuordnen
plot(iris$Petal.Width, iris$Petal.Length, col=ifelse(iris$Species == "setosa",
"red", ifelse(iris$Species == "versicolor", "blue", "green" )))

# Zuordnungen zu Kürzeln für schnelles Plotten
xl <- "Breite der Petalen [cm]"      ;    yl <- "Länge der Petalen [cm]"
sl.seto <- iris$Sepal.Length[iris$Species == "setosa"]
sw.seto <- iris$Sepal.Width [iris$Species == "setosa"]
pl.seto <- iris$Petal.Length[iris$Species == "setosa"]
pw.seto <- iris$Petal.Width [iris$Species == "setosa"]
mn.seto <- "Iris Petalen\nI. setosa"
sl.vers <- iris$Sepal.Length[iris$Species == "versicolor"]
sw.vers <- iris$Sepal.Width [iris$Species == "versicolor"]
pl.vers <- iris$Petal.Length[iris$Species == "versicolor"]
pw.vers <- iris$Petal.Width [iris$Species == "versicolor"]
mn.vers <- "Iris Petalen\nI. versicolor"    
sl.virg <- iris$Sepal.Length[iris$Species == "virginica"]
sw.virg <- iris$Sepal.Width [iris$Species == "virginica"]
pl.virg <- iris$Petal.Length[iris$Species == "virginica"]
pw.virg <- iris$Petal.Width [iris$Species == "virginica"]
mn.virg <- "Iris Petalen\nI. virginica"
# Anmerkung: mit der Funktion by() ließen sich einige Zuordnungen umgehen.

#	mehrere Graphiken auf einem Blatt: relative Größen
layout(matrix(1:3, 1, 3), widths=c(5, 5, 5))
plot(pw.seto, pl.seto , main=mn.seto, xlab=xl, ylab=yl, ylim=c(1,7))
plot(pw.vers, pl.vers , main=mn.vers, xlab=xl, ylab=yl, ylim=c(1,7))
plot(pw.virg, pl.virg , main=mn.virg, xlab=xl, ylab=yl, ylim=c(1,7))

#	mehrere Graphiken auf einem Blatt: absolute Größen (in cm)
# zB. für quadratische plots. Grafikfenster ausreichend groß ziehen!
layout(matrix(1:3, 1 ,3), widths=lcm(c(5, 5, 5)), heights= lcm(5)) 
plot(pw.seto, pl.seto , main=mn.seto, xlab=xl, ylab=yl, ylim=c(1,7))
plot(pw.vers, pl.vers , main=mn.vers, xlab=xl, ylab=yl, ylim=c(1,7))
plot(pw.virg, pl.virg , main=mn.virg, xlab=xl, ylab=yl, ylim=c(1,7))

# Zurücksetzen der Graphik
dev.off()



# 7.4 EDA - Maßzahlen, Boxplots ------------------------------------------------
#	5-Zahlenmaß + mean für jeden Parameter, nach Iris-Arten getrennt   
summary(iris[iris$Species == "setosa",])
summary(iris[iris$Species == "versicolor",])
summary(iris[iris$Species == "virginica",])

#	Boxplot-Vergleiche (zwischen den 3 Iris-Arten) pro Parameter
par(mfrow = c(2,2))
boxplot(sl.seto, sl.vers, sl.virg, notch = T, ylab = "Länge der Sepalen [cm]", 
names = c("I.setosa", "I.versicolor", "I.virginica"), ylim=c(0,8))
means <- data.frame(t(c(mean(sl.seto),mean(sl.vers),mean(sl.virg))))
boxplot(means, add=TRUE, border=2, names=c("", "", ""))

boxplot(sw.seto, sw.vers, sw.virg,  notch = T, ylab = "Breite der Sepalen [cm]",
names = c("I.setosa", "I.versicolor", "I.virginica"), ylim=c(0,8))
means <- data.frame(t(c(mean(sw.seto),mean(sw.vers),mean(sw.virg))))
boxplot(means, add=TRUE, border=2, names=c("", "", ""))

boxplot(pl.seto, pl.vers, pl.virg, notch = T, ylab = "Länge der Petalen [cm]", 
names = c("I.setosa", "I.versicolor", "I.virginica"), ylim=c(0,8))
means <- data.frame(t(c(mean(pl.seto),mean(pl.vers),mean(pl.virg))))
boxplot(means, add=TRUE, border=2, names=c("", "", ""))

boxplot(pw.seto, pw.vers, pw.virg,  notch = T, ylab = "Breite der Petalen [cm]",
names = c("I.setosa", "I.versicolor", "I.virginica"), ylim=c(0,8))
means <- data.frame(t(c(mean(pw.seto),mean(pw.vers),mean(pw.virg))))
boxplot(means, add=TRUE, border=2, names=c("", "", ""))

par(mfrow = c(1,3))
boxplot(iris[iris$Species=="setosa",1:4], ylim=c(0,8), main="Iris setosa")
boxplot(iris[iris$Species=="versicolor",1:4],ylim=c(0,8),main="Iris versicolor")
boxplot(iris[iris$Species=="virginica",1:4], ylim=c(0,8), main="Iris virginica")
par(mfrow = c(1,1))


# Zusammenhänge zwischen Elemente in großen Datensätzen
pairs(iris, pch=16, cex=0.8, col=ifelse(iris$Species=="setosa",
"red", ifelse(iris$Species=="versicolor", "blue", "green")))



# 7.5 Histogramme, QQ-Plots ----------------------------------------------------
#	Histogramme des Parameters Sepal.Length
par(mfrow=c(1,3))
sl <- "Länge der Sepalen [cm]"
hist(sl.seto, xlab=sl, ylab = "Häufigkeit", main = "I. setosa",
xlim=c(4,8), ylim=c(0,18), breaks=seq(4, 8, 0.25)) 
hist(sl.vers, xlab=sl, ylab = "Häufigkeit", main = "I. versicolor",
xlim=c(4,8), ylim=c(0,18), breaks=seq(4, 8, 0.25))
hist(sl.virg, xlab=sl, ylab = "Häufigkeit", main = "I. virginica",
xlim=c(4,8), ylim=c(0,18), breaks=seq(4, 8, 0.25))
                                 
#	QQ-Plots des Parameters Sepal.Length mit eingezeichneten QQ-Linien
qqnorm(sl.seto, main = "I.setosa")
qqline(sl.seto)
qqnorm(sl.vers, main = "I.versicolor")
qqline(sl.vers)
qqnorm(sl.virg, main = "I.virginica")
qqline(sl.virg)
