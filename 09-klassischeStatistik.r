### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 9: klassische Statistik ############################################

# 9.1 Hypothesentests Allgemein
# 9.2.1 Tests auf Normalverteilung
# 9.2.2 Transformation hin zur Normalverteilung
# 9.3 Tests auf Varianzenhomogenität
# 9.4 Unterschied zweier Datensätze
# 9.5 Anova


# 9.1 Hypothesentests Allgemein ------------------------------------------------

# P-Value: Wahrscheinlichkeit, ein Ergebnis (Datenverteilung zB) wie im Test
# zu bekommen, wenn die Nullhypothese zutrifft.
# Wenn p < alpha: H0 ablehnen     alpha= 1-Sign.Niveau, oft  0.05 bei = 95%
# alpha = Wahrsch., Fehler 1. Art zu machen (H0 ablehnen, obwohl zutreffend)
browseURL("http://de.wikipedia.org/wiki/Statistische_Signifikanz")

# Hypothesentest sind schwarz-weiss-Malerei, wo die Welt doch farblich (und
# ultraviolett) ist, machen also nur in 0-1-Entscheidungen Sinn.
# Sind immer noch kritisch zu betrachten, siehe zB:
browseURL("http://www.eecs.qmul.ac.uk/~norman/blog_articles/p_values.pdf")

A <- rnorm(30, mean=154) ; B <- rnorm(30, mean=392)
t.test(A, B)
wilcox.test(A, B)
cor.test(A, B, method="p")

# Überprüfung der Signifikanz der Übereinstimmung zweier Verteilungen
# pairwise.wilcox.test(A, B, p.adjust.method="bonf", paired=FALSE)



fakedaten <- rnorm(1000, mean=97, sd=8.9)
t.test(fakedaten, mu=100, alternative="less") # p-value < 2.2e-16 < alpha
# also H0 ablehnen, und davon ausgehen, dass der wahre Mittelwert aller
# potentiellen Werte kleiner als 100 ist.

hist(fakedaten, col=8, freq=F)
lines(density(fakedaten)) # kerndichteschätzung
lines(70:130, dnorm(70:130,mean=mean(fakedaten),sd=sd(fakedaten)), col=2, xpd=T)
lines(70:130, dnorm(70:130,mean=97,sd=8.9), col=3, xpd=T)


# 9.2.1 Tests auf Normalverteilung ---------------------------------------------
browseURL("www.bb-sbl.de/tutorial/verteilungen/ueberpruefungnormalverteilung")

?binom.test
?chisq.test

shapiro.test(fakedaten)
ks.test(fakedaten, "pnorm", mean(fakedaten), sd(fakedaten))
# Wenn p > 0.05: Nullhypothese annehmen, daten sind normalverteilt.


if(!"nortest" %in% installed.packages()[,1]) install.packages("nortest")
library(nortest)          # mit Lilliefors-Korrektur!
help(package="nortest")
?lillie.test
lillie.test(fakedaten)

ad.test(fakedaten)      # Anderson-Darling test for normality
cvm.test(fakedaten)     # Cramer-von Mises test for normality
lillie.test(fakedaten)  # Lilliefors (Kolmogorov-Smirnov) test for normality
pearson.test(fakedaten) # Pearson chi-square test for normality
sf.test(fakedaten)      # Shapiro-Francia test for normality


# verschiedene Möglichkeiten zur Transformation
# 1/x, 1/sqrt(x), log(x), x^(1/4), x^(1/3), sqrt(x), x^2, x^3, boxcox (unten)


# 9.2.2 Normalisierung mittels BoxCox-transformation ---------------------------
browseURL("http://de.wikipedia.org/wiki/Box-Cox-Transformation")
browseURL(
"http://en.wikipedia.org/wiki/Power_transform#Box.E2.80.93Cox_transformation")

# Daten generieren
set.seed(42) ; Ksat <- rbeta(150, 0.3417, 1.8320)*220
# Verteilung der Daten ist nicht gaussisch:
hist(Ksat, breaks=50, col=8, freq=F)
x <- seq(-5, 200, len=200)
lines(x, dnorm(x, mean(Ksat), sd(Ksat)), lwd=2, col=5) # doesn't fit!

if(!require(car)) {install.packages("car") ; library(car) }
lambda <- powerTransform(Ksat)$lambda ; lambda # 0.2254471
Ksatbc <- bcPower(Ksat, lambda) #bc: boxcox
hist(Ksatbc, col=8, breaks=20, freq=F)
lines(x, dnorm(x, mean(Ksatbc), sd(Ksatbc)), lwd=2, col=5) # fits much better

# Alternative, hier aber gleiches Histogramm als Ergebnis
library(geoR)
lambdageoR <- boxcoxfit(Ksat)$lambda   ; lambdageoR   # 0.2254406
hist(bcPower(Ksat, lambdageoR), col=8, breaks=20, freq=F)

# für Lineare Modelle siehe
library(MASS)
?boxcox() # Box-Cox Transformations for Linear Models



# 9.3 Tests auf Varianzenhomogenität -------------------------------------------
fligner.test( A ~ B )
# p < 0.05 impliziert: Varianzen sind unterschiedlich
var.test()   # anfällig gegenüber Ausreißern
# install.packages("car")
library(car)
leveneTest()

summary(aov( A ~ B))
TukeyHSD(aov(A ~ B))
kruskal.test(A, B)



# 9.4 Unterschied zweier Datensätze --------------------------------------------
?t.test

# Wann welcher Signifikanztest sinnvoll ist, ist hier dargestellt:
browseURL("http://de.wikibooks.org/wiki/GNU_R:_Signifikanztests")


# There are two groups of values. The confidence interval overlaps, which means 
# that the two groups do not significantly differ.
# However, the t-test returns a p-value smaller than the divide of 0.05, 
# indicating a significant difference of the means.
# Why are the resulting interpretations contradictory?

# If two confidence intervals overlap, the difference between the two means 
# still may be significantly different.
browseURL("http://www.jstor.org/stable/view/2983411")
browseURL("http://www.cmaj.ca/content/166/1/65.long")

# Group 1  # Echte Daten aus einer Psychologie-Bachelorarbeit (2013)
G1 <- c(92,98,102,108,99,99,88,97,114,102,114,104,101,88,102,103,102,86,85,90,
 92,93,102,100,100,96,99,97,97,103,101,105,92,93,92,91,102,105,87,92,100,104,99,
 93,102,94,88,94,91,100,93,83,85,90,88,93,99,100,104,102,87,92,104,106,88,98,92,
 90,92,94,93,101,96,92,94,92,90,99,92,97,93,97,98,103,99,95,94,102,98,99,99,103,
 107,111,106,108,104,108,97,94,92,90)
# Group2
G2 <- c(115,107,111,108,102,83,94,90,102,109,106,90,113,85,109,103,102,109,94,
 93,82,96,95,100,107,93,102,108,100,118,88,87,86,87,86,85,93,95,81,99,106,97,90,
 93,84,83,98,119,98,100,87,80,94,90,98,99,114,103,117,123,99,106,107,126,94,91,
 86,90,95,91,90,105,93,89,97,107,102,96,102,104,88,94,109,103,102,100,98,102,95,
 108,92,94,95,99,102,108,97,96,107,123,83,81,89,88,114,82,96,108,109,104,93,121,
 116,117,113,110,87,95,112,97,101,102,105,102,102,97,102,105,99,92,90,86,91,116,
 92,93,109,107,114,108,111,105,116,114,111,96,106,104,106,101,94,100,102,97,81,
 87,85,99,89,82,90,93,109,111,100,96,101,102,86,93)

# the graphic with confidence intervals
CI_1 <- t.test(G1)$conf.int
CI_2 <- t.test(G2)$conf.int
plot(1:2, 1:2, ylim=c(95,101), type="n", xaxt="n", ann=F, las=1)
arrows(x0=1.4, y0=CI_1[1], y1=CI_1[2], angle=90, code=3, length=0.2)
   points(1.4, mean(G1), pch=16)
arrows(x0=1.6, y0=CI_2[1], y1=CI_2[2], angle=90, code=3, length=0.2)
   points(1.6, mean(G2), pch=16)
# Confidence Intervals around the mean overlap. WRONG interpretation: so the 
# two groups are not significantly different from each other

t.test(G1, G2) # p-value = 0.0349 < 0.05 -> reject H0, accept Alternative
# true difference in means is not equal to 0 -> signif. diff. of mean of groups 
# at conf.lev=0.95, you will WRONGLY reject H0 in 5% of (endless many) samples!

# Control interpretation of p-value
t.test(rnorm(100, mean=1), rnorm(100, mean=15)) # p-value < 2.2e-16 
# -> difference is significant

# control settings of t.test-argument var.equal
var.test(G1, G2) # p-value = 4.167e-06 -> true ratio of var.s is not equal to 1
var(G1) ; var(G2) # 42 and 100         -> var.equal = FALSE   is correct

# Konfidenzintervälle der klasisschen (frequentistischen) Statistik sind böse:
browseURL("http://www.entsophy.net/blog/?p=53") # und viele mehr!



# 9.5 Anova --------------------------------------------------------------------
# ANOVA: ANalysis Of VAriances (t.test for >2 groups)  
# Compare the ratio of between group variance to within group variance.
# If the variance caused by the interaction between the samples is much larger
# when compared to the variance that appears within each group,
# then it is because the means aren't the same.
browseURL("http://sociology.about.com/od/Statistics/a/Analysis-of-variance.htm")
browseURL("http://people.richland.edu/james/lecture/m170/ch13-1wy.html")
browseURL("http://en.wikipedia.org/wiki/F_test#One-way_ANOVA_example")

# AOV Beispiel nach:
browseURL("http://users.minet.uni-jena.de/~jschum/biostat/ANOVA.pdf")
peas <- read.table(header=T, text=
"Wdh Kontr. Gluk.2 Frukt.2 Gluk.1_Sacch.2 Frukt.1
1 71 57 58 58 62
2 68 58 61 59 66
3 70 60 56 58 65
4 74 59 58 61 63
5 68 62 57 57 64
6 71 60 56 56 62
7 70 60 61 58 65
8 67 57 60 57 65
9 73 59 57 57 62
10 69 61 58 59 67")
peas
peas <- stack(peas[,-1])
names(peas) <- c("length", "group")
head(peas, 15)
summary(peas)

tapply(peas$length, peas$group, mean)

par(mar=c(3,6,2,1), las=1)
boxplot(length~group, data=peas, col=5, horizontal=T)

AOV <- aov(length ~ group, data=peas)
AOV
summary(AOV)
par(mfrow = c(2, 2))
plot(AOV)


# ANOVA Beispiel nach:
browseURL("www.stat.wisc.edu/~yandell/st571/R/anova.pdf")
# Daten erstellen:
y1 <- c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1)
y2 <- c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
y3 <- c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
y = c(y1, y2, y3)
group <-  rep(1:3, each=7)

# exploratory data analysis (EDA):
plot(group, y, pch=16, col=rgb(0,0,0, 0.2)) # keine überlappenden Punkte
boxplot(y~group)
info <- function(x) c(sum=sum(x), mean=mean(x), var=var(x), n=length(x))
info(y)
tapply(X=y, INDEX=group, FUN=info) # Wende die Funktion "info" für jede "group"
# getrennt auf "y" an (anwenden = apply). Siehe auch Kapitel 11.3 

# Linear model and ANOVA
mod <- lm(y~group) # Mehr dazu im Kapitel 14 (Regression)
mod
plot(y~group); abline(mod)
summary(mod)
str(mod)
mod_aov <- anova(mod)
mod_aov
methods("print")
?print.anova
summary(mod_aov)
str(mod_aov)

# tabled F values:
Df <- mod_aov$Df  # Degrees of freedom of treatment and error
Df
qf(p=c(0.05, 0.01), df1=Df[1], df2=Df[2], lower.tail=FALSE)

# confidence interval on the pooled variance:
SSTrt <- mod_aov["Residuals", "Sum Sq"] # SSTrt: residual sum of squares
SSTrt
SSTrt/qchisq(p=c(0.025, 0.975), df=Df[2], lower.tail=FALSE)



# Versuch, das per Hand zu machen. Klappt noch nicht:
# One-way between groups ANOVA          variation != variance
m1 <- mean(G1); m1 # group mean
m2 <- mean(G2); m2
n1 <- length(G1); n1
n2 <- length(G2); n2
mg <- mean(c(G1,G2)); mg # overall mean (Grand mean)
ssw <- var(G1)*(n1-1) + var(G2)*(n2-1); ssw # within group variation SS(W)
       sum((G1-m1)^2, (G2-m2)^2) # the same
ssb <- n1*(m1-mg)^2 + n2*(m2-mg)^2; ssb # between group variation SS(B)

(ssb/1) / (ssw/(n1+n2-2))  # stimmt so noch nicht!

var.test(G1,G2)$stat

