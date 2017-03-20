### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 18: Optimierung ####################################################

# 18.1 Exp Abnahme grob anpassen
# 18.2 Zu minimierende Größe: RMSE
# 18.3 Zu minimierende Funktion
# 18.4 Ergebnis darstellen
# 18.5 RMSE abhängig von k
# 18.6 Verschiedene Startwerte
# 18.7 verschiedene Methoden


# Parameter mit Optimierungsverfahren genauer schätzen -------------------------
# Exponentielle Temperaturabnahme im Kaffeebecher

# Anmerkung: mehrere Parameter mit optim, 1 einzelner mit optimize

# Daten
Zeit <- c( 0.0,  2.5,  6.0, 10.0, 12.5, 13.6, 17.3, 20.3, 23.9) # Minuten
Temp <- c(43.5, 41.6, 39.6, 37.6, 36.4, 36.2, 35.0, 34.1, 32.9) #°C
plot(Zeit, Temp, las=1, pch=16, ylab="Wassertemperatur [°C]", xlab="Zeit [min]")



# 18.1 Exp Abnahme grob anpassen -----------------------------------------------
T0 <- 44 # Anfangstemperatur
Te <- 25 # Endtemperatur
diff(range(Temp)) / diff(range(Zeit)) / ( mean(Temp)-Te)
k <- -0.036 # Abnahmerate [1/minute]

t <- seq(0, 30, by=0.1)
lines(t, (T0-Te)*exp(k*t)+Te  )  # gar nicht mal so schlecht


param <- c(T0=44, Te=25, k=-0.036) # Anfangsparameter, später für optim
# Temperaturfunktion definieren (abh von parametersatz p und zeit t):
temperaturfun <- function(p, t) {  (p["T0"]-p["Te"])*exp(p["k"]*t)+p["Te"]  }

temperaturfun(p=param, t=5)  # nach 5 Minuten
lines(t, temperaturfun(p=param, t=t) , col=2 )
# gehen die gleichen Parameter wie vorhin rein



# 18.2 Zu minimierende Größe: RMSE ---------------------------------------------
# root mean square error: berechnet Abweichung von x1 zu x2, wird auch zur
# Berechnung des R2 in einer linearen Regression verwendet
rmse <- function(x1,x2)
        {  if(length(x1) != length(x2)) stop("vectors not of equal length")
          sqrt( sum((x1-x2)^2)/length(x2)  )
        } # Funktionsende

# Man könnte auch als andere Größe die Korrelation maximieren:
?cor()

rmse(Temp, temperaturfun(p=param, t=Zeit)  )  # 0.53

cor(Temp, temperaturfun(p=param, t=Zeit)  ) # 0.9986 - schon ziemlich gut
# graphisch zu sehen: geht aber noch viel besser



# 18.3 Zu minimierende Funktion ------------------------------------------------
# optim braucht eine Funktion mit par als erstem Input
minfun <- function(p) rmse(Temp, temperaturfun(p=p, t=Zeit))
minfun(param)     # 0.53

optimized <- optim(par=param, fn=minfun)
optimized
# $value  0.11  Wir hatten angefangen mit 0.53, ist also deutlich verbessert

data.frame(initial=param, optimized=optimized$par)
# So viel hat sich nicht geändert - wir hatten ja auch sehr gute Startwerte


cor(Temp, temperaturfun(p=optimized$par, t=Zeit)  ) # 0.9994, vorher 0.9986
# ist nicht so die ganz geeignete Größe dafür, liefert aber auch das Richtige:
maxfun <- function(p) cor(Temp, temperaturfun(p=p, t=Zeit))
optim(par=param, fn=maxfun, control=list(fnscale=-1) ) # fnscale<0: Maximierung



# 18.4 Ergebnis darstellen -----------------------------------------------------
plot(Zeit, Temp, las=1, pch=16, ylab="Wassertemperatur [°C]", xlab="Zeit [min]")
lines(t, temperaturfun(p=param, t=t) , col=2 )
lines(t, temperaturfun(p=optimized$par, t=t), col=1)
# deutlich sichtbar verbessert
legend("topright", c("observed", "initial", "optimized"), lty=c(NA,1,1),
        pch=c(16,NA,NA), col=c(1,2,1), inset=0.05)
text(15  ,40.5, expression("y = (T0" - "Te) "%.%" "*e^("k "%.%" x")*" "+" Te"))
legend(14  ,40, paste(names(optimized$par), "="), bty="n", adj=1)
legend(14.5,40, round(optimized$par,3), bty="n", adj=0)



# 18.5 RMSE abhängig von k -----------------------------------------------------
k_values <- seq(-0.9, 0.025, len=200)
RMSerror <- rep(NA, length(k_values))
# mit den bereits optimierten anderen beiden Werten
for(i in 1:length(k_values))
   RMSerror[i] <- rmse(Temp, (43.419-28.372)*exp(k_values[i]*Zeit)+28.372)
plot(k_values, RMSerror, type="l", las=1)
# Ist jetzt aber nur eine eindimensionale Betrachtung!



# 18.6 Verschiedene Startwerte -----------------------------------------
vgl_startw <- function(T0=44, Te=25, k=-0.036)
  {param <- c(T0=T0, Te=Te, k=k)
  optimized <- optim(par=param, fn=minfun)
  list(data.frame(initial=param, optimized=optimized$par), RMSE=optimized$value)
  } # Funktionsende

vgl_startw()
vgl_startw(k=-0.2)
vgl_startw(k=-0.9) # k optim = -10.6  -> geht nicht mehr auf das richtige k zu
# Zeigt, dass Startwerte sinnvoll gesetzt werden müssen!!

# Systematisch Grenzen der Methode ausloten
k_test <- seq(0.5, -1, length=5000)

sense <- matrix(0, nrow=length(k_test), ncol=2); colnames(sense)<- c("k","RMSE")
head(sense)

total <- length(k_test)
pb <- winProgressBar(title="progress bar", min=0, max=total)
# Fortschrittsbalken hilft in langsamen for-Schleifen, evtl in Sichtweite ziehen
# Linux/Mac: tkProgressBar im Paket tcltk
for(i in 1:total)
   {sense[i,1] <- k_test[i]
    sense[i,2] <- vgl_startw(k=k_test[i])[[1]][3,2]
    setWinProgressBar(pb, i, title=paste( round(i/total*100, 0), "% done"))
    if(i==total) {Sys.sleep(2)  ; close(pb) }
   } # Ende for Schleife

windows()
plot(sense, type="l", las=1, xlab="initial k", ylab="optimized k")
abline(h=-0.049, col=2, lty=2)
text(-0.8, -0.049, "real k", col=2)



# 18.7 verschiedene Methoden ---------------------------------------------------
# parametersatz mit nur k-Startwert falsch
optim(par=c(T0=43.42, Te=28.37, k=-0.9), fn=minfun, method="Nelder-Mead")$par
# Jetzt richtig, obwohl vorhin mit -0.9 falsch. (Te besser gewählt)
                                                           # Findet echten Wert?
optim(par=c(T0=44, Te=29, k=-0.9), fn=minfun, method="Nelder-Mead")$par # nein
optim(par=c(T0=44, Te=29, k=-0.9), fn=minfun, method="BFGS")$par        # ja
optim(par=c(T0=44, Te=29, k=-0.9), fn=minfun, method="CG")$par          # nein
optim(par=c(T0=44, Te=29, k=-0.9), fn=minfun, method="SANN")$par        # nein
# "L-BFGS-B" und "Brent" gehen hier nicht

# Bei mehr Daten ist die Wahl der Startwerte weniger problematisch
