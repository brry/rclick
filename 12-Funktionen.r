### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 12: Funktionen #####################################################

# 12.1 Funktionseinführung
# 12.2 Einfache Beispiele
# 12.3 Verweis auf Paket berryFunctions
# 12.4 Weitere mögliche Argumente
# 12.5 Lexical scoping - wo kommen die Variablen her?
# 12.6 Input prüfen ist wichtig (Exception handling)
# 12.7 Beispiele für praktische Funktionen
# 12.7.1 Rechteck manuell in Graphik zeichnen
# 12.7.2 Gleitendes Mittel
# 12.7.3 Fonttester
# 12.7.4 SCS Verfahren zur Bestimmung des Abfussbeiwertes



# 12.1 Funktionseinführung -----------------------------------------------------
# Funktionen schreibt man oft, wenn man Code mehrfach ausführen muss/will, oder
# wenn man Routinen für andere Leute schreiben will.
# Generelle Struktur:
Funktionsobjektname <- function(argument1, argument2, ...) {"Rechenanweisungen"}
?"function"



# 12.2 Einfache Beispiele ------------------------------------------------------

# Daten sollen geplottet und mit 7 multipliziert zurückgegeben werden:
myfunct <- function(grappig)
           {
           plot(grappig, type="l")
           return(grappig*7)
           }
# grappig ist Niederländisch und symbolisiert die freie Benennung der Argumente.
myfunct(   c(5,11,3,7)  )
# return() # gibt das was drin steht zurück, und danach wird die Funktion
# nicht weiter ausgeführt! Sollte also erst am Ende stehen.
# Es muss nicht gesetzt werden: die letze Anweisung ("expression") wird auch
# ohne return zurück gegeben:
myfunct <- function(grappig) {plot(grappig, type="l"); grappig*7  }
myfunct(   c(5,11,3,7)  )


# Nullstellen (P-Q-Formel) linearer Funktionen y = x^2 + px + q
pq <- function(p,q)
              {
              w <- sqrt( p^2 / 4 - q )
              c(-p/2-w, -p/2+w)
              } # Ende der Funktion
# Code einrücken erhöht die Menschenlesbarkeit enorm!
# Funktion verwenden
pq(3, -12)   #   -5.274917  2.274917

# Gleich mit Plotten im angegebenen Bereich
pq <- function(p,q,a=-5, b=5) # Standardwerte mit "=" vorgeben (defaults)
         {x <- seq(a, b, len=100)
         plot(x, x^2+p*x+q, type="l"); abline(h=0, v=0)
         w <- sqrt( p^2 / 4 - q )
         c(-p/2-w, -p/2+w)
         }

pq(3, -12) # a und b haben bereits Standardwerte, müssen nicht angegeben sein.
pq(3, -8, a=-7)  # können aber (auch einzeln) geändert werden
pq(3, -8, , 3)
# Nicht namentlich genannte Argumente müssen in der richtigen Reihenfolge sein,
# bei Änderung der Reihenfolge muss man sie aber angeben:
pq( a=-2, q=-12, p=3)


# Funktionen werden zudem oft in Solvern benötigt, zB um ein Minimum finden:
optimize( f=function(x) x^2 + 3*x - 12 ,  interval=c(-5, 5) )
# Siehe Kapitel 18: Optimierung


# Wurzel aus Skalarprodukt zweier Vektoren
?"%*%"
wsk <- function(x, y) drop( sqrt(x%*%y) ) # drop macht aus matrix ein vector  
wsk(1:4, 4:1)
wsk(c(5.2,pi,-3), c(7,-4,7.5))


# Barcodes erstellen
barcode <- function() { # einfache Verwendung von function: Code zusammenfassen
     par(mar=c(0,0,0,0)); layout(matrix(1,1,1), widths=lcm(15), heights=lcm(9))
     t4 <- data.frame(a=1:500, b=ifelse(rnorm(500)>0.9, 1, 0))
     plot(t4, type="n", xaxt="n", yaxt="n" )
     abline(v=t4[t4$b==1,1], lwd=3); box(col="white")    }

windows(record=T)
for (i in 1:20)  barcode()
# mit PG up/dn (Bild) kann man die Bilder durchklicken


# Erste und letze Zeilen einer Tabelle
flast <- function(dat, n=3)  return(dat[c(1:n,(nrow(dat)-n+1):nrow(dat)),])
flast( data.frame(letters, rt(26, 5))  )
# eleganter mit: head, tail
flast <- function(dat, n=3) rbind( head(dat, n), tail( dat, n) )
flast( data.frame(letters, rt(26, 5))  )


# Interferenz durch Pixelabstände
x <- seq(-pi,pi,len=1000)
par(mar=rep(0,4))
coola <- function (a) plot(a*sin(x), a*cos(x), asp=1, type="l")
coolb <- function (a) lines(a*sin(x), a*cos(x))
coola(10)
for(i in 5:99/10) coolb(i)  # Zieh mal die Graphik kleiner und größer...


# In einer Funktion auf Objekte zugreifen: get()
obj_1 <- cumsum(rnorm(1000))
obj_2 <- cumsum(1:10*c(-2,3))
testf <- function(k) plot(  get(paste0("obj_", k)), type="l"  )
testf(1)
testf(2)


# Integrale
f <- function(x) {-x*x +7}
plot(-10:40/10, f(-10:40/10), type="l")
abline(v=0, h=0)
f(1)
integrate(f, 0, 1.5)
lines(c(1.5,1.5,0,0:15/10), c(f(1.5),0,0,f(0:15/10)), col=2, lwd=2)
text(.75, 3, 9.4, col=2)



# 12.3 Verweis auf Paket berryFunctions ----------------------------------------
browseURL("http://cran.r-project.org/web/packages/berryFunctions/index.html")
# Dieses Paket hat unter anderem auch eine Menge meiner Helper-Functions
install.packages("berryFunctions")
library(berryFunctions)
?berryFunctions  # Feedback ist herzlich willkommen!



# 12.4 Weitere mögliche Argumente --------------------------------------------
testdots <- function(x,...) plot(x, ...) # dots expansion, three dots, ellipsis
testdots(3:8)
testdots(3:8, main="zwo") # oder was auch immer.


# to pass ... to a different function:
myfunc <- function(g, ...) {plot(g); list(...)}
myfunc(8, buh=6)


# Für jede Funktion eine eigene Argumentliste:
tp <- function(...) {plot(7:8, ...) ; legend("top", "Text hier", ...)}
tp()
tp(type="o") # legend hat kein type!      die Lösung: berryFunctions::owa
# different Ellipsis passed to different functions, each with their own defaults
?owa # argument overwriting and combining. Read the examples.



# 12.5 Lexical scoping - wo kommen die Variablen her? --------------------------
x <- 2
ls() # x ist ein Objekt (Variable) im Global Environment, dem user workspace
f <- function(y) c(x,y)
f(7) # x wird aus dem globalenv() gezogen, es gibt kein x innerhalb der Funktion
f <- function(y) {x <- 3; print(environment()); c(x,y)}
f(7) # hier kommt x aus dem funktionseigenen temporären environment
x # im globalenv hat x nach wie vor die Zahl 2
# Das ist bei geschachtelten Funktionen komplizierter. Für Fortgeschrittene:
browseURL("http://andrewgelman.com/2014/01/29/stupid-r-tricks-random-scope/")
# Für Anfänger eher geeignet, mit Übungsaufgaben:
browseURL("http://adv-r.had.co.nz/Functions.html")


tf <- function(x) {if(x)buh=8; list(exists("buh", inherits=FALSE), buh)}
tf(T)
tf(F) # geht nicht, buh existiert nicht
buh=7
tf(T)
tf(F) # buh existiert nicht im Function environment, aber in globalenv()



# Nur einmal pro Sitzung warnen/informieren
# r.789695.n4.nabble.com/Display-warning-only-once-in-session-td4696005
myFunc <- local({   # (c): Bill Dunlap, TIBCO Software
   notWarnedYet <- TRUE
   function(x) {
   if (notWarnedYet) {
       warning("myFunc is funky")
       notWarnedYet <<- FALSE # note use of <<-
       }
   sqrt(log2(x))
   }
   })

myFunc(1:5)
myFunc(1:5)
myFunc(1:5)



# 12.6 Input prüfen ist wichtig (Exception handling) ---------------------------
# z.B. sollen in folgender Berechnung der Fläche eines planaren Dreiecks
# 3-stellige Vektoren mit je den x und den y-Koordinaten der Ecken eingehen.
# Bei anderen Eingaben soll mit Fehlermeldung abgebrochen werden.
dreieck <- function(x, y, digits=3)
     {
     if( !is.vector(x) | !is.vector(y) ) stop("Input müssen Vektoren sein!")
     if(length(x) != 3 | length(y) !=3 ) stop("Vektoren sind nicht 3-stellig")
     A <- 0.5*(x[1] * (y[2]-y[3]) + x[2] * (y[3]-y[1]) + x[3] * (y[1]-y[2]))
     return(round(abs(A),digits))
     }

a <- c(1,5.387965,9); b <- c(1,1,5)
plot(a[c(1:3,1)], b[c(1:3,1)], type="l", asp=1)#; grid()

dreieck(a,b)
dreieck(a,b[1:2])

# Always try to make your functions foolproof. That way, if someone else uses it
# wrongly, they'll know what they did wrong. And by the way, this includes you
# yourself, three years later. (or 3 weeks, actually)

# cycle a vector, eg change  1 2 3 4 5 6 7  to  4 5 6 7 1 2 3
Cycle <- function(vector, shift)
      { # make nice indents to keep things readable
      # make sure only vectors are supplied as input
      if(!is.vector(vector)) stop("input has to be a vector")
      # don't mess with shift: It has to be ONE integer, less than vectorlength
      if(length(shift) > 1) stop("shift should be ONE number")
      if(is.na(as.integer(shift))) stop("shift has to be an integer")
      n <- length(vector)
      if(shift >= n) stop("shift has to be smaller than length(vector)")
      # Well, here's the actual thing happening:
      vector[c((shift+1):n, 1:shift)]
      } # end of function

# Test the function
Cycle(1:8, 5)  # 6 7 8 1 2 3 4 5
Cycle(letters, 8)

is.integer(5)   # FALSE     # cannot be used as criterion in function
is.integer(5:6) # TRUE      # see ?":"
typeof(5) # double


# Wat use is this function, you may wonder...
# read on ;-)

# Draw polygon with n corners
n_gon <- function(n)
     { # function start
     op <- par(mar=rep(0,4)) # don't mess with other people's graphics:
     # assign the current par and reset it at the end of your function.
     # Keep track of what's going on:
     # the next line creates n vertices spaced equally along the unit circle
     v <- data.frame(x=cos(1:n*2*pi/n), y=sin(1:n*2*pi/n))
     # Set the plot, but don't plot anything
     plot(v, asp=1, type="n")
     # Here we go: the lines. see comment below about how I figured this out
     segments(x0=v$x, y0=v$y, x1=Cycle(v$x, floor((n-1)/2)),
                              y1=Cycle(v$y, floor((n-1)/2))    )
     # Set the old parameters again:
     par(op)
     } # end of function

n_gon(8)

windows(record=T)
for (i in 3:30) n_gon(i)

# repeatedly clicking 'page up' on this one is not recommended for epileptics:
for (i in 20:300) n_gon(i)

n_gon(50)   # nice interference.
n_gon(100)  # Look at the white bands which are not really existent...

# I guess this is nothing really new, but I was just wondering whether there is
# any regularity behind drawing concentric polygons. I found it is necessary to
# draw a line once from every vertex to the one that is (n-1)/2 vertices later.
# Then I realized, I need to recycle the vector to do that.
# And then I thought it nice to take this as an example of foolprofing functions
# Which is really called exception handling, but foolprofing sounds cooler.
# Useless as this was contentwise, I still hope you learned something...



# 12.7 Beispiele für praktische Funktionen -------------------------------------
# 12.7.1 Rechteck manuell in Graphik zeichnen ----------------------------------
rechteck <- function(...){
     cat("Bitte wähle den Bereich im Graphikfenster aus.", 
         "Klicke zuerst linksoben, dann rechtsunten.\n", sep="\n")
     flush.console() # Damit die Aufforderung in die Console geschrieben wird
     # Ausgaben werden sonst gebuffert und erst am Ende der Funktion geschrieben
     werte <- locator(2)
     polygon(c(werte$x, rev(werte$x)), rep(werte$y,each=2), ...)
     cat("Rechteck gezeichnet.\n",
         "Danke Berry für diese tolle Funktion: berry-b@gmx.de.\n", sep="")
     }

plot(rexp(300), rt(300,5))
rechteck()
rechteck(col=2)
# eine ähnliches Prinzip steckt hinter berryFunctions:::bzoom, siehe 12.7
rechteck(border=3, lwd=4)
rechteck(border=NA, col="white") # damit kann man auch ganz gut Text löschen
?polygon # für die Argumente, die gesetzt werden können.

loeschen <- function() {cat("linksobere, dann rechtsuntere Ecke klicken.\n")
  flush.console()  ; werte <- locator(2)
  polygon(c(werte$x, rev(werte$x)), rep(werte$y,each=2), border=NA, col="white")
  }

plot(rexp(300), rt(300,5))
loeschen()



# 12.7.2 Gleitendes Mittel -----------------------------------------------------
# Siehe auch  BerryFunctions::movAv  sowie  decompose  sowie  zoo::rollaply
movav <- function(dat, steps=7)
{ # Funktionsanfang
  # halbe Fensterbreite (in eine Richtung), dabei gerade Zahlen auffangen
  # (soll immer ungerade sein, damit das Fenster eine definierte Mitte hat):
  st <- round(steps/2-.1,0) 
  # Länge des Eingangsvektors:
  ld <- length(dat)
  # leeren Ergebnisvektor erstellen:
  v <- vector("numeric", ld)
  # Bereich für NA ermitteln (Hälfte des gleitenden Fensters am Anfang + Ende)
  # bei Standard 7 steps erste und letzte drei Werte jeweils := NA
  v[c(1:st, (ld-st+1):ld)] <- NA
  # für den Rest jeweils das Mittel aus dem Fenster um den einzelnen Wert herum
  for(i in (st+1):(ld-st))   v[i]= mean(dat[(i-st):(i+st)],na.rm=TRUE)
  return(v) # Ergebnisausgabe
} # Funktionsende
  

set.seed(29); a <- runif(60, 5,50)
data.frame(a[1:20], movav(a[1:20]))

plot(a, type="o", pch=16, las=1) # sieht wirr aus
lines(movav(a), col=2, lwd=4) # bringt Übersicht rein, zeigt Trends
lines(movav(a,3), col=4, lwd=4)
lines(movav(a,15), col=3, lwd=4) # unterscheidlich starke Glättung
graphics.off()
# Weitere Beispiele in Dokumentation   library(berryFunctions);  ?movAv

# Visualisierung des Funktionsprinzips von movav
# dir.create("MovingAverage_Funktionsprinzip")
# png("MovingAverage_Funktionsprinzip/movav_%02d.png", 1024, 600)
set.seed(1); a <- runif(30, 5,50)
b <- movAv(a)
par(mar=c(3,3,1,1), cex=2)
plot(a, type="o", pch=16, las=1, ylim=c(5,55))
#blines(1:30,a)
windows(record=T)
for (i in 4:27) {
  plot(a, type="o", pch=16, las=1, ylim=c(5,55))
  #blines(1:30,a)
  polygon(x=c(i-4,i-4,i+3,i+3)+0.5, y=c(5,55,55,5), col=rgb(1,0,0, alpha=0.2),
  border=2)
  #blines(1:(i-1), b[1:(i-1)], type="o", pch=16, col=2, lwd=3)
  points(i, b[i], pch=16, col=2)
}
# mit PgUp und PgDn (Bild hoch und runter)-Tasten Bildersequenz ablaufen lassen
windows()
plot(a, type="o", pch=16, las=1, ylim=c(5,55))
lines(1:30, b, type="o", pch=16, col=2, lwd=3)
# dev.off()



# 12.7.3 Fonttester ------------------------------------------------------------
fonttester <- function(f){  par(mar=c(0,0,0,0))
plot(0:100, rep(0,101), type="n", ylim=c(-0.3,2.8)) ;  cx <- 1.5
text(0,2.5, "A B C D E F G H I J K L", adj=0 ,cex=cx)
text(33,2.5, "M N O P Q R S T U V W X", adj=0 ,cex=cx)
text(66,2.5, "Y Z ! ' § $ % & / ( ) =", adj=0 ,cex=cx)
text(0,2,   "a b c d e f g h i j k l", adj=0 ,cex=cx)
text(33,2  , "m n o p q r s t u v w x", adj=0 ,cex=cx)
text(66,2  , "y z 1 2 3 4 5 6 7 8 9 0", adj=0 ,cex=cx)
text(0,1.5, "ß ´ } ] [ { \U00B3 \U00B2 @ \U20AC ' ~", adj=0 ,cex=cx)
text(33,1.5, "* ü ä ö Ü Ä Ö ? # ` ^ °", adj=0 ,cex=cx)
text(66,1.5, "< > | ; : . , - _ + @ \U20AC", adj=0 ,cex=cx)
abline(h=1.25) ; abline(v=c(31,64))
text(0,1  , "A B C D E F G H I J K L", adj=0,font=f ,cex=cx)
text(33,1  , "M N O P Q R S T U V W X", adj=0,font=f ,cex=cx)
text(66,1  , "Y Z ! ' § $ % & / ( ) =", adj=0,font=f ,cex=cx)
text(0,0.5, "a b c d e f g h i j k l", adj=0,font=f ,cex=cx)
text(33,0.5, "m n o p q r s t u v w x", adj=0,font=f ,cex=cx)
text(66,0.5, "y z 1 2 3 4 5 6 7 8 9 0", adj=0,font=f ,cex=cx)
text(0,0  , "ß ´ } ] [ { \U00B3 \U00B2 @ \U20AC ' ~", adj=0,font=f ,cex=cx)
text(33,0  , "* ü ä ö Ü Ä Ö ? # ` ^ °", adj=0,font=f ,cex=cx)
text(66,0  , "< > | ; : . , - _ + @ \U20AC", adj=0,font=f ,cex=cx)
text(102,2.5, paste("font =",f), adj=1, font=2)
text(98,1.8,"ß", font=5, cex=3) }

fonttester(5)

png("fonts%02d.png", width=1000, height=200, units="px")
for(i in 1:40)   fonttester(i)
dev.off()



# 12.7.4 SCS Verfahren zur Bestimmung des Abfussbeiwertes ----------------------
Neff <- function(N) { CN <- 0:100; neff <- (N-5080/CN+50.8)^2/(N+20320/CN-203.2)
par(mar=c(3.2,3,3,3.5))
plot(CN, neff, xlim=c(CN[which(neff<=N)[1]],100),ylim=c(0,N),
main="EffektivNiederschlag in Abhängigkeit des CN-Faktors",
type="l", las=1, mgp=c(2,1,0), yaxs="i", xaxs="i")
legend("topleft", paste("Niederschlag =", N, "mm"), bg="white")
axis(4,seq(0,N,len=11)[c(1,3,5,7,9,11)],c(0,2,4,6,8,10)*10, col.axis=2, las=1)
mtext("Abflussbeiwert  Psi",4,2, col=2)}

Neff(N=15)

windows(record=T)
plot(1, type="n", xlab="CN", ylab="Neff  [mm]", xlim=c(0,110), ylim=c(3,100),
     main="EffektivNiederschlag in Abhängigkeit des CN-Faktors", las=1)
CN <- 1:100
for (N in 1:10*10) { neff <- (N-5080/CN+50.8)^2/(N+20320/CN-203.2)
                     lines(CN[which(neff<=N)],neff[which(neff<=N)], col=N/10) }
for (i in 1:10*10) { text(101,i, paste(i,"mm"), col=i/10, adj=0) }
text(95,100, adj=1, "bei Niederschlag =") ; text(40,50,"Neff =  ", adj=1)
text(40,50,"(N-5080/CN+50.8)^2 /\n(N+20320/CN-203.2)", adj=0)
#
plot(1, type="n", xlab="CN", ylab="Neff  [mm]", xlim=c(0,110), ylim=c(3,100),
     main="EffektivNiederschlag in Abhängigkeit des CN-Faktors", las=1)
CN <- 1:100
for (N in 1:10*10) { neff <- (N-1270/CN+12.7)^2/(N+24130/CN-241.3)
                     lines(CN[which(neff<=N)],neff[which(neff<=N)], col=N/10) }
for (i in 1:10*10) { text(101,i, paste(i,"mm"), col=i/10, adj=0) }
text(95,100, adj=1, "bei Niederschlag =") ; text(40,50,"Neff =  ", adj=1)
text(40,50,"(N-1270/CN+12.7)^2 /\n(N+24130/CN-241.3)", adj=0)
