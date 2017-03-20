### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 4: Graphische Datenauswertung ######################################

# 4.1 Basics: Beschriftungen, Symbole
# 4.2 Linien statt Punkte, Überplotten
# 4.3 Graphikausschnitte, Ränder, Barplots
# 4.4 par
# 4.5 Farben, Legende
# 4.6 Linien, Text, Gitter
# 4.7 Achsenbeschriftungen 2.0
# 4.8 Logarithmierte Achsen mit Linien im Plot
# 4.9 Anordnung mehrerer Graphiken in einem Bild
# 4.10 Graphiken mit Umrandung
# 4.11 Speichern von Bildern: bmp("dateiname.bmp")
# 4.12 Beidseitige y-Achsenbeschriftung
# 4.13 Achsen unterbrechen, Pakete installieren und laden
# 4.14 mehrdimensionale Info in Graphiken
# 4.15 Dreidimensionales Plotten
# 4.16 Fehlerbalken (Error bars)
# 4.17 Fläche unter Graphen füllen
# 4.18 Funktionen zeichnen
# 4.19 zooming in graphics
# 4.20 und ein würdiger Abschluss des Kapitels
# 4.21 Übersicht der wichtigsten Graphikbefehle
   

# integrierter Datensatz (Beispieldaten aus Kapitel 3)
Daten <- stackloss   ; names(Daten) <- c("Air", "Water", "Acid", "StackLoss") 
Daten    # Zuordnungen können unter x-beliebigem Namen laufen, eben zB "Daten".
?stackloss  # aber nicht "data" weil das schon eine Funktion ist

# Grundlegende Graphiken (x-y-Plot)
?plot
plot(Daten$Air, Daten$StackLoss)
# manchmal ist auch pairs interessant:
pairs(Daten)

# Intro in R's Graphikkapazitäten (Befehle sind jetzt egal, später anschauen)
demo(graphics) # in der Console ENTER drücken, dann auf die Bilder klicken
# Evtl. kommt zuerst ein leeres Grafikfenster, dann einmal darin klicken.
dev.off() # Schließt Graphikfenster 

#http://blog.revolutionanalytics.com/2015/01/some-basics-for-base-graphics.html



# 4.1 Basics: Beschriftungen, Symbole ------------------------------------------

# Beschriftungen: main = "Überschrift", sub = "Unterschrift", xlab = "X-Achse"
plot(Daten$Water, Daten$StackLoss, main = "StackLoss abh. der Wassertemperatur",
     sub = "Analyse", xlab = "Wassertemperatur", ylab = "StackLoss")
# Immer wenn ein + in R erscheint, ist die Eingabe noch nicht abgeschlossen.

# später hinzufügen mit title()
plot( c(8,4,14,-3,4,15) , xlab = "") 
# Wenn nur ein Vektor angegeben wird, wird über ganze Zahlen geplottet (Index)
title(main = "Hallo", xlab = "Dies ist die X-Achse")

# Zeilenumbruch im Titel: \n ohne Leerzeichen im Text einfügen
plot(Daten$Water, Daten$StackLoss, main = "tolle Graphik\nStackLoss")

# Punkt- und Schriftgröße verändern: cex = ___  ( Character EXpansion)
plot(Daten$Water, Daten$StackLoss, cex = 2)
plot(Daten$Water, Daten$StackLoss, cex = 2, main = "Schuhgröße - StackLoss",
     cex.main = 2) # Default: cex.main=1.2

# Achsenwerte ("label" = Zahlen / Einheiten an Achsen) skalieren: cex.axis = ___
plot(Daten$Water, Daten$StackLoss, cex.axis = 0.5)

# Ausrichtung der Achsenwerte: las = ___  0 parallel zur Achse,   1 horizontal
plot(Daten$StackLoss, las = 1)     #      2     quer zur Achse,   3 vertikal
                                   # (Label Axis Style)
# Symbole ändern: pch = ___ oder pch="Buchstabe"  zB. ausgefüllter Punkt: pch=16
plot(Daten$Water, pch = 16)    # (Point Character) Siehe Übersicht im Anhang 

# Achsentitel ausrichten (adjust); Werte von 0 (Text links) bis 1(Text rechts)
plot(Daten$Acid, adj = 0.82)

# Kombination von Parameter-settings (Reihenfolge ist egal)
plot(Daten$Air, main = "N in Pflanzen", xlab = "WasserTemp",
ylab = "Stack Loss", las=3, pch = 16, cex = 1.5)

# Abstand Beschriftung:  mgp=c( Achsentitel, Achsenwerte, Achsen)
# Voreinstellung: mgp=c(3,1,0), somit Verschiebung nach innen und außen möglich
plot(Daten$Water, mgp=c(3.5, 1, 0))
plot(Daten$Water, mgp=c(2, 1, 0))  # könnte für MarGinPlacement stehen...
plot(Daten$Water, mgp=c(3, -1, 0))
plot(Daten$Water, mgp=c(3, -3,-2))



# 4.2 Linien statt Punkte, Überplotten -----------------------------------------

k <- c(3,0,-10,8,6,3)   ; l <- c(-4,2,-6,-12,7,-3)
plot(k)  # standard (default) type: "p"  (points)
plot(k, type="l") # Linien
plot(k, type="b", lwd=4) # Liniendicke einstellen
plot(k, type="o", lty=2) # Linientyp einstellen
# Siehe Übersichten im Anhang zu type und lty

# mit dem Befehl points() kann etwas in die Graphik eingefügt werden.
# Die Argumente sind gleich wie bei plot(), allerdings ohne Titel.
plot(k)
points(l, pch=24, bg=2)
lines(l) # wie points mit als Standard type="l"

plot(k, type="o")   # "b": Punkte und Linien. "o" zieht die Linien durch
par(new=T) # Ein danach versandter plot wird über das alte geplottet.
plot(l, type="o", col=6, col.axis=6)  # Achsen vermurkst!
# (Zweiseitige Achsen werden im Kapitelteil 4.12 vorgestellt.)
plot(k, type="b", lty=2) # Plot von gerade wieder (new=T gilt nur für 1 Graphik)
par(new=T)
plot(l, type="o", ylim=c(-10,8), pch="@") 
# Die y-Achsengrenze des ersten Datensatzes reicht nicht, daher ylim: Kap 4.3.



# 4.3 Graphikausschnitte, Ränder, Barplots -------------------------------------

# Plotausschnitt einstellen: xlim = c( untere Grenze, obere Grenze )
plot(Daten$StackLoss,xlim=c(-3,50), ylim=c(-4,80))                       

# Graphik ohne 4%-Erweiterung des Achsenbereiches: yaxs="i" (Y-AXis Style)
plot(c(3,2,5,4), type="o")
plot(c(3,2,5,4), type="o", yaxs="i", xaxs="i")

# Ränder (Margins) einstellen: mar = c(bottom, left, top, right)
# Die Nutzung von par ist im nächsten Abschnitt erklärt.
par(mar = c(5, 4, 4, 2)+0.1  )    # Default Settings (Voreinstellung)
plot(Daten$Water)    # mar muss immer mit par für das Device gesetzt werden
par(mar=c(2.2, 2.0, 0.2, 2.1) ) 
plot(Daten$Water)        

# Barplots:
tabelle <- read.table(header=TRUE, text="
LandN. Verdunstung
Wald  200
Weide 400
Soja  359")
tabelle
barplot(tabelle$Verd, names=tabelle$LandN, main="Verdunstung",
        col=c("forestgreen","green2","wheat"))



# 4.4 par ----------------------------------------------------------------------

# viele der Graphik-Einstellungen können auch mit par eingestellt werden.
# Die danach im gleichen Fenster erstellten Plots haben dann die Eigenschaften.
# Weitere Fenster können mit windows() eröffnet werden.
windows() # geht nur mit dem Betriebssystem Windows, sorry Linuxer. 
# (Macser: selbst Schuld ;-) Kauft was billigeres oder kostenloses...)
# verschiebe mal das neue Fenster (Device 3)
plot(Daten$Air, Daten$Water)
dev.off() # schließt das aktuelle (active) Fenster
graphics.off() # macht alle zu

# Aktuelle Settings anzeigen (Default, da alle Graphikfenster jetzt zu sind)
par()
?par

# Alte Graphikeinstellungen (Parameter) im Graphics:Device speichern
altepar <- par(mar=rep(6,4)) # speichert das alte (!) mar
altepar # in Funktionen oft old.par oder op genannt
plot(1:8)
par(altepar) # vorherigen Rand wieder einstellen
par(new=T, col="red", col.axis="red", col.lab="red") # Farben Siehe 4.5
plot(1:8)

# Alternativ möglich:
old.par <- par(no.readonly=T)  
# Nur nötig, wenn Änderungen im Device mit par(___) vorgenommen werden
# Nicht nötig, wenn Einstellungen in einzelnen Graphiken erfolgen. plot(x,y,___)

dev.off()
# Randplatz minimieren, Graphikraum maximal nutzen:
par(mar=c(3,3,1,1), mgp=c(1.8, 0.6, 0), las=1)
plot(Daten$Air, Daten$Water)


# Einstellungen mit par gelten für Graphen, die im aktuellen, aktiven, Fenster
# erstellt werden. Wird dieses geschlossen, müssen sie erneut gesetzt werden.


# Graphik History (zeichnet einzelne Ausgaben im Graphics:Device auf)
# Bild wechseln mit "Bild up" und "Bild dn" Tasten oder Mausklick
windows(record=T) # oder im Device selbst unter History anklicken
# praktisch: Rechtsklick in der Graphik - "bleibe im Vordergrund" anschalten
graphics.off()



# 4.5 Farben, Legende ----------------------------------------------------------

# Farben setzen: col = ___
plot(Daten$Water, Daten$StackLoss, col = "red")    # oder col=2
plot(Daten$Water, Daten$StackLoss, col = rgb(0,1,0), pch=16)      
# RGB steht für RedGreenBlue. Ermöglicht exakte Farben zB nach Corporate Design
rgb(0,1,0)           # Hat Standardwerte zwischen 0 und 1
col2rgb("green")/255 # Hat Standardwerte zwischen 0 und 255 
plot(1:3, pch=16, cex=3, col= rgb(c(0,.3,.8), c(.3,.5,.5), c(.1,.9,.2) ), las=1)
# Siehe auch ?hcv und ?hsl

plot(Daten$StackLoss,Daten$Air, col.axis=6, font=2) 
# font=2 ist fette Schrift (bold). font.lab ist für die Achsentitel
colors()    # Siehe Anhang und http://fany.checkts.net/Farben.html
                                                
# Die acht Schnellfarben, die standardmäßig in palette() sind:
plot(1:8, col=1:8, pch=16, cex=4, xlab="plot ( 1:8, col=1:8 )")

# Hintergrundfarbe einstellen
par(bg=3)  # BackGround
plot(1,1, pch=16)
dev.off()

# Achsenfarben setzen: par(col=__)
old.par <- par(col=2, cex.axis=2, bg=8)
plot(1,1)
plot(Daten$Water, Daten$Acid)

# alte Grafikeistellungen wiederherstellen
par(old.par)
plot(Daten$Water, Daten$Acid)
# Alternativ das Graphikfenster schließen (geht auch normal übers "Kreuzigen"):
dev.off() # Beim nächsten Plotten sind die Settings wieder im Ursprungsformat

# Farben der Achsenwerte (Einheiten) : col.axis 
# und der Beschriftungen (Labels): col.lab
plot(1, col.axis = "sky blue", col.lab = "brown")


# Symbole mit Farben füllen
plot(1,1, pch= 1, cex=4)        # (Point CHaracter, Character EXpansion)
plot(1,1, pch=21, cex=4)        # pch 21 bis 25 können eingefärbt werden.
plot(1,1, pch=21, cex=4, bg=3)  # --> Übersicht der Symbole im Anhang
# bg hat hier also eine andere Bedeutung, als wenn es in par() gesetzt wird.

# bedingt einfärben: col = ifelse(wenn, dann, sonst)
plot(Daten$Water, Daten$StackLoss, 
     col = ifelse(Daten$StackLoss > 18, "blue", "red"), pch=15)
plot(Daten$Water, Daten$StackLoss,
     col = ifelse(Daten$Acid > 87, "blue", "red"), pch=15)

# Legende hinzufügen: legend(x,y-position der linksoberen Ecke, Einträge, ___)
legend(18, 35, c("Acid > 87", "Acid <= 87"), col=c(4,2), pch=15)
plot( c(1,3,2,5), type="l")  # Eine Übersicht der TYPEs ist im Anhang.
legend("topleft", c("Hallo", "R-Lerner"), lty=1) # Line TYpe siehe Anhang 
legend(3, 4, c("CheckThisOut",1,2), pch=1:3, bg="white")
legend(1.5, 3, c("Party","People"), title="Legende", bty="n", fill=3:4)
legend("bottom", c("Und", "tschüss"), inset=0.01, lty=c(NA, 2), pch=c(3,NA))

# Farbenrad aus der demo(graphics)
pie(rep(1,24), col=rainbow(24), radius=0.9) 
k <- c(3,1,10,8,6,3)
pie(k) # PieCharts sind fast nie geeignet, Daten adäquat zu visualisieren:
browseURL("www.storytellingwithdata.com/2011/07/death-to-pie-charts.html")
browseURL("http://www.perceptualedge.com/articles/08-21-07.pdf")
browseURL("http://www.stevefenton.co.uk/Content/Pie-Charts-Are-Bad/")
browseURL("http://blog.jgc.org/2009/08/please-dont-use-pie-charts.html")
browseURL("http://www.juiceanalytics.com/writing/the-problem-with-pie-charts/")
browseURL("http://eagereyes.org/techniques/pie-charts")



# 4.6 Linien, Text, Gitter -----------------------------------------------------

# Linie(n) einzeichnen: abline(a=Y-Schnittpunkt, b=Steigung, h=Y-wert, v=X-wert)
plot(Daten$Water, Daten$StackLoss)
abline(h=20)        # horizontale, waagerechte Linie
abline(v=24)        # vertikale, senkrechte, Linie
abline(a=-38, b=3)  # schräge Gerade: a = Y-Schnittpunkt, b = Steigung (!)
abline(5, 0.7, col=6, lty=2, lwd=2)   # Gerade ist also y = a + bx

# Graphikfenster öffnen, ohne zu zeichnen: type="n" für none/nothing/nichts/nada
# Verhältnis X zu Y (ASPect ratio): asp = ___ (je gleich groß: asp=1)
plot(c(-2,3), c(-1,5), type="n", xlab="x", ylab="y", asp=1)
abline(h=0, v=0, col="gray60")

# Text hinzufügen: text(x,y-Koordinaten, "text", Ausrichtung vom Punkt aus
text(1, 0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))  # adj: s. Anhang
text(1, 2, "fette Schrift", col = "gray60", font=2)  # font: Übersicht im Anhang

# Schriftart verwenden:
windowsFonts(MyFont = windowsFont("Georgia"))
windowsFonts()           # Für alle Bilder: am Skriptanfang par(family="MyFont")
plot(1:100, type="l")    # einzeln: mit family in plot, text, mtext, etc
plot(1:100, type="l", family="sans") # Arial, bei mir Default
plot(1:100, type="l", family="MyFont") # Georgia
plot(1:100, type="l", family="mono") # Courier New
plot(1:100, type="l", family="serif") # Times New Roman
windowsFonts(Bookman = windowsFont("Bookman Old Style"),
Comic = windowsFont("Comic Sans MS"), Symbol = windowsFont("Symbol"))
plot(1:100, type="l", family="Bookman")
plot(1:100, type="l", family="Comic")
plot(1:100, type="l", family="Symbol")


# Linientyp einstellen: lty = ___
abline(h = -1:5, v = -2:3, col = "lightgray", lty=3)
abline(a=1, b=2, col = 2)
text(1, 3, "abline( 1, 2 )", col=2, adj=c(-.1,-.1))
plot(8:5, pch=22, cex=3, bg=5)
grid(col=2) # überzeichnet die Ergebnisse, diese können neu gelegt werden  
points(8:5, pch=22, cex=3, bg=5)

# LinienEnd-typen einstellen (geht heutzutage auch im Plot-Befehl)
plot(c(5,5), lwd=8, type="l")
par(lend="butt")      #      0="round" (Default), 1="butt", 2="square"
points(c(4,4), lwd=8, type="l")



# 4.7 Achsenbeschriftungen 2.0 -------------------------------------------------

# Achsenbeschriftungen hinzufügen: axis(welche, wo, was)
# welche: 1=unten, 2=links, 3=oben, 4=rechts
plot(67:88, yaxt="n", xaxt="n")
axis(2, 69 )                     # einzelne Werte
axis(2, 76, "Grenze", las=1)     # mit Text (oder anderen Zahlen)
axis(2, 83, "Gr2", las=1, col.axis=2, col=8)  # schön farbig
axis(2, 86, tick=F, las=1)       # ohne Striche
axis(3, 5, labels=F)             # ohne Werte
axis(1, 5:7, col=5, col.axis=2)  # Intervallachse bei mehreren Positionen
axis(1, pretty(10:30) )          # pretty wählt schöne Zahlen
bm <- prettyNum(c(10000, 20000, 30000), big.mark="'"); bm # Tausend-Trennpunkte
axis(4, c(70,75,80), bm)
axis(3, 13:16, col="orange", lwd=0, lwd.ticks=3) # no line
axis(3, pos=par("usr")[3], col.axis=4, font.axis=2)  # Striche nach Innen
axis(2, 78, tcl=0.5, mgp=c(0,-1.8,0), las=1)  # Striche nach Innen (besser)
# die letzte Zahl von mgp ist hier für die Ticks (= Striche) und nicht 
# für die Achsen selbst, wie im Abschnitt Basics
axis(1,15.5, "", tcl=-0.4, col.ticks="purple")



plot(5:9, xlab=expression(Das~underline(sind)~italic(meine)~bold(Daten)~hier)  )
?expression
?plotmath
?axTicks()

plot(c(5,-2,7,8), main=c("per Hand geteilte",""), col.main=2)
title(main=c("","Überschrift"), col.main=4, cex.main=.7)



# 4.8 Logarithmierte Achsen mit Linien im Plot ---------------------------------

# Beschriftungen:
10^(-3:4)
options("scipen"=4) # scientific penalty, siehe ?options
10^(-3:4)
options("scipen"=4)
format(10^(-3:4), big.mark="'", trim=TRUE )
options("scipen"=0)
format(10^(-3:4), big.mark="'" )

install.packages("berryFunctions")
require(berryFunctions)
plot(1, ylim=c(1e-3, 1e4), log="y", yaxt="n", yaxs="i")
logAxis(2)
lines(seq(0.5, 1.5, len=50), 10^runif(50, -3, 4), col=2)

str(logVals()) # vals: values, labs: formatted labels, all: all 10^x values for lines



# 4.9 Anordnung mehrerer Graphiken in einem Bild -------------------------------

# mehrere Graphiken pro Fenster (Layout-Aufteilung)
par(mfrow=c(3,2))     # 3 Zeilen, 2 Spalten - 6 Graphiken in einem Fenster
plot(stackloss[,1:2])
plot(stackloss[,2:3])
# 6 (verschiedene) plot-Befehle an R schicken

# Zurücksetzen auf eine Grafik pro Fenster
par(mfrow=c(1,1)) # oder schließen und mit plot(x, y, ___) neu öffnen

# flexiblere Anordnung mehrerer Graphiken im Graphics:Device
layout(matrix(1:6, 3, 2), widths=c(7, 5), heights=c(1,1,1))
plot(stackloss[,1:2]) # widths und heights werden jeweils anteilig gesetzt
plot(stackloss[,2:3])
?layout

#	mehrere Graphiken auf einem Blatt: absolute Größen (in cm)
# zB. für quadratische plots. Grafikfenster vorher ausreichend groß ziehen!
layout(matrix(1:3, 1 ,3), widths=lcm(c(5, 5, 5)), heights= lcm(5))

# Layout anzeigen
m <- layout(matrix(1:6, 3, 2), widths=c(8,3), heights=c(2,4,1))
layout.show(m)
lay <- layout(matrix(c(1,1,2,2, 5,5,2,2, 3,3,4,4, 3,3,4,4), ncol=4, byrow=TRUE))
layout.show(lay)
lay <- layout(matrix(c(2,2, 2,2, 1,5, 3,5, 4,4, 4,6), ncol=2, byrow=TRUE)  )
layout.show(lay) 
dev.off()

# margin-text (Text in den Rändern)
layout(matrix(1:2, ncol=2), width=c(2.7,1))
plot(1)
mtext("rechts", side=4, col=4)
mtext("plot region", adj=1)
mtext("outer margin", adj=1, outer=TRUE, line=-1)
box("figure", col=2)
mtext("Figure", at=par("fig")[2], adj=1, outer=TRUE, line=-2)


# Graphiken übereinander zeichnen:
browseURL("http://www.statmethods.net/advgraphs/layout.html")
graphics.off() # etwaige alte Einstellungen löschen - neu beginnen
plot(rnorm(10))
par(fig=c(0,0.6, 0.6, 0.95), mar=rep(0,4), new=T)
plot(1, ann=F, axes=F)
pu <- par("usr") ; rect(pu[1],pu[3],pu[2],pu[4], col=3); rm(pu)
par(mar=c(3,4,1,1), new=T)
plot(rnorm(80))

# Aspect Ratio (potentiell wichtig)
x <- (1:50)*0.92  ;  y <- sin((1:50)*0.92) # http://www.amazon.com/dp/0521762936
plot(x,y)
plot(x,y, asp=3)


 
# 4.10 Graphiken mit Umrandung -------------------------------------------------

par(oma=c(1,2,2,4)) # outer margins
lay <- layout(matrix(c(1,1,2,3, 4,4,4,4, 5,5,5, 6), ncol=4, byrow=TRUE)  )
layout.show(lay)
# Wenn man mit Layout arbeitet, sollte man vorm ersten Plot par(cex=1) setzen!
plot(1:9)
par(cex=1)
plot(2:-5)
box(lwd=2, col=7)
box("figure", col=2)
box("inner", col=3)
box("outer", col=4, lwd=4)

graphics.off()

hist(rnorm(20))  # 20 Zufallszahlen aus der Normalverteilung. Mehr in Kap. 5
# Histogramm siehe Kapitel 6.
box(col = "blueviolet", lwd=3, lty=2)
# zB wichtig nach abline, da das den  Rand der Graphik selbst überplottet.

win.graph(width=3, height=2) # Öffnet Graphikfenster bestimmter Größe (inches)



# 4.11 Speichern von Bildern: bmp("dateiname.bmp") -----------------------------

# automatisch im WorkingDirectory, ansonsten im Dateinamen den Pfad einfügen
png("MeinBild.png") # Defaultbildgröße: 480 x 480 Pixel
# danach die Graphikbefehle abschicken
plot(1:10)
dev.off() # Device auch wieder schließen!
getwd() # Schau dir das Bild hier im Explorer deines Computers an...

# Vektorgraphik (ohne Pixel zoombar): pdf
pdf("map.pdf", paper="a4", width=21/2.54, height=29.7/2.54)
par(mar=rep(0,4))
image(volcano)
dev.off()



# praktisch unter Windows:
# Im Graphikfenster - Datei - Speichern als - png
plot(c(7,5,11,2,4), type="b") # aber nicht reproduzierbar!
# *.wmf file that can be copy-pasted into a word file:
win.metafile("mygraph.wmf")          # funktioniert nicht immer gut!
plot(cumsum(rnorm(30)), type="l")    # Und nur unter Windoof.
dev.off()


# Zum Publizieren geeignete Bildformate:
png("MeinBild.png", width=600, height=400, units="px")
# png ist ein gutes, kleines, genaues Format
# 600 x 400 ist ganz gut, um direkt in DIN A4 (Word-)Dokumente einzufügen.
plot(1:10)
dev.off()

png("MeinBild.png", width=4.5, height=5, units="in", pointsize=14, res=200)
# Für Publikation Schriftgröße sinnvoll setzen! http://civilstat.com/?p=557
plot(1:10)
dev.off()

# eps-äquivalente  (Encapsulated  Postscript, kann nachbearbeitet werden)
postscript("versuch.eps", onepage=TRUE, horizontal=FALSE)
plot(rnorm(10))      # wenn dir eps nichts sagt, einfach überspringen.
dev.off()


# Fortlaufende Nummerierung bei mehreren Bildern: %03d im Dateinamen
png("Bild%d.png")    # %03d hängt führende Nullen (leading zeros) vor
par(bg=2)
plot( c(6,9,0,2,8), type="h", lwd=7 )
plot( 1:8 )
# danach:
dev.off()  # sonst ist das letzte Bild noch leer!

# Verschiedene Speichermöglichkeiten:
dir.create("PNG_Test_Bilder")
oldwd <- setwd("PNG_Test_Bilder")

png("Test_A_%d.png") # ohne führende Nullen von 1:n durchnummeriert
for(i in c(1:8, 837, 2087, 2333)) plot(i) # i ist nicht im Dateinamen!
dev.off()

png("Test_B_%4d.png") # mit Leerzeichen
for(i in 1:13) plot(i)
dev.off()

png("Test_C_%04d.png") # mit Nullen
for(i in 56:68) plot(i)
dev.off()


for(i in c(1:8, 837, 2087, 2333))  # mit i im Dateinamen
  {
  png(paste("Test_D_",i,".png", sep="")) 
  plot(i)
  dev.off()
  }

setwd(oldwd)
unlink("PNG_Test_Bilder", recursive=T) # Seid vorsichtig mit diesem Befehl, der
# kann auch das Skript, mit dem ihr es geschrieben habt, löschen!
# Und alles mögliche andere! Wildcards nur nutzen, wenn ihr genau wisst, was die
# auswählen!    Und die gelöschten Dateien landen nicht im Papierkorb!



# 4.12 Beidseitige y-Achsenbeschriftung ----------------------------------------
# Primär und Sekundärachse (Zweite Y-Achse mitbenutzen)

# Lesetipp: http://rwiki.sciviews.org/doku.php?id=tips:graphics-base:2yaxes
# Graphiken mit zwei unkorrelierten Achsen sind problematisch!
# perceptualedge.com/articles/visual_business_intelligence/dual-scaled_axes.pdf
# http://junkcharts.typepad.com/junk_charts/2006/06/illusion_of_suc.html
# http://junkcharts.typepad.com/junk_charts/2006/05/the_crossover_l.html

# Möglichkeit 1, günstig für gleiche y-Bereiche
par(mar=c(2,4,2,4))  # wichtig: rechts mehr Platz machen 
a <- c(6,2,9,0,4)    ; b <-  c(8,7,1,4,6)   ; d <-c(43,65,12,23,27)
plot (a, type="l", las=1, ylab="Variable 1")
lines(b, col=2)
axis(4, 2:9, letters[1:8], col.axis=2, las=1)
mtext("Variable 2", 4, 2.5, col=2)  # mtext("Text", Achse, Abstand)
points(4, 7, cex=2)  # Y-Koordinaten immer von links ("f" geht nicht)

# Bei ungleichen Bereichen muss auf linkes ylim normiert werden
plot (a, type="l", las=1, ylab="Variable 1")
lines(d * 9/65, col=3)
axis(4, pretty(d)*9/65, pretty(d), col.axis=3, las=1)
mtext("Variable 2", 4, 2.5, col=3) 

# Möglichkeit 2, da spart man sich die ganze Normierung:
dev.off() # Schließt altes Graphikfenster
werte <- seq(-6,6,.01)
par(mar=c(2,4,3,4))
plot( werte, dnorm(werte), type="l", las=1, ylab="Dichtefunktion",
      main="Normalverteilung")
# pnorm und dnorm werden im Kapitel 5 vorgestellt.
par(new=T, xaxt="n")
plot(werte, pnorm(werte), type="l", col=2, ylab="", xlab="", yaxt="n")
axis(4, pretty(0:1), col.axis=2, las=1)
mtext("kumulierte Wahrscheinlichkeit", 4, 2.5, col=2)
legend("topleft", c("dnorm", "pnorm"), col=1:2, lty=1)
abline(h=0.3, lwd=2)
par()$usr # Graphikbereich ist jetzt der vom zweiten Plot!



# 4.13 Achsen unterbrechen, Pakete installieren und laden ----------------------

# Cran Mirror auf Berlin setzen (Siehe Kap 10 für dauerhaftes Setting)
options(repos='http://mirrors.softliste.de/cran')

# Package plotrix installieren (einmalig), kann man anschließend auskommentieren
# R > Pakete > Installiere Paket(e)... > CRAN > plotrix     ODER:
install.packages("plotrix") 
# offline Tipp: Zips der Pakete in R/Library speichern.
# dann kann man auf einem anderen Rechner per USB-Stick ohne Internet arbeiten.

# Jetzt das Paket laden (Nach jedem Start von R)
library(plotrix)    # Mit "Anführungsstrichen" ist auch OK
# Breaks werden zum Plot hinzugefügt, bei Überlappung evt. Daten neuzeichnen
plot(4:15, ylim=c(2,15))
axis.break(2, 2.9, style="slash")

# Plot mit größeren Lücken 
gap.plot(c(rnorm(10)+4,rnorm(10)+20), gap=c(8,16),xlab="Index",
ylab="Zufallszahlen",col=c(rep(2,10),rep(3,10)), ytics=c(3,4,5,19,20,21))
legend(12,6,c("High group","Low group"), pch=1, col=3:2)
dev.off()



# 4.14 mehrdimensionale Info in Graphiken --------------------------------------

# Farbe zeigt dritte Dimension:
# Matrix:
volcano[1:10, 1:10]; dim(volcano)
image(volcano)
# Punktdaten:
install.packages("berryFunctions")
library(berryFunctions)
i <- c( 22,  40,  48,  60,  80,  70,  70,  63,  55,  48,  45,  40,  30,  32)
j <- c(  5,  10,  15,  20,  12,  30,  45,  40,  30,  36,  56,  33,  45,  23)
k <- c(175, 168, 163, 132, 120, 117, 110, 130, 131, 160, 105, 174, 190, 183)
colPoints(i,j,k, cex=1.5, pch="+", add=FALSE)


# Dichte der Daten zeigen: sunflowerplot
par(mar=c(3,0,3,0))
sunflowerplot(1,1, xlim=c(1,20))
for (i in 2:20)   sunflowerplot(rep(i,i), rep(1,i), xlim=c(1,20), add=T)
title(main="sunflowerplot")
# for - Schleifen wiederholen Operationen. Details in Kapitel 11.
# Es geht hier erstmal um die Darstellung der Sonnenblumen.

# Transparenzplots
x <- rnorm(2000) # 2000 Zufallszahlen aus der Normalverteilung
y <- rnorm(2000) # Siehe Kapitel 5 für Details.
plot(x, y, col = rgb(0, 0, 0, alpha = 0.2), pch = 16)

# kontinuierliche Dichte zeigen: hdr.boxplot.2d
install.packages("hdrcde")  # nur einmal
require(hdrcde)
x <- c(rnorm(200,0,1),rnorm(200,4,1)) 
y <- c(rnorm(200,0,1),rnorm(200,4,1)) 
par(mfrow=c(1,2)) 
plot(x,y, pch="+", cex=.5) 
hdr.boxplot.2d(x,y) 

# Kontinuierliches Histogramm   install.packages("UsingR")
library(UsingR)
simple.violinplot(decrease ~ treatment,
                  data=OrchardSprays, col="bisque", border="black") 



# 4.15 Dreidimensionales Plotten -----------------------------------------------

# einige Basic-Befehle
x <- 1:10   ; y <- 1:10 ; z <- matrix(rnorm(100), ncol = 10)
round(z, 2)
filled.contour(x, y, z)
contour(x, y, z)
persp(x,y,z)

# 3d-plot von allen Seiten betrachten
# install.packages("rgl") # nur einmal zum downloaden und installieren.
library(rgl) # in jeder Sitzung.
t <- seq(0,30,0.01)  ; x <- 1/pi*t      ; y <- sin(t)   ; z <- cos(t)
open3d() # Fenster in Sichtweite ziehen
plot3d(x,y,z, type = "l", xlab = "X", ylab = "Y", zlab = "Z")
# mit der Maus klicken und ziehen. Cool, was?
r3dDefaults

x <- seq(-5,5,len=41) ; y <- x
z <- outer(x,y,function(x,y){ifelse(x*x+y*y==0,NA,
                                    sin(3*sqrt(x*x+y*y))/sqrt(x*x+y*y))})
open3d()
plot3d(x,y,z, type="l", xlab="X-Achse", ylab="Y-Achse", zlab="Z-Achse")



# Persp benutzen
?volcano   ;  v <- volcano
persp(1:nrow(v), 1:ncol(v), v, theta=135, phi=30, shade=0.75, border=NA)

x <- y <- seq(-5,5,len=41)
z <- outer(x,y,function(x,y){ifelse(x+y==0,NA,(x^2-2*x+3*y)/(x+y))})
for (i in 1:360)
persp(x,y,z, theta=i, shade=0.75, border=NA, ticktype="detailed", expand=0.5)

z2 <- outer(x,y,
function(x,y) ifelse( x*x+y*y==0, NA, sin(3*sqrt(x^2+y^2))/sqrt(x^2+y^2) )   ) 
for (i in 1:360)
persp(x,y,z2, theta=i, phi=i, shade=0.75, border=NA, ticktype="detailed",expand=0.5)
persp(x, y, z, theta=30, phi=30, expand=0.5, col="lightblue")
persp(x, y, z, theta=30, phi=30, expand=0.5, col="lightblue", ltheta=120,
      shade=0.75, ticktype="detailed", xlab="X", ylab="Y", zlab="Z")
graphics.off() # schließt alle Graphikfenster

x <- seq(-10, 10, length = 50)   ;  y <- x
rotsinc <- function(x,y) 
            { sinc <- function(x) { y <- sin(x)/x ; y[is.na(y)] <- 1; y } 
              10 * sinc( sqrt(x^2+y^2) ) }
z <- outer(x, y, rotsinc) ; dim(z)

par(bg = "white",mfrow=c(1,2),mar=rep(1.5,4)) 
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue") 
title(sub=".")## work around persp+plotmath bug 
title(  main = expression(  z == Sinc(sqrt(x^2 + y^2))  )  ) 
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue", 
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Z")
dev.off()

# Baumgrößen in 2D-darstellen --------------------------------
graphics.off() # bestehende Devices mit ihren Einstellungen schließen
Orange
str(Orange) # komischer Objekttyp
Orange$Tree <- as.numeric(Orange$Tree) # factor mit anderen Zahlen...
# Siehe auch Beispiele in ?Orange
plot(Orange[Orange$Tree==1, 2:3], main="Orange Trees",
     xlim=range(Orange$age), ylim=range(Orange$circumference), type="o")
for(i in 2:5 )  lines(Orange[Orange$Tree==i, 2:3], col=i, type="o")
text(x=c(rep(1600,4), 1250), y=c(130,150,170,195,195), paste("Tree", 1:5), 
     adj=1, col=1:5)

# Falls gewünscht: für jeden Baum ein eigenes Plot, je mit gleicher Skalierung
par(mfrow=c(2,3), mar=c(3.5,3,2,1), mgp=c(2,1,0))
for(i in 1:5 )
    {
    plot(Orange[Orange$Tree==i, 2:3], main=paste("Tree Nr.", i), col=i,
         xlim=range(Orange$age), ylim=range(Orange$circumference), type="o")
    box("figure")
    }

# Baumgrößen in 3D ------------------------------------------
open3d()
plot3d(Orange$age, Orange$Tree, Orange$circumference, type="s", col=Orange$Tree)
# Übersichtlicher ist es nicht unbedingt... Aber schön explorativ mit der Maus.



# 4.16 Fehlerbalken (Error bars) -----------------------------------------------

# Funktionen für Konfidenzintervall definieren (Siehe Kap 12)...
cil <- function(dat) return( t.test(dat, conf.level=.95)$conf.int[1] )
ciu <- function(dat) return( t.test(dat, conf.level=.95)$conf.int[2] )
# ... und anwenden:
cil( rnorm(30, mean=5)  ) # untere Grenze des 95%igen Konfidenzintervals

# Integrierter Datensatz Iris (siehe Kapitel 7)
head(iris)
str(iris)

# Werte zuordnen  ( Vektoren mit Positionen der Fehlerbalken definieren )
mitte <- tapply(iris$Sepal.Length, iris$Species, mean)# Funktion mean anwenden
unten <- tapply(iris$Sepal.Length, iris$Species, cil) # auf die Sepal.Length,
oben  <- tapply(iris$Sepal.Length, iris$Species, ciu) # getrennt nach Spezies

# Barplot und x-Positionen der Balken
x.values <- barplot(mitte, ylim=c(0,7), col="cyan") 

# Werte anschauen mit 3 Stellen (2 nach Komma)
print( data.frame(x.values, mitte, unten, oben) , digits=3)

# Fehlerbalken einfügen
arrows(x.values, unten, x.values, oben, angle=90, code=3, length=0.3)
# mit length lässt sich die Strichbreite einstellen.

# Oder mit errbar im Paket Hmisc:   install.packages("Hmisc")
library(Hmisc)
barplot(mitte, space=0, ylim=c(0,7))  ; axis(1, 0:3)
errbar(1:3-.5, mitte, oben, unten, add=T) 
barplot(mitte, space=0, ylim=c(0,7))  ; axis(1, 0:3)
errbar(1:3-.5, rep(NA,3), oben, unten, add=T)  ; box()


# Im Paket "gplots" gibt es die Funktion "plotCI"    install.packages("gplots")
library(gplots) 
?plotCI
# Example with confidence intervals and grid
( hh <- t(VADeaths)[, 5:1] )   ;   ci.l <- hh * 0.85   ;   ci.u <- hh * 1.15
barplot2(hh, beside=TRUE, legend=colnames(VADeaths), ylim=c(0, 100), 
         col=c("lightblue", "mistyrose", "lightcyan", "lavender"),
         main="Death Rates in Virginia", font.main = 4, plot.grid=TRUE,
         sub="Faked 95 percent error bars", col.sub = "gray20", 
         cex.names=1.5, plot.ci=TRUE, ci.l=ci.l, ci.u=ci.u,) 

browseURL("http://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_ptline.R")

# Auch plotrix hat eine Funktion:   install.packages("plotrix")
library(plotrix)
plotrix::plotCI(1:3, mitte, ui=oben, li=unten, pch=1, slty="solid", scol=4)
y <- runif(10) ; err.x <- runif(10) ; err.y <- runif(10)
plotCI(1:10,y, err.y, pt.bg=2, pch=21, xlim=c(0,11))
plotCI(1:10,y, err.x, pt.bg=2, pch=21, err="x", add=TRUE)



# 4.17 Fläche unter Graphen füllen ---------------------------------------------

bspx <- c(1,3,4,5,7,9,10,13)
bspy <- c(7,4,9,2,6,5,9,11)
plot(bspx,bspy, ylim=c(0,15))
letzte <- length(bspx)
polygon( c(bspx, bspx[letzte], bspx[1]) , c(bspy, 1, 1), col=2 )


# Einheitsganglinien mit Mittelwert und Konfidenzband
v <- c(0,1,3,5,6,7,8,7,6,5,4,4,3,3,2,2,2,1,1,1,0,0,0,0)/30  ; lv <- length(v)
set.seed(17) # gibt dem Zufallszahlengenerator einen Startpunkt.
(   m <- matrix(v+runif(lv*10, 0, 0.13), nrow=lv)   )
plot(m[,5], type="n", las=1, ylab="Ordinaten", 
     xlab="Stunden nach Ereignisbeginn", main="Einheitsganglinie")
for(i in 1:10) lines(m[,i])
cia=0 ;  cib=0
for (i in 1:lv) { cia[i]=t.test(unlist(m[i,]), na.rm=T)$conf.int[1]
                  cib[i]=t.test(unlist(m[i,]), na.rm=T)$conf.int[2] }
cia ; cib
polygon(c(1:lv,lv:1), c(cia, cib[lv:1]), col="orange", border="transparent")
cic=0 ;  cid=0
for (i in 1:lv)
          { cic[i]=t.test(unlist(m[i,]), conf.level = 0.7, na.rm=T)$conf.int[1]
           cid[i]=t.test(unlist(m[i,]), conf.level = 0.7, na.rm=T)$conf.int[2] }
polygon(c(1:lv,lv:1), c(cic, cid[lv:1]), col=2, border="transparent")
for(i in 1:10) lines(m[,i])
lines(apply(m, 1, mean, na.rm=T), col=3, lwd=3)
par(lend="butt")
legend("topright",
c("Einzelne UHs","95% Konf. Int.", "70% Konf. Int.", "Mittelwert"),
lwd=c(1,7,7,3),col=c(1,"orange",2,3))
mtext("10 Ereignisbasierte Unit Hydrographs sind zuwenig!",1,-2, adj=.4)


# Einfügen von Rechtecken
plot(1:10)
rect(1,5,3,7, col="green")

# polygon, segments, symbols, arrows, curve, abline, points, lines
# http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=154
# SEHR EMPFEHLENSWERTE SEITE!!!  (Siehe Source Code für die Befehle)



# 4.18 Funktionen zeichnen  ----------------------------------------------------

curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")

plot(cos, xlim = c(-pi,3*pi), n = 1001, col = "blue")

chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n=2001)
curve(chippy, -8, -5)

# Interferenz
x <- seq(-pi,pi,len=1000)
windows(record=T)
par(mar=rep(0,4))
cool <- function (a) plot(a*sin(x), a*cos(x), asp=1, type="l")
# Die for-Schleife zeichnet für jeddes i ein Bild. Mehr in Kap 11.
for (i in 1:500/500)  cool(c(i,1))   # Vorischt: höhere Rechen- und plot-zeit
for (i in -600:300/500)  cool(c(i,i-.43))
# Jetzt kann man mit Pg Up und Pg Dn die 901 Bilder durchklicken...
# Oder die speichern und als GIF animieren...



# 4.19 zooming in graphics -----------------------------------------------------
install.packages("berryFunctions") # mit meinem Paket ---
require(berryFunctions)

a <- rnorm(90); b <- rexp(90)
plot(a,b, las=1)
pointZoom(a,b,,1,40)

d <- data.frame(a,b)
class(d)
plot(d)
pointZoom(d)

install.packages("playwith") # mit einer tollen interaktiven Geschichte ---
require(playwith)
?playwith
playwith( {plot( 1:10, rnorm(10) )
           lines( (1:10) + 0.5, rnorm(10,1,3), col='red' ) })


install.packages("TeachingDemos") # mit einem weiteren Paket ---
require(TeachingDemos)
?zoomplot


# 4.20 und ein würdiger Abschluss des Kapitels ---------------------------------

install.packages("onion")
require(onion) 
data(bunny) 
p3d(bunny,theta=3,phi=104,box=FALSE) 



# 4.21 Übersicht der wichtigsten Graphikbefehle --------------------------------
# Noch to be ergänzt. Für Hinweise wäre ich sehr dankbar!

# high-level plots (fangen neue Graphik an)
plot(x, y)
pairs(data.frame)
coplot(a ~ b | c + d)
qqnorm(x)
qqline(x)
qqplot(x, y)
hist(x)
dotchart(x, ...)
image(x, y, z, ...)
contour(x, y, z, ...)
persp(x, y, z, ...)
barplot(y)
boxplot(y)
curve()
pie()

# low level plots   (fügen zur bestehenden Graphik hinzu)
points()
lines()   # wie points, mit type="l" als default (spart Tipparbeit)
text()
abline()
polygon()
rect()
legend()
title()
axis()
