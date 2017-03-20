
makepdf <- FALSE

# Übersichten für Anhang    # height = width * 29.7 / 21
if(makepdf) pdf("Anhang.pdf", title="Rclick_Handbuch-Berry_Anhang", paper="a4", width=7, height=10)

par(ann=FALSE)

# Erste PDF-Seite---------------------------------------------------------------

layout(matrix(1:4, ncol=1, byrow=TRUE), heights=c(4, 2, 2, 1.5)    )

################################################################################
# type (plotting type: points, lines, stairs, bars)

par(mar=c(0,0,2,0)+.5, cex=1)
a=1:4; b=c(0.5,4,2,1)  ; tp=c("p","l","b","o","c","n","s","S","h")
plot(1:12, type="n", ylim=c(0,14), axes=FALSE)
title(main="plot ( x, y,  type = \"  _  \"  )")
i <- 1
for(y in 3:1)
   for(x in 1:3) {
      points(a+4*x-4, b+5*y-5, type=tp[i])
      text(3.5+4*x-4, 3+5*y-5, paste0("\"",tp[i],"\"")) ; i <- i+1 }
abline(v=c(5,9)-.5, h=c(4,9)+.75, col=8 )
box("figure")

################################################################################
# lty  (Line TYpe, Linientyp)

par(mar=c(0,0,0,0)+.5, cex=1)
x <- c(4, 8, 9,   9)
y <- c(8, 8, 9, 9.8)
plot(x,y, type="n", ylim=c(2,9), xlim=c(1,9), axes=FALSE)
for(n in 0:6) lines(x, y-n, lty=n)
for(n in 0:6) text(3.7, 8-n, n)
text(5, 8.7, "plot ( x, y,  lty = _ )", font=2, cex=1.2)
Names=c("dashed","dotted","dotdash","longdash","twodash")
text(3.2, 8, "\"blank\" (keine Linie)", adj=1)
text(3.2, 7, "\"solid\" (default)", adj=1)
text(3.2, 7-(1:6), paste0("\"", Names, "\""), adj=1)
box("figure")

################################################################################
# adj (Text justieren)

par(mar=c(0,0,2.5,0), cex=1 )
plot(1:6, type="n", ylim=c(4,5.8), axes=FALSE)
title(main="text ( x, y, labels,  adj = _  )")
# abline(v=c(2,4), h=1:6, col=8, lty=3); box()
points(c(2,3.5,5), rep(5.5, 3), pch=16) 
text(2  , 5.5, "Text ohne adj, default=0.5")
text(3.5, 5.5, "adj=1", adj=1)
text(5  , 5.5, "adj=0", adj=0)
points(c(2,3.5,5), rep(5, 3), pch=16) 
text(2  , 5, "adj=1.5", adj=1.5)
text(3.5, 5, "adj=0.8", adj=0.8)
text(5  , 5, "adj=-0.2", adj=-0.2)
points(x=rep(c(3.4, 3.6), each=2), y=rep(c(4.4, 4.6), 2), pch=16)
text(3.6, 4.6, "adj=c(0,0)", adj=c(0,0))
text(3.6, 4.4, "adj=c(0,1)", adj=c(0,1))
text(3.4, 4.6, "adj=c(1,0)", adj=c(1,0))
text(3.4, 4.4, "adj=c(1,1)", adj=c(1,1))
box("figure")

################################################################################
# gray  (Graustufen)

par(mar=c(3, 0.1, 2, 0.1), cex=1)
plot(0:50/50, rep(1,51), col=gray(0:50/50), pch=16, cex=2, axes=FALSE)
axis(1)
title(main="gray (x)", xlab="x", mgp=c(1.5,1,0))
box("figure")

################################################################################

# Nächste PDF-Seite-------------------------------------------------------------

layout(matrix(c(1,2,2), ncol=1, byrow=TRUE)  )
# lend (Line END type, Linienendtypen)

par(mar=c(2.4, 2, 2, 0)+.5, cex=1)
plot(0:3, type="n", lwd=9, xlim=c(0,6), axes=FALSE)
title(main="lines ( x, y,  lend = _  )")
abline(h=0:3, v=c(0:3,6), col=8)  ; axis(2, 0:2, las=1); axis(1, 0:2)
Lwd <- 9
lines(rep(0,2),c(0,3), lwd=Lwd, lend=0)
lines(rep(1,2),c(0,3), lwd=Lwd, lend=1)
lines(rep(2,2),c(0,3), lwd=Lwd, lend=2)
lines(c(3,6),c(0,1), lwd=Lwd, lend=2)
lines(c(3,6),c(1,2), lwd=Lwd, lend=1)
lines(c(3,6),c(2,3), lwd=Lwd, lend=0) ; box()
Names <- c("2  \"square\"", "1  \"butt\"", "0  \"round\"\n(default)      ")
text(4, 0:2+.7, Names)
box("figure")

################################################################################
# font (Schriftart, Symbole)

par(mar=c(0,0,2.5,0)+.5, cex=1)
big <- "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z "#!  '  § $ % & / (  ) ="
small <- "a  b c  d  e f  g  h  i j k  l  m n  o  p q  r  s  t  u  v  w  x  y z 1 2 3 4 5 6 7 8 9 0" 
plot(0:15, 0:15, type="n", ylim=c(7.2, 14.7), yaxs="i", axes=FALSE)
title(main="text ( x, y, labels,  font = _  )")
abline(h=14.7, col=8)
text(0.1  ,14.3,   big, adj=0)
text(0.1  ,13.9, small, adj=0)
text(0.1  ,13.5, "(default, font=0 oder 1)", adj=0)
abline(h=13.1, col=8)
text(0.1  ,12.7,   big , font=5, adj=0)
text(0.1  ,12.3, small , font=5, adj=0)
text(0.1  ,11.9, "(Griechisch (Symbole), font=5). Evtl vom PDF-Viewer nicht dargestellt.", adj=0)
abline(h=11.5, col=8)
text(0.5,11.1 , "normal, font=1", font=1, adj=0)
text(4  ,11.1 , "fett, font=2", font=2, adj=0)
text(7  ,11.1 , "schräg, font=3", font=3, adj=0)
text(11 ,11.1 , "fett schräg, font=4", font=4, adj=0)
abline(h=10.7, col=8)
text(7, 10.3, "in X11, png, etc. zudem folgende fonts (in pdf nicht dargestellt):")      # satz ausformulieren
text(0  ,9.9, "gedrungen: font=6", font=6, adj=0)
text(5  ,9.9, "gedrungen: fett, font=7", font=7, adj=0)
text(11 ,9.9, "gedrungen: schräg, font=8", font=8, adj=0)
text(2  ,9.5, "gedrungen: fett , schräg, font=9", font=9, adj=0)
text(14 ,9.5, "komische Symbole, font=19", adj=1)
abline(h=9.1, col=1)
text(7, 8.7, "zudem: labels = expression(lambda, pi * \"=\" * eta, mu,              # k^2^g
     frac(tau, gamma * \" * \" *Psi), \"b\" %up% \" \" , delta[index], 2^k, \"A\" %->% \"B\"  )")
text(seq(1, 14, len=8), 8.1 , expression(lambda, pi * "=" * eta, mu, frac(tau, gamma * " * " *Psi), "b" %up% "", delta[index], 2^k, "A" %->% "B"  ) )
text(7, 7.5, "demo(plotmath) für eine noch viel größere Auswahl")
box("figure")

################################################################################

# Nächste PDF-Seiten, jeweils volle Seite pro Plot -----------------------------
layout(matrix(1) )
# pch (Point CHaracter, plotsymbol)   (2 Plots!)

par(mar=c(0,0,2,0), cex=1)
plot(1, type="n", ylim=c(0.3, 15.3), xlim=c(0.5, 10.5), yaxs="i", xaxs="i", axes=F) 
title(main="plot ( x, y,  pch =   _  )  - Seite 1")
curp <- 1:10 # current pchs on line
for(y in 15:1)
    {abline(h=y+.25)
    text(1:10, y, curp)
    points(1:10, rep(y-.5, 10),
        pch=ifelse(curp %in% c(26:32,127,129,141,143,144), NA, curp), bg=2)
    curp <- curp+10
    }
text(5.5, 12.5, expression("" %<-% "21:25 mit bg füllbar. 26:32 nicht vergeben"), adj=0)
box("figure")
plot(1, type="n", ylim=c(0.3, 15.3), xlim=c(0.5, 10.5), yaxs="i", xaxs="i", axes=F) 
title(main="plot ( x, y,  pch =   _  )  - Seite 2")
curp <- 151:160 # current pchs on line
for(y in 15:5)
    {abline(h=y+.25)
    text(1:10, y, curp)
    points(1:10, rep(y-.5, 10), pch=ifelse(curp %in% c(157, 160, 256:260), NA, curp), bg=2)
    curp <- curp+10
    }
abline(h=4.25)
#curp <- -1:-10 # current pchs on line    negative pch nicht dargestellt!
#for(y in 3:1)                               # können aber rauskopiert werden...
#    {abline(h=y+.25)
#    text(1:10, y, curp)        # Todo: Fontmetrik für das Zeichen 0x8d unbekannt
#    points(1:10, rep(y-.5, 10), pch=curp, bg=2)
#    curp <- curp-10
#    }
text(5.5, 3.5+.25, "Negative pch Zeichen werden via Device pdf nicht dargestellt
sind aber in png etc oft möglich.")
box("figure")

################################################################################
# colors (Farben)     (3 Plots!)

par(mar=c(0,0,2,0)) 
cl <- colors()[-(152:253)] #  gray weglassen (Wiederholung als grey). 555 Farben übrig
# Großteil der greys weglassen:
cl <- cl[-(163:245)] ; cl[163:174] <- c("grey6", "grey10", "grey15",
                                 paste0("grey", 2:9*10), "grey94") # cl[155:185]
cl <- c(1:8, cl) # Schnellfarben vorneweg length(cl)
# 480 Farben übrig = 3*4*40 (Seiten * Spalten * Zeilen) 
curc <- 1:40 # current colors
for(Page in 1:3)
  {plot(1, xlim=c(-0.1, 4.1), ylim=c(-0.2, 40.1), type="n", axes=F, xaxs="i", yaxs="i")
  title(main=paste("colors (  )  - Seite", Page))  ; box()
  for(Column in 0:3)
    {rect(xleft=Column, xright=Column+0.2, ybottom=39:0, ytop=40:1-.1,
          col=cl[curc] , border = NA)
    text(Column+0.25, 40:1-.5,
       ifelse(cl[curc] %in% paste0("grey", c(6,15,94)), "...", cl[curc]), adj=c(0,0.5))
    curc <- curc + 40
    }
  box("figure")
  }
# http://research.stowers-institute.org/efg/R/Color/Chart/


if(makepdf) dev.off()


################################################################################


if(FALSE)
{


for(i in (1:255)[-c(26:31,127,129,141,143,144, 157)])
{
pdf("Anhang.pdf")
plot(i, pch=i)
dev.off()
flush.console()
}


arrows

# Pdf kann nicht alle fonts und symbole
png("versuch.png") ; plot(1, pch=-2); text(1.3, 1, "b", font=19) ; dev.off()
pdf("versuch.pdf") ; plot(1, pch=-2); text(1.3, 1, "b", font=19) ; dev.off()


# Symbole, Hershey-fonts, plotmath 

demo(Hershey)
dev.off()
demo(plotmath)
dev.off()

plot(1:10, type="n", xlab="", ylab="", main = "plot math & numbers")
theta <- 1.23 ; mtext(bquote(hat(theta) == .(theta)), line= .25)
for(i in 2:9)
    text(i,i+1, substitute(list(xi,eta) == group("(",list(x,y),")"),
                           list(x=i, y=i+1)))

?Hershey     # zB für Kyrillische Buchstaben
# write.table(matrix(paste("\\", 100:499, sep=""), ncol=50, byrow=T), 
# "hersheycodes.txt", col.names=F, row.names=F, sep=",")
SY <- c("\100", "\138","\200","\201","\202","\203","\204","\205","\208","\210",
"\211","\212","\213","\214","\215","\216","\243","\263","\277",
"\300","\301","\302","\303","\304","\305","\306","\307","\308","\309","\310",
"\311","\312","\313","\314","\315","\316","\317","\318","\319","\320","\321",
"\322","\323","\324","\325","\326","\327","\328","\329","\330","\331","\332",
"\333","\334","\335","\336","\337","\338","\339","\340","\341","\342","\343",
"\344","\345","\346","\347","\348","\349","\441","\442","\443","\444","\445",
"\446","\447","\448","\449","\450","\451","\452","\453","\454","\455","\456",
"\457","\458","\459","\460","\461","\462","\463","\464",
"\472","\473","\474","\475","\476","\477","\478")
plot(-1:-10, type="n")
for (k in 1:10) for (i in 1:10)  
text(i,-k, SY[i+10*k-10], vfont=c("serif","cyrillic"), cex=2)

################################################################################


# animierte Darstellung einer parameterabhängigen Funktion

# Definitionsbereich angeben: seq(von, bis, Schrittweite)
# ( kleine Schrittwerte geben hohe Präzision aber auch viele Bilder! )
# nach Einstellen der Werte jeweils die Zeile abschicken.
t   <- seq(-2,2, by=0.1)

# X- und Y-teil des Funktionsvektors angeben.
x   <- t^2-1
y   <- t*(t^2-1)

# Titel des Plots eingeben
tx <- "t²-1"
ty <- "t(t²-1]"

# X- und Y-Grenzen des Plots angeben
xgr <- c(-6,6)
ygr <- c(-6,6)

# Speicherort einstellen:
bmp("C:/Dokumente und Einstellungen/berrystud/Desktop/Parameter/parameter%02d.bmp")

# Nächste Zeilen durchlaufen lassen, Titelstandort mit adjust
# (0 = links, 1 = rechts), sowie Textstandort in Koordinaten nach Bedarf ändern
for (i in 1:length(t)) {
 plot(x[i],y[i], xlab="x", ylab="y", font.lab=2, xlim=xgr, ylim=ygr, pch=16, main="r(t) = (     )", cex.main=3, las=1)
 title(main=c(tx,ty), adj=0.63)
 text(-6,6, "t =", adj=c(0,1)); text(-5.4,6, as.expression(t[i]), adj=c(0,1))
 lines(x[1:i], y[1:i], col="darkgrey")}

# Nächste Zeilen durchlaufen lassen für Standbilder mit progressiv verbreitertem Graphen
for (i in 1:6) {
 plot(x,y, xlab="x", ylab="y", xlim=xgr, ylim=ygr, type="l", lwd=i,
      main="r(t) = (     )", cex.main=3)
 title(main=c(tx,ty), adj=0.63) }

# Diese Zeile für ein leeres Bild vor dem Neustart der Animation mehrmals abschicken.
plot(x,y, xlab="x", ylab="y", xlim=xgr, ylim=ygr, type="n",
     main="r(t) = (     )", cex.main=3)
title(main=c(tx,ty), adj=0.63)

# Speicherung der Bilder beenden
dev.off()

# Die Einzelbilder mit Ulead GIF Animator LightEdition oder ähnlichen Programmen
# zu einer Animation zusammenfügen.

################################################################################
}