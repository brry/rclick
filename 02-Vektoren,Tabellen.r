### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 2: Vektoren und Tabellen ###########################################

# 2.1 Vektoren erstellen + Indizieren
# 2.2 rep, seq, : 
# 2.3 Vektoren mit character 
# 2.4 Basic Statistics 
# 2.5 oft verwendete Operatoren 
# 2.6 Tabellen 
# 2.7 Tabellenindizierung 
# 2.8 Tabellen zusammenführen 
# 2.9 Zeilen und Spaltennamen 
# 2.10 Matrizen


# 2.1 Vektoren erstellen: c + Indizieren: [] -----------------------------------
# Vektoren erstellen, Elemente durch Kommas trennen
vekt1 <- c(1 , -2 , 3, 7:11)       # c steht für combine oder concatenate
vekt1
mode(vekt1)

# Auf Vektorelement zugreifen mit eckigen Klammern
vekt1 [5]
vekt1[5:6]

# Vektor mit einer entfernten Stelle erstellen/anzeigen: [-Element]
vekt2 <- vekt1 [-5]
vekt2

# Manipulation einzelner Elemente
vekt2[5] <- 28
vekt2

# Hinzufügen von Elementen. Zwischenkomponenten werden als NA eingefügt.
vekt2[10] <- -13
vekt2

vekt2[7]*3 # Zeigt nur das Ergebnis einer Berechnung an, ändert nicht den Vektor
vekt2
( k <- c(2,4,5) ) # Zuordnung in Klammern zeigt entstehendes Objekt gleich an
vekt2[k]    # zugegriffen wird immer vektoriell


# 2.2 rep, seq, : --------------------------------------------------------------
# Mehrfaches Wiederholen einer Zahl im Vektor: rep(Zahl, AnzahlWiederholungen)
vekt3 <- rep(2, 10)             # repeat
vekt3

# Sequenz: seq(von, bis, Schrittweite)
(  vekt4 <- seq(1, 5, 0.5)  )

# Sequenz: seq(von, bis, len = Anzahl Elemente)
(  vekt5 <- seq(1, 3, len=15)  )     # len kurz für length.out
seq(7, 6, -1/3) # Bei absteigenden Sequenzen negativen "by"-wert nutzen!

8:13 # Ganze Zahlen (integer) von 8 bis 13
-3:6

# folglich beim Entfernen beim Indizieren Klammern verwenden:
vekt1
vekt1[-(3:6)] # und nicht etwa so:
vekt1[-3:6] # Fehler: nur Nullen dürfen mit negativen Indizes gemischt werden


# 2.3 Vektoren mit character ---------------------------------------------------
# Zahlen als Text   (R kann damit nicht rechnen, ist als Überschrift sinnvoll)
(  vekt6 <- as.character(1:3)  )

# Zusammengesetzte Vektoren
(  vekt7 <- c(vekt1, -7:10, vekt2)  )

# Namen zuordnen
(  vekt8 <- c(2:4)  ) 
vekt9 <- c("a","b","c")
vekt9
names(vekt8) <- vekt9
vekt8       # ist nach wie vor ein Vektor, und keine Tabelle!
vekt8["b"]

seq(7, 8, .3)
rep(1:4, 2)
rep(1:4, each=2)

paste("WunderbareSache", 3:5)  # macht einen Vektor mit Modus character
paste(2, "hot", 4, "U")
paste("SeeYouL", vekt7[5], "ter", sep="")  # Auf bestehende Objekte zugreifen
# sep steht für Separation, dem Trennzeichen zwischen den Zeichenketten.


# 2.4 Basic Statistics ---------------------------------------------------------
Noten <- c(3.0, 2.3, 5, 1.3, 4, 1.7)
Noten # Objekt anzeigen lassen
mode(Noten) # numeric – also Zahlen

mean(Noten) # Mittelwert
var(Noten) # Varianz
sd(Noten) # Standardabweichung
median(Noten)# Median: Zahl an mittlerer Stelle der nach Größe sortierten Werten


# 2.5 oft verwendete Operatoren ------------------------------------------------
length(vekt1)# Anzahl Komponenten (Elemente) eines Vektors
length(vekt6)
max(vekt1)   # Wert des größten Elements in vekt1
max(vekt2)
max(vekt2, na.rm=T)     # NA Werte nicht berücksichtigen (steht für NA.ReMove)
vekt2 <- replace(vekt2, 8:9, 5) # ersetzt die Elemente 8:9 zum Wert 5
vekt2
max(vekt1, vekt2)
range(x)     # entspricht c(min(x), max(x))
which.max(x) # Index des größten Elements in x
which.min(x) # Index des kleinsten Elements in x
sum(vekt1)   # Summe der einzelnen Elemente
vekt2
sum(vekt2)
cumsum(vekt2)   # kumulierte Summe bis zum jeweiligen Element
sum(vekt1, vekt2) # sum nimmt mehrere Daten, ohne dass diese erst 
# zu einem Vektor zusammengefügt werden müssen. Bei mean ist das zB anders:
mean(vekt1, vekt2) # geht nicht
mean( c(vekt1, vekt2) )
# prod()       # Produkt
# union()      # Teilmenge
# intersect()  # Schnittmenge
# rev()        # reverse, kehrt Reihenfolge um
# sort()       # sortiert Vektoren

# Ist ein bestimmter Wert (5) in einem Datensatz (y) enthalten?
y <- 3:17
is.element(5, y) # identisch zu x %in% y
2 %in% y

# ><= Abfragen 
(  k <- c(1,5,3,2,7,6,1,2,8)  )
which(k==2)          # Welche Elemente sind = 2 ?
k[k==2] <- 2.5       # ändere alle 2 zu 2.5
k
subset(k, k>3)       # Welche Werte haben die Elemente mit Wert > 3 ?



# 2.6 Tabellen -----------------------------------------------------------------
# Dataframes = Tabellen mit verschiedenen Datentypen
# data.frame( Spaltenname = Vektor, zweiteSpalteÜberschrift = andererVektor)
tab1 <- data.frame(N=11:17, K=letters[1:7], Logisch=(1:7)>2)
tab1   # data.frame ist das gängigste Tabellenformat.

# Anzahl Reihen(Zeilen) / Spalten im Datensatz
nrow(tab1)
ncol(tab1)
dim(tab1)
# häufige Fehlermeldung:  "Fehler in ____nrow(xx)____ : Argument hat Länge 0 "
# evtl ist xx ein Vektor --> nehme length(xx) statt nrow(xx) / dim(xx)

# Namen der einzelnen Datenspalten aufrufen
names(tab1)

str(tab1)   # STRucture: Zeigt Objekttyp sowie Datentyp einzelner Spalten an
class(tab1) # data.frame
mode(tab1)  # Obertyp list, hier erstmal irrelevant



# 2.7 Tabellenindizierung ------------------------------------------------------
# Auf Elemente der Tabelle zugreifen: Objekt[Zeile,Spalte] 
tab1[2,3]    # Ausgabe des zweiten Elementes der dritten Spalte
# Beim weglassen der Angabe von Zeile oder Spalte werden alle genommen:
tab1[ ,2:3]  # Ausgabe einzelner Spalten als Spalte (Dataframe)
tab1[3,]     # Ausgabe einer Zeile

tab1["K"]    # Ausgabe einer Spalte als Dataframe
tab1 [3]     # andere Methode für's gleiche
tab1[,3]     # Ausgabe der Spalte als Vektor
tab1$K       # dito
tab1[1:4,3]  # Ausgabe eines Teils der Spalte als Vektor
tab1[1:4,3, drop=F] # Bleibt ein data.frame 
# Noch mehr Beispiele in Kapitel 3.3

as.character(tab1) # geht manchmal nicht für Tabellen oder macht Vektor draus

# Zeilen entfernen
d <- data.frame( a=1:10 , b=11:20 ) 
d   
(  e <- d[-(3:5),]  )

# Wertebedingt indizieren
blub <- data.frame(w=c("a001","b476","b342","a677"), A=c(4,5,3,2), 
                   B=c(2,4,3,6), C=c(2,8,4,1)  )
blub
blub[,2] <5
blub[blub$A<5,]
blub[blub$A<5 & blub$B<5,]
blub[blub[,2]<5 & blub[,3]<5,]



# 2.8 Tabellen zusammenführen --------------------------------------------------
# Mit rbind(a,b,d) lassen sich die Vektoren a, b und d reihenweise zu einer
# Matrix zusammenführen.
# Im Gegensatz dazu verbindet cbind() die Vektoren spaltenweise zu einer Matrix.
# data.frame() macht das auch so wie cbind.
f <- data.frame(a=11:14, b=58:55, d=236:233)
g <- data.frame(a=33:36, b=76:79, d=-28:-31)
f
g
rbind(f,g)  # Spaltenanzahl ncol der beiden Matrizen muss gleich sein
cbind(f,g)  # Zeilenanzahl nrow  der beiden Matrizen muss gleich sein



# 2.9 Zeilen- und Spaltennamen zuordnen ----------------------------------------
a <- data.frame( 5:-1, c(3,5,1,9,6,4,8) )    ; a
colnames(a) <- c("alles","klar")             ; a
rownames(a) <- LETTERS[a$klar]               ; a



# 2.10 Matrizen ----------------------------------------------------------------
# matrix ist ein weiterer Objekttyp. Hat anders als Dataframe nur 1 Datentyp!
# Matrix erstellen und füllen
(m <- matrix(NA, nrow=5, ncol=3))
m[4,2] <- 17    ;  m
m[1,] <- 8      ;  m
m[,3] <- 3:7    ;  m
(  m[5,] <- LETTERS[4:6]  )
m # matrix kann nur 1 Modus haben, darum wird jetzt alles in char umgewandelt!
noquote(m) # Gibt die Character ohne Anführungsstriche aus
str(m); class(m); typeof(m) # bleibt eine matrix mit charactern


# Lineare Gleichungssysteme lösen                   x1  x2      b
A <- matrix(c(16,4,74,16), ncol=2, byrow=T) ; A  #  16   4  |  25
b <- c(25,113)                                   #  74  16  | 113
solve(A, b) #  1.30 1.05
1.3 * 16 + 1.05 *  4 # 25
1.3 * 74 + 1.05 * 16 # 113  Die Resultate sind richtig...

