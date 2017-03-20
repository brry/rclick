### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 1: Grundlagen ######################################################

# 1.1 Hilfe aufrufen
# 1.2 Rechenoperatoren
# 1.3 Zuordnungen
# 1.4 Objektarten
# 1.5 Grund-Datentypen (Übersicht)
# 1.6 Objekttypen (Übersicht)


# Kommentare mit einem Kreuz (#) beginnen, werden von R nicht ausgewertet.
# Leerzeichen und -zeilen werden nicht ausgewertet; sie dienen der Übersicht.
# Mehrere Befehle in einer Zeile können mit ; voneinander getrennt werden.

# schon früh sollte man sich angewöhnen, bequem zu programmieren:
browseURL("http://RclickHandbuch.wordpress.com/install-r/tinn-r/")
# Besonders: TinnR Options > Shortcuts  sowie  Return focus to editor
# STRG + F : Find. Suche nach Wörtern. Weiter mit F3
# STRG + 9 schreibt Klammern und setzt den Cursor in deren Mitte

# Wer gerne mit VideoTutorials lernt, könnte hier schauen:
browseURL("https://www.youtube.com/user/Tutorlol")
# Ansonsten sei hier nochmals auf die Literaturliste im Anhang verwiesen...



# 1.1 Hilfe aufrufen -----------------------------------------------------------
# ? Befehl() oder    help.search("Befehl")
?mean()                # Befehlsteil in Klammern wird ignoriert
help.search("mean")    # kürzer: ??mean
help.start()           # HTML Hilfe (Automatischer Start des Browsers)
# am schnellsten: cursor auf Befehl im TinnR setzen, F1 drücken
# bei Befehlen mit Punkt: den ganzen Befehl zuerst markieren.
help.search("Kolmogorov Smirnov")
?ks.test



# 1.2 Rechenoperatoren ---------------------------------------------------------
7+11
3*9/5
3^2
16:21            # ganze Zahlen (Integer) von 16 bis 21 (als Vektor)
sqrt(81)         # Wurzel  (gleiches Ergebnis bei 81^0.5 )
abs(-12)         # absolutwert (Betrag)
sin(2.4*pi)      # ACHTUNG: keine Kommas (,) als Dezimaltrennzeichen verwenden!
exp(3)           # e^3
log(100)         # log(x) ist im R ln(x)
log10(100)       # log10(x) ist der Logarithmus mit Basis 10
factorial(5)     # 5! (5 Fakultät)
0.0002           # xx e yy bedeutet:   xx * 10 ^ yy  (Exponentialschreibweise,
3e5              # auch Scientific Notation genannt)
1.9343E+03 + 2
print(pi, digits=16) # print für eine benutzerdefinierte Ausgabe
options("digits"=16) # Ab jetzt jede Ausgabe mit 16 Nachkommastellen
sin(0.8)
options("digits"=7)  # wieder zurück auf die DEFAULT = Standard Einstellung
-1/0  ;  0/0  ;  1/0 # Inf = Infinity = unendlich; NaN = Not a number



# 1.3 Zuordnungen  (Englisch: Assignment) --------------------------------------
# Werte, Zeichenketten, Aussagen... als Variablen speichern (Objekten zuordnen)
# R unterscheidet Großschreibung!
probe1 <- 17.5 # Punkt als Dezimaltrennzeichen verwenden
# Anzeigen der zugeordneten Daten
probe1         # Das ist jetzt ein Objekt im Workspace, nur in dieser R-Sitzung.
probe1 + 12    # Jetzt kann damit auch weitergerechnet werden.

p2 <- 1 > 2        # Ist 1 größer als 2? Ergebnis wird als p2 gespeichert.
p2                 # Symbole möglichst kurz aber wiedererkennbar wählen
p3 <- sin (5)      # Werte in Radiant (pi = 180°)     x = alpha*pi/180°
sin(45*pi/180)     # sinus von 45°
p4 <- "ein Text"   # Anführungsstriche nicht vergessen
p4
p5 <- TRUE         # oder kürzer: p5 <- T  (bzw F für FALSE)
sqrt(9030) -> p6   # Zuordnungen in beiden Richtungen möglich, aber "->"
# sollte vermieden werden, da es sehr ungewöhnlich, daher schlecht lesbar ist.
p7 = 7:15          # Zuordnen mit "=" ist möglich, aber ungünstig:
p7                 # http://r.789695.n4.nabble.com/assign-td2291499.html
# Funktioniert als Zuordnung nur direkt im Skript, nicht in Funktionen.

# Eine Zuordnung zu bereits belegten Namen und Funktionen ist NICHT sinnvoll!  
# Dazu gehören zB  c, t, data, F, T, sin, ls.  Data ist OK, weil großgeschrieben
# Außer bei FALSE, NA, etc (?reserved) ist es möglich, aber nicht empfehlenswert


# Wird die Zuordnung in Klammern gesetzt, wird sie gleichzeitig aufgerufen
(p8 <- 2854) # Das ist recht ungewöhnlich, man sollte es daher meiden.

?"<-" # ruft die Hilfe zur Zuordnung auf



# 1.4 Objektarten: -------------------------------------------------------------
# Abfragen des Datentyps:
mode(probe1)             # numeric - Zahl
str(probe1) # STRucture  # num
mode(p4)                 # character - Zeichenkette (Buchchstaben)
mode(p7)                 # numeric
str(p7)                  # int - Integer, ganze Zahl, Unterform von num (-> 1.5)

# Abfrage, welche Zuordnungen bestehen
ls()
ls.str()

# Zuordnungen löschen (Objekt aus dem Workspace löschen)
rm(p7)
p7 # Fehler: Objekt 'p7' nicht gefunden
rm(list=ls()) # entfernt alle Objekte, räumt also auf. Im TinnR F11 drücken.


p9 <- "1.3" # passiert manchmal beim Einlesen von Dateien (Kap 3)
p9
2*p9             # geht nicht
as.numeric(p9)   # jetzt ist aus dem Text eine Zahl geworden
2*as.numeric(p9) # und kann damit entsprechend gerechnet werden
p9               # das Objekt selbst wurde dabei nicht verändert
is.numeric(p9)   # is.___ fragt ab, ob ein Objekt ein bestimmter Datentyp ist

as.character(6:8) # as.___ erzwingt eine Objektart-änderung.
as.numeric(TRUE)
as.logical(0:1)  # FALSE = 0,  TRUE = 1



# 1.5 Grund-Datentypen: --------------------------------------------------------
?numeric      # Zahlen
?  integer    # ganze Zahlen           / werden von mode als numeric ausgegeben,
?  double     # KommaZahlen, NaN, Inf  / str unterscheidet genauer
?logical      # Wahrheitswerte: T=TRUE, F=FALSE
?character    # Zeichenketten wie "Ueberschrift"
?complex      # Komplexe Zahlen wie 7+3i
?factor       # Kategorie, kategorielle Variablen



# 1.6 Objekttypen --------------------------------------------------------------
?vector       # Vektor
?data.frame   # gängigste Form einer Tabelle: verschiedene Datentypen in Spalten
?matrix       # Jedes Element gleichen Datentyps! zweidimensionaler array
?array        # mehrdimensionale Matrix (zB für räumliche Daten über Zeit) 
?list         # mehrere Modi in einem Objekt möglich, Indzierung mit [[n]]
?ts           # Zeitreihe    
# und einige mehr, die hier noch nicht wichtig sind.

 

mode(p9)      # Daten- oder Objekt-typ, einiges ist dabei zusammengefasst
typeof(p9)    # R-intern, vom Nutzer fast nie verwendet, genauer als mode
str(p9)       # Gesamtstruktur vor allem komplizierter Objekte
class(p9)     # Objekttyp
class(7:8)    # zeigt bei Vektoren den Datentyp an
is.vector(7:8)# für Abfragen, ob ein Objekttyp "vector" ist.
