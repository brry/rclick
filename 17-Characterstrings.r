### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 17: Character Stings (Zeichenketten) ###############################

# 17.1 Wilde Einf?hrung
# 17.2 Suchen und ersetzen
# 17.3 Anwendungen Suchen und ersetzen
# 17.4 Zeichen regelm??ig ersetzen, Wildcards, Regex
# 17.5 Ausgabe in die Konsole
# 17.6 Anf?hrungszeichen
# 17.7 in Buchformat drucken
# 17.8 Google PDF Links extrahieren
# 17.9 Wildes Ende (Verschiedenes)


# 17.1 Wilde Einf?hrung --------------------------------------------------------
paste("WunderbareSache", 3:5)  # macht einen Vektor mit Modus character
paste(2, "hot", 4, "U", sep=" ") # Standardeinstellung seperator: Leerzeichen
einVektor <- 5:9
paste("SeeYouL", einVektor[4], "ter", sep="") # Auf bestehende Objekte zugreifen
Buchstaben <- c("einige verschiedene", "Buch", "staben")
Buchstaben   
length(Buchstaben)
nchar(Buchstaben)

paste(Buchstaben, "tolle", sep="_")
paste(Buchstaben, sep="")
paste(Buchstaben, collapse="")
# alle Leerzeichen ersetzen (mehr in 17.2 und 17.3)
Buchst <- paste(Buchstaben, collapse=""); Buchst
sub(" ", "", Buchst) # substitute (ersetze) " " in Buchst mit ""

# in R vordefiniert ist schon das alphabet als Objekt
letters
LETTERS

nchar("Monster Party") # Anzahl Zeichen; " " (Leerzeichen) ist auch ein Zeichen!
substring("Monster Party", first=c(1,3,5), last=c(2,5,9) ) # "t" zweifach drin
substr("Monster Party", start=c(1,3,5), stop=c(2,5,7)) # kann nicht vektorisiert
strsplit("Die ?rzte - Monster Party", " - ")[[1]]

s <- "am Donnerstag oder Freitag"
s1 <- strsplit(s, " ")[[1]]          # split string into single words
s2 <- sapply(strsplit(s1, ""), rev)  # reverse letters in each word
s3 <- sapply(s2, paste, collapse="") # combine to new words
paste(s3, collapse=" ")              # paste into one single string
"ma gatsrennoD redo gatierF"


# Backslash "\" : Escape Character
# "\n" f?r Zeilenumbruch (newline), zB in Graphik?berschriften und -Texte
# "\t" f?r Tabstopp, zB f?r write.table("path/file.txt", yourdata, sep="\t")
# "\\" f?r Backslash als Zeichen
# "\"" f?r " als Zeichen
# "\U" f?r Unicode, zB.:
plot(1)   # Unicode symbols without "expression"
mtext("\U00D8:  5.4 [ \U{00B0}C]", col=2) # average and degree symbol (no space)
mtext(expression(paste("Temp [",degree,"C]")), adj=0)
mtext("2 * 2\U00B2 = 2 \U00B3", col=2, adj=1)
browseURL("http://unicode-table.com/de/")

# control characters \a, \b, \f, \n, \r, \t, \v               see ?regex
# octal and hexadecimal representations like \040 and \0x2A

num <- c(3.2, 12.03, 789)
num
paste(num)# schneller zu tippen als as.character(num)
sprintf("%5.1f", num) # C-style String Formatting: 5 Zeichen, 1 nach dem Komma
sprintf("%6.1f", num)
sprintf("%7.2f", num)
sprintf("%07.2f", num) # pad with zeros (mit f?hrenden Nullen f?llen)
sprintf("  %07.2f", num) # Leerzeichen am Anfang


# 17.2 Suchen und ersetzen -----------------------------------------------------
schoko <- c("Lindor", "Ritter Sport", "Lindor","Frigor", "Milka", "More Lindor")
schoko
match("Milka", schoko)  # wo kommt "Milka" in schoko vor? (exakter Match)
grep("or", schoko)      # Elemente 1,2,3 und 6 von Schoko enthalten "or"
match("Lindor", schoko) # liefert nur das erste Element, das "Lindor" ist.
"Lindor" %in% schoko    # logische Antwort
grep("Lindor", schoko)  # liefert alle Teilmatches
# Ersetzen (SUBstitution)
sub("or", "__", schoko)   # Letztes Lindor unver?ndert  (First matches)
gsub("or", "_", schoko)  # alle Matches, Global SUBstitution
sapply(gregexpr("or", schoko), function(x) sum(x>0)) # Wie oft komt "or" vor?


Kette <- "Lange ZeichenKette"
grep("g", Kette)
grep("u", Kette) # schwierig in Bedingungen etc zu verwenden, daher
grepl("g", Kette)
# grepl vektorisiert:
grepl("h|e", Kette)

grepl("u", Kette)
"u" %in% Kette    # geht schneller
grepl("t", "D:/A_Data") # liefert TRUE, wie erwartet.
"t" %in% "D:/A_Data"  # FALSE: %in% ist bin?rer Operator f?r match,
# match liefert nur die erste exakte (komplette) ?bereinstimmung.


# an welchen Stellen einer Zeichenkette ist ein bestimmter Buchstabe:
unlist(gregexpr("e", Kette))

# Regular expressions
words <- c("ab(","abc (", "abcd(", "abc.(", "abc", "abc(th)", "abc (th)")
grep("abc\\(", words, value=TRUE)
grep("abc\\(|bcd\\(", words, value=TRUE)


# 17.3 Anwendungen Suchen und ersetzen -----------------------------------------
# Eine Funktion soll den Speicherort einer Datei nur in die Console schreiben,
# wenn der Dateispeicherort nicht als kompletter Pfad genannt wurde, also
# das Working Directory (Arbeitsverzeichnis) als Ort verwendet wurde:
Datei <- "Beispiel.txt"
if( !  ( grepl("/", Datei) | grepl("\\", Datei, fixed=T)  )  )  #"
       cat("Hier ist deine Datei gespeichert:",getwd(),"\n")
Datei <- "D:/A Data/Beispiel.txt"
# analog zum obigen Vorgehen
Datei <- "D:\\A Data\\Beispiel.txt"
# analog zum analogen Vorgehen


# Number of occurences of character
# wie oft ein Buchstabe in einer Zeichenkette vorkommt
# elegantere L?sungen willkommen!
nocc <- function(pattern, x, ignore.case=FALSE)
  sum(nchar(x)-nchar(gsub(pattern,"",x,ignore.case=ignore.case)))/nchar(pattern)
#
nocc(pattern="M", x="Moin an diesem schoenen Morgen") # 2
nocc("m", "Moin an diesem schoenen Morgen") # 1
nocc("M", "Moin an diesem schoenen Morgen", ign=T) # 3 
nocc("m", "Moin an diesem schoenen Morgen", ign=T) # 3
nocc("n", "Moin an diesem schoenen Morgen") # 5
nocc(c("M","n"), "Moin an diesem schoenen Morgen") # nicht vektorisierbar, also
sapply( c("M","n"), nocc, x="Moin an diesem schoenen Morgen")
nocc("n", c("Moin an diesem", "schoenen Morgen"))
                                           # gibt nur Gesamtsumme statt 2 und 3
# Lesetipp: Die Beispiele in 
?chartr


# Quersumme (digit sum)
quersumme <- function(dat) # vektor mit (Komma-)Zahlen
   sapply(strsplit(sub(".","",dat,fixed=T),""), function(x)sum(as.numeric(x)) )
#
quersumme(3.14159) # = 23 = 3+1+4+1+5+9
quersumme( c(9.37, 2.1) )
quersumme(1:30)
plot(quersumme(1:80), type="o") # ansteigende Periodizit?t
plot(quersumme(1:800), type="o") # Selbst?hnlichkeit (Fraktalit?t)
plot(quersumme(1:8000), type="l")



# 17.4 Zeichen regelmaessig ersetzen, Wildcards, Regex -------------------------
browseURL("http://www.regular-expressions.info/rlanguage.html")
x1 <- c("1.5 mg" , "50 mg" , "100 mg" , "0.5 mg") ; x1
x2 <- gsub(" mg", "", x1) ; x2       # ersetze alle " mg" in x1 mit "" (nichts)
as.numeric(x2)

# mit fixed=T in gsub kann man auch "[" etc (Sonderzeichen) ersetzen:
x3 <- c("Konz. [mg/l]" , "Masse [g]" , "Porositaet [%]" , "Anzahl [-]") ; x3
gsub(" [g]", "", x3, fixed=TRUE)

# Ausflug: Introduction von Wildcards:
x4 <- c("Konz." , "Masse" , "Porositaet" , "Anzahl", "Akt ") ; x4
grep("[t]$", x4) # die Indizes aller Elemente in x4 die auf "t" enden
# alternative Methode mit Wildcard im bekannten Stil (Platzhalter: "*")
grep(glob2rx("*t"), x4)
grep(glob2rx("*ss*"), x4) # Welche Elemente von x4 beinhalten "ss"
grep("ss", x4) # macht das gleiche, nur weniger kompliziert ;-)

gsub(glob2rx("*t"), "tt", x4) # mit Wildcard ganzes Element ersetzen
gsub(".*t$", "tt", x4) # Hier steht das gleiche als Regular expression (?regex)
gsub("[t]$", "tt", x4) # nur t zu tt aendern

# zur?ck zu x3: Alle Einheiten entfernen (zB. um Objekte als Dateinamen zu 
# verwenden); ohne glob2rx, also mit Wildcard in reg exp 
x3
gsub(" \\[.*", "", x3)   # "\\" um aus der Escape Sequenz zu escapen und "["  
# analog zu fixed=T f?r exact matching zu nehmen; ".*" als Wildcard
gsub("ss.*", "lange,so sieht_manwas", x3)
# Alternativ mit strsplit
x5 <- strsplit(x3, split=" [", fixed=T) ; x5
unlist(x5)[(1:length(x5))*2-1]
# Sofern in den Zeichenketten keine weiteren "[" sind:


# Remove trailing zeros
x <- "0.710000"
sub("0+$", '', x)

# add trailing zeros:
formatC( round( 5.2, 3 ), format='f', digits=3 )

# Remove leading and trailing white spaces
x <- "     middle of the string      "
sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE) 
sub(" +$", '', x) # nur Leerzeichen am Ende der Zeichenkette entfernen
sub("^\\s+", "", x) # nur am Anfang


x <- c("  -some string" , "- leading minus sign")
sub("^[[-]]*", "\\1", x, perl=TRUE)



# 17.5 Ausgabe in die Konsole --------------------------------------------------
a <- print(c(1:3,"Hallo")); a
b <-   cat(1:3,"Hallo")   ; b
c <- paste(1:3,"Hallo")   ; c
# print schreibt in die Konsole und gibt den Wert auch zur?ck
# cat schreibt nur in die Konsole, der zugeordnete value ist NULL
# cat zeigt keine Anf?hrungsstriche!
# paste schreibt, wenn zugeordnet, nichts, sondern gibt einen zusammengef?gten
# character Vektor zur?ck. Wenn nur der Befehl ausgef?hrt wird, gibt es das 
# Ergebnis an die Konsole (wie sonst bei Funktionen auch immer der Fall).



# 17.6 Anf?hrungszeichen -------------------------------------------------------
a <- "Ein \"sch?ner\" Text" ; a  ; cat(a, "\n")
plot(1, main=a)     # alles Roger
b <- "Ein 'noch sch?nerer' Text"; b ; cat(b, "\n") ; plot(1, main=b)
cc <- "Ein \"toller' Text"; cc ; cat(cc, "\n")   #'
# http://tolstoy.newcastle.edu.au/R/help/02b/5198.html



# 17.7 Dokument in Buchformat drucken ------------------------------------------
# Brosch?rendruck zum Falten und Durchbl?ttern wie im Buch.
# Dazu m?ssen die Seiten in der richtigen Reihenfolge (siehe Funktion) und 
# Anordnung gedruckt werden (siehe Kommentar unten).
# Beispiel f?r 12 Seiten: 12,1,2,11,10,3,4,9,8,5,6,7
# Ungelesene Links:
# http://www.helpster.de/a5-buchdruck-in-word-vorbereiten-so-gelingt-s_54969
# http://www.ehow.com/how_4915094_print-booklet-adobe.html

Buchformatdruck <- function(n, sep=",") # n: number of pages in document
    {                                   # sep: seperator required by printer
    p <- ceiling(n/4)*4 # p: number of pages including empty pages at the back
    d <- p/4 # d: number of double pages
    o <- "" # o: order of pages
    for(i in 1:d)
    o <- paste(o, p-2*(i-1), 2*i-1, 2*i, p-2*(i-1)-1, sep=",")
    cat(substr(o, start=2, stop=nchar(o)), "\n")
    }

Buchformatdruck(12)
Buchformatdruck(13) # hier w?ren leere Seiten 14 bis 16 anzuh?ngen

# Beidseitig drucken, Drehung an langer Kante
# Word 2010:
# Seiten wie von Funktion angegeben
# 2 Seiten pro Blatt, Seite Einrichten -> gegen?berliegende Seiten

# Foxit Reader 4.3:
# Printer Properties - Querformat
# Print Range: Pages [Ergebnis der Funktion]
# Print Handling: # Margins 0.1 in # Page Scaling fit to paper
         # Page Arrange multiple pages per sheet # rotate normal
         # page order vertical # Pages per sheet custom 2 by 1 # Autocenter on



# 17.8 Google PDF Links extrahieren --------------------------------------------
# PDF-Links aus der Google-Suche sind ganz sch?n lang - ich hab noch keinen
# anderen Weg gefunden, nur den tats?chlichen Link zu erhalten.
# Die Links sehen zB so aus:
googlink <- paste0("http://www.google.de/url?sa=t&rct=j&q=semilog&source=web&c",
"d=7&ved=0CHgQFjAG&url=http%3A%2F%2Fwww.math.neu.edu%2Ffries%2Fsemilog-ex-hwk.",
"pdf&ei=8qnZT-qUKInCtAbU6-DFCA&usg=AFQjCNEwix4dJhFfWbKj86cBv5QeysgbVA&cad=rja")

library(berryFunctions)
googleLink2pdf # in berryFunctions

googleLink2pdf(googlink)
googleLink2pdf("http://www.google.de/url?sa=t&rct=j&q=gr%C3%BC%C3%9Fe%20pdf
&source=web&cd=1&ved=0CFkQFjAA&url=http%3A%2F%2Fwww.fischerverlage.de%2Fsixcms
%2Fmedia.php%2F308%2FLP_978-3-10-022303-6.pdf&ei=hq_ZT7TOKI7GtAaVs5HfBw&usg=
AFQjCNEfynh6ICrjiqI6D2phge7puMQzxQ&cad=rja")



# 17.9 Wildes Ende (Verschiedenes) --------------------------------------------
strReverse <- function(x)
                   sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
strReverse(c("abc", "Statistics"))

plot(1, type="n")
text(1,1, expression(rho ~ theta))

a <- c("A_A","A_B","C_A","BB","A_Asd") 
a[grepl("^[AB]", a)]

# Interessantes aus der grep-und-co Hilfe mit "[" und "]":
grep("[a-k]", letters) # siehe Abschnitt character class in 
?regex 

# Thema Format:
as.character
l <- sprintf("%02.0f",1:12)
l
cat(sprintf("%s is %6.2f feet tall\n", "Sven", 7.1) )

## re-use one argument three times, show difference between %x and %X
xx <- sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames=list(rep("", 16), "%d%x%X"))
xx
noquote(format(xx, justify="right"))
sprintf("%s %d", "test", 1:3)


## Double all 'a' or 'b's;  "\" must be escaped, i.e., 'doubled'
gsub("([ab])", "\\1_\\1_", "abc and ABC")

txt <- c("The", "licenses", "for", "most", "software", "are",
  "designed", "to", "take", "away", "your", "freedom",
  "to", "share", "and", "change", "it.",
   "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
   "is", "intended", "to", "guarantee", "your", "freedom", "to",
   "share", "and", "change", "free", "software", "--",
   "to", "make", "sure", "the", "software", "is",
   "free", "for", "all", "its", "users")

grep("[gu]", txt, value = TRUE)
sub("[b-e]",".", txt)



