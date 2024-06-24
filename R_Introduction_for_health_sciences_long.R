#### Introduction to R ##

library(pacman)
p_load(lubridate, tidyverse)


# TODO
# - cool tricks in R
# - useful applications for health sciences (epi packages and co)

today()
"2024-06-24"

# Die folgenden Code-Snippets und Bsp. sind in keinster Weise abschliessend und sollen einen kurzen Eindruck
# vom Funktionsumfang von R geben und die allerwichtigsten Funktionen/Methoden erklaeren.

# Die Main-Message dieses Abschnitts ist, Moeglichkeiten und Denkmuster fuer Datenmanipulation aufzuzeigen.
# Ein konkretes Problem wird meist in Verbindung mit Online-Quellen geloest.
# Wir sind in einem raschen und fundamentalen Wandel mit GPT.
# Generative KI, insbesondere (derzeit) GPT4Turbo hilft beim Erstellen von R-Code immens!

# Teilweise wird im Folgenden Englisch und Deutsch vermischt, was kein Problem darstellen sollte (und beabsichtigt ist).
# Man bemerke, dass Code normalerweise eher ausschliesslich auf Englisch kommentiert wird und üblicherweise
# sollen Kommentare eher das "Warum" und nicht das "Was" erklaeren. Fuer didaktische Zwecke ist das aber sinnvoll.

# Untige Ausfuehrungen basieren auf https://r4ds.had.co.nz/introduction.html!
# Viele weitere kostenlose R-Einfuehrungen findet man online, siehe z.B. auch Youtube.
# Siehe e.v. auch das package learnr (rechts oben unter "Tutorial").
# Siehe auch https://de.wikipedia.org/wiki/R_(Programmiersprache) fuer einen Ueberblick ueber die Sprache selbst.

# First things first: 

# Why R (or Stata / SPSS / SAS....) and not just Excel? (see e.g. https://www.northeastern.edu/graduate/blog/r-vs-excel/)
# - ease-of-use: learning curve; point-and-click vs. programming
# - replicability
# - interaction with data, subsetting, transforming, visualizing ....
# - large datasets? (Seit Excel 2007 kann ein Tabellenblatt 1.048.576 Zeilen und 16.384 Spalten (A bis XFD), 
#   also 17.179.869.184 Zellen umfassen. Davor war die Groesse auf 65.536 Zeilen und 256 Spalten (A bis IV), also 16.777.216 Zellen, begrenzt.)
#   Data sets with about one million to one billion records can also be processed in R, but need some additional effort
# - price? R free, 
# - speaking from experience: licences are a pain!!
# - expandability in Excel? -> packages in R! 
# - ....

# Btw: Fuer Excel gibt es das R-Plug-in RExcel

# 1) Very basics #####

# Set working directory to source file location:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
# Session -> Set Working Directory -> To Source File Location

# Get your packages:
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman) # Installiert und ladet Packages automatisch
}
pacman::p_load(tidyverse, # https://tidyverse.tidyverse.org/
               readxl,
               writexl, # Einlesen und ausgeben von Excel-Files
               DataExplorer, # Erstellt einen kurzen, schnellen Ueberblick ueber den Datensatz
               gtsummary, # Creates presentation-ready tables 
               table1, # schoene Table 1
               flextable, # schoenere Tabellen
               utils, nycflights13, gapminder, Lahman, devtools)
# From time to time check for updates of packages: Tools -> "Check for Package Upates"

# Cite packages when you use them (not all, but the unusual ones maybe)
citation("readxl")

# Befehl ausführen:
# str/command + ENTER
# "Run" rechts oben


# What R-version do I have?
R.Version()

# Check if R-version is up-do-date automatically:
# If you have devtools installed:
devtools::source_url("https://github.com/jdegenfellner/ZHAW_Teaching/raw/main/Check_R_version_if_up_to_date.R")


# c() concatenate = zusammenhaengen
c(1,2)
x <- c(1,2,3,4,5) # erzeuge Vektor  .... "<-" Zuweisungsoperator, Pfeil
x = c(1,2,3,4,5)
class(x)
d <- c("Yes","No") # erzeuge String-Vektor
class(d)
x
( x <- c(1,2,3,4,5) ) # Die zusaetzlichen Klammern geben den Zuweisungswert in der Konsole unten aus, ganz praktisch.
2 -> y # Zuweisungsoperator geht auch in die andere Richtung
y = 12 # oder so
12 = y # error

rm(y) # lösche Objekt y

# We start very simple:

# how to execute a primitive command, use R as calculator
1 + 2
sin(pi / 2)
log(12) # natuerliche log, Basis e

# wichtig!
?log # make use of the documentation by ? and function-name
help(log)

log( c(2,3) ) # function is applied to all elements in vector
log(2.3) # "." ist ein Komma
log(2, base = 3) # Basis 3
log(2,3,12) # error

# Allgemein
# function( ... Argumente durch Beistrich getrennt .... )

# !!!
# Getting help and learning more: "give a man a fish and you feed him for a day; 
#teach a man to fish and you feed him for a lifetime"

# -> Dr.Google :) <-
# and today:
# GPT4

# example..."change column-names R"
# stackoverflow: "If this site would be down, half of the software development industry would be incapable of working."

# Bsp.
# "R clear console..." #Strg + L

# use help for R-functions with ? + function-name
?plot


# Zuweisungsoperator(en)
1 -> b
2 <- b # error
(b <- 2)
b = 3

rm(b) # rm ... remove; Objekt loeschen
c() <- 3

# Naming conventions for variables?
# case sensitive (Gross und Kleinschreibung), aber das bemerkt man schnell, upper case, lower case
grosse_Variable <- 3
Grosse_Variable

# concatenate - Operator (haenge zusammen)
x <- c(1,2,3,4) # erzeuge Vektor

x <- c(1:5, 23, 45)
seq(1, 100, by = 2) # steige um 2 -> R create sequence....
y <- seq(1, 10, length.out = 5)
y

# wichtig!!!
str(x) # structure
str(flights)
class(flights)
View(flights) # neues Fenster
class(x)
class(flights)
dd <- c("we","ggt")
x <- "2"
log(2)
x*2

# Einfache Statistiken:
# Btw: Was ist ueberhaupt eine "Statistik"? (https://de.wikipedia.org/wiki/Stichprobenfunktion)
mean(y)
median(y)
median( c(x,y) )
median(x,y) # Was macht das hier?

x <- rnorm(1000) # Erzeuge eine Stichprobe von 100 standardnormalverteilten Werten N(mean = 0, sigma = 1)
hist(x) # Histogramm
mean(x)
sd(x) # Standardabweichung
var(x) # Varianz
summary(x) # Univariate Zusammenfassung # summary anderen Objekten ... 

# Univariat (betrachte genau 1 Variable) vs. Multivariat (betrachte mehrere Variablen !gleichzeitig!)? 

# Minimalversion einer Funktion:
polynom <- function(x, y = 4){ # y hat als Vorgabe-Wert 4
  x^2 + 3*x + 5*x*y + y^3
}
polynom(1) # 1^1+3*1+5*1*4+4^3
polynom(1,0) # y=0


# e.g. einfache for-Schleifen
for(i in 1:5){
  print(i)
}
for(i in c(1,-3,5,7)){
  print(i^3)
} # 1^2 3^2 5^2 ...



# 2) Data Visualization ####

# Schauen wir uns den Datensatz kurz an:

str(mpg) # tibble vs data.frame # structure
class(mpg)
is.data.frame(mpg)

View(mpg) # geht z.B. auf Command-Line nicht. Geht aber hier in RStudio!!

library(data.table)
mpg <- as.data.table(mpg)
mpg <- as_tibble(mpg)

x <- c("1", "3", "5")
x
class(x)
x <- as.numeric(x)
x

# wichtig!!!
# "$" Operator
mpg$trans
mpg$manufacturer[1:10] # spuckt den Character-Vector aus
# Subsetting
#mpg[Zeilen,Spalten]
mpg[1:10, 4:7]$trans
mpg[1:10, c("displ", "cyl")]
mpg[1:10, c(3,5)] # c = concatenate = zusammenfügen

head(mpg) # zeige die ersten Zeilen
tail(mpg) # zeige die letzten Zeilen
headTail(mpg) # zeige die ersten und letzten Zeilen
mpg


# base R commands:
barplot(table(mpg$manufacturer))

# versus using ggplot2
ggplot(mpg, aes(x = manufacturer)) +
  geom_bar(stat = "count") +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Titel") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title


# ggplot() creates a coordinate system that you can add layers to!

# base R Plot:
plot(mpg$displ, mpg$hwy) # relativ langweilig und funktionell nicht so stark wie ggplot, optisch nicht ansprechend
cor(mpg$displ, mpg$hwy) # correlation, was ist das???

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), position = "jitter") # negative relationship between engine size (displ) and fuel efficiency (hwy)

# ggplot(data = mpg) alone creates an empty plot!

# Prompt: "Lets pimp this plot... title "Scatterplot for 
# displacement and highway mileage", color red if displacement > 6, 
# color green if displacement <4; add a smoothing line; omit the 
# legend title, center the title"

# [first iteration, colors where not quite there yet...]

mpg <- mpg %>% mutate(cat_hwy = case_when(
  displ < 4 ~ "green",
  displ >= 4 & displ <=6 ~ "blue",
  displ >6 ~ "red"
))

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(color = cat_hwy), position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Scatterplot for Displacement and Highway Mileage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("green" = "green", "blue" = "blue", "red" = "red"))
p


summary(mpg) # liefert eine 5-Punkte Zusammenfassung der kontinuierlichen Variablen im Datensatz


dim(mpg) # liefert die Dimensionen (Anzahl der Zeilen, Anzahl der Spalten)
mpg[2,4] #look at single element in the data frame
mpg[2,4] <- 1998
# hier koennte man auch einzelen Elemente veraendern: z.B. mpg[2,4] <- 1998


# bis hierhier am 15.11.23#################

mpg[ order(mpg$manufacturer, decreasing = TRUE), ] # change order, sort in descending order for manufacturer

# Workspace und Objekte sichern und laden:
# or
#saveRDS(mpg, "mpg.RDS")
#mpg <- readRDS("mpg.RDS")
mpg

save.image(file = "my_workspace.RData")
load("my_workspace.RData")


mpg <- mpg %>% # pipe operator
  arrange(desc(manufacturer))

# wichtig!!!
colnames(mpg) # kann man bequem aendern
#colnames(mpg)[1] <- c("MANUF_new")

unique(mpg$class) # Welche einzigartigen levels hat dieser Faktor?
table(mpg$class)
length(unique(mpg$class)) # Wieviele einzigartige Levels hat der Faktor?

# One more dimension as information: car class
mpg %>% ggplot() + 
  geom_point(aes(x = displ, y = hwy, color = class)) +  # Verwende versch. Farben fuer die Fahrzeugklasse
  ggtitle("Titel") 

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), color = "blue") + 
  xlab("displacement") + 
  ggtitle("Titel") # + ......

# nice feature: split plot into subplots
ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# fuer zwei kategoriale Variablen
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)  + # 12 Subgruppen
  ggtitle("Mileage vs displacement categorized via zylinders and drive train") +
  xlab("engine displacement, in litres") + 
  ylab("highway miles per gallon")
# Interpretation der Grafik?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(drv ~ cyl)  + # 12 Subgruppen
  ggtitle("Mileage vs displacement categorized via zylinders and drive train") +
  xlab("engine displacement, in litres") + 
  ylab("highway miles per gallon")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth()

ggplot(data = mpg) +
  geom_smooth(aes(x = displ, y = hwy, color = drv), show.legend = TRUE)

# Btw: Einfache lineare Regression mit kleinster Quadrate-Methode, was war das nochmal?? 
# Was kann schiefgehen?
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth(method = "lm") # "lm"=linear model. Einfachste Variante.

# jitter -> addiert zufaellige Fehler dazu, damit Punkte nicht uebereinander liegen; sieht optisch besser aus. Vgl. mit vorher
ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), position = "jitter") # standard for parameter position="identity"

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy)) # so sieht es ohne jitter aus


# Weitere Darstellungen

# data set diamonds
# Let's try to create this plot using GPT:....
ggplot(data = diamonds) + 
  geom_bar(aes(x = cut)) # simple bar chart

# Let's explain this with GPT:....
bar <- ggplot(data = diamonds) + 
  geom_bar(
    aes(x = cut, fill = cut), # fill: fuer die Farbe wird hier die Variable cut verwendet, R has 657 built-in named colours, which can be listed with grDevices::colors().
    show.legend = FALSE, # no legend
    width = 0.5 # Bar width. By default, set to 90% of the resolution of the data.
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL) # keine Labels

bar + coord_flip() # erst jetzt wird der Graph gezeichnet!
bar + coord_polar()


# 3) READ data from somewhere else.... ####

# Einlesen von Daten ist oft sehr (!) muehsam; z.B. aufgrund von gleichzeitiger 
# Verwendung von "." und "," als Dezimaltrennzeichen!
# Weiters koennten diverse Zeilenumbrueche im Datensatz sein, die das Einlesen 
# weniger "straightforward" machen...
# Diese Schwierigkeiten findet man bei den Toy-Datensaetzen nie.

# Directly from the web?
crab <- read.table("http://faculty.washington.edu/kenrice/rintro/crab.txt", 
                   sep = " ", dec = ".", header = TRUE) 
# sep="\t" liest z.B. Tab-getrennte Tabellen
str(crab)
View(crab)
summary(crab)
colnames(crab)
# Aendere z.B. NUR den 1. Spaltennamen:
colnames(crab)[1] <- "col_new"
# erstelle neue Variable aus einer alten:

crab$new_var <- crab$width*10 # base R 
# or
crab <- as.data.table(crab)
crab[, new_var_1 := width*10] # data.table syntax
# or
crab <- crab %>% mutate(new_var_3 = width*10) # dplyr

# Loesche Variable/Spalte:
crab$new_var_1 <- NULL # base R
# or
#crab[, new_var_1 := NULL] # data.table syntax
# or
crab <- crab %>% dplyr::select(-new_var_3) # select für Spalten

# or read just from a local path on your hard disk

# set working directory to file source location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# oder via Menu

# Excel Beispiel:
df <- readxl::read_xlsx("bike_excel_small.xlsx") # Note that there is (in my case) another command with the identical name in the package officer, hence I added the "readxl::"
df <- readxl::read_xlsx("./Data/bike_excel_small.xlsx") # mit Unterordner
str(df)
df$dteday <- as.POSIXct(df$dteday, format = "%d.%m.%Y") # Create date
str(df)
df$dteday[1]
df$dteday[2]
df$dteday[1] < df$dteday[2] # TRUE

# Schreibe data frame df in Excel:
df
getwd()
write_xlsx(df, "./Data/df_out.xlsx")

# Textfiles
?read.csv
?read.csv2
?read.table

# -> mehr zum Einlesen unten!

# 4) Datentransformation mit dplyr, ein sehr elegantes Package ####
# dplyr ist Teils des tidyverse

library(nycflights13)
library(tidyverse)
# Take careful note of the conflicts message that's printed when you load the tidyverse. 
# It tells you that dplyr overwrites some functions in base R. 
# If you want to use the base version of these functions after loading dplyr, 
# you'll need to use their full names: stats::filter() and stats::lag().

str(flights)
# neu: POSIXct - Format! Sehr praktisch fuer Zeit- und Datumsangaben! 
# Ermoeglicht z.B. Zeitvergleiche mit >/</==
flights$time_hour[c(1,100)]
# compare:
flights$time_hour[c(1)] < flights$time_hour[c(100)]

# Tibbles are data frames, but slightly tweaked to work better in the tidyverse.
flights # ein etwas groesserer Datensatz, 336k Zeilen

# Wichtige Funktionen zur Datenmanipulation in dplyr:

# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).
# These can all be used in conjunction with group_by() which changes the 
# scope of each function from operating on the entire dataset to operating on it group-by-group. 
# -->> These six functions provide the verbs for a language of data manipulation.

filter(flights, month == 1, day == 1) # doppeltes "=" fuer Vergleiche!

( dec25 <- filter(flights, month == 12, day == 25) ) # zeige Resultat direkt.
# Speichert auch den zurueckgegebenen Datensatz und mit den Klammern wird das Ergebnis sofort ausgegeben



# Fun (non-intuitive) Facts:
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1 # 0.02040816*49 = 0.9999998 (beschraenkte Mantissenlaenge)

near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)
# oder man verwendet etwas wie:
abs(1 / 49 * 49 - 1) < 10^(-5)

# logische Operatoren
1 == 1 # gleich
1 > 2 # groesser
3 >= 3 # groesser gleich
3 != 4 # ungleich, nicht gleich
TRUE | TRUE # logisches ODER
c(TRUE,TRUE) & c(TRUE,FALSE) # elementweise
TRUE & FALSE # logisches UND
xor(TRUE, TRUE) # exklusives oder
xor(TRUE, FALSE)

# Mengenoperationen
x <- 1:7
y <- 4:10
x
y
union(x, y) # Mengenvereinigung
setdiff(x, y) # Mengendifferenz x - y
setdiff(y, x) # also nicht symmetrisch 

# weiter mit den dplyr-Funktionen

# Filter

filter(flights, month == 11 | month == 12)
# mit base R wuerde das so aussehen:
subset(flights, month == 11 | month == 12)

identical(filter(flights, month == 11 | month == 12), 
          subset(flights, month == 11 | month == 12)) # TRUE, gibt exakt dasselbe aus

# noch eine Variante:
nov_dec <- filter(flights, month %in% c(11, 12)) # %in%-operator - sehr nuetzlich!
# -> in einer for-Schleife

unique(flights$month) # Welche unterschiedlichen Werte hat die Variable "month"?

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120 & dep_delay <= 120) # identisch
#Btw: De-Morgan Regeln, wie gehen die nochmal?

# Missing values - wichtig, da es dauernd vorkommt! 
# NA .... 'Not Available' / Missing Values
NA > 5
10 == NA
NA + 10 # wichtig z.B. beim arithm. Mittel.
d <- c(1,7,56,NA)
mean(d, na.rm = TRUE)
NA / 2

NA == NA # why?
# Let x be Mary's age. We don't know how old she is.
x <- NA
# Let y be John's age. We don't know how old he is.
y <- NA
# Are John and Mary the same age?
x == y
# We don't know!

# ueberpruefe Klasse von Objekten mit is.XXXXXX()
is.na(x)
is.na(d)
sum(is.na(d)) # FALSE FALSE FALSE  TRUE ... 0,0,0,1

is.numeric(45.4)
class(45.4)
class(45)
is.character("hallo")
is.character("1")

# Recycling Bsp.
1:3 + 1:12 # recycling
data.frame(x = 1:3, y = 1:12)
data.frame(x = 1:3, y = 1:10) # Error
1:3 + 1:10 # Was passiert hier?

# Zusammengesetzte Ausdruecke (character) erzeugen:
x <- 12
y <- 3
paste0(x, "und", y) # 0 heisst kein Abstand dazwischen, Unterschiedliche Datentypen gehen, hier character und integer
paste0(x, y) # kein Abstand
paste(x, "und", y)
paste(x, "und", y, sep = "")



# arrange() works similarly to filter() except that instead of selecting rows, it changes their order.
arrange(flights, year, month, day)
arrange(flights, year, month, desc(day))

sum(is.na(flights)) # Es gibt NAs in dem Datensatz flights, was erzeugt is.na(flights)?
# -> vis_miss()
sum(is.na(flights$year)) # aber z.B. nicht in der Variable year

arrange(flights, desc(is.na(dep_time))) # ordnet die NAs in Variable dep_time zuerst an.

# Pipe-Operator (spaeter mehr, diesen gibt es seit kurzem auch in Base-R)
flights %>% arrange(desc(dep_delay)) # Uebergebe Datensatz "flights" und mache etwas damit'


# select() ...  useful subset using operations based on the names of the variables.
dplyr::select(flights, year, month, day) # waehle nur diese drei Variablen aus und zeige das Ergebnis
dplyr::select(flights, year:day) # Zeige Bereich von Variablen, von year nach day. -> praktisch, da man hier nicht die Spalennummern zaehlen muss
dplyr::select(flights, -(year:day)) # Was passiert hier?
# nuetzlich:
starts_with("abc") # matches names that begin with "abc".
dplyr::select(flights, starts_with("s"))
ends_with("xyz")   # matches names that end with "xyz".
contains("ijk")    # matches names that contain "ijk".

dplyr::select(flights, contains("time"))

dplyr::rename(flights, tail_num = tailnum) # Variable umbenennen, braucht man rel. oft, insb. am Anfang einer Analyse

dplyr::select(flights, time_hour, air_time, everything()) # time_hour, air_time, dann der Rest


# mutate() always adds new columns at the end of your dataset 

flights_sml <- dplyr::select(flights,  # Schauen wir uns einen kleineren Datensatz an, nur bestimmte Spalten werden ausgewaehlt
                             year:day, 
                             ends_with("delay"), 
                             distance, 
                             air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay, # erzeugt also neue Spalten
       speed = distance / air_time * 60
)
head(flights_sml) # da nicht abgespeichert - gleicher Datensatz!

transmute(flights, # Behaelt nur die neuen Variablen
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)


# summarise() collapses a data frame to a single row

summarise(flights, delay = mean(dep_delay, na.rm = TRUE)) # einfaches arithmetisches Mittel, nimmt alle delays

# __PIPE- Operator----

# Bsp ohne Pipe
by_dest <- group_by(flights, dest)
delay <- dplyr::summarise(by_dest,
                          count = n(),
                          dist = mean(distance, na.rm = TRUE),
                          delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
delay

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) + # alpha .. Durchsichtigkeit der Punkte aus [0,1]
  geom_smooth(se = FALSE) # se steht fuer standard error, zeichnet Konfidenzintervall um die geschaetzte Kurve.

# Ausgabe in eigenem Fenster
x11()

# Jetzt dasselbe Bsp MIT Pipe-Operator:
flights %>% 
  group_by(dest) %>% 
  dplyr::summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL") %>% ggplot(aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# selbes Ergebnis, gut lesbarer Code ohne Kommentare

# Note: Working with the pipe is one of the key criteria for belonging to the tidyverse. 
# The only exception is ggplot2: it was written before the pipe was discovered


# Gruppierung aufloesen
daily <- group_by(flights, year, month, day)
daily %>% 
  ungroup() %>%             # no longer grouped by date
  dplyr::summarise(flights = n())  # all flights


# Bsp: Find all groups bigger than a threshold:
flights %>%   # Wieviele Fluege sind in jeder Destination?
  group_by(dest) %>% 
  dplyr::summarise(n())
# Filter
flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

# 5) Exploratory Data Analysis EDA ####

# a) Generate questions about your data.
# b) Search for answers by visualizing, transforming, and modelling your data.
# c) Use what you learn to refine your questions and/or generate new questions.

# What type of variation occurs within my variables?
# What type of covariation occurs between my variables?

# Visualizing distributions
table(diamonds$cut) # categorical variable, is it ordinal or nominal?

diamonds <- diamonds %>% group_by(cut) %>% mutate(count_cut = n())
ggplot(data = diamonds ) +
  geom_bar(mapping = aes(x = fct_reorder(cut, count_cut, .desc = TRUE)) ) # z.B. der Groesse nach fallend

# to examine the distribution of a continuous variable, use a histogram:
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
# Man versucht hier die Dichte einer stetigen Zufallsvariable grafisch darzustellen

# overlay multiple histograms in the same plot
smaller <- diamonds %>% 
  filter(carat < 3) # betrachte z.B. nur Diamanten mit <3 Karat
ggplot(data = smaller, aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01) + 
  scale_x_continuous(breaks = seq(0, 3, by = 0.5))
# "Das ideale Gewicht fuer einen klassischen Verlobungsring liegt zwischen 1,00 und 1,50 Karat.
# Das durchschnittliche Karatgewicht eines diamantenen Verlobungsringes liegt etwas ueber 1 Karat, 
# naemlich 1,18 Karat. Die durchschnittliche Groesse eines diamantenen 
# Verlobungsringes variiert je nach Land und Region."


# are there outliers/unusual values?

# One definition could be:
# "An outlier is an observation that lies an abnormal distance from other values in a random sample from a population. 
# In a sense, this definition leaves it up to the analyst (or a consensus process) to decide what will be considered
# abnormal. Before abnormal observations can be singled out, it is necessary to characterize normal observations."
# see also https://en.wikipedia.org/wiki/Outlier

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) # y ... width in mm (0--58.9)

# zoom to small values of the y-axis
ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# betrachte die "ungewoehnlichen" Beobachtungen einzeln:
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% # | logisches ODER
  dplyr::select(price, x, y, z) %>% # x ... length in mm (0--10.74)
  arrange(y) # geordnet nach y
unusual


# Covariation 

# If variation describes the behavior within a variable, covariation describes the behavior between variables. 
# Covariation is the tendency for the values of two or more variables to vary together in a related way. 

# Btw, wie ist Kovarianz und Correlation definiert? Was sagen diese Groessen aus?

# Example: how the price of a diamond varies with its quality:
ggplot(data = diamonds, aes(x = price)) + 
  geom_freqpoly(aes(colour = cut), binwidth = 500)
# schlecht Vergleichbar

# Fuer einen besseren Vergleich betrachte man die Dichte, d.h. die Flaeche unter dem Polygon ist jetzt 1.
# Btw, was ist eine Wahrscheinlichkeitsdichte?
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Vl koennen wir das einfach als Boxplot darstellen:
ggplot(data = diamonds, aes(x = cut, y = price)) + # auf der x-Achse ist die kategorielle Variable 
  geom_boxplot()                                             # cut, auf der y-Achse die kontinuierliche Variable price
# Vergleich der Zentralwerte jetzt leichter
# supports the counterintuitive finding that better quality diamonds are cheaper on average!

# Vl moechte man die Boxplots nach dem Median ordnen, Bsp:
ggplot(data = mpg, aes(x = class, y = hwy)) + # hwy .. highway miles per gallon
  geom_boxplot() # Verbrauch ohne Ordnung

ggplot(data = mpg) +
  geom_boxplot(aes(x = reorder(class, hwy, FUN = median), y = hwy)) + 
  coord_flip() # um 90 Grad drehen

# Visualizierung von zwei kategorischen Variablen
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))


# Visualisiere zwei kontinuierliche Variablen
# oft kann man einfach mal mit base-R plotten, wenn es schnell gehen soll
plot(diamonds$carat, diamonds$price) # bisschen haesslicher

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
# damit es nicht so voll aussieht, fuege man Transparenz hinzu
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100) # mit alpha aus [0,1]

# Der Trick mit den bins funktioniert nicht nur in Histogrammen, geht auch in 2D
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
library(hexbin)
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price)) # noch aesthetischer mit Hexagons


# Auch eine einzelne Variable kann man in bins stecken:
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) # cut_width() zerlegt die Variable in Abschnitte der Laenge 0.1

# Muster/Cluster
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting)) +
  xlab("Eruption time in mins") + 
  ylab("Waiting time to next eruption (in mins)") 
# 2D sieht man die Cluster mit freiem Auge. 
# -> longer wait times are associated with longer eruptions
# Mehrere Dimensionen erfordern multivariate Methoden -> siehe Clusteranalyse spaeter!


# Normalerweise laesst man bekannte Argumente weg, wie z.B. mapping oder die ersten beiden Argumente von aes()
# kuerzer: 
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# 6) Wata Wrangling (=streiten, hadern, zanken) - the art of getting your data into R in a useful form for visualisation and modelling ####

# tibbles, sind data.frames, nur eine verbesserte Version davon.
str(iris)
class(iris)
iris_tibble <- as_tibble(iris)
class(iris_tibble) # ist also noch immer ein data.frame

# z.B. kann man Variablennamen fuer Spalten verwenden, die sonst nicht gehen:
tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

df <- data.frame( `:)` = "smile", 
                  ` ` = "space",
                  `2000` = "number")
df # gibt inkorrekte Spaltennamen
# es gibt weitere Unterschiede, sind aber im Moment nicht so entscheidend. 


# illustrativ verwenden wir read_csv. Sehr oft sind Daten in Form von csv-Files gegeben.

# Versuchen wir einen realen (rechteckigen/tabularen) Datensatz von Bike-Rentals einzulesen:
bike <- read_csv("day.csv", col_names = TRUE)
bike #shows tibble
head(bike) # zeige die ersten Zeilen
tail(bike) # zeige die letzten Zeilen
# oder
bike <- read_csv(file.choose()) # Waehle Pfad manuell aus, oft ganz praktisch. Wenn möglich absolute Pfade vermeiden.
# Variante
bike <- read_csv(file.choose(), skip = 2, col_names = FALSE) # lasse die ersten beiden Zeilen aus, weil z.B. Metadaten drin stehen und erste Zeile wird nicht als Spaltennamen interpretiert

# BEM: wenn es um Geschwindigkeit geht: verwende package data.table und fuer das Einlesen fread()
# Bsp. Einlesen eines Datensatzes mit ca 50 mio Zeilen und <10 Spalten: <1 min
# data.table ist vom Syntax nicht so schoen wie dplyr, aber in manchen belangen schneller.

# definiere, was ein NA ist:
read_csv("a,b,c\n1,2,.", na = ".")# -9/-99/./...
# Was machen wir, wenn zwei verschiedene Kodierungen fuer NA verwendet werden? 
# -> na = c("", "NA"), das Argument nimmt netterweise einen Vektor
# Manchmal sind missing values als -9/-99/-999 definiert (z.B. bei genetischen Daten); warum koennte das ein Problem sein?

# ev auch nuetzlich, parsing
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
parse_integer(c("1", "231", ".", "456"), na = ".")
str(parse_date(c("2010-01-01", "1979-10-14")))

# Packages fuer weitere rechteckige Datenformate
# haven  --> reads SPSS, Stata, and SAS files.
# readxl --> reads excel files (both .xls and .xlsx).

# Write to a file! very important!

getwd() # Zeigt das aktuelle Arbeitsverzeichnis an
# Um das Arbeitsverzeichnis auf den Speicherort der aktuellen Datei zu stellen: 
# Session --> Set Working Directory --> To Source File Location
write_xlsx(bike, "bike_excel.xlsx")
df

# es gibt natuerlich noch andere write-Befehle:

# Factors, ziemlich wichtig!!!
# Was sind Factors? Im wesentlichen kategoriale Variablen

# In R, factors are used to work with categorical variables, variables that have a fixed and known
# set of possible values. They are also useful when you want to display character vectors in
# a non-alphabetical order.

# forcats package in tidyverse
# https://cran.r-project.org/web/packages/forcats/forcats.pdf

x1 <- c("Dec", "Apr", "Jan", "Mar") # gegeben ist eine Variable
x1
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
sort(y1) # sortiert nach den Faktorlevels, wie angegeben

# nicht vorkommende Faktorlevels werden zu NAs
x2 <- c("Dec", "Apr", "Jam", "Mar") #inkludiert Tippfehler
y2 <- factor(x2, levels = month_levels)
y2

y2 <- parse_factor(x2, levels = month_levels) # gibt eine Warnung aus

factor(x1) # keine Factor-Levels vorgegeben, daher alphabetisch von den Eintraegen

levels(y2) # nicht alle Levels belegt.
y2

# Example dataset General Social Survey, which is a long-running US survey conducted by 
# the independent research organization NORC at the University of Chicago. The survey has thousands of questions
gss_cat
?gss_cat # "?" geht natuerlich nur, weil der Datensatz ueber ein Package kommt

gss_cat %>%
  dplyr::count(race)

ggplot(gss_cat, aes(race)) +
  geom_bar() # beachte die Standardstatistik fuer geom_bar(): stat="count" --> siehe ?geom_bar()

# Bsp. Which relig does denom (denomination) apply to? How can you find out with a table? 
# How can you find out with a visualisation?

gss <- gss_cat %>% 
  filter(denom != "Not applicable") %>% # filtere NAs heraus
  dplyr::select(relig, denom)
ggplot(gss, aes(relig)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) # eine Möglichkeit relative Frequenzen anzuzeigen -> wie wuerden wir danach suchen??

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  dplyr::summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point() # wer schaut wieviel fern?
ggplot(relig_summary, aes(age, tvhours)) + geom_point() # Trend fuer Alter? Zusammenhang duerfte komplexer sein....


# oft gebraucht aus optischen Gruenden: Nach Groesse ordnen
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) + # Faktor relig wird nach tvhours umgeordnet
  geom_point()
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours, .desc = TRUE))) + # dasselbe nur absteigend
  geom_point()
relig_summary %>% # dasselbe nur etwas eleganter
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) + # hier wieder ein +, da ggplot nach dem pipe-operator entwickelt wurde
  geom_point()

# Age vs. income??
rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  dplyr::summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + # ordner den Faktor rincome (reported income) aufsteigend nach (mean) age
  geom_point() # Zusammenhang sieht so aus, wie man ihn erwartet


# nette Variante; 
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable", "$6000 to 6999"))) + # fct_relevel hat das argument after="Not applicable"
  geom_point()

# nochmal: Ordnung fuer Bar plots
gss_cat %>%
  mutate(marital = marital %>% 
           fct_infreq() %>% 
           fct_rev()
  ) %>% # fct_rev() kehrt einfach die Anordnung des Faktors um
  ggplot(aes(marital)) +                                       # fct_infreq() ordnet nach Haeufigkeit der Beobachtungen
  geom_bar()


# Faktor-Levels kann man auch bequem aendern:
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  dplyr::count(partyid)

# Grosse Gruppen von Faktoren (mit vielen Beobachtungen) zusammenfassen
gss_cat %>%
  mutate(relig = fct_lump(relig)) %>% # ein bisschen zu grob zusammengefasst
  dplyr::count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>% # fasse die 10 groessten Guppen zusammen
  dplyr::count(relig, sort = TRUE) %>%
  print(n = Inf) # gibt ALLE Zeilen aus


# Dates and Times, falls Zeit..... :)




# 7) Programmierung ####

# Thinking about code as a vehicle for communication is important because every project you do 
# is fundamentally collaborative. Even if you’re not working with other people, you’ll definitely
# be working with future-you! Writing clear code is important so that others (like future-you) can 
# understand why you tackled an analysis in the way you did.

# a) pipe-Operator
# b) Funktionen
# c) Datenstrukturen: Vektoren, Listen
# d) Iterationen: Schleifen



# Pipe
# %>% ist enthalten in
library(magrittr) # in tidyverse enthalten


# Functions

# Writing a function has three big advantages over using copy-and-paste: 
# - You can give a function an evocative name that makes your code easier to understand.
# - As requirements change, you only need to update code in one place, instead of many.
# - You eliminate the chance of making incidental mistakes when you copy and paste 
#(i.e. updating a variable name in one place, but not in another).

# Bsp. fuer eine Funktion
# Compute confidence interval around mean using normal approximation

mean_ci <- function(x, conf = 0.95) { # default value ist hier also 0.95 (falls nicht explizit anders beim Funktionsaufruf angegeben)
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100) 
mean_ci(x)
mean_ci(x, conf = 0.99) # Wird das Konfidenzintervall breiter oder schmaeler? 

# geht das eigentlich auch, wenn wir nicht aus der stetigen Gleichverteilung auf [0,1] ziehen?
x <- rbeta(10^6,1,1)
mean_ci(x)
1/(1+1) #Erwartungswert einer Beta-Verteilung mit den entsprechenden Parametern

# Funktionen koennen auch beliebig viele Argumente haben
commas <- function(...) stringr::str_c(..., collapse = ", ") # string-concatenate Funktion, haengt einfach Strings zusammen und dazwischen wird ein Komma platziert.
commas(letters[1:10]) # nehme die ersten 10 Buchstaben des Alphabets
#> [1] "a, b, c, d, e, f, g, h, i, j"


# Rueckgabewerte - Normalerweise ist das der zuletzt berechnete Wert. 
# Man kann aber auch frueher explizit etwas zurueckgeben mit return()
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}
complicated_function(vector()) # vector() hat Laenge 0 

# Umgebung einer Funktion:
f <- function(x) {   # Kein Fehler, da R auch außerhalb der Funktion nacht y sucht ( Sorry an alle Informatiker :) )
  x + y
} 
# R uses rules called lexical scoping to find the value associated with a name. Since y is not defined inside the function, 
# R will look in the environment where the function was defined:
y <- 100
f(10)

# Superzuweisungsoperator "<<-"
f_out <- function(x){
  z <<- x
  zz <- x
}
f_out(12)
z
zz

# Objekt vs. Character:
dfdfdd
"dfdfdd"

# R-Eigenheiten:
`+` <- function(x, y) {  # An diesem Bsp. sieht man aber schoen, dass "+" nicht vom Himmel faellt und schon gar nicht als Infix-Operator definiert werden muss.
  if (runif(1) < 0.1) {  # random uniform number / erzeuge 1 gleichverteilte Zufallszahl
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
`+`(2,4) #ODER
1 + 2
table(replicate(1000, 1 + 2)) # was macht replicate?
rm(`+`) # unbedingt wieder loeschen, da hier das Additionssymbol umdefiniert wurde!!!


# Vektoren - zwei Eigenschaften: Typ und Laenge

# fun fact. In R gibt es keine Skalare, es gibt nur Vektoren:
is.vector(1)

typeof(letters)
typeof(1:10)

x <- list("a", "b", 1:10) # Listen koennen also auch unterschiedliche Dateiformate haben.
length(x)
class(x)
x[[3]][[4]] # greift auf das vierte Element des dritten Elementes der Liste x zu. 
unlist(x) # jetzt koennte man wieder nach integers oder characters parsen
class(unlist(x))

# logische Vektoren
x <- c(TRUE, TRUE, FALSE, NA)
sum(x) # Was kommt hier raus?
sum(x, na.rm = TRUE) 

# Integer and double vectors are known collectively as numeric vectors. 
# In R, numbers are doubles by default. To make an integer, place an L after the number:
typeof(1)
typeof(1L)

# for doubles:
c(-1, 0, 1) / 0 # nicht wie gewohnt in der normalen Mathematik, da ja nicht definiert!! Im Taschenrechner würde ERROR angezeigt werden.

is.finite(c(-Inf, NaN, Inf))
is.infinite(c(-Inf, NaN, Inf))

# Note that each type of atomic vector has its own missing value:
NA            # logical
NA_integer_   # integer
NA_real_      # double
NA_character_ # character
# werden aber implizit transformiert, nice to know

# Scalars and recycling rules
sample(10) + 100 # sample(): wichtiger Befehl!!
runif(10) > 0.5
# !!The shorter vector is repeated, or recycled, to the same length as the longer vector.

# Vorsicht beim erstellen von tibbles
tibble(x = 1:4, y = 1:2)
#> Error: Tibble columns must have compatible sizes.
#> * Size 4: Existing data.
#> * Size 2: Column `y`.
#> ℹ Only values of size one are recycled.

tibble(x = 1:4, y = rep(1:2, each = 2))
tibble(x = 1:4, y = 1:2) # error

# Subsetting for vectors:
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)] # Was kommt hier raus?

x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)] # Was kommt hier raus?

x[0] # fuer viele Anwender ungewohnt. Index faengt bei 1 an.
x[1]

x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0] # modulo

# Was macht das hier?
mean(is.na(x))
2/7

# Attribute (braucht man ab und zu, nicht so wichtig fuer den Anfang)
x <- 1:10
attr(x, "greeting") # siehe https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/attr
# das Attribut "greeting" von x existiert nicht, daher NULL

attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

# three very important attributes
# - Names are used to name the elements of a vector.
# - Dimensions (dims, for short) make a vector behave like a matrix or array.
# - Class is used to implement the S3 object oriented system.


# Augmented vectors, because they are vectors with additional attributes, including class

# Factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
class(x)
attributes(x)

# Dates in R are numeric vectors that represent the number of days since 1 January 1970.
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)
attr(x, "class") <- NULL # Was machen wir hier?
attributes(x)
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)
# Mit POSIX kann man auch Zeitvergleiche machen 
# -> keinesfalls anfangen Daten/Zeiten elementar zu vergleichen 
# -> MERKE: es gibt fuer (fast) alles ein package!


# tibbles
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

# traditionelle data.frames:
df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)

# Ab jetzt purrr-package; ersetzt die alten Funktionen apply(), lapply(), tapply()

# mapping functions:
# map() makes a list.
# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_dbl() makes a double vector.
# map_chr() makes a character vector.

# Each function takes a vector as input, applies a function to each piece, and then 
# returns a new vector that’s the same length (and has the same names) as the input. 
# The type of the vector is determined by the suffix to the map function.

# purrr-Funktionen sind in C implementiert - schneller...

# wie oben nur mit den mapping-Funktionen:
map_dbl(df, mean)

df %>% map_dbl(mean)

# Bsp
map(1:5, runif) # wendet runif auf mit jedem Element aus 1:5 als Argument für runif() an, also wird eine Liste runif(1), runif(2) ... zuruerckgegeben
# map() Returns a list the same length as .x.

# ueber zwei Vektoren gleichzeitig iterieren
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) %>% str()
# dh hier wird folgendermaßen iteriert:
# Schritt 1: mu=5, sigma=1
# Schritt 2: mu=10, sigma=5
# Schritt 3: mu=-3, sigma=10

# pmap() mit mehreren...

# Walk
# Walk is an alternative to map that you use when you want to call a function 
# for its side effects, rather than for its return value. You typically do this 
# because you want to render output to the screen or save files to disk - the important 
# thing is the action, not the return value. 

x <- list(1, "a", 3)
x %>% 
  walk(print)
# Was macht das hier?
x %>% 
  print()


# keep()
iris %>% 
  keep(is.factor) %>% 
  str()

#discard()
iris %>% 
  discard(is.factor) %>% 
  str()
