# RScript-Template #
# Erstellt fuer CAS-Gesundheitswissenschaften 2023

# 1) Infos----
# Ganz oben ist es sinnvoll, zu erklaeren, was das Skript ca. macht und 
# wer dieses wann verfasst hat. Der Code sollte so geschrieben und kommentiert
# sein, dass man selbst nach einigen Jahren noch immer versteht, was gemacht 
# wurde.

# 2) Relative Pfade----
# R-File muss gespeichert sein.
# Entweder geht man auf "Session" -> "Set Working Directory" -> "To Source File Location"
# oder man fuehrt folgenden Befehl aus:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# um das Arbeitsverzeichnis dorthin zu legen, wo das R-File liegt.
# Anm.: Man kann auch mit R-Projekten arbeiten, wenn man moechte. 
# Mein Tipp: Jedes Projekt in einem Ordner mit Unterordnern.
getwd() # get working directory...

# 3) libraries laden-----
# bliebte sind folgende:
library(pacman) # Installiert und ladet Packages automatisch
pacman::p_load(tidyverse, # https://tidyverse.tidyverse.org/
readxl,
writexl, # Einlesen und ausgeben von Excel-Files
DataExplorer, # Erstellt einen kurzen, schnellen Ueberblick ueber den Datensatz
gtsummary, # Creates presentation-ready tables 
table1, # schoene Table 1
flextable, # schoenere Tabellen
utils)
# ...und viele viele mehr

# 4) Daten einlesen----

# a) csv/tsv/txt-Files, dies sind einfach normale Textdateien mit unterschiedlichen
# Trennzeichen fuer die Spalten
?read.csv
read.csv("Davis.csv", sep = ",", stringsAsFactors = TRUE, header = TRUE)
# sep = "," definiert, wie die Spalten getrennt sind im Textfile (csv)
# header = TRUE, heisst, dass die erste Zeile fuer die Spaltennamen verwendet wird.
# stringsAsFactors = TRUE, Strings werden direkt als Faktoren in R definiert und nicht nur als Character (also Text)

# b) Excel Dateien lesen/schreiben:
?read_excel
write_xlsx()

# 5) Datenmanipulation-----
# - Data-Cleaning (Outlier)
# - Deskriptive Statistiken, Explorative Datenanalyse (EDA)
# - Handling of Missing values
# - Konkrete Modelle schaetzen (Regression und co)

# 6) Workspace oder einzelne Objekte speichern-----
# _a) Workspace (inkl. Variablen)----
save.image("my_workspace.RData")
load("my_workspace.RData")
# _b) einzelne Objekte----
saveRDS(...Variable oder Object, ...Pfad bzw. Dateiname...)
readRDS()


