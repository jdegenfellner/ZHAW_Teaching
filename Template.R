# RScript-Template
# Erstellt fuer CAS-Gesundheitswissenschaften 2023

# 1) Infos----
# Ganz oben ist es sinnvoll, zu erklaeren, was das Skript ca. macht und 
# wer dieses wann verfasst hat. Der Code sollte so geschrieben und kommentiert
# sein, dass man selbst nach einigen Jahren noch immer versteht, was gemacht 
# wurde.

# 2) Relative Pfade----
# Entweder geht man auf "Session" -> "Set Working Directory" -> "To Source File Location"
# oder man fuehrt folgenden Befehl aus:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# um das Arbeitsverzeichnis dorthin zu legen, wo das R-File liegt (dieses muss 
# natuerlich vorher abgespeichert werden)
# Anm.: Man kann auch mit R-Projekten arbeiten, wenn man moechte. 

# 3) libraries laden-----
# bliebte sind folgende:
library(tidyverse) # https://tidyverse.tidyverse.org/
library(readxl)
library(writexl) # einlesen und ausgeben von Excel-Files
library(DataExplorer) # Erstellt einen kurzen, schnellen ueberblick ueber den Datensatz
library(gtsummary) # Creates presentation-ready tables 
library(table1) # schoene Table 1
library(flextable) # schoenere Tabellen
library(utils)
# und viele viele mehr

# 4) Daten einlesen----
# a) csv/tsv/txt-Files, dies sind einfach normale Textdateien mit unterschiedlichen
# Trennzeichen fuer die Spalten
?read.csv
# Bsp:
read.csv("Davis.csv", sep = ",", stringsAsFactors=TRUE, header = TRUE)
# b) Excel Dateien:
?read_excel
write_xlsx()

# 5) Datenmanipulation-----
# Deskriptive Statistiken
# Data-Cleaning (Outlier)
# Handling of Missing values
# Exploratory Data Analysis (EDA)
# Konkrete Modelle schaetzen (Regression und co)

# 6) Workspace oder einzelne Objekte speichern-----
# a) Workspace
save.image("my_workspace.RData")
load("my_workspace.RData")
# b) einzelne Objekte
saveRDS(...Variable oder Object, ...Pfad bzw. Dateiname...)
readRDS()


