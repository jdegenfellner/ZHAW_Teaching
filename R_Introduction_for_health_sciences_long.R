#### Introduction to R ####

# Maybe interesting:
#   https://www.reddit.com/r/rstats/comments/1ak05u7/what_are_some_cool_r_packages_to_use_in_2024/

# Get your packages - functional extensions to base R:--------
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman) # Install and load package in one
}
pacman::p_load(tidyverse, # https://tidyverse.tidyverse.org/
               readxl,
               writexl, # read write Excel files
               DataExplorer, # Data overview
               gtsummary, # Creates presentation-ready tables 
               table1, # nice table 1
               flextable, # nicer tables
               nycflights13, gapminder, Lahman, # data sets
               devtools, # Collection of package development tools.
               lubridate, # date functions
               Hmisc, # Harrell Miscellaneous
               data.table, # for fast operations on large data sets
               psych, # A general purpose toolbox developed originally for personality, psychometric theory and experimental psychology. 
               visdat, # visualize missing values
               tictoc, # timing 
               hexbin,
               broman, # Bivariate Binning into Hexagon Cells
               officer) # The officer package facilitates access to and manipulation of 'Microsoft Word' and 'Microsoft PowerPoint'
# Cite packages when you use them
citation("readxl")
sessionInfo() # to see everything about what is used in my current session.

today()
"2024-11-12"

# File-Info----
# The following code snippets and examples are by no means exhaustive and are 
# intended to give a brief impression of R's functionality and explain the most
# important functions/methods.

# The main message of this section is to show possibilities and ways of thinking
# for data manipulation.
# A specific problem is usually solved in conjunction with online sources.

# ChatGPT, helps IMMENSELY in creating R code!
# Use Github Copilot for auto completion of code snippets!

# Many of the following explanations are based on 
# https://r4ds.had.co.nz/introduction.html!
# https://r4ds.hadley.nz/ is the second edition.
# Thank you, Hadley!

# Many more free R introductions can be found online, see e.g., also YouTube.
# Also see the package learnr (top right under "Tutorial").
# Also see https://en.wikipedia.org/wiki/R_(programming_language) for an 
# overview of the language itself.

# First things first:

# Why R (or Stata / SPSS / SAS....) and not just Excel? (see e.g., https://www.northeastern.edu/graduate/blog/r-vs-excel/)
# - ease-of-use: learning curve; point-and-click vs. programming
# - replicability
# - interaction with data, subsetting, transforming, visualizing ....
# - large datasets? (Since Excel 2007, a worksheet can contain 1,048,576 rows and 16,384 columns (A to XFD), 
#   comprising 17,179,869,184 cells. Before that, the size was limited to 65,536 rows and 256 columns (A to IV), comprising 16,777,216 cells.)
#   Data sets with about one million to one billion records can also be processed in R, but need some additional effort
# - Price: R is free,
# - Speaking from experience: licenses are a pain!!
# - Expandability in Excel? -> packages in R!
# - ....
# Btw: For Excel, there is the R plug-in RExcel.



# 0) Before you start #####

# _RStudio layout----

# _How to setup your working environment-----
# - Relative paths (and trick below) or
# - RProjects (File -> New Project -> New Directory -> New Project)


# Trick: Set working directory to source file location:
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )
getwd() # get working directory
# equivalent with: 
# Menu: Session -> Set Working Directory -> To Source File Location

# _Execute a command from the script:----
# str/command + ENTER; or
# "Run" button (upper right)

# _What R-version do I have?---------
R.Version()
# Check if R-version is up-do-date automatically:
# If you have devtools installed:
devtools::source_url("https://github.com/jdegenfellner/ZHAW_Teaching/raw/main/Check_R_version_if_up_to_date.R")

# _R clear console: Command + L----

# _Clear (almost) entire RStudio:------
devtools::source_url("https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Cleanup_RStudio.R")


# 1) Very basics ----
# 
# c() concatenate
c(1,2)
x <- c(1,2,3,4,5) # create vector  .... "<-" assignment operator
x
x = c(1,2,3,4,5)
class(x) # "numeric"
d <- c("Yes","No") # create string vector
class(d) # "character"
x
( x <- c(1,2,3,4,5) ) # show result immediately
2 -> y # other direction also possible
y
y = 12 # also possible
12 = y # error

rm(y) # remove y
y # object not found

# We start very simple:

# how to execute a primitive command, use R as calculator
1 + 2
sin(pi / 2)
log(12) # "ln"; natural log, Basis e

# important!!
?log # make use of the documentation by ? and function-name
help(log)

log( c(2,3) ) # function is applied to all elements in vector
log(2, base = 3) # basis 3
log(2,3,12) # error

# in general:
# function( ... arguments separated by "," .... )

# !!!
# Getting help and learning more: "give a man a fish and you feed him for a day; 
# teach a man to fish and you feed him for a lifetime"

# -> Dr.Google :) <-
# and currently:
# GPT4o



# Naming conventions for variables?
# case sensitive
large_Variable <- 3
Large_Variable # not found

# sequences
seq(from = 1, to = 100, by = 2) # increase by 2 -> R create sequence....
y <- seq(1, 10, length.out = 5) # 5 equidistant points between 1 and 10
y


str(x) # structure, important!
flights
str(flights)
flights$year # go into column and show vector
flights$year[1] # first element
flights$year[1:10] # first 10 elements
class(flights)
View(flights) # opens new tab with non-editable overview
class(x)
class(flights)
x <- "2"
x*2 # error; Error in x * 2 : non-numeric argument to binary operator
log(2)

# simple statistics in base R:
mean(flights$arr_time) # NA (non applicable)
is.na(flights$arr_time) # logical vector
sum(is.na(flights$arr_time)) # how many NAs?
mean(flights$arr_time, na.rm = TRUE) # remove NAs; = 1502.055

mean(y) # mean {base}
median(y)
median(c(2,3,4,12)) # 3.5
median(c(2,3,4,120)) # 3.5 -> robust against outlier
median( c(x,y) ) # error, different type
median(c(2,3,4,12), c(45,2,3,1)) # What does this do?

# BIS HIERHER TAG 1----

x <- rnorm(1000) # sample of 1000 random number with X ~ N(mean = 0, sd = 1)
hist(x) # histogram in base R (later with ggplot2) -> see also: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Density_plot_boxplot_below.R
boxplot(x) # base R boxplot
mean(x) # mean is an estimator for the population mean ~ 0
median(x)
sum(x <= -0.06026) # how many values are smaller than -0.06026?
sd(x) # standard deviation ~ 1
var(x) # variance # "erwartungstreu" (im Durchschnitt korrekt)
1/(length(x)-1)*sum( (x-mean(x))^2 ) # same
summary(x) # univariate summary: Min, 1st Quartile, Median, Mean, 3rd Quartile, Max


# Create functions:
polynom <- function(x, y = 4){ # y has default value of 4
  x^2 + 3*x + 5*x*y + y^3 # last value is returned
}
polynom(1) # 1^1 + 3*1 + 5*1*4 + 4^3
1^1 + 3*1 + 5*1*4 + 4^3
polynom(x = 1,y = 0) # y = 0

polynom_useless <- function(x, y = 4){ # y has default value of 4
  x^2 + 3*x + 5*x*y + y^3 # last value is returned
  return(23) # please return the value 23 and ignore what came before
}
polynom_useless(x = 2, y = 4) # 23


# loops (that I need the most)
for(i in 1:5){
  print(i)
}
for( i in c(1,-3,5,7) ){
  print(i^3)
} # 1^2 3^2 5^2 ...

# sometimes a while loop is useful
i <- 1
while(i < 10){
  print(i)
  i <- i + 1
}


# 1.1) Tips and Tricks in R--------

# _document outline----

# _comment out whole sections:----
# step 1: mark the section you want to comment out
# step 2: shift + command + c
# 
# x<-1
# y<-2
# z<-3

# expand....

# If not already mentioned: use the "document outline" in RStudio to
# navigate through your script and give it structure!


# 2) Data Visualization ----
str(mpg) # tibble vs data.frame # structure
class(mpg)
is.data.frame(mpg) # TRUE
is.numeric(mpg$manufacturer) # FALSE
is.numeric(mpg$year) # TRUE
View(mpg)
create_report(mpg) # DataExplorer, creates a quick overview as html


# conversions
mpg <- as.data.table(mpg)
mpg <- as_tibble(mpg)
is.data.frame(mpg) # TRUE

x <- c("1", "3", "5")
x
x[3]*3 # error
class(x)
x <- as.numeric(x) # conversion
x
is.numeric(x) # TRUE

?mpg # data set included in ggplot2
# Fuel economy data from 1999 to 2008 for 38 popular models of cars
View(mpg)

# What other data sets are in R?
?datasets # package ‘datasets’
library(help = "datasets")


# "$" Operator
mpg$trans
mpg$manufacturer[100:110] # elements 100 to 110 in manufacturer vector
# Subsetting
#mpg[Zeilen,Spalten]

# How to look at data in detail
mpg[1:10,] # first 10 rows
mpg[1:10, 2:3] # only rows 1 to 10 and columns 2 and 3
mpg[, 2:3] # all rows and columns 2 and 3
mpg[1:10, c("displ", "cyl")]
mpg[1:10, c("displ", "cyl")]$cyl
mpg[1:10, 4:7]

# change a single entry in a data set
mpg_ <- mpg
mpg_[1,1] <- "audi_neu"
mpg_[1,1]
mpg_

mpg[1:10, c("displ", "cyl")]
mpg[1:10, c(3,5)]

head(mpg) # show first 10 rows
tail(mpg, n = 12) # show last 10 rows
headTail(mpg) # first and last rows (package psych)
mpg


# Base R commands:
table(mpg$manufacturer) # frequency table
table(mpg$manufacturer)/sum(table(mpg$manufacturer))*100 # relative frequencies
sum(table(mpg$manufacturer)/sum(table(mpg$manufacturer))*100) # = 100 %
barplot( table(mpg$manufacturer) )
cor(mpg$displ, mpg$hwy) # correlation (Pearson)
plot(mpg$displ, mpg$hwy) # Scatterplot, x-y

# versus using ggplot2
mpg %>%  # so-called pipe operator (also included in R by now as "|>")
  ggplot(aes(x = fct_infreq(manufacturer) %>% fct_rev())) + # GPT: What does this command do...?"
  geom_bar(stat = "count") + # add a barplot
  coord_flip() + # flip the plot
  xlab("") + ylab("") + # no labels
  ggtitle("Title of the plot") +
  theme(plot.title = element_text(hjust = 0.5))

# base R Plot:
plot(mpg$displ, mpg$hwy) 
ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), position = "jitter") 
# negative relationship between engine size (displ) and fuel efficiency (hwy)

ggplot(data = mpg) # creates an empty plot!

# GPT-Prompt: "Lets pimp this plot... title "Scatterplot for 
# displacement and highway mileage", color red if displacement > 6, 
# color green if displacement <4; add a smoothing line; omit the 
# legend title, center the title"

# [first iteration, colors where not quite there yet...]

mpg <- mpg %>% mutate(cat_hwy = case_when(
  displ < 4 ~ "green",
  displ >= 4 & displ <= 6 ~ "blue",
  displ > 6 ~ "red"
))

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(color = cat_hwy), position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "loess", se = FALSE) +
  #geom_smooth(method = "lm", se = FALSE) + # lm = linear model
  ggtitle("Scatterplot for Displacement and Highway Mileage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("green" = "green", "blue" = "blue", "red" = "red"))
p

# BISHER TAG 2----

# What are the red dots and relatively fuel efficient points in the plot?
# Package "dplyr" by Hadley Wickham.
mpg %>% dplyr::filter(displ > 6 & hwy > 20)
# inefficient cars with displ > 6?
mpg %>% filter(displ > 6 & hwy < 20)
# most efficient cars
mpg %>% filter(displ < 2 & hwy > 40)

summary(mpg)


dim(mpg) # number of rows and cols
mpg[2,4] # look at single element in the data frame
#mpg[2,4] <- 1998 # change single entry
mpg[2,4]*2
as.numeric(mpg[2,4]) # convert to numeric

mpg[ order(mpg$manufacturer, decreasing = TRUE), ] # change order, sort in descending order for manufacturer
# or with dplyr:
mpg %>% arrange(desc(manufacturer)) # descending


# Save R whole workspace or single objects in R:
saveRDS(mpg, "mpg.RDS") # nice feature
mpg <- readRDS("mpg.RDS")

save.image(file = "my_workspace.RData")
load("my_workspace.RData")

# Very useful:
colnames(mpg) # column names of data frame
#colnames(mpg)[1] <- c("MANUF_new")

unique(mpg$class) # unique entries in vector

table(mpg$class) # frequency table (absolute frequence table)
table(mpg$class)/sum(table(mpg$class))*100 # relative frequencies

length(unique(mpg$class)) # how many different entries are there?

# One more dimension as information: car class
mpg %>% ggplot() + 
  geom_point(aes(x = displ, y = hwy, color = class)) +  # different colors for vehicle class
  ggtitle("Title") 

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), color = "blue") + 
  xlab("displacement") + 
  ggtitle("Titel") # + ......

# nice feature: split plot into subplots
mpg %>% dplyr::filter(year %in% 1999:2001) %>%
ggplot() + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) # stratify the plots after "class"
# Does the relationship between displ and hwy change within classes?

# two categorical variables
mpg %>% ggplot() + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)  + # 12 sub groups -> 3 are empty; stratify by cyl and drv
  ggtitle("Mileage vs displacement categorized via zylinders and drive train") +
  xlab("engine displacement, in litres") + 
  ylab("highway miles per gallon") + 
  theme(plot.title = element_text(hjust = 0.5))
# There seem to be no data points with 4 or 5 cylinders and rear wheel drive

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(drv ~ cyl)  + # 9 sub groups, the empty ones are omitted here
  ggtitle("Mileage vs displacement categorized via zylinders and drive train") +
  xlab("engine displacement, in litres") + 
  ylab("highway miles per gallon")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth(method = "loess") # add a smoothing line; https://en.wikipedia.org/wiki/Local_regression
# loess = locally estimated scatterplot smoothing

ggplot(mpg) +
  geom_smooth(aes(x = displ, y = hwy, color = drv), show.legend = TRUE)

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth(method = "lm") # "lm"=linear model. least squares estimated regression line

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), position = "jitter") # standard for parameter position="identity"
?geom_jitter

?datasets
library(help = "datasets")

# further examples
# data set diamonds
ggplot(data = diamonds) + 
  geom_bar(aes(x = cut)) # simple bar chart

bar <- ggplot(data = diamonds) + 
  geom_bar(
    aes(x = cut, fill = cut), # fill: R has 657 built-in named colours, which can be listed with grDevices::colors().
    show.legend = FALSE, # no legend
    width = 0.5 # Bar width. By default, set to 90% of the resolution of the data.
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL) # keine Labels

bar + coord_flip() # now graph is shown
bar + coord_polar() # do not do this!

# 3) READ data ----

# Reading data is often very (!) tedious; e.g., due to the simultaneous use 
# of "." and "," as decimal separators or different definition of missing values!

# Furthermore, there could be various line breaks in the dataset, 
# making reading less "straightforward"...
# You'll never find these difficulties with toy datasets.

# 3.1) Read directly from the web:--------
crab <- read.table("http://faculty.washington.edu/kenrice/rintro/crab.txt", # source
                   sep = " ", # column separator, here space
                   dec = ".", # decimal point
                   header = TRUE) # use first column for column names
# sep="\t" would read tab-separated files, always check the raw file to see the separator!
str(crab) # structure
View(crab) # works only in RStudio
summary(crab)
colnames(crab)
colnames(crab)[3]

# create new variable and add to data set
crab$new_var <- crab$width*10 # base R 
# or
#crab <- as.data.table(crab)
#crab[, new_var_1 := width*10] # data.table syntax
# or
crab <- crab %>% mutate(new_var_3 = width*10) # dplyr
crab

crab %>%
  dplyr::select(last_col(2):last_col()) # explicitely take "select" from dplyr (sometimes you have packages loaded with identical command-names...)

# expand...

# Delete Variable/Spalte:
crab$new_var_3 <- NULL # base R
crab %>% head()
# or
#crab[, new_var_1 := NULL] # data.table syntax
# or
crab <- crab %>% dplyr::select(-new_var)
crab %>% head() # worked


# 3.2) or read just from a local path on your hard disk--------

# Excel:
getwd() # get working directory
df <- readxl::read_xlsx("./Data/bike_excel_small.xlsx") # Note that there is (in my case) another command with the identical name in the package officer, hence I added the "readxl::"
# The command looks for a file named bike_excel_small.xlsx in the folder "Data" in the current working directory.
# Make sure that the current working directory is set correctly!

# column names: Do not use "Umlaute", " ", special characters, or numbers at the beginning of the column names!
# Example: Rotational_value_1,...
# Don't: "Erster Messung ß?"

str(df)
df$dteday <- as.POSIXct(df$dteday, format = "%d.%m.%Y") # Create date
str(df)
df$dteday[1]
df$dteday[2]
# We can now compare dates
df$dteday[1] < df$dteday[2] # TRUE

# BIS HIERHER TAG 3 ----------

# _write Excel:-------
df
getwd()
write_xlsx(df, "./Data/df_out.xlsx")

# _write Table to Word---------
ft <- flextable(df[, 1:5]) # only first 5 columns

# Save the flextable to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("")  # Add an empty paragraph for spacing

# Specify the output file path
output_path <- "./Data/df_table.docx"
print(doc, target = output_path)

# Text files
?read.csv
?read.csv2
?read.table

# Also check out: https://readr.tidyverse.org/


# 4) Data transformation using dplyr-----
# dplyr is part of tidyverse
?tidyverse # https://www.tidyverse.org/packages/

# Take careful note of the conflicts message that's printed when you load the tidyverse. 
# It tells you that dplyr overwrites some functions in base R. 
# If you want to use the base version of these functions after loading dplyr, 
# you'll need to use their full names: stats::filter() and stats::lag().

?flights
str(flights)
# POSIXct: convenient to date calculations 
# comparisons with >/</==
flights$time_hour[c(1,100)]
# compare:
flights$time_hour[c(1)] < flights$time_hour[c(100)]

# Tibbles are data frames, but slightly tweaked to work better in the tidyverse.
flights # larger data set, 336k rows

# __Important functions in dplyr, the basic grammar:-------

# - Pick observations by their values: filter().
# - Reorder the rows: arrange().
# - Pick variables by their names: select().
# - Create new variables with functions of existing variables: mutate().
# - Collapse many values down to a single summary: summarise().
# - These can all be used in conjunction with group_by() which changes the 
#   scope of each function from operating on the entire dataset to operating on it group-by-group. 
# -->> These six functions provide the verbs for a language of data manipulation.

flights %>% filter(month == 1, day == 1) # "==" for comparisons

( dec25 <- flights %>% filter(month == 12, day == 25) ) # directly show result
# saves the resulting tibble to dec25

# expand.....


# Fun (non-intuitive) Facts:
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1 # 0.02040816*49 = 0.9999998 (limited mantissa length)

options(digits = 22)
1/49
# 0.02040816326530612082046
options(digits = 10)

# google:
0.02040816326*49 # 0.99999999974

near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)
# or:
abs(1 / 49 * 49 - 1) < 10^(-5)


# logical Operators
1 == 1 # equal
1 > 2 # larger
3 >= 3 # larger or equal
3 != 4 # not equal
TRUE | TRUE # logical OR
c(TRUE,TRUE) & c(TRUE,FALSE) # element wise
TRUE & FALSE # logical AND
xor(TRUE, TRUE) # exclusive OR
xor(TRUE, FALSE)

ind <- c(1,3,5,18,21)
for(i in ind){
  print(i)
}
# very helpful
1:60 %nin% ind # Hmisc package


# set operations
x <- 1:7
y <- 4:10
x
y
union(x, y) # union
setdiff(x, y) # difference x - y
setdiff(y, x) # is not symmetric 


# Filter
filter(flights, month == 11 | month == 12)
# base R:
subset(flights, month == 11 | month == 12) # base R

identical(filter(flights, month == 11 | month == 12), 
          subset(flights, month == 11 | month == 12)) # TRUE

# or:
filter(flights, month %in% c(11, 12)) # %in%-operator

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120 & dep_delay <= 120) # identical (De Morgan)

# Missing values - wichtig, da es dauernd vorkommt! 
# NA .... 'Not Available' / Missing Values
NA > 5
10 == NA
NA + 10 # e.g. in mean() relevant
d <- c(1,7,56,NA)
mean(d, na.rm = FALSE) # NA
mean(d, na.rm = TRUE)
NA / 2 # NA

NA == NA # why?
# Let x be Mary's age. We don't know how old she is.
x <- NA
# Let y be John's age. We don't know how old he is.
y <- NA
# Are John and Mary the same age?
x == y
# We don't know!

# check classes is.XXXXXX()
is.na(x) # very important!
is.na(d)
sum(is.na(d)) # FALSE FALSE FALSE  TRUE ... 0,0,0,1

is.numeric(45.4)
class(45.4)
class(45)
is.character("hallo")
is.character("1")

# Recycling Bsp.
1:3 + 1:12 # recycling the shorter one
data.frame(x = 1:3, y = 1:12) # recycling the shorter one
data.frame(x = 1:3, y = 1:10) # Error, no multiple of the other
1:3 + 1:10 # What happens?

# pasting:
x <- 12
y <- 3
paste0(x, "and", y) # 0 means no space between, different data types are allowed, here character and integer
paste0(x, y) # no space
paste(x, "and", y)
paste(x, "and", y, sep = "")



# arrange() works similarly to filter() except that instead of selecting rows, it changes their order.
arrange(flights, year, month, day)
arrange(flights, year, month, desc(day))

sum(is.na(flights)) # there are NAs in the data set
vis_miss(flights) # error... too many rows
flights %>% dplyr::slice_sample(n = 1000) %>% vis_miss() # only 1000 rows

sum(is.na(flights$year)) # not in years

# select() ... useful subset using operations based on the names of the variables.
dplyr::select(flights, year, month, day) # select only these three variables and show the result
dplyr::select(flights, year:day) # select range of variables from year to day. -> practical as you don't need to count column numbers
dplyr::select(flights, -(year:day)) # What happens here?
# useful:
#starts_with("abc") # matches names that begin with "abc".
dplyr::select(flights, starts_with("s"))
#ends_with("xyz")   # matches names that end with "xyz".
#contains("ijk")    # matches names that contain "ijk".
dplyr::select(flights, contains("time"))

dplyr::rename(flights, tail_num = tailnum) # rename

dplyr::select(flights, time_hour, air_time, everything()) # time_hour, air_time, and the rest
# https://tidyselect.r-lib.org/reference/everything.html

# mutate() always adds new columns at the end of your dataset 

flights_sml <- dplyr::select(flights, # smaller data set
                             year:day, 
                             ends_with("delay"), 
                             distance, 
                             air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)
head(flights_sml) # We did not save, same data set as before

transmute(flights, # only keeps new columns
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)


# summarise() collapses a data frame to a single row

summarise(flights, delay = mean(dep_delay, na.rm = TRUE)) # simple mean

# __more PIPE-Operator----

# without pipe
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
delay %>% ggplot(aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) + # alpha .. Durchsichtigkeit der Punkte aus [0,1]
  geom_smooth(se = FALSE) # se steht fuer standard error, zeichnet Konfidenzintervall um die geschaetzte Kurve.


# same with Pipe-Operator:
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

# Note: Working with the pipe is one of the key criteria for belonging to the tidyverse. 
# The only exception is ggplot2: it was written before the pipe was discovered


# undo group
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

# 5) Exploratory Data Analysis EDA ----

# a) Generate questions about your data.
# b) Search for answers by visualizing, transforming, and modelling your data.
# c) Use what you learn to refine your questions and/or generate new questions.

# What type of variation occurs within my variables?
# What type of covariation occurs between my variables?

?diamonds # Prices of over 50,000 round cut diamonds

# Visualizing distributions
# simple frequency table
table(diamonds$cut) # categorical variable, is it ordinal or nominal?

diamonds <- diamonds %>% group_by(cut) %>% mutate(count_cut = n())
ggplot(data = diamonds ) +
  geom_bar(aes(x = fct_reorder(cut, count_cut, .desc = TRUE)) ) + # z.B. falling order
  xlab("")

# to examine the distribution of a continuous variable, use a histogram:
ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), binwidth = 0.5)
# This attempts to graphically represent the density of a continuous random variable

# overlay multiple histograms in the same plot
smaller <- diamonds %>% 
  filter(carat < 3) # consider, for example, only diamonds with <3 carats
ggplot(data = smaller, aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01) + 
  scale_x_continuous(breaks = seq(0, 3, by = 0.5))
# "The ideal weight for a classic engagement ring is between 1.00 and 1.50 carats.
# The average carat weight of a diamond engagement ring is slightly over 1 carat,
# specifically 1.18 carats. The average size of a diamond engagement ring
# varies by country and region."

# see https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Density_plot_boxplot_below.R
# for a nice histogram with boxplot below

# Are there outliers/unusual values?

# One definition could be:
# "An outlier is an observation that lies an abnormal distance from other values in a random sample from a population. 
# In a sense, this definition leaves it up to the analyst (or a consensus process) to decide what will be considered
# abnormal. Before abnormal observations can be singled out, it is necessary to characterize normal observations."
# see also https://en.wikipedia.org/wiki/Outlier

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) # y = width in mm (0--58.9)

# zoom to small values of the y-axis
ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# look at unusual ones:
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% # | logical OR
  dplyr::select(price, x, y, z) %>% # x ... length in mm (0--10.74)
  arrange(y) 
unusual


# Covariation 

# If variation describes the behavior within a variable, covariation describes the behavior between variables. 
# Covariation is the tendency for the values of two or more variables to vary together in a related way. 

# Example: how the price of a diamond varies with its quality:
ggplot(data = diamonds, aes(x = price)) + 
  geom_freqpoly(aes(colour = cut), binwidth = 500)
# difficult to compare

# For a better comparison, consider the density, i.e., the area under the polygon is now 1.
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Maybe we can simply represent this as a boxplot:
ggplot(diamonds, aes(x = reorder(cut, price, FUN = median), y = price)) + # on the x-axis is the categorical variable 
  geom_boxplot()   +                                # cut, on the y-axis the continuous variable price
  xlab("")
# Comparison of central values is now easier
# Supports the counter-intuitive finding that better quality diamonds are cheaper on average!


# Visualization of two categorical variables
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# Visualize two continuous variables
# often you can just use base-R plotting if you need something quick
plot(diamonds$carat, diamonds$price) # a bit ugly

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
# to make it look less cluttered, add transparency
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100) # with alpha in [0,1]

# The trick with bins works not only in histograms, it also works in 2D
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price)) # library hexbin; even more aesthetic with hexagons

# You can also bin a single variable:
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) # cut_width() divides the variable into segments of length 0.1

# Patterns/Clusters
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting)) +
  xlab("Eruption time in mins") + 
  ylab("Waiting time to next eruption (in mins)") 
# In 2D, you can see the clusters with the naked eye.
# -> longer wait times are associated with longer eruptions
# Multiple dimensions require multivariate methods -> see cluster analysis later!

# shorter
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# 6) Wata Wrangling - the art of getting your data into R in a useful form for visualisation and modelling ----

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
bike <- read_csv("./Data/day.csv", col_names = TRUE)
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






# 7) Programming ####

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


# 8) Health science specific code--------