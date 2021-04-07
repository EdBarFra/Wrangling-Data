library(tidyverse)
library(dslabs)    #includes readr
library(readxl)
# library()    list of packages installed in different
#              libraries on your computer:

# getCRANmirrors()
# install.packages("vioplot")
# installed.packages()
# remove.packages("vioplot")
# check what packages need an update
# old.packages()
# update.packages()
# ?() and help() are the first source of documentation in a
#        package
# browseVignettes(package="ggplot2")
# see working directory
getwd()

# change your working directory
setwd("C:/Users/edbar/Documents/Wrangling")
getwd()

system.file()
system.file(package = "dslabs")
dir <- system.file("extdata", package="dslabs")
dir
packageDescription("tidyverse")
ls("package:tidyverse")
help(package = "tidyverse")
list.files(dir)
filename <- "murders.csv"
fullpath <- file.path(dir, filename)
fullpath
file.copy(fullpath, getwd())
# This function takes two arguments: the file to copy
# and the name to give it in the new directory.
file.copy(fullpath, "murders.csv")
list.files()
file.exists("murders.csv")
filename %in% list.files(file.path(dir, "extdata")) 

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read.csv(filename)  ## base R function, creates data.frame
dat <- read_csv(filename)  ## tidyverse function, creates tibble
dat
# View(dat)

#read using full path
dat <- read_csv(fullpath)
head(dat)

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(dir, filename))
dat
dat1 <- read.csv(file.path(dir, filename1))
dat1
dat2 <- read.csv(file.path(dir, filename2))
dat2

path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files
filename <- "ssa-death-probability.csv"  
totalpath <- file.path(path, filename)
file.copy(totalpath, filename)
read.csv(filename)

path <- system.file("extdata", package = "dslabs")
filename <- "murders.csv"
x <- scan(file.path(path, filename), sep = ",", what = "c")
x[1:10]


url <- "https://raw.githubusercontent.com/rafalab/dslabs
         /master/inst/extdata/murders.csv"
dat <- read.csv(url)
dat
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
dat
file.remove(tmp_filename)

# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(filename)
dat2
class(dat2$abb)
class(dat2$region)

