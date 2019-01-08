#
# plot5.R - PM2.5 Baltimore vehicle emissions analysis by year
#
# Question: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#

library(ggplot2)
library(dplyr)
library(ggthemes)

files_for_analysis <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("./data")){
        download.file(files_for_analysis, "raw_data.zip")
        unzip("raw_data.zip", exdir = "data")
}

## Read source files
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")
