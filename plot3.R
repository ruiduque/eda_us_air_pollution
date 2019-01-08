#
# plot3.R - PM2.5 US emissions by year for Baltimore and source type
#
# Question: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases
# in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)
library(ggthemes)
library(dplyr)

files_for_analysis <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("./data")){
        download.file(files_for_analysis, "raw_data.zip")
        unzip("raw_data.zip", exdir = "data")
}

## Read source files
NEI <- readRDS("data/summarySCC_PM25.rds")

# Retain only records for the city of Baltimore (fips == "24510") and columns - type, year & Emissions
NEI_sum <- NEI %>%
        filter(fips == "24510") %>%
        group_by(year, type) %>%
        summarise(Emissions = sum(Emissions))

# Create graph of sums and means for each type or source
png(filename="plot3.png")
ggplot(NEI_sum, aes(factor(year), Emissions)) + 
#        geom_line(aes(group = type), colour = "grey80") +
        geom_bar(stat = "identity", fill = "dodgerblue") +
#        geom_point(aes(colour = type)) +
        geom_smooth(aes(colour = type)) +
        facet_grid(.~type) +
        labs(title = "Baltimore emissions by year and source") +
        labs(x = "Calendar Year") +
        labs(caption = "Source: EPA National Emissions Inventory web site") +
        theme_economist_white()
#       scale_y_continuous(minor_breaks = seq(0,max(NEI_sum$Emissions), by = 100))
#        scale_x_continuous(breaks = c(1999,2002,2005,2008))
dev.off()



