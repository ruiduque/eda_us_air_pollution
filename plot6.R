#
# plot6.R - PM2.5 Baltimore vs Los Angeles vehicle emissions analysis by year
#
# Question: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
# in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?
#

library(ggplot2)
library(dplyr)
library(ggthemes)
library(stringr)

files_for_analysis <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("./data")){
        download.file(files_for_analysis, "raw_data.zip")
        unzip("raw_data.zip", exdir = "data")
}

## Read source files
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Determination of vehicle sectors to consider. Following analysis of the SCC dataset, I will 
## proceed with the assumption that the following Emission Sectors are representative of
## all mobile vehicle emissions sources of type on-road available in the dataset:
## 1- "Mobile - On-Road Gasoline Light Duty Vehicles"
## 2- "Mobile - On-Road Gasoline Heavy Duty Vehicles"
## 3- "Mobile - On-Road Diesel Light Duty Vehicles"
## 4- "Mobile - On-Road Diesel Heavy Duty Vehicles"
scc_mobile <- unique(grep("mobile", SCC$EI.Sector, ignore.case = T, value = T))
scc_mobile_levels <- subset(SCC, EI.Sector %in% scc_mobile)[,c("SCC", "EI.Sector")]


### Filter the main dataset by Baltimore or Los Angeles (fips="24510" or "06037"), source types "ON-ROAD" & EI.Sectors = Mobile.
### Create new column with the name of the city based on the fips code.
### Summarise by year & city 
NEI_sum <- NEI %>%
        filter((fips == "24510" | fips =="06037") & type == "ON-ROAD" & SCC %in% scc_mobile_levels$SCC) %>%
        left_join(scc_mobile_levels, by = "SCC") %>%
        mutate(City = if_else(fips == "24510", "Baltimore", "Los Angeles")) %>%
        group_by(year, EI.Sector, City) %>%
        summarise(Emissions = sum(Emissions, na.rm = T))

## Remove "Mobile - " from the EI.Sector var to allow better plotting
NEI_sum$EI.Sector <- str_replace(NEI_sum$EI.Sector, "Mobile - On-Road", "")

## Plot resulting dataset
png(filename="plot6.png")
ggplot(NEI_sum, aes(factor(year), Emissions, fill = EI.Sector)) + 
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_grid(.~City) +
        labs(title = "Motor vehicle emissions") +
        theme(plot.title = element_text(face = "bold", size = 12)) +
        labs(subtitle = "Baltimore v Los Angles for periods between 1999-2008") +
        theme(plot.subtitle = element_text(face = "bold", size = 12)) +
        labs(x = "Calendar Year", y = "Total Emissions in (Tons)") +
        labs(caption = "Source: EPA National Emissions Inventory web site")
dev.off()
