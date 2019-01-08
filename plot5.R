#
# plot5.R - PM2.5 Baltimore vehicle emissions analysis by year
#
# Question: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
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

## Determination of vehicle sectors to consider. Following analysis of the SCC dataset, I will 
## proceed with the assumption that the following Emission Sectors are representative of
## all mobile vehicle emissions sources of type on-road available in the dataset:
## 1- "Mobile - On-Road Gasoline Light Duty Vehicles"
## 2- "Mobile - On-Road Gasoline Heavy Duty Vehicles"
## 3- "Mobile - On-Road Diesel Light Duty Vehicles"
## 4- "Mobile - On-Road Diesel Heavy Duty Vehicles"
scc_mobile <- unique(grep("mobile", SCC$EI.Sector, ignore.case = T, value = T))
scc_mobile_levels <- subset(SCC, EI.Sector %in% scc_mobile)[,c("SCC", "EI.Sector")]

### Filter the main dataset by Baltimore (fips="24510"), source types "ON-ROAD" & EI.Sectors = Mobile.
### Summarise by year & EI.Sectors for plotting
NEI_sum <- NEI %>%
        filter(fips == "24510" & type == "ON-ROAD" & SCC %in% scc_mobile_levels$SCC) %>%
        left_join(scc_mobile_levels, by = "SCC") %>%
        group_by(year, EI.Sector) %>%
        summarise(Emissions = sum(Emissions, na.rm = T))

## Remove "Mobile - " from the EI.Sector var to allow better plotting
NEI_sum$EI.Sector <- str_replace(NEI_sum$EI.Sector, "Mobile - On-Road", "")

## Plot resulting dataset
png(filename="plot5.png")
ggplot(NEI_sum, aes(factor(year), Emissions)) + 
        geom_bar(stat = "identity", fill = "dodgerblue") +
        facet_grid(.~EI.Sector) +
        labs(title = "Emissions from motor vehicle sources from 1999–2008 in Baltimore City") +
        theme(plot.title = element_text(face = "bold", size = 14)) +
        labs(subtitle = "Showing on-road source types for completeness") +
        theme(plot.subtitle = element_text(face = "italic", size = 12)) +
        labs(x = "Calendar Year", y = "Total Emissions in (Tons)") +
        labs(caption = "Source: EPA National Emissions Inventory web site")
dev.off()