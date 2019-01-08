#
# plot4.R - PM2.5 US emissions analysis by year for coal combustion related sources
#
# Question: Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

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

## Determine EI Sectors that refer to coal and related sub-levels of the SCC hierarchy
## Assuming SCC is constructed as a multi-level hierararchy of nodes where the top node
## has a direct relation with the underlying nodes.
## The three EI.Sector's of interest are:
## 1. "Fuel Comb - Electric Generation - Coal" 
## 2. "Fuel Comb - Industrial Boilers, ICEs - Coal"
## 3. "Fuel Comb - Comm/Institutional - Coal" 
scc_coal <- unique(grep("coal", SCC$EI.Sector, ignore.case = T, value = T))
scc_coal_levels <- subset(SCC, EI.Sector %in% scc_coal)[,c("SCC", "EI.Sector")]

### Filter the main dataset by desired SCC & summarise by year & EI.Sectors for plotting
NEI_sum <- NEI %>%
        filter(SCC %in% scc_coal_levels$SCC) %>%
        left_join(scc_coal_levels, by = "SCC") %>%
        group_by(year, EI.Sector) %>%
        summarise(Emissions = sum(Emissions, na.rm = T))

## Plot resulting dataset
png(filename="plot4.png")
ggplot(NEI_sum, aes(factor(year), Emissions/10^05)) + 
        geom_bar(stat = "identity", fill = "dodgerblue") +
        facet_grid(.~EI.Sector) +
        labs(title = "Coal combustion-related emissions by Year in the US") +
        theme(plot.title = element_text(face = "bold", size = 16)) +
        labs(subtitle = "Showing source sectors for completeness") +
        theme(plot.subtitle = element_text(face = "italic", size = 12)) +
        labs(x = "Calendar Year", y = "Total Emissions in (10^05 Tons)") +
        labs(caption = "Source: EPA National Emissions Inventory web site") +
        scale_y_continuous(minor_breaks = seq(0,max(NEI_sum$Emissions/10^05), by = 1))
dev.off()