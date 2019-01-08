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
scc_coal <- unique(grep("coal", SCC$EI.Sector, ignore.case = T, value = T))
scc_coal_levels <- subset(SCC, EI.Sector %in% scc_coal)

### Filter the main dataset by desired SCC & summarise by year for plotting
NEI_sum <- NEI %>%
        filter(SCC %in% scc_coal_levels$SCC) %>%
        group_by(year) %>%
        summarise(Emissions = sum(Emissions, na.rm = T))

## Plot resulting dataset
png(filename="plot4.png")
ggplot(NEI_sum, aes(factor(year), Emissions)) + 
        geom_bar(stat = "identity", fill = "dodgerblue") +
        labs(title = "** US coal combustion-related emissions by Year **") +
        labs(caption = "Source: EPA National Emissions Inventory web site") +
        labs(x = "Calendar Year", y = "Total Emissions") +
        theme_stata()

dev.off()