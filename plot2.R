#
# plot2.R - PM2.5 Yearly emissions analysis for the city of Baltimore
#
# Question: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
#

files_for_analysis <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("./data")){
        download.file(files_for_analysis, "raw_data.zip")
        unzip("raw_data.zip", exdir = "data")
}

## Read source file
NEI <- readRDS("data/summarySCC_PM25.rds")

# Retain only records for the city of Maryland (fips == "24510")
NEI <- subset(NEI, fips == "24510")

# Calculate sum & mean of emissions by year
year_mean <- aggregate(Emissions ~ year, data = NEI, mean)
year_sum <- aggregate(Emissions ~ year, data = NEI, sum)

# Plot graph showing total and mean emissions by year
png(filename="plot2.png")
par(mar = c(5,5,2,5))
barplot(year_sum$Emissions,
        names = factor(year_sum$year),
        main = "PM2.5 Emissions by Year in the City of Baltimore",
        xlab = "Calendar Year",
        ylab="Total Emissions in (Tons)",
        col = "dodgerblue")
par(new = T)
plot(year_mean$year,
     year_mean$Emissions,
     type = "b",
     col = "red3", pch=16,
     axes=F,
     xlab=NA, ylab=NA,
     cex=1.2)
axis(side = 4)
mtext(side = 4, line = 3, 'Mean emmissions in (Tons)')
legend("topright", lty=c(1,1), col = c("grey","red"), legend = c("Total (bars)", "Mean (line)"))
dev.off()
