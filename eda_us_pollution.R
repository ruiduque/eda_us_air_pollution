##
## Air pollution in the US - Exploratory Data Analysis
##
## Rui Santos - 01.2019

# Get file for Year = 1999
pm0 <- read.table("pm25_data/RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
dim(pm0)        
head(pm0)

# Read column names
cnames <- readLines("pm25_data/RD_501_88101_1999-0.txt", 1)

# Assign column names to pm0
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- make.names(cnames[[1]]) #make.names replaces blank space in column names with dots "."
head(pm0)

# Extract pm25 sample column
x0 <- pm0$Sample.Value
str(x0)
summary(x0) # 13217 NA values...

# Find proportion of missing values
mean(is.na(x0)) #about 11% missing

# Let's read the datafile for 2012
pm1 <- read.table("pm25_data/RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
dim(pm1)        
head(pm1)

# Column names are the same so just need to assign the cnames to pm1
names(pm1) <- make.names(cnames[[1]]) #make.names replaces blank space in column names with dots "."
head(pm1)

# Extract pm25 sample column
x1 <- pm1$Sample.Value
str(x1)
summary(x1) # 73133 NA values...

# Find proportion of missing values
mean(is.na(x1)) #about 5% missing

# Comparison of 1999 pm2.5 and 2012 pm2.5
boxplot(log10(x0), log10(x1))

# Inspect negative values in 2012
summary(x1)
negative <- x1<0
str(negative)
sum(negative, na.rm = TRUE) #26474 negative values in 2012
mean(negative, na.rm = TRUE) #2% of the entire dataset


# Look at dates
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates,"month") #Most of the readings are from the winter months

hist(dates[negative], "month") #Just checking if there are any indication about negative values...they occure more in winter, but no conclusion can be drawn

# Let's now look at a single monitor location to find a matching monitor that we could use for analysis
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

# Concatenate County.Code with Site.ID to allow intersection of both years to find matching monitor stations
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")

both <- intersect(site0, site1)
str(both) # 10 monitor stations that are in both datasets

# We need to find a monitor that exists in both time periods but also that have lots of observations.
# Create new variable county.site as above but add it to the original dataset

pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

# subset the original datasets to only state = 36 "New York" and site/monitors that exist in both datasets
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

# Look at number of rows by monitor
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

# Based on the number of observations, let's select monitor 63.2008 - go back to original dataset and get data
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm0sub)
dim(pm1sub)

# Plot 
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)

dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

# Plot both datasets side-by-side
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))

plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm = T))

# Need to make y axis the same to allow like for like comparison
rng <- range(x0sub, x1sub, na.rm = T)
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))

plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

## From observation of the two plots, we can determine that the average pm2.5 decreased from 1992 to 2002 and also that 
## the spikes verified in 1992 are no longer occurring in 2012.

## Let's now look at state level ---


