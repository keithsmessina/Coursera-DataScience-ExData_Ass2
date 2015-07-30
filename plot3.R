# Read data into data frames
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

# Load ggplot2 library
library(ggplot2)

# Save a vector of the years in the data frame
years <- unique(NEI$year)

# Save a vector of the types of emissions measurements in the data frame
type <- unique(NEI$type)

# Initialize new data frame to hold yearly totals
totalPM25 <- data.frame(year = c(1:(length(years)*length(type))),
                        emissions = c(1:(length(years)*length(type))),
                        type = c(1:(length(years)*length(type))))

# Create counter for loop to start at new row
counter <- 0

# Loop over each year, totaling emissions over each year for only Baltimore, MD
# and then looping over each data source type and then saving it to the new
# data frame
for(i in 1:length(years)){
  for(j in 1:length(type)){
    totalPM25$emissions[counter + j] <- sum(NEI$Emissions[NEI$year == years[i] &
                                                            NEI$fips == "24510" &
                                                            NEI$type == type[j]])
    totalPM25$year[counter + j] <- years[i]
    totalPM25$type[counter + j] <- type[j]
  }
  
  # Increment counter to start at next open row
  counter <- counter + length(type)
}

# Open PNG device to save the plot
png("plot3.png")

# Plot the data in panel of four plots
print(qplot(year, emissions, data = totalPM25, facets = type~.) +
  geom_line(colour="red") +
  labs(title = "Emissions of PM25 in Baltimore, MD per Data Source") +
  labs(x="Year", y="Emissions (tons)"))

# Close the device to save the plot
dev.off()