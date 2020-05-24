library(ggplot2)
SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))

# Prevents histogram from printing in scientific notation
# NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]

# Subset vehicle related data
vehicle_related <- grepl("vehicle", SCC[, SCC.Level.Two], ignore.case=TRUE) 
vehicleSCC <- SCC[vehicle_related, SCC]
vehiclesNEI <- NEI[NEI[, SCC] %in% vehicleSCC,]
BaltimoreVehicleNEI <- vehiclesNEI[fips == '24510']

png(filename='plot5.png')

ggplot(BaltimoreVehicleNEI, aes(factor(year), Emissions)) +
    geom_bar(stat = "identity", fill = "navy", width = 0.3) +
    xlab("Year") +
    ylab(expression("Total pm 2.5 Emission (Tons)")) +
    labs(title = "pm 2.5 vehicle emissions of Baltimore City 1999-2008") + 
    theme_minimal()
dev.off()