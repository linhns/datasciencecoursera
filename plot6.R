library(ggplot2)
SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))

# Prevents histogram from printing in scientific notation
# NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]

# Subset vehicle related data
vehicle_related <- grepl("vehicle", SCC[, SCC.Level.Two], ignore.case=TRUE) 
vehicleSCC <- SCC[vehicle_related, SCC]
vehiclesNEI <- NEI[NEI[, SCC] %in% vehicleSCC,]
BaltimoreLAVehicleNEI <- vehiclesNEI[fips %in% c("24510", "06037")]

png(filename='plot6.png')

ggplot(BaltimoreLAVehicleNEI, aes(factor(year), Emissions, fill = fips)) +
    geom_bar(stat = "identity", aes(fill = year), width = 0.3) +
    facet_grid(.~fips) +
    xlab("Year") +
    ylab(expression("Total pm 2.5 Emission (kilotons)")) +
    labs(title = "Baltimore & LA pm 2.5 Emissions by Vehicle 1999-2008") +
    theme_minimal()
dev.off()