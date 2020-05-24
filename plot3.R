library(ggplot2)
SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]
BaltimoreNEI <- NEI[fips == '24510',]

png(filename='plot3.png')

ggplot(BaltimoreNEI, aes(factor(year), Emissions)) +
    geom_line(stat = "identity") +
    facet_grid(.~type, scales = "free", space = "free") +
    xlab("year") +
    ylab(expression("Total pm 2.5 Emission (Tons)")) +
    labs(title = "pm 2.5 Emissions of Baltimore City 1999-2008 by Source Type")

dev.off()