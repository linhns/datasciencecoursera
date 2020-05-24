library(ggplot2)
SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))


# NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]

# Subset coal combustion related data
combustion_related <- grepl("comb", SCC[, SCC.Level.One], ignore.case=TRUE)
coal_related <- grepl("coal", SCC[, SCC.Level.Four], ignore.case=TRUE) 
combustionSCC <- SCC[combustion_related & coal_related, SCC]
index <- NEI[,SCC] %in% combustionSCC
combustionNEI <- NEI[index]

png(filename='plot4.png')

ggplot(combustionNEI, aes(factor(year), Emissions)) +
    geom_bar(stat = "identity", fill = "navy", width = 0.3) +
    xlab("Year") +
    ylab(expression("Total pm 2.5 Emission (Tons)")) +
    labs(title = "pm 2.5 Coal Combustion emissions of US 1999-2008")
dev.off()