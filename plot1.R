SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]
NEI <- NEI[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("Emissions"), 
           by = year]

png(filename='plot1.png')

barplot(NEI[, Emissions]
        , names = NEI[, year]
        , xlab = "Years", ylab = "Emissions"
        , main = "Emissions over the Years")

dev.off()