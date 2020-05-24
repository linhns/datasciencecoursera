SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]
BaltimoreNEI <- NEI[fips == '24510', lapply(.SD, sum, na.rm = TRUE), .SDcols = c("Emissions"), 
           by = year]

png(filename='plot2.png')

barplot(BaltimoreNEI[, Emissions]
        , names = BaltimoreNEI[, year]
        , xlab = "Years", ylab = "Emissions"
        , main = "Emissions over the Years")

dev.off()