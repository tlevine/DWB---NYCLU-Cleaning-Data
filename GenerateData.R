setwd("/Users/Maryclare/Desktop/Data")

library(plyr)

data2010 <- read.csv("2010.csv", stringsAsFactors = FALSE)
data2009 <- read.csv("2009.csv", stringsAsFactors = FALSE)
data2008 <- read.csv("2008.csv", stringsAsFactors = FALSE)
data2007 <- read.csv("2007.csv", stringsAsFactors = FALSE)

cat("Read in data", "\n")

####For testing.
#data2010 <- data2010[1:100,]
#data2009 <- data2009[1:100,]
#data2008 <- data2008[1:100,]
#data2007 <- data2007[1:100,]

varstodrop <- c("crimesusp", "othfeatr", "rescode","rescode", "premtype", "premname", "addrnum", "stname", "stinter", "crossst", "aptnum", "state", "zip", "sector","dettypeCM", "lineCM")

data2010 <- data2010[,names(data2010)[!names(data2010) %in% varstodrop]]
data2009 <- data2009[,names(data2009)[!names(data2009) %in% varstodrop]]
data2008 <- data2008[,names(data2008)[!names(data2008) %in% varstodrop]]
data2007 <- data2007[,names(data2007)[!names(data2007) %in% varstodrop]]

write.csv(data2010, "data2010merge.csv", row.names = FALSE, na = ".")
save(data2010, file = "data2010merge.RData")
write.csv(data2009, "data2009merge.csv", row.names = FALSE, na = ".")
save(data2009, file = "data2009merge.RData")
write.csv(data2008, "data2008merge.csv", row.names = FALSE, na = ".")
save(data2008, file = "data2008merge.RData")
write.csv(data2007, "data2007merge.csv", row.names = FALSE, na = ".")
save(data2007, file = "data2007merge.RData")

cat("Dropped big strings", "\n")

data2010$rownums <- seq(1, nrow(data2010), 1)
data2009$rownums <- seq(1, nrow(data2009), 1)
data2008$rownums <- seq(1, nrow(data2008), 1)
data2007$rownums <- seq(1, nrow(data2007), 1)

cat("Made row IDs for Later Merge", "\n")

MakeBinary <- function(data) {
	for (i in 1:ncol(data)) {
		u <- unique(data[,i])
		if (length(u) <= 3 && "Y" %in% u & "N" %in% u) {
			data[,i] <- ifelse(data[,i] == "Y", 1, ifelse(data[,i] == "N", 0, NA))
		}
	}
	return(data)
}

data2010 <- MakeBinary(data2010)
data2009 <- MakeBinary(data2009)
data2008 <- MakeBinary(data2008)
data2007 <- MakeBinary(data2007)

cat("Replaced Y/N's with 1/0's", "\n")

system.time(data <- rbind.fill(data2010, data2009, data2008, data2007))

cat("Stacked the data", "\n")

save(data, file = "datalump.RData")
write.csv(data, "datalump.csv", row.names = FALSE, na = ".")

cat("Wrote raw data", "\n")

###There's one pesky empty observation. I am deleting it.
#data <- data[data$eyecolor == " ", ]

data$recstat <- ifelse(data$recstat == "A", 0, 1)

data$inout <- ifelse(data$inout == "I", 0, ifelse(data$inout == "O", 1, NA))

data$trhsloc <- ifelse(data$trhsloc == " ", NA, ifelse(data$trhsloc == "H", 0, ifelse(data$trhsloc == "P", 1, ifelse(data$trhsloc == "T", 2, NA))))

data$typeofid <- ifelse(data$typeofid == "O", 0, ifelse(data$typeofid == "P", 1, ifelse(data$typeofid == "R", 2, ifelse(data$typeofid == "V", 3, NA))))

data$officrid <- ifelse(data$officrid == " ", NA, ifelse(data$officrid == "0", 0, ifelse(data$officrid == "I", 1, NA)))

data$adtlrept <- ifelse(data$adtlrept == " ", NA, ifelse(data$adtlrept == "0", 2, ifelse(data$adtlrept == "N", 0, ifelse(data$adtlrept == "Y", 1, NA))))

data$offverb <- ifelse(data$offverb == " ", NA, ifelse(data$offverb == "0", 0, ifelse(data$offverb == "V", 1, NA)))

data$offshld <- ifelse(data$offshld == " ", NA, ifelse(data$shld == "0", 0, ifelse(data$offshld == "S", 1, NA)))

data$sex <- ifelse(data$sex == "F", 0, ifelse(data$sex == "M", 1, NA))

data$race <- ifelse(data$race == " ", NA, ifelse(data$race == "A", 0, ifelse(data$race == "B", 1, ifelse(data$race == "I", 2, ifelse(data$race == "P", 3, ifelse(data$race == "Q", 4, ifelse(data$race == "U", 5, ifelse(data$race == "W", 6, ifelse(data$race == "Z", 7, NA)))))))))

data$haircolr <- ifelse(data$haircolr == " ", NA, ifelse(data$haircolr == "BA", 0, ifelse(data$haircolr == "BK", 1, ifelse(data$haircolr == "BL", 2, ifelse(data$haircolr == "BR", 3, ifelse(data$haircolr == "DY", 4, ifelse(data$haircolr == "FR", 5, ifelse(data$haircolr == "GY", 6, ifelse(data$haircolr == "RA", 7, ifelse(data$haircolr == "RD", 8, ifelse(data$haircolr == "SN", 9, ifelse(data$haircolr == "SP", 10, ifelse(data$haircolr == "WH", 11, ifelse(data$haircolr == "XX", 12, ifelse(data$haircolr == "ZZ", 13, NA)))))))))))))))

data$eyecolor <- ifelse(data$eyecolor == " ", NA, ifelse(data$eyecolor == "BK", 0, ifelse(data$eyecolor == "BL", 1, ifelse(data$eyecolor == "BR", 2, ifelse(data$eyecolor == "DF", 3, ifelse(data$eyecolor == "GR", 4, ifelse(data$eyecolor == "GY", 5, ifelse(data$eyecolo == "HA", 6, ifelse(data$eyecolor == "MA", 7, ifelse(data$eyecolor == "MC", 8, ifelse(data$eyecolor == "P", 9, ifelse(data$eyecolor == "PK", 10, ifelse(data$eyecolor == "VI", 11, ifelse(data$eyecolor == "XX", 12, ifelse(data$eyecolor == "Z", 13, ifelse(data$eyecolor == "ZZ", 14, NA))))))))))))))))

data$addrtyp <- ifelse(data$addrtyp == " ", NA, ifelse(data$addrtyp == "L", 1, NA))

data$build <- ifelse(data$build == " ", NA, ifelse(data$build == "H", 0, ifelse(data$build == "M", 1, ifelse(data$build == "T", 2, ifelse(data$build == "T", 2, ifelse(data$build == "U", 3, ifelse(data$build == "Z", 4, NA)))))))

data$post <- ifelse(data$post == "PP", -99, data$post)

data$dettypcm <- ifelse(data$dettypcm == " ", NA, ifelse(data$dettypcm == "CM", 1, NA))

data$city <- ifelse(data$city == " ", NA, ifelse(data$city == "BRONX", 0, ifelse(data$city == "BROOKLYN", 1, ifelse(data$city == "MANHATTAN", 2, ifelse(data$city == "QUEENS", 3, ifelse(data$city == "STATEN IS", 4, ifelse(data$city == "STATEN ISLAND", 4, NA)))))))

cat("Recoded Variables", "\n")

data$dob <- ifelse(nchar(data$dob) == 7, paste("0", data$dob, sep = ""), data$dob)
data$dob <- as.Date(as.character(data$dob), "%m%d%Y")

data$datestop <- ifelse(nchar(data$datestop) == 7, paste("0", data$datestop, sep = ""), data$datestop)
data$datestop <- as.Date(as.character(data$datestop), "%m%d%Y")

for (i in 1:ncol(data)) {
	if (!names(data)[i] %in% c("crimsusp", "sumoffen", "arstoffn", "dob", "datestop")) {
		data[,i] <- as.numeric(data[,i])
		cat(i, "\n")
		}
}


cat("Made Numeric", "\n")

write.csv(data[,names(data)[!names(data) %in% c("crimsusp", "sumoffen", "arstoffn")]], "data.csv", row.names = FALSE, na = ".")
save(data[,names(data)[!names(data) %in% c("crimsusp", "sumoffen", "arstoffn")]], file = "data.RData")
write.csv(data, "all.csv", row.names = FALSE, na = ".")
save(data, file = "all.RData")

cat("Saved All Data", "\n")

for (i in 2007:2010) {
	write.csv(data[data$year == i,], paste("data", i, ".csv", sep = ""), row.names = FALSE, na = ".")
	save(data[data$year == i,], file = paste("data", i, ".RData", sep = ""))
}

cat("Saved Data By Year", "\n")