# Load CO2 DATA
library(readr)
CO2 <- read_delim("data/CO2.csv", delim = "\t", 
                  escape_double = FALSE, trim_ws = TRUE)
View(CO2)
