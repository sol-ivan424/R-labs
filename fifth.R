if ("quantmod" %in% rownames(installed.packages()) == FALSE) 
  { install.packages("quantmod") } 
library(quantmod) 
if ("stringr" %in% rownames(installed.packages()) == FALSE) 
  { install.packages("stringr") } 
library(stringr) 
downloadable_stocks <- c("ATVI", "^IXIC") 
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01")) 
df <- data.frame(get(downloadable_stocks[1])) 
downloadable_stocks <-  stringr::str_remove(downloadable_stocks, "[:punct:\\^]")
rm(list = downloadable_stocks) 