library(tseries)
library(xts)
library(zoo)
library(gtrendsR)


startmon <- function(x){
  return(as.Date.factor(format(x , "%Y-%m-01")))
}

endmon <- function(x){
  return((startmon(startmon(as.Date.factor(x+32))) - 1))
}


## download gtrends data on a monthly basis ##

searches = list()
k <- 1
## set starting date
start.date <- as.Date.factor("2016-01-01")
## set ending date
end.date <- as.Date.factor("2022-09-01")                    
date <- start.date

while(date < end.date){
  interval <- (c(as.character(date),as.character(endmon(date))))
  searches[[k]] <- gtrends(keyword = "Ethereum",
                           time = paste(interval[1] , interval[2]), 
                           low_search_volume = TRUE,
                           gprop = "web")
  k = k+1
  date <- startmon(as.Date.factor(date + 32))
  Sys.sleep(120)
}

length(searches)

### append start dates to each list object ###
k<-1
while(k<=length(searches)){
  searches[[k]]$startdate <- searches[[k]]$interest_over_time$date[1] 
  k <- k+1
}

## download monthly hit volume scores for the same period ##

## this part is necessary if period is greater than 90 days, otherwise skip it ## 

interval <- c(as.character(start.date) , as.character(end.date))  

searchesmonthly <- gtrends(keyword = "Ethereum",
                           time = paste(interval[1] , interval[2]), 
                           low_search_volume = TRUE,
                           gprop = "web")

### normalize daily hit scores ###

monthlyhitscore <- searchesmonthly$interest_over_time$hits/100

k <- 1
hits <- list()

hits[[1]] <- searches[[1]]$interest_over_time$hits*monthlyhitscore[1]
hits.f <- c(hits[[1]])
k <- k+1
while(k<=length(searches)){
  hits[[k]] <- searches[[k]]$interest_over_time$hits*monthlyhitscore[k]
  hits.f <- c(hits.f , hits[[k]])
  k <- k+1
}

dates <- seq(as.Date("2016-01-01") , length = length(hits.f) , by = "days")
hits.f <- as.xts(hits.f , order.by = dates)/max(hits.f)
plot(hits.f)

## write hit data into a csv file
write.zoo(hits.f, file = "data/ETH/google_trends.csv", sep = ",", index.name = "t", col.names = TRUE)

##############################################