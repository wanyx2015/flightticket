library(rvest)
library(lubridate)
library(RSelenium)

###
###  Setting parameters
###
fromCity <- c("Shanghai")
toCity <- c("Paris")
departDate <- "28-Jan-2016"
numOfWeeks <- 40
###
###  End of parameters
###



setwd("D:/01.Personal/Coursera/01 Data Science/JHU/flightticket")

source("./qatar_include.R")

startCrawl <- function (fromCity, toCity, departDate, numOfWeeks) {
    #startServer()
    
    Sys.sleep(10)
    
    # connect to server phantomjs
    rd <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "phantomjs") 
    rd$open() 
    rd$setImplicitWaitTimeout(30000)
    rd$maxWindowSize()
    
    # navigate to data request page
    rd$navigate("http://www.qatarairways.com/global/en/homepage.page") 
    
    rd$deleteAllCookies()
    
    # c <- getCookiesWithExpiry(rd)
    # sapply(c, '[[', 'expiry')
    # sapply(c, function(e) as.POSIXct(e$expiry, origin = "1970-01-01"))
    # 
    
    selectOneWayTrip(rd)
    setFromCity(rd, fromCity)
    setToCity(rd, toCity)
    setDepartDate(rd, departDate)
    submitForm(rd)
    
    Sys.sleep(20)
    
    from <- rd$findElement('css selector', '#search_pod')
    from <- from$getElementText()
    
    to <- rd$findElement('css selector', '#search_poa')
    to <- to$getElementText()
    
    
    html_filename = sub(".txt", ".html", getFilename(from, to))
    
    ## retrive the html source and write to disk
    output <- rd$getPageSource(header = TRUE)
    #write(output[[1]], file = html_filename)
    
    result <- paste(from, to, sep = "-")
    
    for (i in 1:numOfWeeks){
        
        temp <- getPriceInfo(rd)
        result <- c(result, temp)
        
        nextPage(rd)
        Sys.sleep(10)
    }
    
    filename = getFilename(from, to)
    write(result, f=filename)
    
    #rd$closeServer()
    
    
}

for(from in fromCity){
    for (to in toCity){
        tryCatch({
            print(paste(from, to))
            startCrawl(from, to, departDate, numOfWeeks)
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
}


