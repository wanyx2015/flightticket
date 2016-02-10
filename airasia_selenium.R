library(rvest)
library(lubridate)
library(RSelenium)

###
###  Setting parameters
###
fromCity <- c("Beijing", 
              "Chengdu",
              "Wuhan", 
              "Hangzhou", 
              "Changsha")
toCity <- c("Chiang Mai",
            "Kuala Lumpur",
            "Sydney", 
            "Bali", 
            "Penang", 
            "Gold Coast",
            "Phuket",
            "Langkawi",
            "Singapore")
departDate <- format(today() + days(1), format="%m/%d/%Y")
numOfWeeks <- 90
###
###  End of parameters
###



setwd("D:/01.Personal/Coursera/01 Data Science/JHU/flightticket")

source("./airasia_include.R")

startCrawl <- function (fromCity, toCity, departDate, numOfWeeks) {
    startServer()
    
    Sys.sleep(10)
    
    # connect to server phantomjs
    rd <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "chrome") 
    rd$open() 
    rd$setImplicitWaitTimeout(30000)
    rd$maxWindowSize()
    
    # navigate to data request page
    rd$navigate("http://www.airasia.com/cn/en/home.page") 
    #rd$navigate('https://booking.airasia.com/Flight/Select?s=True&o1=HGH&d1=KUL&ADT=1&dd1=2016-05-04&mon=true')
    
    rd$deleteAllCookies()
    
    # c <- getCookiesWithExpiry(rd)
    # sapply(c, '[[', 'expiry')
    # sapply(c, function(e) as.POSIXct(e$expiry, origin = "1970-01-01"))
    # 
    
    from <- setFromCity(rd, fromCity)
    to <- setToCity(rd, toCity)
    setDepartDate(rd, departDate)
    selectOneWayTrip(rd)
    
    submitForm(rd)
    
    Sys.sleep(10)
    
    html_filename = sub(".txt", ".html", getFilename(from, to))
    
    result <- data.frame()
    
    for (i in 1:numOfWeeks){
        
        temp <- as.data.frame(getPriceInfo(rd, from, to))
        
        result <- rbind(result, temp)
        result <- unique(result)
        str(result)
        
        nextPage(rd)
        Sys.sleep(5)
    }
    
    filename = getFilename(from, to)
    write.csv(result, file = filename)
    
    rd$closeall()
    rd$quit()
    rd$closeServer()
}

for(from in fromCity){
    for (to in toCity){
        tryCatch({
            print(paste(from, to))
            startCrawl(from, to, departDate, numOfWeeks)
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
}


