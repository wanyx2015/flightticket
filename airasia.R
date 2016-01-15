#data <- read.csv("activity.csv")

library(rvest)
library(lubridate)


###
###  Start of program configuration
###

from <- "PVG"
to <- "DPS"
startday <- "2016-05-13"
startday <- today()
numOfTry <- 5
numOfDays <- 360

###
###  End of program configuration
###


source ("./qatar_include.R")
setwd("D:/01.Personal/Coursera/01 Data Science/JHU/flightticket")

getPrice <- function(url, whichday){
    
    #url <- "https://booking.airasia.com/Flight/Select?s=False&o1=PVG&d1=DPS&ADT=1&dd1=2016-05-13&mon=true"
    airasia <- read_html(url)
    
    multistop <- airasia %>%
        html_nodes(".avail-stops") %>%
        html_text() %>%
        trimws
    
    nextday <- airasia %>%
        html_nodes(".avail-table-next-day") %>%
        html_text() %>%
        trimws
    
    if(length(nextday) >0){
        print("Arrive next day......")
    }
    
    if(length(multistop) >0){
        print("multiple stop......")
    }
    
    
    lowfare <- airasia %>%
        html_nodes(".LF") %>%
        html_nodes(".avail-fare-price") %>%
        html_text() %>%
        trimws()
    
    
    ticket_no <- length(lowfare)
    
    
    flighttime <- airasia %>%
        html_nodes(".avail-table-detail") %>%
        html_nodes(".avail-table-bold") %>%
        html_text() %>%
        trimws()
    
    departtime <- flighttime[1]
    arrivetime <- flighttime[2]
    
    
    d <- airasia %>%
        html_nodes(".avail-table-detail") %>%
        html_nodes("div") %>%
        html_text() %>%
        trimws()
    
    if(length(d) == 0){
        print ("no ticket...")
        return(NULL)
    }
    
    if (length(multistop) >0){
        ifelse(length(nextday)>0, size <- 44, size <- 43)
        idx <- seq(from=1, to=(size * ticket_no), by = size) - 1
        
        
        departtime <- d[idx +2]
        departap <- d[idx + 3]
        
        
        flightno1 <- d[idx +12]
        flightno2 <- d[idx +17]
        
        arrivetime <- d[idx +28]
        arriveap <- d[idx +29]
        
        #nextday <- d[7]
        
        totalhour <- NA
    }else{
        departap <- d[3]
        departtime <- d[2]
        arriveap <- d[6]
        arrivetime <- d[5]
        #nextday <- d[7]
        flightno1 <- d[13]
        flightno2 <- NA
        totalhour <- d[18]
    }
    
    
    #flight <- c(flightno, departap, departtime, arriveap, arrivetime, totalhour, lowfare)
    flight <- data.frame(flightdate = as.character(whichday),
                         flightno1 = flightno1, 
                         flightno2 = flightno2, 
                         depart = departap, 
                         daparttime = departtime, 
                         arriveap = arriveap, 
                         arrivetime = arrivetime, 
                         totalhour = totalhour, 
                         lowfare = lowfare,
                         checkdate = today())
    
    
    print(flight)
    flight
    
}

airasia_airport_list <- function(){
    url <- "http://www.airasia.com/cn/en/home.page?cid=1"
    airasia <- read_html(url)
    ap <- airasia %>% html_nodes("option") %>% html_attrs() %>% trimws
    return(ap[-1])
}







if(from == to) break
newday <- startday
url1 <- "https://booking.airasia.com/Flight/Select?s=False&o1="
url2 <- "&d1="
url3 <- "&ADT=1&dd1="
url4 <- "&mon=true"

# sample URL:  https://booking.airasia.com/Flight/Select?s=False&o1=PVG&d1=DPS&ADT=1&dd1=2016-05-13&mon=true


url <- paste(url1, from, url2, to, url3, sep = "")

f <- data.frame(stringsAsFactors = FALSE)

tryagain <- numOfTry

for (i in 1:numOfDays){
    
    failed <- FALSE
    closeAllConnections()
    newday <- startday + days(i)
    newurl <- paste(paste(url, newday, sep = ""), url4, sep = "")
    print(newurl)
    
    tryCatch({
        result <- getPrice(newurl, newday)
    }, error=function(e){
        failed <- TRUE
        cat("ERROR :",conditionMessage(e), "\n")
    })
    
    
    while(is.null(result)) {
        
        # result is null, so try again...
        print(paste("Failed, try again ...", tryagain))
        
        # number of try used up?
        if(tryagain >0){
            
            tryagain <- tryagain -1
            
            tryCatch({
                result <- getPrice(newurl, newday)
            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})                    
            
            Sys.sleep(5)
        }
        else if (tryagain ==0){
            tryagain <- numOfTry
            break
        }
    }
    
    
    if (!is.null(result)) {
        f <- rbind(f, result)
        tryagain <- numOfTry
    }
    
    if(i == 5 & nrow(f) == 0){
        print ("No flight......")
        break
    }
    
    result <- NULL
    
    Sys.sleep(3)
}

filename <- getFilename(from, to)
if(nrow(f) > 0) write.csv(f, file = filename)

