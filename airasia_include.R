library(rvest)
library(lubridate)
library(RSelenium)

getCookiesWithExpiry <- function(rd){
    cookies <- rd$getAllCookies()
    #rd$deleteAllCookies()
    #sapply(cookies, "[[", "domain")
    # select cookie with expiry
    selectedcookies <- lapply(cookies, function(e){ if(!is.null(e$expiry)) e })
    # filter qatar cookies
    selectedcookies <- selectedcookies[grep("qatar", sapply(selectedcookies, "[[", "domain"))]
    # remove NULL cookie in the list
    selectedcookies[sapply(selectedcookies, is.null)] <- NULL
    #sapply(selectedcookies, "[[", 'expiry')
    selectedcookies
}

deleteCookies <- function (cookies, rd){
    # get cookie names
    selectedcookienames <- sapply(cookies, "[[", 'name')
    # delete old cookies
    for( cookiename in selectedcookienames) rd$deleteCookieNamed(cookiename)
}

addExpiry <- function (cookies, days = 10){
    # update expiry date, default extend 10 days
    newcookies <- lapply(cookies, function(e){
        e$expiry <- as.integer(as.POSIXct(as.integer(e$expiry), 
                                          origin = "1970-01-01") + days(days))
        e
    })
}


# add cookie with new expiry
# for (c in newcookies){
    #rd$addCookie(name = c$name, value = c$value, domain = c$domain, path = c$path)
# }

#rd$addCookie(name = newcookies[[1]]$name, value = newcookies[[1]]$value, domain = newcookies[[1]]$domain, path = newcookies[[1]]$path)
# 
# selectedcookies <- cookies[grep("qatar", sapply(cookies, "[[", "domain"))]
# 
# selectedcookies <- cookies[grep("expiry", sapply(selectedcookies, "[[", "expiry"))]


# select one way trip
selectOneWayTrip <- function (rd) {
  onewayElem <- rd$findElement(using = 'css selector', value = "#oneWayTrip")
  onewayElem$highlightElement()
  rd$mouseMoveToLocation(webElement = onewayElem)
  onewayElem$click()
  #onewayElem$clickElement()
  Sys.sleep(2)
}

setFromCity <- function (rd, city) {
  ## select from city
  fromElem <- rd$findElement(using = 'css selector', value = '#fromInput')
  fromElem$highlightElement()
  fromElem$sendKeysToElement(list(city))
  Sys.sleep(2)
  fromElem$getElementText()
  
  ## close pop up list
  popup <- rd$findElement(using = 'css selector', value = '#fromFlyoutBody > ol > li.flyout-selector')
  popup$highlightElement()
  f <- popup$getElementText()
  popup$clickElement()
  Sys.sleep(2)
  return(f)
  
}

setToCity <- function (rd, city) {
  ## select to city
  toElem <- rd$findElement(using = 'css selector', value = '#toInput')
  toElem$highlightElement()
  toElem$sendKeysToElement(list(city))
  Sys.sleep(2)
  
  ## close pop up list
  popup <- rd$findElement(using = 'css selector', value = '#toFlyoutBody > ol > li:nth-child(2)')
  popup$highlightElement()
  f <- popup$getElementText()
  popup$clickElement()
  Sys.sleep(2)
  return(f)
}

setDepartDate <- function (rd, d) {
  ## select depart time 28-Jan-2016
  departElem <- rd$findElement('css selector', "#search_from_date")
  #departElem$clearElement()
  #departElem$sendKeysToElement(list(d))
  departElem$highlightElement()
  departElem$clickElement()
  Sys.sleep(2)
  
  #popup <- rd$findElement('css selector',  
  #                        '#ui-datepicker-div > div.ui-datepicker-group.ui-datepicker-group-first > table > tbody > tr:nth-child(1) > td.ui-datepicker-days-cell-over.ui-datepicker-current-day.ui-datepicker-today > a')
  #popup$highlightElement()
  #popup$clickElement()
  #Sys.sleep(2)
}

# select depart time menu item
#dateMenu <- rd$findElement('xpath', '//*[@id="ui-datepicker-div"]/div[4]/table/tbody/tr[2]/td[3]/a')
#dateMenu$clickElement()
#Sys.sleep(2)

submitForm <- function (rd) {
  ## submit the form
  submitElem <- rd$findElement('css selector', "#searchButton")
  submitElem$highlightElement()
  #rd$mouseMoveToLocation(webElement = submitElem)
  #submitElem$click()
  submitElem$clickElement()
}

getFilename <- function (from, to) {
    random_num <- round(abs(rnorm(100)[1]) * 100000, 0)
    ts <- gsub(" ", "_", gsub(":", "-", Sys.time()))
    fname <- paste(from, "-", to, "-", ts, ".txt", sep = "")
}


getPriceInfo <- function (rd, from, to) {
    lf_date <- rd$findElements('css selector', '.low-fare-date')
    lf_price <- rd$findElements('css selector', '.low-fare-price')
    
#     lapply(lf_date, function(e) {e$highlightElement()})
#     lapply(lf_price, function(e) {e$highlightElement()})
    
    flight_date <- unlist(lapply(lf_date, function(e) {e$getElementText()}))
    flight_price <- unlist(lapply(lf_price, function(e) {e$getElementText()}))
    
    check_date <- as.character(today())
    
    temp <- data.frame(from,
                       to,
                       flight_date, 
                       flight_price, 
                       check_date,
                       stringsAsFactors = FALSE, 
                       row.names = NULL)
    print(temp)
    #temp <- gsub(",\n", ", ", temp)
    #temp <- gsub("2016\n", "", temp)
    
    return(temp)
}

nextPage <- function (rd) {
    next_page <- rd$findElement('css selector', '.low-fare-right')
    next_page$highlightElement()
    next_page$clickElement()
}


cheapFlight <- function (df) {
    require(dplyr)
    
    row.names(df) <- NULL
    colnames(df) <- c('no', 'from', 'to', 'flight_date', 'price', 'check_date')
    #str(df)
    #head(df)
    df <- mutate(df, price1 = sub('CNY', '', price))
    df <- mutate(df, price1 = sub(',', '', price1))
    
    df$price1 <- as.numeric(df$price1)
    top10 <- quantile(df$price1, na.rm = TRUE, probs = 0.1)
    filter(df, price1 <= top10)
}
