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
  onewayElem <- rd$findElement(using = 'css selector', value = "#joneway")
  onewayElem$highlightElement()
  rd$mouseMoveToLocation(webElement = onewayElem)
  onewayElem$click()
  #onewayElem$clickElement()
  Sys.sleep(2)
}

setFromCity <- function (rd, city) {
  ## select from city
  xpath <- '//*[@id="FromTemp"]'
  fromElem <- rd$findElement(using = 'xpath', value = xpath)
  fromElem$highlightElement()
  fromElem$sendKeysToElement(list(city))
  Sys.sleep(2)
  
  # select pop up menu
  fromMenu <- rd$findElement('css selector', "#ui-active-menuitem")
  fromMenu$highlightElement()
  fromMenu$clickElement()
  Sys.sleep(3)
}

setToCity <- function (rd, city) {
  ## select to city
  xpath <- '//*[@id="ToTemp"]'
  toElem <- rd$findElement(using = 'xpath', value = xpath)
  toElem$highlightElement()
  toElem$sendKeysToElement(list(city))
  Sys.sleep(2)
  
  # select pop up menu
  toMenu <- rd$findElement('css selector', "#ui-active-menuitem")
  toMenu$highlightElement()
  toMenu$clickElement()
  Sys.sleep(2)
}

setDepartDate <- function (rd, d) {
  ## select depart time 28-Jan-2016
  departElem <- rd$findElement('css selector', "#departing")
  departElem$clearElement()
  departElem$sendKeysToElement(list(d))
  #departElem$clickElement()
  Sys.sleep(2)
}

# select depart time menu item
#dateMenu <- rd$findElement('xpath', '//*[@id="ui-datepicker-div"]/div[4]/table/tbody/tr[2]/td[3]/a')
#dateMenu$clickElement()
#Sys.sleep(2)

submitForm <- function (rd) {
  ## submit the form
  submitElem <- rd$findElement('css selector', "#bookFlight")
  submitElem$highlightElement()
  rd$mouseMoveToLocation(webElement = submitElem)
  submitElem$click()
  #submitElem$clickElement()
}

getFilename <- function (from, to) {
    random_num <- round(abs(rnorm(100)[1]) * 100000, 0)
    ts <- gsub(" ", "_", gsub(":", "-", Sys.time()))
    fname <- paste(from, "-", to, "-", ts, ".txt", sep = "")
}


getPriceInfo <- function (rd, result) {
    p <- rd$findElements('css selector', '.w-nav-item')
    
    temp <- sapply(p, function(e) e$getElementText())
    temp <- gsub(",\n", ", ", temp)
    temp <- gsub("2016\n", "", temp)
    
    return(temp)
}

nextPage <- function (rd) {
    page1 <- '//*[@id="boundRepeat:0:calRepeat:0:weeklynavigator"]'
    page2 <- '//*[@id="boundRepeat:0:calRepeat:1:weeklynavigator"]'
    page3 <- '//*[@id="boundRepeat:0:calRepeat:2:weeklynavigator"]'
    page4 <- '//*[@id="boundRepeat:0:calRepeat:3:weeklynavigator"]'
    page5 <- '//*[@id="boundRepeat:0:calRepeat:4:weeklynavigator"]'
    page6 <- '//*[@id="boundRepeat:0:calRepeat:5:weeklynavigator"]'
    page7 <- '//*[@id="boundRepeat:0:calRepeat:6:weeklynavigator"]'
    nextPage <- '//*[@id="boundRepeat:0:next_RT"]'
    
    pages <- c(page1, page2, page3, page4, page5, page6, page7)
    
    #page <- rd$findElements("xpath", page7)
    
    #rd$refresh()
    #Sys.sleep(20)
    
    page <- rd$findElement('xpath', nextPage)
    page$highlightElement()
    
    rd$mouseMoveToLocation(webElement = page)
    page$click()
    #page$clickElement()
}




# economy promo
#OB_0_EPPRNAR2

# economy saver
#OB_0_ECRRNAR2

# economy value
#OB_0_ECSFNAR2

# economy flexi
#OB_0_ECFFNAR2

# business promo
#OB_0_BPPRNAR2

# business saver
#OB_0_BURSNAR2