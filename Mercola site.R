library(tidyverse)
library(dplyr)
library(rvest)
library(RSelenium)
library(robotstxt)

scrape.delay <- 3


driver <- rsDriver(browser="firefox")
remote_driver <- driver[["client"]]
remote_driver$open()

#########################################################################
# Login
#########################################################################
remote_driver$navigate("https://login.mercola.com/Login.aspx?ReturnUrl=https://www.mercola.com")

#send username
username <- remote_driver$findElement(using = "id", value = "ctl00_ContentPlaceHolder1_UserName")
username$clearElement()
username$sendKeysToElement(list("thepineweaver@gmail.com"))

#send password and Enter
passwd <- remote_driver$findElement(using = "id", value = "ctl00_ContentPlaceHolder1_Password")
passwd$clearElement()
passwd$sendKeysToElement(list("IAL620Scrape"))

button <- remote_driver$findElement(using = "id", value = "ctl00_ContentPlaceHolder1_LoginButton")
button$clickElement()

###########################################################################
# Get URL's
###########################################################################

#archived newsletters from Mercola's site
remote_driver$navigate("https://articles.mercola.com/sites/Newsletter/NewsLetter-Archive.aspx")

#create empty vector for url's in loop
scrape.urls <- vector()



###using option to select months in dropdown box
for (i in 1:11){
  option <- remote_driver$findElement(using = 'xpath', "//select[@id='bcr_bcr_bcr_ddlMonth']/option[@value='i']")
  option$clickElement()
  
  #using selector to get url's
  scrape.list <- remote_driver$findElements(using = 'xpath', value = '//*[(@id = "bcr_bcr_bcr_UpanelMonth")]//a')
  

for (j in seq_along(scrape.list)) {
  url.add <- scrape.list[[j]]$getElementAttribute('href')
  scrape.urls <- c(scrape.urls, url.add)
}}

scrape.urls <- scrape.urls[-length(scrape.urls)]


##########################################################################
# Use scrape.urls to get text
###########################################################################
# Creates an empty vector that will be filled data by the 'for loop' below
page.text <- vector()
page.date <- vector()

for (i in seq_along(scrape.urls)) {
  new.url <- (scrape.urls[i])
  remote_driver$navigate(paste(new.url))
  
  #Collects text content from pages
  text.add <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody ")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add <- paste(text.add, collapse="")
  
  #Collects the date from pages
  date.add <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()

  page.text <- c(page.text, text.add)
  page.date <- c(page.date, date.add)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls <- as.character(scrape.urls)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data <- tibble('url'=scrape.urls, 'date'=page.date, 'text'=page.text)
scrape.data <- apply(scrape.data,2,as.character)
write.csv(scrape.data, 'mercola.csv')
