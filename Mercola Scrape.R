library(tidyverse)
library(dplyr)
library(rvest)
library(RSelenium)
library(robotstxt)

#scrape delay of 3 seconds for occasional system sleep
scrape.delay <- 3

###Create remote driver for firefox browser
driver <- rsDriver(browser="firefox")
remote_driver <- driver[["client"]]
remote_driver$open()

#########################################################################
# Login
#########################################################################
#navigate to Mercola site, login screen
remote_driver$navigate("https://login.mercola.com/Login.aspx?ReturnUrl=https://www.mercola.com")

#send username
username <- remote_driver$findElement(using = "id", value = "ctl00_ContentPlaceHolder1_UserName")
username$clearElement()
username$sendKeysToElement(list("thepineweaver@gmail.com"))

#send password and Enter
passwd <- remote_driver$findElement(using = "id", value = "ctl00_ContentPlaceHolder1_Password")
passwd$clearElement()
passwd$sendKeysToElement(list("IAL620Scrape"))

#click to login
button <- remote_driver$findElement(using = "id", value = "ctl00_ContentPlaceHolder1_LoginButton")
button$clickElement()

###########################################################################
# Get URL's
###########################################################################

#navigate to archived newsletters from Mercola's site
remote_driver$navigate("https://articles.mercola.com/sites/Newsletter/NewsLetter-Archive.aspx")

#create empty vector for url's in loop
scrape.urls <- vector()

#loop to gather url's from multiple years and months using dropdown box for page selection
###########################################################################################
##using option to select years in dropdown box (value = 2020-2008)
for (i in 2008:2020) {
  option.year <- remote_driver$findElement(using = 'xpath', paste("//select[@id='bcr_bcr_bcr_ddlYear']/option[@value=", i, "]", sep=""))
  Sys.sleep(scrape.delay)
  option.year$clickElement()

#using option to select months in dropdown box (value 1=January, etc....)
for (j in 1:12) {
  option.month <- remote_driver$findElement(using = 'xpath', paste("//select[@id='bcr_bcr_bcr_ddlMonth']/option[@value=", j, "]", sep=""))
  Sys.sleep(scrape.delay)
  option.month$clickElement()

  #using selector to get url's
  scrape.list <- remote_driver$findElements(using = 'xpath', value = '//*[(@id = "bcr_bcr_bcr_UpanelMonth")]//a')

for (k in seq_along(scrape.list)) {
  url.add <- scrape.list[[k]]$getElementAttribute('href')
  scrape.urls <- c(scrape.urls, url.add)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}}}

####remove urls that start with mailto
scrape.urls <- scrape.urls[lapply(scrape.urls,function(x) length(grep("mailto",x,value=FALSE))) == 0]

#saving a backup for scraped url's
scrape.urls.tosave <- scrape.urls
#saving a backup file of scraped url's
scrape.urls.tosave<- as.character(scrape.urls.tosave)
scrape.data.tosave <- tibble("url" =scrape.urls.tosave)
write.csv(scrape.data.tosave, 'mercolaurls.csv')

##########################################################################
# Use scrape.urls to get text for 2020 articles
###########################################################################
######open mercolaurls.csv and save to a list
scrape.urls <- vector()
scrape.urls <- read.csv('mercolaurls.csv', header = FALSE, stringsAsFactors=FALSE)[,-1]
scrape.urls <- scrape.urls[-1]

####subset 2020 urls
scrape.urls.2020 <- scrape.urls[8577:9306]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2020 <- vector()
page.date.2020 <- vector()
page.subject.2020 <- vector()

for (i in seq_along(scrape.urls.2020)) {
  new.url.2020 <- (scrape.urls.2020[i])
  remote_driver$navigate(paste(new.url.2020))
  
  #Collects text content from pages
  text.add.2020 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2020 <- paste(text.add.2020, collapse="")
  
  #Collects the date from pages
  date.add.2020 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2020 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2020 <- c(page.text.2020, text.add.2020)
  page.date.2020 <- c(page.date.2020, date.add.2020)
  page.subject.2020 <- c(page.subject.2020, subject.add.2020)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2020 <- as.character(scrape.urls.2020)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2020 <- tibble('url'=scrape.urls.2020, 'date'=page.date.2020,'title'=page.subject.2020 ,'text'=page.text.2020)
scrape.data.2020 <- apply(scrape.data.2020,2,as.character)

write.csv(scrape.data.2020, 'mercola.2020.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2020 <- page.text.2020
#saving a backup file of scraped text
scrape.text.tosave.2020<- as.character(scrape.text.tosave.2020)
scrape.text.tosave.2020 <- tibble(scrape.text.tosave.2020)
write.csv(scrape.text.tosave.2020, 'mercolatext.2020.csv')

#saving a backup for scraped dates
scrape.date.tosave.2020 <- page.date.2020
#saving a backup file of scraped dates
scrape.date.tosave.2020<- as.character(scrape.date.tosave.2020)
scrape.date.tosave.2020 <- tibble(scrape.date.tosave.2020)
write.csv(scrape.date.tosave.2020, 'mercoladate.2020.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2020 <- page.subject.2020
#saving a backup file of scraped titles
scrape.titles.tosave.2020<- as.character(scrape.titles.tosave.2020)
scrape.titles.tosave.2020 <- tibble(scrape.titles.tosave.2020)
write.csv(scrape.titles.tosave.2020, 'mercolatitles.2020.csv')

##########################################################################
# Use scrape.urls to get text for 2019 articles
###########################################################################
####subset 2019 urls
scrape.urls.2019 <- scrape.urls[7545:8576]
head(scrape.urls.2019)
tail(scrape.urls.2019)

#remove broken links and pages without articles (recipes)
scrape.urls.2019 <- scrape.urls.2019[grep("archive", scrape.urls.2019)]
scrape.urls.2019 <- scrape.urls.2019[lapply(scrape.urls.2019,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2019 <- scrape.urls.2019[lapply(scrape.urls.2019,function(x) length(grep("how-to-cook",x,value=FALSE))) == 0]
scrape.urls.2019 <- scrape.urls.2019[lapply(scrape.urls.2019,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2019 <- vector()
page.date.2019 <- vector()
page.subject.2019 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2019)) {
  new.url.2019 <- (scrape.urls.2019[i])
  remote_driver$navigate(paste(new.url.2019))
  
  #Collects text content from pages
  text.add.2019 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2019 <- paste(text.add.2019, collapse="")
  
  #Collects the date from pages
  date.add.2019 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2019 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2019 <- c(page.text.2019, text.add.2019)
  page.date.2019 <- c(page.date.2019, date.add.2019)
  page.subject.2019 <- c(page.subject.2019, subject.add.2019)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2019 <- as.character(scrape.urls.2019)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2019 <- tibble('url'=scrape.urls.2019, 'date'=page.date.2019,'title'=page.subject.2019 ,'text'=page.text.2019)
scrape.data.2019 <- apply(scrape.data.2019,2,as.character)

write.csv(scrape.data.2019, 'mercola.2019.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2019 <- page.text.2019
#saving a backup file of scraped text
scrape.text.tosave.2019<- as.character(scrape.text.tosave.2019)
scrape.text.tosave.2019 <- tibble(scrape.text.tosave.2019)
write.csv(scrape.text.tosave.2019, 'mercolatext.2019.csv')

#saving a backup for scraped dates
scrape.date.tosave.2019 <- page.date.2019
#saving a backup file of scraped dates
scrape.date.tosave.2019<- as.character(scrape.date.tosave.2019)
scrape.date.tosave.2019 <- tibble(scrape.date.tosave.2019)
write.csv(scrape.date.tosave.2019, 'mercoladate.2019.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2019 <- page.subject.2019
#saving a backup file of scraped titles
scrape.titles.tosave.2019<- as.character(scrape.titles.tosave.2019)
scrape.titles.tosave.2019 <- tibble(scrape.titles.tosave.2019)
write.csv(scrape.titles.tosave.2019, 'mercolatitles.2019.csv')

##########################################################################
# Use scrape.urls to get text for 2018 articles
###########################################################################
####subset 2018 urls
scrape.urls.2018 <- scrape.urls[6541:7544]
head(scrape.urls.2018)
tail(scrape.urls.2018)

length(scrape.urls.2018)

#remove broken links and pages without health articles (recipes)
scrape.urls.2018 <- scrape.urls.2018[grep("archive", scrape.urls.2018)]
scrape.urls.2018 <- scrape.urls.2018[lapply(scrape.urls.2018,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2018 <- scrape.urls.2018[lapply(scrape.urls.2018,function(x) length(grep("how-to-cook",x,value=FALSE))) == 0]
scrape.urls.2018 <- scrape.urls.2018[lapply(scrape.urls.2018,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]
scrape.urls.2018 <- scrape.urls.2018[lapply(scrape.urls.2018,function(x) length(grep("Newsletter-Archive",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2018 <- vector()
page.date.2018 <- vector()
page.subject.2018 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2018)) {
  new.url.2018 <- (scrape.urls.2018[i])
  remote_driver$navigate(paste(new.url.2018))
  
  #Collects text content from pages
  text.add.2018 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2018 <- paste(text.add.2018, collapse="")
  
  #Collects the date from pages
  date.add.2018 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2018 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2018 <- c(page.text.2018, text.add.2018)
  page.date.2018 <- c(page.date.2018, date.add.2018)
  page.subject.2018 <- c(page.subject.2018, subject.add.2018)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2018 <- as.character(scrape.urls.2018)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2018 <- tibble('url'=scrape.urls.2018, 'date'=page.date.2018,'title'=page.subject.2018 ,'text'=page.text.2018)
scrape.data.2018 <- apply(scrape.data.2018,2,as.character)

write.csv(scrape.data.2018, 'mercola.2018.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2018 <- page.text.2018
#saving a backup file of scraped text
scrape.text.tosave.2018<- as.character(scrape.text.tosave.2018)
scrape.text.tosave.2018 <- tibble(scrape.text.tosave.2018)
write.csv(scrape.text.tosave.2018, 'mercolatext.2018.csv')

#saving a backup for scraped dates
scrape.date.tosave.2018 <- page.date.2018
#saving a backup file of scraped dates
scrape.date.tosave.2018<- as.character(scrape.date.tosave.2018)
scrape.date.tosave.2018 <- tibble(scrape.date.tosave.2018)
write.csv(scrape.date.tosave.2018, 'mercoladate.2018.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2018 <- page.subject.2018
#saving a backup file of scraped titles
scrape.titles.tosave.2018<- as.character(scrape.titles.tosave.2018)
scrape.titles.tosave.2018 <- tibble(scrape.titles.tosave.2018)
write.csv(scrape.titles.tosave.2018, 'mercolatitles.2018.csv')

##########################################################################
# Use scrape.urls to get text for 2017 articles
###########################################################################
####subset 2017 urls
scrape.urls.2017 <- scrape.urls[5657:6540]
head(scrape.urls.2017)
tail(scrape.urls.2017)

length(scrape.urls.2017)

#remove broken links and pages without health articles (recipes)
scrape.urls.2017 <- scrape.urls.2017[grep("archive", scrape.urls.2017)]
scrape.urls.2017 <- scrape.urls.2017[lapply(scrape.urls.2017,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2017 <- scrape.urls.2017[lapply(scrape.urls.2017,function(x) length(grep("how-to-cook",x,value=FALSE))) == 0]
scrape.urls.2017 <- scrape.urls.2017[lapply(scrape.urls.2017,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]
scrape.urls.2017 <- scrape.urls.2017[lapply(scrape.urls.2017,function(x) length(grep("Newsletter-Archive",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2017 <- vector()
page.date.2017 <- vector()
page.subject.2017 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2017)) {
  new.url.2017 <- (scrape.urls.2017[i])
  remote_driver$navigate(paste(new.url.2017))
  
  #Collects text content from pages
  text.add.2017 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2017 <- paste(text.add.2017, collapse="")
  
  #Collects the date from pages
  date.add.2017 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2017 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  
  page.text.2017 <- c(page.text.2017, text.add.2017)
  page.date.2017 <- c(page.date.2017, date.add.2017)
  page.subject.2017 <- c(page.subject.2017, subject.add.2017)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2017 <- as.character(scrape.urls.2017)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2017 <- tibble('url'=scrape.urls.2017, 'date'=page.date.2017,'title'=page.subject.2017 ,'text'=page.text.2017)
scrape.data.2017 <- apply(scrape.data.2017,2,as.character)

write.csv(scrape.data.2017, 'mercola.2017.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2017 <- page.text.2017
#saving a backup file of scraped text
scrape.text.tosave.2017<- as.character(scrape.text.tosave.2017)
scrape.text.tosave.2017 <- tibble(scrape.text.tosave.2017)
write.csv(scrape.text.tosave.2017, 'mercolatext.2017.csv')

#saving a backup for scraped dates
scrape.date.tosave.2017 <- page.date.2017
#saving a backup file of scraped dates
scrape.date.tosave.2017<- as.character(scrape.date.tosave.2017)
scrape.date.tosave.2017 <- tibble(scrape.date.tosave.2017)
write.csv(scrape.date.tosave.2017, 'mercoladate.2017.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2017 <- page.subject.2017
#saving a backup file of scraped titles
scrape.titles.tosave.2017<- as.character(scrape.titles.tosave.2017)
scrape.titles.tosave.2017 <- tibble(scrape.titles.tosave.2017)
write.csv(scrape.titles.tosave.2017, 'mercolatitles.2017.csv')

##########################################################################
# Use scrape.urls to get text for 2016 articles
###########################################################################
####subset 2016 urls
scrape.urls.2016 <- scrape.urls[4827:5656]
head(scrape.urls.2016)
tail(scrape.urls.2016)

length(scrape.urls.2016)

#remove broken links and pages without health articles (recipes)
scrape.urls.2016 <- scrape.urls.2016[grep("archive", scrape.urls.2016)]
scrape.urls.2016 <- scrape.urls.2016[lapply(scrape.urls.2016,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2016 <- scrape.urls.2016[lapply(scrape.urls.2016,function(x) length(grep("how-to-cook",x,value=FALSE))) == 0]
scrape.urls.2016 <- scrape.urls.2016[lapply(scrape.urls.2016,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]
scrape.urls.2016 <- scrape.urls.2016[lapply(scrape.urls.2016,function(x) length(grep("Newsletter-Archive",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2016 <- vector()
page.date.2016 <- vector()
page.subject.2016 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2016)) {
  new.url.2016 <- (scrape.urls.2016[i])
  remote_driver$navigate(paste(new.url.2016))
  
  #Collects text content from pages
  text.add.2016 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2016 <- paste(text.add.2016, collapse="")
  
  #Collects the date from pages
  date.add.2016 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2016 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2016 <- c(page.text.2016, text.add.2016)
  page.date.2016 <- c(page.date.2016, date.add.2016)
  page.subject.2016 <- c(page.subject.2016, subject.add.2016)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2016 <- as.character(scrape.urls.2016)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2016 <- tibble('url'=scrape.urls.2016, 'date'=page.date.2016,'title'=page.subject.2016 ,'text'=page.text.2016)
scrape.data.2016 <- apply(scrape.data.2016,2,as.character)

write.csv(scrape.data.2016, 'mercola.2016.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2016 <- page.text.2016
#saving a backup file of scraped text
scrape.text.tosave.2016<- as.character(scrape.text.tosave.2016)
scrape.text.tosave.2016 <- tibble(scrape.text.tosave.2016)
write.csv(scrape.text.tosave.2016, 'mercolatext.2016.csv')

#saving a backup for scraped dates
scrape.date.tosave.2016 <- page.date.2016
#saving a backup file of scraped dates
scrape.date.tosave.2016<- as.character(scrape.date.tosave.2016)
scrape.date.tosave.2016 <- tibble(scrape.date.tosave.2016)
write.csv(scrape.date.tosave.2016, 'mercoladate.2016.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2016 <- page.subject.2016
#saving a backup file of scraped titles
scrape.titles.tosave.2016<- as.character(scrape.titles.tosave.2016)
scrape.titles.tosave.2016 <- tibble(scrape.titles.tosave.2016)
write.csv(scrape.titles.tosave.2016, 'mercolatitles.2016.csv')

##########################################################################
# Use scrape.urls to get text for 2015 articles
###########################################################################
####subset 2015 urls
scrape.urls.2015 <- scrape.urls[4036:4826]
head(scrape.urls.2015)
tail(scrape.urls.2015)

length(scrape.urls.2015)

#remove broken links and pages without health articles (recipes)
scrape.urls.2015 <- scrape.urls.2015[grep("archive", scrape.urls.2015)]
scrape.urls.2015 <- scrape.urls.2015[lapply(scrape.urls.2015,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2015 <- scrape.urls.2015[lapply(scrape.urls.2015,function(x) length(grep("how-to-cook",x,value=FALSE))) == 0]
scrape.urls.2015 <- scrape.urls.2015[lapply(scrape.urls.2015,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]
scrape.urls.2015 <- scrape.urls.2015[lapply(scrape.urls.2015,function(x) length(grep("Newsletter-Archive",x,value=FALSE))) == 0]
scrape.urls.2015 <- scrape.urls.2015[lapply(scrape.urls.2015,function(x) length(grep("10-workout-mistakes",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2015 <- vector()
page.date.2015 <- vector()
page.subject.2015 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2015)) {
  new.url.2015 <- (scrape.urls.2015[i])
  remote_driver$navigate(paste(new.url.2015))
  
  #Collects text content from pages
  text.add.2015 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2015 <- paste(text.add.2015, collapse="")
  
  #Collects the date from pages
  date.add.2015 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2015 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2015 <- c(page.text.2015, text.add.2015)
  page.date.2015 <- c(page.date.2015, date.add.2015)
  page.subject.2015 <- c(page.subject.2015, subject.add.2015)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2015 <- as.character(scrape.urls.2015)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2015 <- tibble('url'=scrape.urls.2015, 'date'=page.date.2015,'title'=page.subject.2015 ,'text'=page.text.2015)
scrape.data.2015 <- apply(scrape.data.2015,2,as.character)

write.csv(scrape.data.2015, 'mercola.2015.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2015 <- page.text.2015
#saving a backup file of scraped text
scrape.text.tosave.2015<- as.character(scrape.text.tosave.2015)
scrape.text.tosave.2015 <- tibble(scrape.text.tosave.2015)
write.csv(scrape.text.tosave.2015, 'mercolatext.2015.csv')

#saving a backup for scraped dates
scrape.date.tosave.2015 <- page.date.2015
#saving a backup file of scraped dates
scrape.date.tosave.2015<- as.character(scrape.date.tosave.2015)
scrape.date.tosave.2015 <- tibble(scrape.date.tosave.2015)
write.csv(scrape.date.tosave.2015, 'mercoladate.2015.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2015 <- page.subject.2015
#saving a backup file of scraped titles
scrape.titles.tosave.2015<- as.character(scrape.titles.tosave.2015)
scrape.titles.tosave.2015 <- tibble(scrape.titles.tosave.2015)
write.csv(scrape.titles.tosave.2015, 'mercolatitles.2015.csv')

##########################################################################
# Use scrape.urls to get text for 2014 articles
###########################################################################
####subset 2014 urls
scrape.urls.2014 <- scrape.urls[3365:4035]
head(scrape.urls.2014)
tail(scrape.urls.2014)

length(scrape.urls.2014)

#remove broken links and pages without health articles (recipes)
scrape.urls.2014 <- scrape.urls.2014[grep("archive", scrape.urls.2014)]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("how-to-cook",x,value=FALSE))) == 0]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("Newsletter-Archive",x,value=FALSE))) == 0]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("5-bad-exercises",x,value=FALSE))) == 0]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("physical-activity-bone",x,value=FALSE))) == 0]
scrape.urls.2014 <- scrape.urls.2014[lapply(scrape.urls.2014,function(x) length(grep("cellulite-removal-power",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2014 <- vector()
page.date.2014 <- vector()
page.subject.2014 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2014)) {
  new.url.2014 <- (scrape.urls.2014[i])
  remote_driver$navigate(paste(new.url.2014))
  
  #Collects text content from pages
  text.add.2014 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2014 <- paste(text.add.2014, collapse="")
  
  #Collects the date from pages
  date.add.2014 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2014 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2014 <- c(page.text.2014, text.add.2014)
  page.date.2014 <- c(page.date.2014, date.add.2014)
  page.subject.2014 <- c(page.subject.2014, subject.add.2014)
 
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}
tail(page.date.2014)
scrape.urls.2014 <- as.character(scrape.urls.2014)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2014 <- tibble('url'=scrape.urls.2014, 'date'=page.date.2014,'title'=page.subject.2014 ,'text'=page.text.2014)
scrape.data.2014 <- apply(scrape.data.2014,2,as.character)

write.csv(scrape.data.2014, 'mercola.2014.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2014 <- page.text.2014
#saving a backup file of scraped text
scrape.text.tosave.2014<- as.character(scrape.text.tosave.2014)
scrape.text.tosave.2014 <- tibble(scrape.text.tosave.2014)
write.csv(scrape.text.tosave.2014, 'mercolatext.2014.csv')

#saving a backup for scraped dates
scrape.date.tosave.2014 <- page.date.2014
#saving a backup file of scraped dates
scrape.date.tosave.2014<- as.character(scrape.date.tosave.2014)
scrape.date.tosave.2014 <- tibble(scrape.date.tosave.2014)
write.csv(scrape.date.tosave.2014, 'mercoladate.2014.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2014 <- page.subject.2014
#saving a backup file of scraped titles
scrape.titles.tosave.2014<- as.character(scrape.titles.tosave.2014)
scrape.titles.tosave.2014 <- tibble(scrape.titles.tosave.2014)
write.csv(scrape.titles.tosave.2014, 'mercolatitles.2014.csv')

##########################################################################
# Use scrape.urls to get text for 2013 articles
###########################################################################
####subset 2013 urls
scrape.urls.2013 <- scrape.urls[2703:3364]
head(scrape.urls.2013)
tail(scrape.urls.2013)

length(scrape.urls.2013)

#remove broken links
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("whole-body-vibration-training",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("water-fluoridation-facts",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("cellulite-elimination",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("post-workout-muscle-soreness",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("grounding-sitting-health-effects",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("2013/06/21/testosterone-levels",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("squat-acceleration-training",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("exercise-may-prevent-heart-disease",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("watermelon-for-muscle-soreness",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("low-testosterone",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("stroke-prevention-vigorous-exercise",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("squat-exercises",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("foam-roller-exercises",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("sitting-standing-up",x,value=FALSE))) == 0]
scrape.urls.2013 <- scrape.urls.2013[lapply(scrape.urls.2013,function(x) length(grep("/the-greater-good",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2013 <- vector()
page.date.2013 <- vector()
page.subject.2013 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2013)) {
  new.url.2013 <- (scrape.urls.2013[i])
  remote_driver$navigate(paste(new.url.2013))
  
  #Collects text content from pages
  text.add.2013 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2013 <- paste(text.add.2013, collapse="")
  
  #Collects the date from pages
  date.add.2013 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2013 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2013 <- c(page.text.2013, text.add.2013)
  page.date.2013 <- c(page.date.2013, date.add.2013)
  page.subject.2013 <- c(page.subject.2013, subject.add.2013)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2013 <- as.character(scrape.urls.2013)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2013 <- tibble('url'=scrape.urls.2013, 'date'=page.date.2013,'title'=page.subject.2013 ,'text'=page.text.2013)
scrape.data.2013 <- apply(scrape.data.2013,2,as.character)

write.csv(scrape.data.2013, 'mercola.2013.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2013 <- page.text.2013
#saving a backup file of scraped text
scrape.text.tosave.2013<- as.character(scrape.text.tosave.2013)
scrape.text.tosave.2013 <- tibble(scrape.text.tosave.2013)
write.csv(scrape.text.tosave.2013, 'mercolatext.2013.csv')

#saving a backup for scraped dates
scrape.date.tosave.2013 <- page.date.2013
#saving a backup file of scraped dates
scrape.date.tosave.2013<- as.character(scrape.date.tosave.2013)
scrape.date.tosave.2013 <- tibble(scrape.date.tosave.2013)
write.csv(scrape.date.tosave.2013, 'mercoladate.2013.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2013 <- page.subject.2013
#saving a backup file of scraped titles
scrape.titles.tosave.2013<- as.character(scrape.titles.tosave.2013)
scrape.titles.tosave.2013 <- tibble(scrape.titles.tosave.2013)
write.csv(scrape.titles.tosave.2013, 'mercolatitles.2013.csv')

##########################################################################
# Use scrape.urls to get text for 2012 articles
###########################################################################
####subset 2012 urls
scrape.urls.2012 <- scrape.urls[2054:2702]
head(scrape.urls.2012)
tail(scrape.urls.2012)

length(scrape.urls.2012)

#remove broken links and pages without health articles (recipes)
scrape.urls.2012 <- scrape.urls.2012[grep("archive", scrape.urls.2012)]
scrape.urls.2012 <- scrape.urls.2012[lapply(scrape.urls.2012,function(x) length(grep("ibuprofen-use",x,value=FALSE))) == 0]
scrape.urls.2012 <- scrape.urls.2012[lapply(scrape.urls.2012,function(x) length(grep("power-plate",x,value=FALSE))) == 0]
scrape.urls.2012 <- scrape.urls.2012[lapply(scrape.urls.2012,function(x) length(grep("cold-water-immersion",x,value=FALSE))) == 0]
scrape.urls.2012 <- scrape.urls.2012[lapply(scrape.urls.2012,function(x) length(grep("magnesium-benefits",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2012 <- vector()
page.date.2012 <- vector()
page.subject.2012 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2012)) {
  new.url.2012 <- (scrape.urls.2012[i])
  remote_driver$navigate(paste(new.url.2012))
  
  #Collects text content from pages
  text.add.2012 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2012 <- paste(text.add.2012, collapse="")
  
  #Collects the date from pages
  date.add.2012 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2012 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2012 <- c(page.text.2012, text.add.2012)
  page.date.2012 <- c(page.date.2012, date.add.2012)
  page.subject.2012 <- c(page.subject.2012, subject.add.2012)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2012 <- as.character(scrape.urls.2012)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2012 <- tibble('url'=scrape.urls.2012, 'date'=page.date.2012,'title'=page.subject.2012 ,'text'=page.text.2012)
scrape.data.2012 <- apply(scrape.data.2012,2,as.character)

write.csv(scrape.data.2012, 'mercola.2012.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2012 <- page.text.2012
#saving a backup file of scraped text
scrape.text.tosave.2012<- as.character(scrape.text.tosave.2012)
scrape.text.tosave.2012 <- tibble(scrape.text.tosave.2012)
write.csv(scrape.text.tosave.2012, 'mercolatext.2012.csv')

#saving a backup for scraped dates
scrape.date.tosave.2012 <- page.date.2012
#saving a backup file of scraped dates
scrape.date.tosave.2012<- as.character(scrape.date.tosave.2012)
scrape.date.tosave.2012 <- tibble(scrape.date.tosave.2012)
write.csv(scrape.date.tosave.2012, 'mercoladate.2012.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2012 <- page.subject.2012
#saving a backup file of scraped titles
scrape.titles.tosave.2012<- as.character(scrape.titles.tosave.2012)
scrape.titles.tosave.2012 <- tibble(scrape.titles.tosave.2012)
write.csv(scrape.titles.tosave.2012, 'mercolatitles.2012.csv')

##########################################################################
# Use scrape.urls to get text for 2011 articles
###########################################################################
####subset 2011 urls
scrape.urls.2011 <- scrape.urls[1267:2053]
head(scrape.urls.2011)
tail(scrape.urls.2011)

length(scrape.urls.2011)

#remove broken links and pages without health articles (recipes)
scrape.urls.2011 <- scrape.urls.2011[grep("archive", scrape.urls.2011)]
scrape.urls.2011 <- scrape.urls.2011[lapply(scrape.urls.2011,function(x) length(grep("recipe",x,value=FALSE))) == 0]
scrape.urls.2011 <- scrape.urls.2011[lapply(scrape.urls.2011,function(x) length(grep("how-to-make",x,value=FALSE))) == 0]
scrape.urls.2011 <- scrape.urls.2011[lapply(scrape.urls.2011,function(x) length(grep("why-muscles-are-sore-after-workouts",x,value=FALSE))) == 0]
scrape.urls.2011 <- scrape.urls.2011[lapply(scrape.urls.2011,function(x) length(grep("you-are-never-too-old-to-start-exercising",x,value=FALSE))) == 0]
scrape.urls.2011 <- scrape.urls.2011[lapply(scrape.urls.2011,function(x) length(grep("power-plate",x,value=FALSE))) == 0]
scrape.urls.2011 <- scrape.urls.2011[lapply(scrape.urls.2011,function(x) length(grep("thats-one-way-to-come-down-the-stairs",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2011 <- vector()
page.date.2011 <- vector()
page.subject.2011 <- vector()

scrape.urls.2011.mod <- scrape.urls.2011[447:778]

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2011.mod)) {
  new.url.2011 <- (scrape.urls.2011.mod[i])
  remote_driver$navigate(paste(new.url.2011))
  
  #Collects text content from pages
  text.add.2011 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2011 <- paste(text.add.2011, collapse="")
  
  #Collects the date from pages
  date.add.2011 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2011 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2011 <- c(page.text.2011, text.add.2011)
  page.date.2011 <- c(page.date.2011, date.add.2011)
  page.subject.2011 <- c(page.subject.2011, subject.add.2011)

  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2011 <- as.character(scrape.urls.2011)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2011 <- tibble('url'=scrape.urls.2011, 'date'=page.date.2011,'title'=page.subject.2011 ,'text'=page.text.2011)
scrape.data.2011 <- apply(scrape.data.2011,2,as.character)

write.csv(scrape.data.2011, 'mercola.2011.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2011 <- page.text.2011
#saving a backup file of scraped text
scrape.text.tosave.2011<- as.character(scrape.text.tosave.2011)
scrape.text.tosave.2011 <- tibble(scrape.text.tosave.2011)
write.csv(scrape.text.tosave.2011, 'mercolatext.2011.csv')

#saving a backup for scraped dates
scrape.date.tosave.2011 <- page.date.2011
#saving a backup file of scraped dates
scrape.date.tosave.2011<- as.character(scrape.date.tosave.2011)
scrape.date.tosave.2011 <- tibble(scrape.date.tosave.2011)
write.csv(scrape.date.tosave.2011, 'mercoladate.2011.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2011 <- page.subject.2011
#saving a backup file of scraped titles
scrape.titles.tosave.2011<- as.character(scrape.titles.tosave.2011)
scrape.titles.tosave.2011 <- tibble(scrape.titles.tosave.2011)
write.csv(scrape.titles.tosave.2011, 'mercolatitles.2011.csv')

##########################################################################
# Use scrape.urls to get text for 2010 articles
###########################################################################
####subset 2010 urls
scrape.urls.2010 <- scrape.urls[670:1266]
head(scrape.urls.2010)
tail(scrape.urls.2010)

length(scrape.urls.2010)

#remove broken links and pages without health articles (recipes)
scrape.urls.2010 <- scrape.urls.2010[grep("archive", scrape.urls.2010)]
scrape.urls.2010 <- scrape.urls.2010[lapply(scrape.urls.2010,function(x) length(grep("the-truth-about-statin-drugs-revealed",x,value=FALSE))) == 0]
scrape.urls.2010 <- scrape.urls.2010[lapply(scrape.urls.2010,function(x) length(grep("everyone-needs-to-hear-this-more-often",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2010 <- vector()
page.date.2010 <- vector()
page.subject.2010 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2010.mod)) {
  new.url.2010 <- (scrape.urls.2010.mod[i])
  remote_driver$navigate(paste(new.url.2010))
  
  #Collects text content from pages
  text.add.2010 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2010 <- paste(text.add.2010, collapse="")
  
  #Collects the date from pages
  date.add.2010 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2010 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2010 <- c(page.text.2010, text.add.2010)
  page.date.2010 <- c(page.date.2010, date.add.2010)
  page.subject.2010 <- c(page.subject.2010, subject.add.2010)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2010 <- as.character(scrape.urls.2010)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2010 <- tibble('url'=scrape.urls.2010, 'date'=page.date.2010,'title'=page.subject.2010 ,'text'=page.text.2010)
scrape.data.2010 <- apply(scrape.data.2010,2,as.character)

write.csv(scrape.data.2010, 'mercola.2010.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2010 <- page.text.2010
#saving a backup file of scraped text
scrape.text.tosave.2010<- as.character(scrape.text.tosave.2010)
scrape.text.tosave.2010 <- tibble(scrape.text.tosave.2010)
write.csv(scrape.text.tosave.2010, 'mercolatext.2010.csv')

#saving a backup for scraped dates
scrape.date.tosave.2010 <- page.date.2010
#saving a backup file of scraped dates
scrape.date.tosave.2010<- as.character(scrape.date.tosave.2010)
scrape.date.tosave.2010 <- tibble(scrape.date.tosave.2010)
write.csv(scrape.date.tosave.2010, 'mercoladate.2010.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2010 <- page.subject.2010
#saving a backup file of scraped titles
scrape.titles.tosave.2010<- as.character(scrape.titles.tosave.2010)
scrape.titles.tosave.2010 <- tibble(scrape.titles.tosave.2010)
write.csv(scrape.titles.tosave.2010, 'mercolatitles.2010.csv')

##########################################################################
# Use scrape.urls to get text for 2009 articles
###########################################################################
####subset 2009 urls
scrape.urls.2009 <- scrape.urls[439:669]
head(scrape.urls.2009)
tail(scrape.urls.2009)

length(scrape.urls.2009)

#remove broken links and pages without health articles (recipes)
scrape.urls.2009 <- scrape.urls.2009[grep("archive", scrape.urls.2009)]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2009 <- vector()
page.date.2009 <- vector()
page.subject.2009 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2009)) {
  new.url.2009 <- (scrape.urls.2009[i])
  remote_driver$navigate(paste(new.url.2009))
  
  #Collects text content from pages
  text.add.2009 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2009 <- paste(text.add.2009, collapse="")
  
  #Collects the date from pages
  date.add.2009 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2009 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2009 <- c(page.text.2009, text.add.2009)
  page.date.2009 <- c(page.date.2009, date.add.2009)
  page.subject.2009 <- c(page.subject.2009, subject.add.2009)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2009 <- as.character(scrape.urls.2009)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2009 <- tibble('url'=scrape.urls.2009, 'date'=page.date.2009,'title'=page.subject.2009,'text'=page.text.2009)
scrape.data.2009 <- apply(scrape.data.2009,2,as.character)

write.csv(scrape.data.2009, 'mercola.2009.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2009 <- page.text.2009
#saving a backup file of scraped text
scrape.text.tosave.2009<- as.character(scrape.text.tosave.2009)
scrape.text.tosave.2009 <- tibble(scrape.text.tosave.2009)
write.csv(scrape.text.tosave.2009, 'mercolatext.2009.csv')

#saving a backup for scraped dates
scrape.date.tosave.2009 <- page.date.2009
#saving a backup file of scraped dates
scrape.date.tosave.2009<- as.character(scrape.date.tosave.2009)
scrape.date.tosave.2009 <- tibble(scrape.date.tosave.2009)
write.csv(scrape.date.tosave.2009, 'mercoladate.2009.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2009 <- page.subject.2009
#saving a backup file of scraped titles
scrape.titles.tosave.2009<- as.character(scrape.titles.tosave.2009)
scrape.titles.tosave.2009 <- tibble(scrape.titles.tosave.2009)
write.csv(scrape.titles.tosave.2009, 'mercolatitles.2009.csv')

##########################################################################
# Use scrape.urls to get text for 2008 articles
###########################################################################
####subset 2008 urls
scrape.urls.2008 <- scrape.urls[1:438]
head(scrape.urls.2008)
tail(scrape.urls.2008)

length(scrape.urls.2008)

#remove broken links and pages without health articles (recipes)
scrape.urls.2008 <- scrape.urls.2008[grep("archive", scrape.urls.2008)]
scrape.urls.2008 <- scrape.urls.2008[lapply(scrape.urls.2008,function(x) length(grep("the-truth-about-statin-drugs-revealed",x,value=FALSE))) == 0]

# Creates an empty vector that will be filled data by the 'for loop' below
page.text.2008 <- vector()
page.date.2008 <- vector()
page.subject.2008 <- vector()

#scrape pages for text, date and titles
for (i in seq_along(scrape.urls.2008)) {
  new.url.2008 <- (scrape.urls.2008[i])
  remote_driver$navigate(paste(new.url.2008))
  
  #Collects text content from pages
  text.add.2008 <- remote_driver$findElement(using='css selector',"#bcr_FormattedBody")$getElementText()
  
  #Collapses all the separate <p> text content into one string of text
  text.add.2008 <- paste(text.add.2008, collapse="")
  
  #Collects the date from pages
  date.add.2008 <- remote_driver$findElement(using='css selector',"#bcr_PostDate")$getElementText()
  
  #Collects the title from pages
  subject.add.2008 <- remote_driver$findElement(using='css selector',"#bcr_lblSubject")$getElementText()
  
  page.text.2008 <- c(page.text.2008, text.add.2008)
  page.date.2008 <- c(page.date.2008, date.add.2008)
  page.subject.2008 <- c(page.subject.2008, subject.add.2008)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay) 
}

scrape.urls.2008 <- as.character(scrape.urls.2008)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data.2008 <- tibble('url'=scrape.urls.2008, 'date'=page.date.2008,'title'=page.subject.2008,'text'=page.text.2008)
scrape.data.2008 <- apply(scrape.data.2008,2,as.character)

write.csv(scrape.data.2008, 'mercola.2008.csv')

###Save backups

#saving a backup for scraped text
scrape.text.tosave.2008 <- page.text.2008
#saving a backup file of scraped text
scrape.text.tosave.2008<- as.character(scrape.text.tosave.2008)
scrape.text.tosave.2008 <- tibble(scrape.text.tosave.2008)
write.csv(scrape.text.tosave.2008, 'mercolatext.2008.csv')

#saving a backup for scraped dates
scrape.date.tosave.2008 <- page.date.2008
#saving a backup file of scraped dates
scrape.date.tosave.2008<- as.character(scrape.date.tosave.2008)
scrape.date.tosave.2008 <- tibble(scrape.date.tosave.2008)
write.csv(scrape.date.tosave.2008, 'mercoladate.2008.csv')

#saving a backup for scraped titles
scrape.titles.tosave.2008 <- page.subject.2008
#saving a backup file of scraped titles
scrape.titles.tosave.2008<- as.character(scrape.titles.tosave.2008)
scrape.titles.tosave.2008 <- tibble(scrape.titles.tosave.2008)
write.csv(scrape.titles.tosave.2008, 'mercolatitles.2008.csv')

#######################################################################################################
####end selenium connection
remote_driver$close()


