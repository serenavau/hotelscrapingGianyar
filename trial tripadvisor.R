library(rvest)
library(purrr)

gianyar <- read_html("https://www.tripadvisor.com/Hotel_Review-g297701-d7022088-Reviews-The_Kayon_Resort-Ubud_Gianyar_Bali.html")

gianyar <- read_html("https://www.tripadvisor.com/Hotel_Review-g297701-d7022088-Reviews-or{}-The_Kayon_Resort-Ubud_Gianyar_Bali.html#REVIEWS")


totalReviews <- gianyar %>%
  html_nodes(".hotels-hotel-review-overview-Reviews__seeAllReviews--2kGem")%>%
  html_text()

totalReviews <-strsplit(totalReviews, " ")[[1]][1]
totalReviews<-as.numeric(gsub(",", "", totalReviews))
totalReviews

map_df(1:270, function(i){
  
  page <- read_html(sprintf(gianyar, i))
  
  data.frame(pageNO = html_text(html_nodes(page, ".pageNum")),Heading = html_text(html_nodes(page, ".noQUotes")),
            Author = html_text(html_nodes(page, ".__resizeWatch")),
            UserLoc = html_text(html_nodes(page, ".userLoc")),
            Rev = html_text(html_nodes(page, ".review-container"))
  )
}) -> TripAdvisorGianyar

reviews <- gianyar %>%
  html_nodes("#REVIEWS .partial_entry")%>%
  html_text()
reviews

## Using Selenium to scrape

library(RSelenium)
library(xml2)
library(tidyverse)
library(rvest)

# docker run -d -p 4445:4444 selenium/standalone-chrome
# access browser after setup virtual machine in docker
remDr <- RSelenium::remoteDriver(remoteServerAddr = "192.168.99.100",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

page <- "https://www.tripadvisor.com/Hotel_Review-g297701-d7022088-Reviews-or{}-The_Kayon_Resort-Ubud_Gianyar_Bali.html#REVIEWS"

remDr$navigate(page)

remDr$setTimeout(type = "implicit", milliseconds = 10000)

remDr$screenshot(display = TRUE) # check browser page

x <- seq(1,10) # set number of pages
df <- NULL 
text.clean <- NULL

for (i in x) {
  
  gianyar.web.element <- remDr$findElement(using = 'css selector', ".hotels-hotel-review-community-content-review-list-parts-ExpandableReview__cta--3_zOW") # find more button
  gianyar.web.element$clickElement() # click more button

  gianyar.review.element <- remDr$findElements(using = 'css selector', ".common-text-ReadMore__content--2ufdh") # scrape review
  css.text.headers <- unlist(lapply(gianyar.review.element, function(x) {x$getElementText()}))

  text.clean <- gsub("\\n", "", css.text.headers) # strip html markings

  gianyar.next.button <- remDr$findElement(using = 'css selector', ".next") #click next
  gianyar.next.button$clickElement() #click next button
  
  df[[i]] <- text.clean

}
