library(rvest)
library(RSelenium)
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
