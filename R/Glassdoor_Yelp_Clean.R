library(xlsx)
setwd("~/HappyWorkers")
fast.food.chains<-read.xlsx("data/fast food chains.xlsx",sheetIndex=1,stringsAsFactors=FALSE,header=TRUE)

fast.food.chains$COMPANY.CHAIN.NAME <- gsub("[[:space:]]+$", "", fast.food.chains$COMPANY.CHAIN.NAME)
fast.food.chains$COMPANY.CHAIN.NAME <- gsub("/.*", "", fast.food.chains$COMPANY.CHAIN.NAME)
fast.food.chains$COMPANY.CHAIN.NAME[fast.food.chains$COMPANY.CHAIN.NAME=="Papa Murphy's"] <-"Papa Murphys"
fast.food.chains$COMPANY.CHAIN.NAME[fast.food.chains$COMPANY.CHAIN.NAME=="Checkers"] <-"Checkers Drive-In"


partner.id <- xxxx
api.key <- xxxx

glassdoor.data<- function(query) {
  query <- gsub(" ", "%20", query)
  url <- paste0("http://api.glassdoor.com/api/api.htm?t.p=", 
                partner.id, "&t.k=", api.key, "&userip=0.0.0.0&useragent=&format=json&v=1&action=employers&q=", query, "&country=US")
  
  test <- readLines(url)
  
  library(RJSONIO)
  results<- RJSONIO::fromJSON(paste(test, collapse = "\n"))
  return(results$response$employers[[1]])
}

top.fast.food <- fast.food.chains$COMPANY.CHAIN.NAME

glassdoor.results <- as.list(rep(NA, length(top.fast.food)))
names(glassdoor.results) <- top.fast.food
for (i in top.fast.food){
  print(i)
  glassdoor.results[[i]] <- glassdoor.data(i)
}

company.info<- function(company){
  print(company)
  glassdoor.agg.frame <- data.frame(name=glassdoor.results[[company]]$name,
                                    overallRating= glassdoor.results[[company]]$overallRating,
                                    numberOfRatings=glassdoor.results[[company]]$numberOfRatings,
                                    ratingDescription=glassdoor.results[[company]]$ratingDescription,
                                    cultureAndValuesRating=glassdoor.results[[company]]$cultureAndValuesRating,
                                    seniorLeadershipRating=glassdoor.results[[company]]$seniorLeadershipRating,
                                    compensationAndBenefitsRating=glassdoor.results[[company]]$compensationAndBenefitsRating,
                                    careerOpportunitiesRating=glassdoor.results[[company]]$careerOpportunitiesRating,
                                    workLifeBalanceRating=glassdoor.results[[company]]$workLifeBalanceRating,
                                    recommendToFriendRating=glassdoor.results[[company]]$recommendToFriendRating,
                                    sectorId=glassdoor.results[[company]]$sectorId,
                                    industryName=glassdoor.results[[company]]$industryName,
                                    stringsAsFactors=F)
  return(glassdoor.agg.frame)
}


all.company.info<- lapply(top.fast.food,FUN=company.info)
all.company.frame<-do.call(rbind, all.company.info)

# Yelp Api Information

require(httr)
require(httpuv)
require(jsonlite)


consumerKey<-  xxxx
consumerSecret  <- xxxx
token<- 	xxxx
token_secret<- 	xxxx
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)

yelp.by.city<- function(fast.food.name,city,state){
  city <- gsub(" ", "%20", city)
  fast.food.name<- gsub(" ", "%20", fast.food.name)
  yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",20,"&location=",city,"%20",state,"&term=",fast.food.name)
  locationdata=GET(yelpurl, sig)
  locationdataContent = content(locationdata)
  locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
  return(data.frame(locationdataList))
}

library(xlsx)
largest.cities<-read.xlsx("data/largest.us.cities.xlsx",stringsAsFactors=F,header=T,sheetIndex=1)
largest.cities$state<-gsub(" ", "",largest.cities$state)

results.big.company<-as.list(rep(NA,length(fast.food.chains$COMPANY.CHAIN.NAME)))
names(results.big.company) <-fast.food.chains$COMPANY.CHAIN.NAME

for (j in names(which(is.na(results.big.company)))) {
  results.city<-as.list(rep(NA,length(largest.cities$city.name)))
  names(results.city)<-largest.cities$city.name
  for (i in largest.cities$city.name) {
    print(j)
    print(i)
    current <- try(yelp.by.city(fast.food.name = j, 
                                city = i, 
                                state = largest.cities$state[largest.cities$city.name==i]))
    if(class(current)!="try-error") {
      results.city[[i]] <- current
    }
  }
  results.big.company[[j]] <- results.city
  save(results.big.company, file = "city_yelp_results.rda")
}
# load(file = "city_yelp_results.rda")


getRelevantCols <- function(city.name,company.name) {
  df <- results.big.company[[company.name]][[city.name]]
  if (is.na(df)) {
    return(NA)
  }
  small.df<-data.frame( business.rating= unlist(df$businesses.rating),
                        business.id= unlist(df$businesses.id),
                        business.is.closed= unlist(df$businesses.is_closed),
                        business.review.count= unlist(df$businesses.review_count),
                        business.name= unlist(df$businesses.name),
                        city = city.name, stringsAsFactors=FALSE)
  return(small.df)
} 

full.yelp.data<-as.list(rep( NA, length(names(results.big.company))))

names(full.yelp.data) <-names(results.big.company)
for (i in names(results.big.company)) {
  print(i)
  simplified.companies<- lapply(names(results.big.company[[i]]), 
                                company.name=i, FUN= getRelevantCols )
  companies.cities<-do.call(rbind, simplified.companies)
  companies.cities$correct.business<-i
  full.yelp.data[[i]]<-companies.cities
}

full.yelp<-do.call(rbind,full.yelp.data)

full.yelp$business.name<-gsub("[[:punct:]]","", full.yelp$business.name)
full.yelp$correct.business<-gsub("[[:punct:]]","", full.yelp$correct.business)
full.yelp$business.name<-tolower(full.yelp$business.name)
full.yelp$correct.business<-tolower(full.yelp$correct.business)

full.yelp<- full.yelp[full.yelp$correct.business==full.yelp$business.name,]

glassdoor<-all.company.frame
glassdoor$name<- tolower(gsub("[[:punct:]]","", glassdoor$name))

unique(glassdoor$name)[!unique(glassdoor$name) %in% unique(full.yelp$correct.business)]
glassdoor$name[glassdoor$name=="sonic"]<- "sonic drivein"
glassdoor$name[glassdoor$name=="popeyes"]<-"popeyes louisiana kitchen"
glassdoor$name[glassdoor$name=="five guys burgers  fries"]<-"five guys" 
glassdoor$name[glassdoor$name=="baskin robbins"]<-"baskinrobbins"
glassdoor$name[glassdoor$name=="jersey mikes"]<-"jersey mikes subs" 


yelp.reviews<- tapply(full.yelp$business.rating,full.yelp$correct.business ,mean, na.rm=TRUE)
yelp.reviews<-data.frame(yelp.reviews)
yelp.reviews$company<-rownames(yelp.reviews)

final<- merge(glassdoor, yelp.reviews, by.x="name", by.y="company", all.x=T)
final$cultureAndValuesRating <-as.numeric(final$cultureAndValuesRating)
final$compensationAndBenefitsRating <-as.numeric(final$compensationAndBenefitsRating)
final$recommendToFriendRating <- as.numeric(final$recommendToFriendRating)
final$seniorLeadershipRating <-as.numeric(final$seniorLeadershipRating)
final$careerOpportunitiesRating<-as.numeric(final$careerOpportunitiesRating)
final$workLifeBalanceRating <-as.numeric(final$workLifeBalanceRating)

formatted.names <- data.frame(better.name = names(results.big.company),
                              easy.name = na.omit(unique(full.yelp$business.name)),
                              stringsAsFactors = FALSE)

final <-merge(final, formatted.names, by.x="name", by.y="easy.name", all.x=T)

library(ggplot2)
library(ggrepel)

ggplot(final, aes(x=overallRating,y=yelp.reviews))+geom_point()+
  geom_text(aes(label=final$name))

workLife <- ggplot(final, aes(x=workLifeBalanceRating,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Work Life Balance Rating (1-5)")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

compensation <- ggplot(final, aes(x=compensationAndBenefitsRating,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Compensation and Benefits Rating (1-5)")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

culture <- ggplot(final, aes(x=cultureAndValuesRating,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Culture and Values Rating (1-5)")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

careerops<-ggplot(final, aes(x=careerOpportunitiesRating,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Career Opportunities Rating (1-5)")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

seniorlead<-ggplot(final, aes(x=seniorLeadershipRating,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Senior Leadership Rating (1-5)")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

overallrate<- ggplot(final, aes(x=overallRating ,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Overall Rating (1-5)")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

friendsrating<- ggplot(final, aes(x=recommendToFriendRating  ,y=yelp.reviews))+geom_point(colour = "red")+
  geom_text_repel(aes(label=final$better.name)) +
  xlab("Percent Would Recommend to Friend")+
  ylab("Average Yelp Reviews (Across top 96 cities)")

culturelm<- summary(lm(data=final, yelp.reviews~ cultureAndValuesRating))
compensationlm<- summary(lm(data=final, yelp.reviews~ compensationAndBenefitsRating))
wklifelm<- summary(lm(data=final, yelp.reviews~ workLifeBalanceRating))
careeropslm<- summary(lm(data=final, yelp.reviews~ careerOpportunitiesRating))
seniorlm<- summary(lm(data=final, yelp.reviews~ seniorLeadershipRating))
friendlm<- summary(lm(data=final, yelp.reviews~ recommendToFriendRating))
overall.lm<- summary(lm(data=final, yelp.reviews~ overallRating))


ggsave("worklifebalance.png", plot = workLife, 
       width = 12, height = 8, units = "in",
       dpi = 600)
ggsave("compensation.png", plot = compensation, 
       width = 12, height = 8, units = "in",
       dpi = 600)
ggsave("culture.png", plot = culture, 
       width = 12, height = 8, units = "in",
       dpi = 600)
ggsave("seniorlead.png", plot = seniorlead, 
       width = 12, height = 8, units = "in",
       dpi = 600)
ggsave("overallrate.png", plot = overallrate, 
       width = 12, height = 8, units = "in",
       dpi = 600)
ggsave("friendsrating.png", plot = friendsrating, 
       width = 12, height = 8, units = "in",
       dpi = 600)

multiplelm<-summary(lm(data=final, yelp.reviews ~ cultureAndValuesRating+
                         seniorLeadershipRating+compensationAndBenefitsRating
                       +careerOpportunitiesRating+workLifeBalanceRating ))