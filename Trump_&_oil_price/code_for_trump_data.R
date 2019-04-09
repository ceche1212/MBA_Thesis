library(rjson)
library(dplyr)
library(lubridate)
library(stringr)

#uploading 2016 data

trump2016<-fromJSON(file = "condensed_2016.json")

head(trump2016)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2016,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data
                                  
bbb<-data.frame(texto=map(trump2016,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#joining both variables tweets and data

aaa$date<-bbb

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2016

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2016<-aaa

rm(aaa,bbb,trump2016)

#-----------------------importing 2017 data-------------------------------------------------------------------------

trump2017<-fromJSON(file = "condensed_2017.json")

head(trump2017)

aaa<-data.frame(texto=map(trump2017,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2017,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#joining both variables tweets and data

aaa$date<-bbb

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2017

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2017<-aaa

rm(aaa,bbb,trump2017)

#-----------------------importing 2018 data-------------------------------------------------------------------------

trump2018<-fromJSON(file = "condensed_2018.json")

head(trump2018)

aaa<-data.frame(texto=map(trump2018,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2018,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#joining both variables tweets and data

aaa$date<-bbb

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2018

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2018<-aaa

rm(aaa,bbb,trump2018)

#-----------------------importing 2019 data-------------------------------------------------------------------------

trump2019<-fromJSON(file = "condensed_2019.json")

head(trump2019)

aaa<-data.frame(texto=map(trump2019,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2019,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#joining both variables tweets and data

aaa$date<-bbb

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-"2019"

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2019<-aaa

rm(aaa,bbb,trump2019)

#------------------------------------joining all data---------------------------------------------------------------

trump_tweets<-bind_rows(tweets2016,tweets2017,tweets2018,tweets2019)

trump_tweets<-trump_tweets %>% arrange(.,date)

trump_tweets$V1<-str_to_lower(trump_tweets$V1)

#variable looking for an oil word used

# "oil |gas |opec |pipeline |energy |barrel |oil price|drilling |oil production|gas price"

trump_tweets$oil_related_word<-grepl("oil |gas |opec |pipeline |energy |barrel |oil price|drilling |oil production|gas price",
      trump_tweets$V1,ignore.case = TRUE)

save(trump_tweets,file = "trump_tweets.RData")

trump_oil<-trump_tweets %>% group_by(date) %>% summarise(oil_word_count= sum(oil_related_word))


#in case we want to search for more specific words-----------------------------------------------------------------

trump_tweets$oil_word<-grepl("oil ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$gas_word<-grepl("gas ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$opec<-grepl("opec ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$pipeline<-grepl("pipeline ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$energy<-grepl("energy ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$barrel<-grepl("barrel ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$oil_price_word<-grepl("oil price",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$drilling<-grepl("drilling ",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$oil_production_word<-grepl("oil production",trump_tweets$V1,ignore.case = TRUE)

trump_tweets$gas_price_word<-grepl("gas price",trump_tweets$V1,ignore.case = TRUE)



#-------------importing oil prices data------------------------------------------------------------------------

oil_price<-read.csv("brent-crude-oil-prices-10-year-daily-chart.csv")

#changin the date structure

oil_price$date<-mdy(oil_price$date)

#filtering for only values between 2016(trump became president) and 2019

oil_price<-oil_price %>% filter(.,date > ymd("2016-01-01") & date < ymd("2019-05-05"))


#------------------joining everything together-----------------------------------------------------------------

trump_oil<-left_join(trump_oil,oil_price,by="date")


trump_oil<-trump_oil %>% mutate(oil_keyword=ifelse(oil_word_count>0,TRUE,FALSE))

#t-test oil word vs oil price

t.test(trump_oil$value~trump_oil$oil_keyword)

#real distrbution of tweets versus oil price------------------------------------------------------------------

data.frame(table(cut(trump_oil$value,3),trump_oil$oil_keyword)) %>%
        filter(Var2==TRUE) %>%
        select(.,-Var2) %>%
        dplyr::rename(.,Price_range=Var1,Tweets=Freq) %>%
        knitr::kable()
        
        

data.frame(table(cut(trump_oil$value,3),trump_oil$oil_keyword)) %>%
        filter(Var2==TRUE) %>%
        select(.,-Var2) %>%
        dplyr::rename(.,Price_range=Var1,Tweets=Freq) %>% 
        ggplot(aes(x=Price_range,y=Tweets,fill=Price_range))+geom_bar(stat="identity")+
        labs(y="Trump Oil Tweets",
             title = "Trump Tweets distribution by price oil range",
             x="Oil price Ranges")

#chi-square expected distribution------------------------------------------------------------------------------

#chi-square independence test
chisq.test(table(cut(trump_oil$value,3),trump_oil$oil_keyword))


data.frame(chisq.test(table(cut(trump_oil$value,3),trump_oil$oil_keyword))$expected) %>% 
        select(.,TRUE.) %>% mutate(Price_range=rownames(.)) %>% dplyr::rename(.,Chisq_Expected_Tweets=TRUE.) %>%
        select(.,Price_range,Chisq_Expected_Tweets) %>% knitr::kable()


data.frame(chisq.test(table(cut(trump_oil$value,3),trump_oil$oil_keyword))$expected) %>% 
        select(.,TRUE.) %>% mutate(Price_range=rownames(.)) %>% dplyr::rename(.,Tweets=TRUE.) %>%
        select(.,Price_range,Tweets) %>%
        ggplot(aes(x=Price_range,y=Tweets,fill=Price_range))+geom_bar(stat="identity")+
        labs(y="Expected Tweets by Chi-square Dist.",
             title = "Chi-square expected tweets distribution by price oil range",
             x="Oil price Ranges")


#-------------------Trump Oil tweets vs date-------------------------------------------------------------------

trump_oil %>% mutate(Year=year(date)) %>%
        group_by(Year) %>%
        summarise(Tweet_by_year=sum(oil_keyword==TRUE)) %>% knitr::kable()


trump_oil %>% mutate(Year=year(date)) %>%
        group_by(Year) %>%
        summarise(Tweet_by_year=sum(oil_keyword==TRUE)) %>% 
        filter(Year<2019) %>%
        ggplot(.,aes(y=Tweet_by_year,x=Year))+geom_point(size=5,color="Red")+
        geom_smooth(method = "lm",se=FALSE)

temporal<-trump_oil %>% mutate(Year=year(date)) %>%
        group_by(Year) %>%
        summarise(Tweet_by_year=sum(oil_keyword==TRUE)) %>% 
        filter(Year<2019) %>% as.data.frame()

modelo_temporal<-lm(data = temporal,Tweet_by_year~Year)

Year_2019=data.frame(Year=c(2019))

predict(modelo_temporal,newdata = Year_2019)


#day after oil tweet variable creation-------------------------------------------------------------------------

day_after<-c(0,trump_oil$oil_keyword)

day_after<-day_after[-length(day_after)]

trump_oil$day_after<-day_after

trump_oil<-trump_oil %>% mutate(day_after=ifelse(day_after>0,TRUE,FALSE))

#2days after oil tweet variable creation

twoday_after<-c(0,0,trump_oil$oil_keyword)
twoday_after<-twoday_after[-length(twoday_after)]
twoday_after<-twoday_after[-length(twoday_after)]

trump_oil$two_day_after<-twoday_after

trump_oil<-trump_oil %>% mutate(two_day_after=ifelse(two_day_after>0,TRUE,FALSE))


#3days after oil tweet variable creation

threeday_after<-c(0,0,0,trump_oil$oil_keyword)
threeday_after<-threeday_after[-length(threeday_after)]
threeday_after<-threeday_after[-length(threeday_after)]
threeday_after<-threeday_after[-length(threeday_after)]

trump_oil$three_day_after<-threeday_after

trump_oil<-trump_oil %>% mutate(three_day_after=ifelse(three_day_after>0,TRUE,FALSE))

#joining all variables together

trump_oil<-trump_oil %>% 
        mutate(tweet_pattern=case_when(oil_keyword==TRUE ~ "Oil_tweet_day",
                                                        day_after==TRUE & oil_keyword==FALSE  ~ "day_after_oil_tweet",
                                                        two_day_after==TRUE & day_after==FALSE & oil_keyword==FALSE ~"two_day_after_tweet",
                                       three_day_after==TRUE & two_day_after==FALSE & day_after==FALSE & oil_keyword==FALSE  ~"Three_day_after",
                                       three_day_after==FALSE ~"no_oil_tweet"))


#saving the dataframe in an Rdata file





#anova test

aov(trump_oil$value~trump_oil$tweet_pattern) %>% summary()

TukeyHSD(aov(trump_oil$value~trump_oil$tweet_pattern))

trump_oil %>% mutate(tweet_pattern=as.factor(tweet_pattern)) %>% 
        mutate(tweet_pattern=fct_reorder(tweet_pattern,value,fun=median)) %>%
        ggplot(.,aes(x=reorder(tweet_pattern,value),y=value,color=tweet_pattern))+geom_boxplot()


trump_oil_tweet_effect<-trump_oil %>% 
        filter(tweet_pattern!="no_oil_tweet")




t.test(trump_oil_tweet_effect$value~trump_oil_tweet_effect$tweet_pattern)


ggplot(trump_oil_tweet_effect,aes(x=date,y=value,color=tweet_pattern))+geom_boxplot()

lm(trump_oil_tweet_effect$value~trump_oil_tweet_effect$tweet_pattern) %>% broom::tidy()


save(trump_oil,file = "trump_oil.RData")
