# data downloaded manually from https://ourworldindata.org/war-and-peace

library(dplyr)

data<-read.csv("number-of-conflicts-and-incidences-of-one-sided-violence.csv")

total_war <- data %>% group_by(Year) %>% summarise(sum(Number.of.conflicts.and.incidences.of.one.sided.violence))

total_war<- total_war%>% filter(Year > 1972)

anios_sequencia<-seq(1973,2016,by=1)

meses<-data.frame()

for(i in seq(1:length(anios_sequencia))){
        
        for(j in seq(1:12)){
                
                meses<-rbind(meses,j)
                
                
                
        }
        
        
}

colnames(meses)<-c("month")

#building the year columns

anios<-data.frame()

for (i in seq(1:length(anios_sequencia))) {
        
        for(j in seq(1:12)){
                
                anios<-rbind(anios,anios_sequencia[i])
                
                
                
        }
        
        
}

colnames(anios)<-c("Year")

#building the war colum of the data frame

wars<-data.frame()

for (i in seq(1:length(total_war$Year))) {
        
        for(j in seq(1:12)){
                
                wars<-rbind(wars,total_war$`sum(Number.of.conflicts.and.incidences.of.one.sided.violence)`[i])
                
                
                
        }
        
        
}

colnames(wars)<-c("Total_conflicts")

data1<-cbind(anios,meses,wars)

data1$Date<-paste0(as.character(data1$Year),"-",as.character(data1$month),"-","01")

data1$Units<-"Number of conflicts"

data1$Date<-as.Date(data1$Date,format = "%F")

data1<-data1[,c(4,2,1,3,5)]

Active_Conflicts<-data1

save(Active_Conflicts,file = "Active_conflicts.RData")

