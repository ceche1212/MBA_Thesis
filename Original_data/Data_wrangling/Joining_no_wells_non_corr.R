library(dplyr)
library(purrr)
library(stringr)

list<-list.files()

str_detect(list,"RData")

list<-list[str_detect(list,"RData")]

#uploading all files

for (item in list){
        
        load(item)
}

load("Wells.RData")

rm(item,list)

#joining the data into a single dataset

Dataset1<-Rigs

#Rig activity and US_Population

Dataset1$US_Population<-US_Population$US_Population

# + US_Politics

Dataset1$Units<-NULL

Dataset1$President_Party<-US_Politics$Presidents_party
Dataset1$Senate<-US_Politics$Senate
Dataset1$House<-US_Politics$House

# + life_exp

Dataset1$life_exp<-US_life_exp$Life_Expectancy

# + US_GDP

Dataset1$GDP<-US_GDP$GDP

# + Oil_Production

Dataset1$Oil_Production<-Production$Oil_Production

# + Oil_stocks

Dataset1$Oil_Stock<-petrolstock$Petrol_Stock

# + Oil_Price filter removing last observation to join the data

v<-c(NA,NA,NA,NA,NA)

Oil_Price<-rbind(Oil_Price,v)

Dataset1$Oil_Price<-Oil_Price$Oil_Price

# + HDD

HDD<-rbind(HDD,v,v)

Dataset1$HDD<-HDD$HDD

# + CDD

CDD<-rbind(CDD,v,v)

Dataset1$CDD<-CDD$CDD

#+ energy comsumption

energy_consumption<-rbind(energy_consumption,v,v)

Dataset1$Energy_Consumption<-energy_consumption$Energy_Consumption

#+ net imports energy

net_imports_energy<-rbind(net_imports_energy,v,v)

Dataset1$Net_imports_Energy<-net_imports_energy$net_imports_energy

#+ Oil_Reserves

for (i in seq(1:(nrow(Dataset1)-nrow(Reserves_Oil)))) {
        
        Reserves_Oil<-rbind(Reserves_Oil,v)
        
}

Dataset1$Oil_Reserves<-Reserves_Oil$Oil_Reserves

#+ Active_Conflicts

for (i in seq(1:(nrow(Dataset1)-nrow(Active_Conflicts)))) {
        
        Active_Conflicts<-rbind(Active_Conflicts,v)
        
}

Dataset1$Active_Conflicts<-Active_Conflicts$Total_conflicts

#+ Wells

for (i in seq(1:(nrow(Dataset1)-nrow(Wells)))) {
        
        Wells<-rbind(Wells,v)
        
}

Dataset1$Drilled_Wells<-Wells$drilled_wells

#Organizing data types

Dataset1$Month<-as.factor(Dataset1$Month)
Dataset1$Year<-as.numeric(Dataset1$Year)
Dataset1$President_Party<-as.factor(Dataset1$President_Party)
Dataset1$Senate<-as.factor(Dataset1$Senate)
Dataset1$House<-as.factor(Dataset1$House)

Oil_US<-Dataset1

save(Oil,file="Oil.RData")



