library(tidyverse)
library(dplyr)
library(ggplot2)
library(BAS)
library(corrplot)
library(purrr)
library(RColorBrewer)
library(gridExtra)
library(broom)
library(cluster)
library(lubridate)



load("Oil_US.RData")

OIL_MODIFIED<-Oil_US

# Recession Varible

recesions<-list(ymd("1973-09-01")%--%ymd("1975-03-01"),
                ymd("1980-01-01")%--%ymd("1980-08-01"),
                ymd("1981-07-01")%--%ymd("1982-09-01"),
                ymd("1990-07-01")%--%ymd("1991-03-01"),
                ymd("2001-03-01")%--%ymd("2001-09-01"),
                ymd("2007-12-01")%--%ymd("2009-06-01"))


OIL_MODIFIED<-OIL_MODIFIED %>% mutate(Eco_Rec=ifelse(Date %within% recesions,"Recession","Normal"))



#creating the season variable

OIL_MODIFIED$Month<-as.numeric(OIL_MODIFIED$Month)

OIL_MODIFIED<-OIL_MODIFIED %>% mutate(Season=case_when(Month %in% c(3,4,5) ~ "Spring",
                                       Month %in% c(6,7,8) ~ "Summer",
                                       Month %in% c(9,10,11) ~ "Autumn",
                                       Month %in% c(12,1,2) ~"Winter"))


#Creating variable to define the degree of control of a political party on the goverment

OIL_MODIFIED<-OIL_MODIFIED %>% mutate(Gov_ctrl_type = ifelse(
        President_Party == House & House == Senate & President_Party == Senate,"Total","shared"))




OIL_MODIFIED<-OIL_MODIFIED %>% mutate(Gov_ctrl_party = case_when(
        Gov_ctrl_type == "Total" & President_Party == "Republican" ~ "Republican",
        Gov_ctrl_type == "Total" & President_Party == "Democrat" ~ "Democrat",
        Gov_ctrl_type == "shared" ~ "shared"))



save(OIL_MODIFIED,file="Oil_Modified.RData")


prop.table(table(OIL_MODIFIED$Eco_Rec))
prop.table(table(OIL_MODIFIED$Gov_ctrl_type))
prop.table(table(OIL_MODIFIED$Gov_ctrl_party))

