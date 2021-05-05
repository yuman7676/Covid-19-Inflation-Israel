
#title: "COVID-19 Inflation 2020 Weights"


#setup
library(tidyverse)

#Import Data

#Import Data
setwd("G:/zrtm/Yuval/COVID-19 Infaltion")
df<- read.csv("cc-DATA.csv")
first.date<-as.Date(df[1,1],"%Y-%m-%d")
last.date<-as.Date(df[nrow(df),1],"%Y-%m-%d")
names(df) <- sub("_SUM", "", names(df))

fill_missing_dates<-seq.Date(first.date,last.date,by="day")
df<-df %>%
  mutate(date=as.Date(df$date,"%Y-%m-%d")) %>%
  complete(date=fill_missing_dates)
df[is.na(df)] <- 0

group.names<-read_excel("group_names.xlsx", sheet = "Research Groups")
detail.names<-read_excel("group_names.xlsx", sheet = "Details")
cc.daily.long<-melt(df,id.vars=c("date"))
names(cc.daily.long)[names(cc.daily.long)=="variable"]<-"RNames"
cc.long<-merge(cc.daily.long,group.names,by=c("RNames"))
cc.long.1<-cc.long%>%group_by(date,ResearchG)%>%summarise(value=sum(value))
#Detailed Indicies
cc.long.2<-merge(detail.names,cc.daily.long,by=c("RNames"))
cc.long.2<-cc.long.2%>%group_by(date,DetailG)%>%summarise(value=sum(value))

cc.long.T<-cc.long%>%group_by(date)%>%summarise(value=sum(value))%>%mutate(ResearchG="Total")
cc.long<-dplyr::bind_rows(cc.long.1,cc.long.T)

cc.long.T2<-cc.long.T%>%rename(DetailG=ResearchG)
cc.long.2<-dplyr::bind_rows(cc.long.2,cc.long.T2)

##monthly averges 
#General
cc.monthly1 <- cc.long.1 %>%
  filter(year(date)==2020) %>%
  group_by(month(date),ResearchG) %>%
  summarize(average=mean(value))
#Detailed
cc.monthly2 <- cc.long.2 %>%
  filter(year(date)==2020) %>%
  group_by(month(date),DetailG) %>%
  summarize(average=mean(value))
#Assumeming that the mean of January and Fubeurary is the "normal" expendeture
change.rate1 <- function(data, x, m) {
  (cc.monthly1$average[cc.monthly1$ResearchG==x & cc.monthly1$`month(date)`==m]
   -(cc.monthly1$average[cc.monthly1$ResearchG==x & cc.monthly1$`month(date)`==1]
     +cc.monthly1$average[cc.monthly1$ResearchG==x & cc.monthly1$`month(date)`==2])/2)/
    (cc.monthly1$average[cc.monthly1$ResearchG==x & cc.monthly1$`month(date)`==1]
     +cc.monthly1$average[cc.monthly1$ResearchG==x & cc.monthly1$`month(date)`==2])
}
change.rate2 <- function(x,m) {
  (cc.monthly2$average[cc.monthly2$DetailG==x & cc.monthly2$`month(date)`==m]
   -(cc.monthly2$average[cc.monthly2$DetailG==x & cc.monthly2$`month(date)`==1]
     +cc.monthly2$average[cc.monthly2$DetailG==x & cc.monthly2$`month(date)`==2])/2)/
    (cc.monthly2$average[cc.monthly2$DetailG==x & cc.monthly2$`month(date)`==1]
     +cc.monthly2$average[cc.monthly2$DetailG==x & cc.monthly2$`month(date)`==2])
}

cc.monthly1 <- cc.monthly1 %>%
  mutate(change_rate=change.rate1(x=ResearchG, m=`month(date)`)) 

cc.monthly2 <- cc.monthly2 %>%
  mutate(change_rate=change.rate2(x=DetailG, m=`month(date)`))
#calculating COVID-19 weights

#Total change in expinditure
Total.Average <- cc.monthly2$average[cc.monthly2$DetailG=='Total']
Total.Change <-1+ (((Total.Average-((Total.Average[1]+Total.Average[2])/2))/(Total.Average[1]+Total.Average[2]))/2)
#Food
Food <- 
  cc.monthly1 %>% 
  filter(ResearchG=="FoodStores") %>% 
  mutate(wt=141.49) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#House Maintance
House_Maintance <-
  cc.monthly1 %>%
  filter(ResearchG=="ServElecWater"|ResearchG=="ProfServ") 
Housing_Maintance_mean <- c(mean(House_Maintance$change_rate[House_Maintance$`month(date)`==1]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==2]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==3]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==4]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==5]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==6]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==7]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==8]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==9]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==10]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==11]),
                            mean(House_Maintance$change_rate[House_Maintance$`month(date)`==12]))
Wt_maintance <- c(91.31,91.31,91.31,91.31,91.31,91.31,91.31,91.31,91.31,91.31,91.31,91.31) 
df_Maintance <- data.frame(Housing_Maintance_mean,Wt_maintance)
df_Maintance <- df_Maintance %>% 
  mutate(`month(date)`=c(1:12)) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=((Housing_Maintance_mean+1)*Wt_maintance)*Total.Change)
#Furniture
Furniture <- 
  cc.monthly2 %>%
  filter(DetailG=="MG_ElecGoods"|DetailG=="	MG_Furniture") 
Furniture_mean <- c(mean(Furniture$change_rate[Furniture$`month(date)`==1]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==2]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==3]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==4]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==5]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==6]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==7]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==8]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==9]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==10]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==11]),
                    mean(Furniture$change_rate[Furniture$`month(date)`==12]))
Wt_Furniture <- c(36.47,36.47,36.47,36.47,36.47,36.47,36.47,36.47,36.47,36.47,36.47,36.47) 
df_Furniture <- data.frame(Furniture_mean,Wt_Furniture)
df_Furniture <- df_Furniture %>%
  mutate(`month(date)`=c(1:12)) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(Furniture_mean+1)*Wt_Furniture*Total.Change)
#Clothing
Clothing <- 
  cc.monthly2 %>% 
  filter(DetailG=="MG_Clothes") %>% 
  mutate(wt=30.62) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#Health
Health <- 
  cc.monthly1 %>% 
  filter(ResearchG=="MedServPharm") %>% 
  mutate(wt=56.80) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#Education
Education <- 
  cc.monthly2 %>% 
  filter(DetailG=="S_Educ") %>% 
  mutate(wt=58.35) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#Entertainment
Entertainment <- 
  cc.monthly2 %>% 
  filter(DetailG=="S_Leisure") %>% 
  mutate(wt=86.52) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#Transport
Transport <- 
  cc.monthly1 %>% 
  filter(ResearchG=="FuelTransportPost") %>% 
  mutate(wt=202.65) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#Misc
Misc <- 
  cc.monthly1 %>%
  group_by(`month(date)`) %>%
  summarise(total=sum(average)) %>%
  mutate(change_rate=(total-mean(1121415724.59741926,959923656.15344822))/mean(1121415724.59741926,959923656.15344822)) %>%
  mutate(wt=54.53) %>%
  mutate(Total.Change=Total.Change[`month(date)`]) %>%
  mutate("COVID-19_wt"=(change_rate+1)*wt*Total.Change)
#Housing & Summary
Summary <- data.frame(
  month=c(1:12),
  Food=Food$`COVID-19_wt`,
  Maintance=df_Maintance$`COVID-19_wt`,
  Furniture=df_Furniture$`COVID-19_wt`,
  Clotihng=Clothing$`COVID-19_wt`,
  Health=Health$`COVID-19_wt`,
  Education=Education$`COVID-19_wt`,
  Entertainment=Entertainment$`COVID-19_wt`,
  Transnsport=Transport$`COVID-19_wt`,
  Misc=Misc$`COVID-19_wt`
) 

Summary<- Summary %>%
  mutate(Housing=(1000-rowSums(Summary)+month)) 