
# this script generates a report of manual and camera counts

library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")


#extract prior babine data

# dates.babine <- excel_sheets("BabineFence_DailyCounts_2021.xlsx")
# dates.babine <- dates.babine[2:81]
# 
# #cleaner dates, check that they match
# date.range <- as_date(c(ymd("2021-07-14"):ymd("2021-10-01")))
# data.frame(dates.babine,date.range)
# 
# summer.babine <- data.frame(date=date.range,lg.SK = NA,jk.SK =NA, 
#                             lg.CO=NA,jk.CO=NA,
#                             PK=NA, lg.CH=NA,jk.CH=NA, ST=NA)
# 
# for (i in 1:80){
#   tmp <- read_excel("BabineFence_DailyCounts_2021.xlsx", 
#            sheet = i+1,range = "D18:K18",col_names = F)
#   summer.babine[i,2:9] <- tmp
# }
# 
# summer.babine$crew <- "LBN"
# 
# write_excel_csv(summer.babine, "Daily.tally.BABINE.csv")

#updated excel file with all manual data:
manual.count.daily <- read_excel("Daily.tally.BABINE.xlsx") %>% 
  mutate(date = as.Date(date))


#read in camera data file

cam.data <- read_csv("2021-10-13data.dump.csv")

(daily.summary.cam <- cam.data %>% 
  filter(date %in% c(ymd("2021-10-7"):ymd("2021-10-11"))) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(lg.SK = sum(Sock, na.rm=T), 
            jk.SK = sum(jackSock, na.rm=T),
            lg.CO = sum(Coho, na.rm=T),
            jk.CO=NA,
            PK=NA,
            lg.CH = sum(Chin, na.rm=T),
            jk.CH = sum(jackChin, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(Steelhead, na.rm=T),
            crew="DFO"))

#combine 

all.daily <- rbind(daily.summary.cam, manual.count.daily) %>% 
  group_by(date) %>% 
  summarize(lg.SK = sum(lg.SK), 
            jk.SK = sum(jk.SK),
            lg.CO = sum(lg.CO),
            jk.CO = sum(jk.CO),
            PK = sum(PK),
            lg.CH = sum(lg.CH),
            jk.CH = sum(jk.CH),
            BTDV = sum(BTDV),
            ST = sum(ST))


#daily summary

daily.sum.stacked <- all.daily%>% 
  gather("species","daily.count",-date)

ggplot(daily.sum.stacked)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1.5)+
  geom_vline(aes(xintercept=ymd("2021-10-02")))

#just looking at end of Sept-Oct
ggplot(daily.sum.stacked)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1.5)+
  scale_x_date(limits=c(ymd("2021-09-25","2021-10-15")))+
  geom_vline(aes(xintercept=ymd("2021-10-02")))+
  scale_y_continuous(limits=c(0,3000))

#just salmon
daily.sum.stacked.salmon <- all.daily %>% 
  select(-c(BTDV,ST)) %>% 
  gather("species","daily.count",-date)

ggplot(daily.sum.stacked.salmon)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  scale_x_date(limits=c(ymd("2021-07-15","2021-10-15")))#+
  # geom_vline(aes(xintercept=ymd("2021-10-06")))+
  # scale_y_continuous(limits=c(0,2500))

#just coho
daily.sum.stacked.salmon <- all.daily %>% 
  select(date,lg.CO) %>% 
  gather("species","daily.count",-date)

ggplot(daily.sum.stacked.salmon)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  scale_x_date(limits=c(ymd("2021-07-15","2021-10-15")))#+
# geom_vline(aes(xintercept=ymd("2021-10-06")))+
# scale_y_continuous(limits=c(0,2500))


#extract hourly pattern from data 
# NOTE: doesn't make sense with cams not running all night
(hourly.summary.cam <- cam.data %>% 
  mutate(hour=hour(starttime)) %>% 
  filter(date %in% c(ymd("2021-10-7"):ymd("2021-10-11"))) %>% 
  group_by(hour) %>% 
  summarize(Sx = sum(Sock, na.rm=T), 
            SxJk = sum(jackSock, na.rm=T),
            Co = sum(Coho, na.rm=T),
            Ck = sum(Chin, na.rm=T),
            CkJk = sum(jackChin, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(Steelhead, na.rm=T)))





#hourly summary

hourly.sum.stacked <- hourly.summary.cam %>% 
  gather("species","hourly.count",-hour)

ggplot(hourly.sum.stacked)+
  geom_line(aes(x=hour, y=hourly.count, colour=species), size=2)

#trying to add new data to an existing excel spreadsheet 

original <- read_excel("2021-10-12data.dump.xlsx")
additional <- read_excel("2021-10-13data.dump.xlsx")

str(original)
str(additional) #more rows, more data analysed

#I want to retain all existing data and add on new empty data

orig.data <- original %>% 
  filter(!is.na(Analyzer.signoff))
uniq <- unique(orig.data$files)

addit.data <- additional %>% 
  filter(files != uniq)

rewrite <- rbind(orig.data, addit.data)
