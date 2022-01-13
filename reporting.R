
# This script analyses the Babine coho extension project in 2021. 
# It began as a weekly reporting script (with Reporting.Rmd), then transitioned to 
# a more complete reporting script for end of year reporting

#Author: K. Peck
#Started: Oct 2021


library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")

# citation("tidyverse")
# citation("lubridate")
# citation("readxl")
# citation("ggplot2")


#extract prior babine data

# dates.babine <- excel_sheets("FinalQAQC-BabineFence_DailyCounts&Summary_2021_KP.xlsx")
# dates.babine <- dates.babine[2:81]
# #
# # #cleaner dates, check that they match
#  date.range <- as_date(c(ymd("2021-07-14"):ymd("2021-10-01")))
#  data.frame(dates.babine,date.range)
# 
#  summer.babine <- data.frame(date=date.range,lg.SK = NA,jk.SK =NA,
#                              lg.CO=NA,jk.CO=NA,
#                              PK=NA, lg.CH=NA,jk.CH=NA, ST=NA,
#                              BTDV=NA,crew="LBN",chutes="Chutes 1-7",
#                              time1="07:00",airtemp1=NA,watertemp1=NA,
#                              waterlevel1=NA,
#                              time2="19:00",airtemp2=NA,watertemp2=NA,
#                              waterlevel2=NA)
# 
# for (i in 1:80){
#    tmp <- read_excel("FinalQAQC-BabineFence_DailyCounts&Summary_2021_KP.xlsx",
#             sheet = i+1,range = "D18:K18",col_names = F)
#    tmp2 <- t(read_excel("FinalQAQC-BabineFence_DailyCounts&Summary_2021_KP.xlsx",
#                      sheet = i+1,range = "D21:D23",col_names = F))
#    tmp3 <- t(read_excel("FinalQAQC-BabineFence_DailyCounts&Summary_2021_KP.xlsx",
#                         sheet = i+1,range = "G21:G23",col_names = F))
#    summer.babine[i,2:9] <- tmp
#    summer.babine[i,14:16] <-tmp2
#    summer.babine[i,18:20] <-tmp3
#  }
#  head(summer.babine)
# 
#  write_csv(summer.babine, "Daily.tally.BABINE.csv")





# read in manual count data ####
manual.count.daily <- read_excel("Daily.tally.BABINE.xlsx") %>% 
  mutate(date = as.Date(date), method="manual") %>% 
  mutate(CO = lg.CO+jk.CO) %>% 
  mutate(RBCT = NA,MW = NA, SU = NA) %>% 
  select(c(date,lg.SK,jk.SK,CO,PK,lg.CH,jk.CH,ST,BTDV,RBCT,MW,SU,
           crew,chutes,method))
str(manual.count.daily)


 


# #read in raw camera data ####

cam.data1 <- read_csv("2021VideoData_4Oct-3Nov2021.csv",
                     col_types = list(date=col_character(),
                                      Sock=col_double(),
                                      jackSock=col_double(),
                                      Coho=col_double(),
                                      Steelhead=col_double(),
                                      BTDV=col_double(),
                                      Rainbow=col_double(),
                                      Whitefish=col_double(),
                                      Sucker=col_double(),
                                      Chin=col_double(),
                                      jackChin=col_double(),
                                      PK=col_double(),
                                      Other=col_double())) %>%
              mutate(date = dmy(date), hour=hour(starttime)) %>%
  filter(chute.open %in% "Y")
str(cam.data1)


cam.data2 <- read_csv("2021VideoData_4Nov-25Nov2021.csv",
                     col_types = list(date=col_date(),
                                      Sock=col_double(),
                                      jackSock=col_double(),
                                      Coho=col_double(),
                                      Steelhead=col_double(),
                                      BTDV=col_double(),
                                      Rainbow=col_double(),
                                      Whitefish=col_double(),
                                      Sucker=col_double(),
                                      Chin=col_double(),
                                      jackChin=col_double(),
                                      PK=col_double(),
                                      Other=col_double())) %>%
              mutate(chute.open = "Y", hour=hour(starttime))
str(cam.data2)
# 
# cam.data <- rbind(cam.data1,cam.data2)

#read in QA'd data
cam.data <- read_excel("2021VideoData_QAd.xlsx",
                       col_types = c("text","text","date","text",
                                     "text","numeric","numeric",
                                     "numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric",
                                     "numeric","numeric","text","text",
                                     "text","numeric")) %>% 
  mutate(starttime = as_datetime(starttime),date=as_date(date))
  


#### QA #### 

#date ranges for each trap
cam.data %>% 
  group_by(trap) %>% 
  summarize(trap.opened=min(date),trap.closed=max(date)) %>% 
  arrange(trap)

#hourly operation summaries by trap
qa <- cam.data %>%
  filter(chute.open %in% "Y") %>%
  mutate(hr = hour(starttime))

ggplot(qa)+
  geom_histogram(aes(x=hr, fill=trap),binwidth=1, position = "dodge")


#match motion detect video to continuous footage to catch
# unreviewed data
fishdrive1.c <- read_csv("videodata.c_5-Oct_to_4-Nov.csv",
                         col_types = list(files=col_character(),
                                          trap=col_character(),
                                          date=col_date(),
                                          type=col_character(),
                                          hour=col_integer()))

(zeros <- fishdrive1.c %>% 
  right_join(cam.data1, by=c("date", "hour", "trap")) %>% 
  arrange(date, trap,hour) %>% 
  group_by(date, trap,hour) %>% 
  summarize(all.fish = sum(Sock, jackSock, Coho, BTDV, Steelhead, 
                Rainbow, Whitefish, Sucker, Chin, jackChin,
                Other, PK, na.rm=T)) %>% 
  filter(all.fish %in% 0))



# # # # # # # # # # # # # # # # # #
#### Temp and water level #### # # 
# # # # # # # # # # # # # # # # # #

#fill in last water temps and levels from data sheets

water.temp.level <- read_excel("Daily.tally.BABINE.xlsx") %>% 
  mutate(date = as.Date(date), hr1 = hour(time1),min1 = minute(time1),
         sec1 = second(time1), datetime1 = ymd_hms(paste(date,hr1,min1,sec1)),
         hr2 = hour(time2),min2 = minute(time2),
         sec2 = second(time2), datetime2 = ymd_hms(paste(date,hr2,min2,sec2))) %>% 
  select(c(date,datetime1, hr1, airtemp1,watertemp1,waterlevel1,
           datetime2, hr2, airtemp2,	watertemp2,	waterlevel2,	comments))
tail(water.temp.level)

water1 <- water.temp.level %>% 
  select(datetime=datetime1, airtemp=airtemp1, watertemp=watertemp1,
         waterlevel=waterlevel1)

water2 <- water.temp.level %>% 
  select(datetime=datetime2, airtemp=airtemp2, watertemp=watertemp2,
         waterlevel=waterlevel2)
temps.stack <- rbind(water1, water2) %>% 
  pivot_longer(!datetime, names_to = "measure") %>% 
  filter(measure != "waterlevel")


plot.temps <- ggplot(data=temps.stack)+
  geom_line(aes(x=datetime, y=value, col=measure), size=.75)+
  labs(x="Date",y="Temperature (deg. C)",col="")
plot.temps



#### reporting date ranges ####

cam.date.range <- c(ymd("2021-10-06"):ymd("2021-11-26"))
extension.date.range <- c(ymd("2021-10-02"):ymd("2021-11-26"))
plot.date.limit <- c(ymd("2021-10-03","2021-11-26"))
table.date.range <- c(ymd("2021-10-20"):ymd("2021-11-26"))


(daily.summary.cam <- cam.data %>% 
  mutate(date=ymd(date)) %>% 
  filter(date %in% extension.date.range) %>%
  filter(!is.na(Analyzer.signoff)) %>% 
  filter(chute.open %in% "Y") %>% 
  group_by(date) %>% 
  summarize(lg.SK = sum(Sock, na.rm=T), 
            jk.SK = sum(jackSock, na.rm=T),
            CO = sum(Coho, na.rm=T),
            PK=sum(PK, na.rm=T),
            lg.CH = sum(Chin, na.rm=T),
            jk.CH = sum(jackChin, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(Steelhead, na.rm=T),
            RBCT = sum(Rainbow, na.rm=T),
            MW = sum(Whitefish, na.rm=T),
            SU = sum(Sucker, na.rm=T),
            crew="DFO", 
            method="cam",
            chutes=paste(unique(trap),collapse = ",")) %>% 
    mutate(date=ymd(date)) )

#### BC fish daily counts ####

(daily.summary.cam.BC <- cam.data %>% 
    filter(date >= ymd("2021-10-02")) %>%
    filter(!is.na(Analyzer.signoff)) %>% 
    filter(chute.open %in% "Y") %>% 
    group_by(date) %>% 
    summarize(BTDV = sum(BTDV, na.rm=T),
              ST = sum(Steelhead, na.rm=T),
              RBCT = sum(Rainbow,na.rm=T),
              MW = sum(Whitefish, na.rm=T),
              SU = sum(Sucker, na.rm=T),
              crew="DFO/LBN", 
              method="cam",
              chutes=paste(unique(trap),collapse = ",")) %>% 
    mutate(date=ymd(date)) )

#write_csv(daily.summary.cam.BC,"daily.summary.cam.BC.csv" )

cam.stacked.BC <- daily.summary.cam.BC %>% 
  select(-c(crew,method,chutes)) %>% 
  gather("species","daily.count",-date) %>% 
  mutate(method="cam")


plot.daily.BC <- ggplot(cam.stacked.BC)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)
plot.daily.BC

daily.summary.cam.BC %>% 
  summarize(BTDV = sum(BTDV, na.rm=T),
            ST = sum(ST, na.rm=T),
            RBCT = sum(RBCT,na.rm=T),
            MW = sum(MW, na.rm=T),
            SU = sum(SU, na.rm=T))


####Compare Cam vs. Manual ####

#columns separate
sep.daily <- manual.count.daily %>% 
  left_join(daily.summary.cam, by="date",suffix=c(".m",".c"))

#rbind 
manual.stacked <- manual.count.daily %>% 
  select(-c(crew,method,chutes)) %>% 
  gather("species","daily.count",-date) %>% 
  mutate(method="manual") %>% 
  mutate(traps = ifelse(date >= ymd("2021-07-14")& date <= ymd("2021-10-03"),7,
                  ifelse(date == ymd("2021-10-04"),6,
                         ifelse(date == ymd("2021-10-05"),4,
                                ifelse(date >= ymd("2021-10-06") & date <= ymd("2021-10-15"),3,
                                       ifelse(date >= ymd("2021-10-16") & date <= ymd("2021-10-18"),1,0))))))

cam.stacked <- daily.summary.cam %>% 
  select(-c(crew,method,chutes)) %>% 
  gather("species","daily.count",-date) %>% 
  mutate(method="cam") %>% 
  mutate(traps = ifelse(date >= ymd("2021-10-04")& date <= ymd("2021-10-05"),1,
                        ifelse(date == ymd("2021-10-06"),2,
                               ifelse(date >= ymd("2021-10-07")& date <= ymd("2021-11-06"),4,
                                      ifelse(date >= ymd("2021-11-07"),3,0)))))


stack.all<- rbind(manual.stacked, cam.stacked) 

stack.all.salmon <- stack.all %>% 
  filter((species %in% c("lg.SK","jk.SK","CO","PK","lg.CH","jk.CH","ST")) ) %>% 
  arrange(date)

plot.cam.vs.man <- ggplot(stack.all.salmon)+
  geom_line(aes(x=date, y=daily.count, col=species,linetype=method))+
  scale_x_date(limits=plot.date.limit)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))

plot.cam.vs.man

stack.all.salmon.sum  <- stack.all.salmon %>% 
  group_by(date,method) %>% 
  summarize(all.fish=sum(daily.count,na.rm=T), open.traps=unique(traps))

ggplot(stack.all.salmon.sum)+
  geom_line(aes(x=date, y=all.fish, col=method))+
  scale_x_date(limits=plot.date.limit)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))+
  labs(y="total daily fish counted")

plot.cam.vs.man2 <- ggplot(stack.all.salmon.sum, aes(x=date, 
                                              y=all.fish, 
                                              fill = method)) +                           # ggplot2 with default settings
  geom_bar(stat = "identity")+
  scale_x_date(limits=plot.date.limit,
               date_breaks = "2 days")+
  #geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))+
  labs(y="daily fish count (all species)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot.cam.vs.man2

# plot cam vs man traps as rate by trap
str(stack.all.salmon)

stack.all.sum.numtrap <- stack.all.salmon.sum %>% 
  mutate(fishpertrap = all.fish/open.traps)

plot.cam.vs.man3 <- ggplot(stack.all.sum.numtrap, aes(x=date, 
                                              y=fishpertrap, 
                                              fill = method)) +                           # ggplot2 with default settings
  geom_bar(stat = "identity",position = "dodge")+
  scale_x_date(limits=c(ymd("2021-09-20"),ymd("2021-10-20")),
               date_breaks = "3 days")+
  #geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))+
  labs(y="fish count per open trap (all species)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot.cam.vs.man3


## Jacks, manual vs. Cam ####

daily.SK <- stack.all.salmon %>% 
  filter(species %in% c("lg.SK", "jk.SK"))

ggplot(daily.SK)+
  geom_line(aes(x=date,y=daily.count, col=species, linetype=method))+
  scale_x_date(limits=c(ymd("2021-10-01"),ymd("2021-10-20")))+
  scale_y_continuous(limits = c(0,1500))

# jack as proportion of lg SK

SK.daily <- manual.count.daily %>% 
  rbind(daily.summary.cam) %>% 
  mutate(jk.SK.rate = jk.SK/(lg.SK+jk.SK)) %>% 
  select(date,method,lg.SK,jk.SK,jk.SK.rate)

ggplot(SK.daily)+
  geom_line(aes(x=date,y=jk.SK.rate,col=method))



#combine into one df #

all.daily <- rbind(daily.summary.cam, manual.count.daily) %>% 
  group_by(date) %>% 
  summarize(lg.SK = sum(lg.SK, na.rm=T), 
            jk.SK = sum(jk.SK, na.rm=T),
            CO = sum(CO, na.rm=T),
            PK = sum(PK, na.rm=T),
            lg.CH = sum(lg.CH, na.rm=T),
            jk.CH = sum(jk.CH, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(ST, na.rm=T),
            RBCT = sum(RBCT, na.rm=T),
            MW = sum(MW, na.rm=T),
            SU = sum(SU, na.rm=T),
            crew= paste(unique(crew),collapse=","),
            method= paste(unique(method),collapse=","),
            chutes=paste(unique(chutes), collapse=","))

#summary tables #
table.end <- all.daily %>% 
  filter(date %in% table.date.range) %>%
  mutate(date=format(date,"%d-%b-%y")) %>% 
  select(Date=date,`Large SK`=lg.SK,`Jack SK`=jk.SK,
         CO,PK,`Large CH`=lg.CH,`Jack CH`=jk.CH,`BT/DV`=BTDV,
         ST,method)
table.end

table.totals <- all.daily %>% 
  mutate(period=ifelse(date %in% c(ymd("2021-10-02"):ymd("2021-11-26")),
                        "extension","regular"))%>% 
  dplyr::group_by(period) %>% 
  dplyr::summarize(`Large SK`=sum(lg.SK, na.rm = T),`Jack SK`=sum(jk.SK, na.rm=T),
         CO=sum(CO, na.rm=T),PK=sum(PK,na.rm=T),`Large CH`=sum(lg.CH,na.rm=T),
         `Jack CH`=sum(jk.CH,na.rm=T),`BT/DV`=sum(BTDV,na.rm=T),
         ST=sum(ST, na.rm=T)) %>% 
  arrange(desc(period))
table.totals


#export daily counts 

#write_csv(all.daily, "Daily.counts.all.Babine.csv")



# plots, daily counts ####

daily.sum.stacked <- all.daily%>% 
  select(-c(method, chutes, crew)) %>% 
  gather("species","daily.count",-date)

plot.daily.all.spp <- ggplot(daily.sum.stacked)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))
plot.daily.all.spp


#just salmon
daily.sum.stacked.salmon <- daily.sum.stacked %>% 
  filter(species %in% c("lg.SK","jk.SK","CO","lg.CH","jk.CH","PK"))



plot.daily.salmon <- ggplot(daily.sum.stacked.salmon)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  scale_x_date(limits=c(ymd("2021-07-15"),plot.date.limit[2]))+
  geom_vline(aes(xintercept=ymd("2021-10-06")))#+
  # scale_y_continuous(limits=c(0,2500))
plot.daily.salmon



#just looking at end of Sept-Oct
plot.daily.salmon.end <- ggplot(daily.sum.stacked.salmon)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  scale_x_date(limits=plot.date.limit,
               date_breaks = "2 days")+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1300))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot.daily.salmon.end



#just coho
daily.sum.stacked.coho <- daily.sum.stacked %>% 
  filter(species %in% c("CO"))

plot.daily.coho <- ggplot(daily.sum.stacked.coho)+
  #geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  geom_line(aes(x=date, y=daily.count), size=1)+
  geom_text(aes(x=ymd("2021-11-01"),y=300,
                label=paste0("Total Coho in 2021:\n",
                             sum(daily.count))))+
  scale_x_date(limits=c(ymd("2021-08-15"),plot.date.limit[2]),
               date_breaks = "2 weeks",date_labels = "%d-%b")+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  labs(y="daily coho count", x="", col=NULL)


plot.daily.coho

#plots, cumulative counts#### 

cumul.daily <- all.daily %>% 
  mutate(lg.SK = cumsum(lg.SK),
         jk.SK = cumsum(jk.SK),
         lg.CH = cumsum(lg.CH),
         jk.CH = cumsum(jk.CH),
         CO = cumsum(CO),
         PK = cumsum(PK)) %>% 
  select(date,lg.SK,jk.SK,lg.CH,jk.CH,CO,PK) %>% 
  gather("species","cumulative.count",-date)

plot.cumul.daily <- ggplot(cumul.daily)+
  geom_line(aes(x=date, y=cumulative.count, col=species),size=1.25)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))
plot.cumul.daily

cumul.daily.coho <- cumul.daily %>% 
  filter(species %in% "CO")

plot.cumul.daily.coho <- ggplot(cumul.daily.coho)+
  geom_line(aes(x=date, y=cumulative.count, col=species),size=1.25)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))
plot.cumul.daily.coho




####Fish in time####

#extract hourly pattern from data 
# NOTE: doesn't make sense with cams not running all night
#   so should code in another column for 24 hr cams

library(suncalc)
#make df of dawn and dusk for babine
# getSunlightTimes(date = c(as_date("2022-01-10"):as_date("2022-01-12")), 
#                  lat =  55.4, lon = -126.7, tz = "America/Los_Angeles")

daylight.hrs <-distinct(cam.data, date) %>%
  with(., getSunlightTimes(date = date,
                           lat =  55.4, lon = -126.7, tz = "America/Los_Angeles",
                           keep = c('sunrise', 'sunset','dusk','dawn')))  

cam.data.daytime <- cam.data %>% 
  left_join(daylight.hrs) %>% 
  mutate(hour=hour(starttime),date.hr = ymd_h(paste(date,hour))) %>% 
  mutate(daytime = ifelse(starttime >= dawn & starttime <= dusk,"day","night")) %>% 
  filter(chute.open %in% "Y")  

hourly.summary.cam <- cam.data.daytime %>% 
  group_by(date, date.hr) %>% 
  summarize(num.chutes = length(unique(trap)),
            Sx = sum(Sock, na.rm=T), 
            SxJk = sum(jackSock, na.rm=T),
            Co = sum(Coho, na.rm=T),
            Ck = sum(Chin, na.rm=T),
            CkJk = sum(jackChin, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(Steelhead, na.rm=T),
            RBCT = sum(Rainbow, na.rm=T),
            WF = sum(Whitefish, na.rm=T),
            tot.fish = sum(Sx, SxJk, Co, Ck, CkJk),
            fish.per.chute = tot.fish/num.chutes) 

(byhour.summary.cam <- cam.data.daytime %>% 
    mutate(hour=hour(starttime),date.hr = ymd_h(paste(date,hour))) %>%
    filter(chute.open %in% "Y") %>% 
    group_by(hour) %>% 
    summarize(num.chutes = length(unique(trap)),
              Sx = sum(Sock, na.rm=T), 
              SxJk = sum(jackSock, na.rm=T),
              Co = sum(Coho, na.rm=T),
              Ck = sum(Chin, na.rm=T),
              CkJk = sum(jackChin, na.rm=T),
              BTDV = sum(BTDV, na.rm=T),
              ST = sum(Steelhead, na.rm=T),
              RBCT = sum(Rainbow, na.rm=T),
              WF = sum(Whitefish, na.rm=T),
              tot.fish = sum(Sx, SxJk, Co, Ck, CkJk),
              fish.per.chute = tot.fish/num.chutes)) 

(bydaynight.summary.cam <- cam.data.daytime %>% 
    mutate(hour=hour(starttime),date.hr = ymd_h(paste(date,hour))) %>%
    filter(chute.open %in% "Y") %>% 
    group_by(daytime, date) %>% 
    summarize(num.chutes = length(unique(trap)),
              Sx = sum(Sock, na.rm=T), 
              SxJk = sum(jackSock, na.rm=T),
              Co = sum(Coho, na.rm=T),
              Ck = sum(Chin, na.rm=T),
              CkJk = sum(jackChin, na.rm=T),
              BTDV = sum(BTDV, na.rm=T),
              ST = sum(Steelhead, na.rm=T),
              RBCT = sum(Rainbow, na.rm=T),
              WF = sum(Whitefish, na.rm=T),
              tot.fish = sum(Sx, SxJk, Co, Ck, CkJk),
              fish.per.chute = tot.fish/num.chutes,
              num.hrs = length(unique(hour)),
              fish.per.chute.hr = fish.per.chute/num.hrs)) 


ggplot(data=bydaynight.summary.cam)+
  geom_line(aes(x=date, y=fish.per.chute, col=daytime))+
  scale_x_date(limits = c(ymd("2021-10-05"),ymd("2021-10-15")))+
  labs(title="# Fish, corrected for chutes open")

ggplot(data=bydaynight.summary.cam)+
  geom_line(aes(x=date, y=fish.per.chute.hr, col=daytime))+
  scale_x_date(limits = c(ymd("2021-10-05"),ymd("2021-10-15")))+
  labs(title="# Fish, corrected for chutes open \nand hrs of day & night")

ggplot(data=bydaynight.summary.cam)+
  geom_line(aes(x=date, y=tot.fish, col=daytime))+
  scale_x_date(limits = c(ymd("2021-10-05"),ymd("2021-10-15")))+
  labs(title="Total # Fish")






