
# this script generates a report of manual and camera counts

library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")


#extract prior babine data

# dates.babine <- excel_sheets("Final Copy of BabineFence_DailyCounts&Summary_2021.xlsx")
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
#    tmp <- read_excel("Final Copy of BabineFence_DailyCounts&Summary_2021.xlsx", 
#             sheet = i+1,range = "D18:K18",col_names = F)
#    tmp2 <- t(read_excel("Final Copy of BabineFence_DailyCounts&Summary_2021.xlsx", 
#                      sheet = i+1,range = "D21:D23",col_names = F))
#    tmp3 <- t(read_excel("Final Copy of BabineFence_DailyCounts&Summary_2021.xlsx", 
#                         sheet = i+1,range = "G21:G23",col_names = F))
#    summer.babine[i,2:9] <- tmp
#    summer.babine[i,14:16] <-tmp2
#    summer.babine[i,18:20] <-tmp3
#  }
#  head(summer.babine)
#  
#  write_csv(summer.babine, "Daily.tally.BABINE.csv")



# read in manual data ####
manual.count.daily <- read_excel("Daily.tally.BABINE.xlsx") %>% 
  mutate(date = as.Date(date), method="manual") %>% 
  mutate(CO = lg.CO+jk.CO) %>% 
  select(c(date,lg.SK,jk.SK,CO,PK,lg.CH,jk.CH,ST,BTDV,crew,chutes,method))
str(manual.count.daily)

#read in camera data ####
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
              mutate(date = dmy(date))
str(cam.data1)

cam.data2 <- read_csv("2021-11-15data.dump.csv",
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
              mutate(chute.open = "Y")
str(cam.data2)

cam.data <- rbind(cam.data1,cam.data2)
str(cam.data)

#Short QA# 

range(cam.data$date)

qa <- cam.data %>% 
  filter(chute.open %in% "Y") %>% 
  mutate(hr = hour(starttime)) 

ggplot(qa)+
  geom_histogram(aes(x=hr, fill=trap),binwidth=1, position = "dodge")

#plot each chute over time
unique(qa$trap)
chute3 <- qa %>% 
  filter(trap %in% c("Chute 3"))
chute4 <- qa %>% 
  filter(trap %in% c("Chute 4"))
chute5 <- qa %>% 
  filter(trap %in% c("Chute 5"))
chute6 <- qa %>% 
  filter(trap %in% c("Chute 6"))

ggplot(chute3)+
  geom_histogram(aes(x=starttime,fill=trap), binwidth=60*60)
ggplot(chute4)+
  geom_histogram(aes(x=starttime,fill=trap), binwidth=60*60)
ggplot(chute5)+
  geom_histogram(aes(x=starttime,fill=trap), binwidth=60*60)
ggplot(chute6)+
  geom_histogram(aes(x=starttime,fill=trap), binwidth=60*60)

#reporting

cam.date.range <- c(ymd("2021-10-06"):ymd("2021-11-14"))
plot.date.limit <- c(ymd("2021-10-03","2021-11-14"))
table.date.range <- c(ymd("2021-10-20"):ymd("2021-11-14"))

(daily.summary.cam <- cam.data %>% 
  mutate(date=ymd(date)) %>% 
  filter(date %in% cam.date.range) %>%
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
            crew="DFO", 
            method="cam",
            chutes=paste(unique(trap),collapse = ",")) %>% 
    mutate(date=ymd(date)) )

(daily.summary.cam.BC <- cam.data %>% 
    mutate(date=ymd(date)) %>% 
    filter(date %in% c(ymd("2021-10-06"):ymd("2021-11-14"))) %>%
    filter(!is.na(Analyzer.signoff)) %>% 
    filter(chute.open %in% "Y") %>% 
    group_by(date) %>% 
    summarize(BTDV = sum(BTDV, na.rm=T),
              ST = sum(Steelhead, na.rm=T),
              RBCT = sum(Rainbow,na.rm=T),
              MW = sum(Whitefish, na.rm=T),
              SU = sum(Sucker, na.rm=T),
              crew="DFO", 
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

#Compare Cam vs. Manual ####

#columns separate
sep.daily <- manual.count.daily %>% 
  left_join(daily.summary.cam, by="date",suffix=c(".m",".c"))

manual.stacked <- manual.count.daily %>% 
  select(-c(crew,method,chutes)) %>% 
  gather("species","daily.count",-date) %>% 
  mutate(method="manual")
cam.stacked <- daily.summary.cam %>% 
  select(-c(crew,method,chutes)) %>% 
  gather("species","daily.count",-date) %>% 
  mutate(method="cam")

manual.stacked$daily.count

stack.all<- rbind(manual.stacked, cam.stacked) 

stack.all.SKCO <- rbind(manual.stacked, cam.stacked) %>% 
  filter((species %in% c("CO","jk.SK","lg.SK")) ) %>% 
  arrange(date)

plot.cam.vs.man <- ggplot(stack.all.SKCO)+
  geom_line(aes(x=date, y=daily.count, col=species,linetype=method))+
  scale_x_date(limits=plot.date.limit)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))

plot.cam.vs.man

stack.all.sum  <- stack.all %>% 
  group_by(date,method) %>% 
  summarize(all.fish=sum(daily.count,na.rm=T))

ggplot(stack.all.sum)+
  geom_line(aes(x=date, y=all.fish, col=method))+
  scale_x_date(limits=plot.date.limit)+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))+
  labs(y="total daily fish counted")

plot.cam.vs.man2 <- ggplot(stack.all.sum, aes(x=date, 
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


#combine into one df ####

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
            crew= paste(unique(crew),collapse=","),
            method= paste(unique(method),collapse=","),
            chutes=paste(unique(chutes), collapse=","))

#summary table ####
table.end <- all.daily %>% 
  filter(date %in% table.date.range) %>%
  mutate(date=format(date,"%d-%b-%y")) %>% 
  select(Date=date,`Large SK`=lg.SK,`Jack SK`=jk.SK,
         CO,PK,`Large CH`=lg.CH,`Jack CH`=jk.CH,`BT/DV`=BTDV,
         ST,method)
table.end



#export daily counts 

write_csv(all.daily, "Daily.counts.all.Babine.csv")



# plots, daily counts - all spp ####

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
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  scale_x_date(limits=c(ymd("2021-08-15"),plot.date.limit[2]))+
  geom_vline(aes(xintercept=ymd("2021-10-06")))#+
# scale_y_continuous(limits=c(0,2500))
plot.daily.coho

#cumulative sum# 

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






#extract hourly pattern from data 
# NOTE: doesn't make sense with cams not running all night
#   so should code in another column for 24 hr cams
(hourly.summary.cam <- cam.data %>% 
  mutate(hour=hour(starttime)) %>% 
  filter(date %in% c(ymd("2021-10-7"):ymd("2021-10-12"))) %>% 
  filter(trap %in% c("Chute 5","Chute 6")) %>% 
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

