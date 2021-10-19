
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



# read in manual data ####
manual.count.daily <- read_excel("Daily.tally.BABINE.xlsx") %>% 
  mutate(date = as.Date(date), method="manual") %>% 
  mutate(CO = lg.CO+jk.CO) %>% 
  select(-c(lg.CO,jk.CO))


#read in camera data ####

cam.data <- read_csv("2021-10-18data.dump.csv")

(daily.summary.cam <- cam.data %>% 
  mutate(date=ymd(date)) %>% 
  filter(date %in% c(ymd("2021-10-6"):ymd("2021-10-17"))) %>%
  filter(!is.na(Analyzer.signoff)) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(lg.SK = sum(Sock, na.rm=T), 
            jk.SK = sum(jackSock, na.rm=T),
            CO = sum(Coho, na.rm=T),
            PK=NA,
            lg.CH = sum(Chin, na.rm=T),
            jk.CH = sum(jackChin, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(Steelhead, na.rm=T),
            crew="DFO", 
            method="cam",
            chutes=paste(unique(trap),collapse = ",")))



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

stack.all<- rbind(manual.stacked, cam.stacked) 

stack.all.SKCO <- rbind(manual.stacked, cam.stacked) %>% 
  filter((species %in% c("CO","jk.SK","lg.SK")) ) %>% 
  arrange(date)

plot.cam.vs.man <- ggplot(stack.all.SKCO)+
  geom_line(aes(x=date, y=daily.count, col=species,linetype=method))+
  scale_x_date(limits=c(ymd("2021-10-05","2021-10-17")))+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))

plot.cam.vs.man

stack.all.sum  <- stack.all %>% 
  group_by(date,method) %>% 
  summarize(all.fish=sum(daily.count,na.rm=T))

ggplot(stack.all.sum)+
  geom_line(aes(x=date, y=all.fish, col=method))+
  scale_x_date(limits=c(ymd("2021-10-04","2021-10-12")))+
  geom_vline(aes(xintercept=ymd("2021-10-06")))+
  scale_y_continuous(limits=c(0,1000))+
  labs(y="total daily fish counted")

plot.cam.vs.man2 <- ggplot(stack.all.sum, aes(x=date, 
                                              y=all.fish, 
                                              fill = method)) +                           # ggplot2 with default settings
  geom_bar(stat = "identity")+
  scale_x_date(limits=c(ymd("2021-10-03","2021-10-17")),
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
  filter(date %in% ymd("2021-09-30"):ymd("2021-10-15")) %>%
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
  scale_x_date(limits=c(ymd("2021-07-15","2021-10-15")))+
  geom_vline(aes(xintercept=ymd("2021-10-06")))#+
  # scale_y_continuous(limits=c(0,2500))
plot.daily.salmon



#just looking at end of Sept-Oct
plot.daily.salmon.end <- ggplot(daily.sum.stacked.salmon)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=1)+
  scale_x_date(limits=c(ymd("2021-10-01","2021-10-15")),
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
  scale_x_date(limits=c(ymd("2021-08-15","2021-10-15")))+
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

