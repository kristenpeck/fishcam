
# this script generates a report of manual and camera counts

library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")

#read in camera data file

cam.data <- read_csv("2021-10-12data.dump.csv")

names(cam.data)

daily.summary.cam <- cam.data %>% 
  filter(date %in% c(ymd("2021-10-7"):ymd("2021-10-11"))) %>% 
  group_by(date) %>% 
  summarize(Sx = sum(Sock, na.rm=T), 
            SxJk = sum(jackSock, na.rm=T),
            Co = sum(Coho, na.rm=T),
            Ck = sum(Chin, na.rm=T),
            CkJk = sum(jackChin, na.rm=T),
            BTDV = sum(BTDV, na.rm=T),
            ST = sum(Steelhead, na.rm=T))

daily.summary.cam

sum.stacked <- daily.summary.cam %>% 
  gather("species","daily.count",-date)

ggplot(sum.stacked)+
  geom_line(aes(x=date, y=daily.count, colour=species), size=2)

