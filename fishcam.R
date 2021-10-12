
library("fs")
library("tidyverse")
library("lubridate")
library("readxl")

getwd()

#extract file names and info
files <- list.files(path = ".",pattern="*.m4v",recursive = T)

donnees <- data.frame(files) %>% 
  mutate(trap = substr(files,1,7), date = substr(files,9,18),
         type= ifelse(grepl("\\M",files),"M","C")) %>% 
  filter(type %in% "M") %>% 
  mutate(starttime = ymd_hms(paste(date,substr(files,31,38)),
                                  tz = "GMT")) %>% #need to put as GMT because otherwise excel will screw it up
  mutate(Sock=NA,jackSock=NA,Coho=NA,BTDV=NA,Steelhead=NA,
         Whitefish=NA, Sucker=NA,Chin=NA,jackChin=NA,Other=NA,Comments=NA)
         
write_csv(donnees, "test.data.dump.csv",na = "")





