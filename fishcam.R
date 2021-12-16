# This script is used to dump the metadata from the motion-detecting 
#   video files to a csv before entering data to cut down on time
# Also appends new empty metadata to completed video file

#Author: Kristen Peck, 11 Oct 2021

#load packages
library("fs")
library("tidyverse")
library("lubridate")
library("readxl")

# check that you are in the correct working directory 
#  (should be /Volumes/Fish Drive)
getwd()
setwd("E:/")

#extract file names and info from video files (anything with .m4v extension)
files <- list.files(path = ".",pattern="*.m4v",recursive = T)

#filter for motion videos only
videodata <- data.frame(files) %>% 
  mutate(trap = substr(files,1,7), date = substr(files,9,18),
         type= ifelse(grepl("\\M",files),"M","C")) %>% 
  filter(type %in% "M", trap != "Sbear c") %>% 
  mutate(starttime = ymd_hms(paste(date,substr(files,31,38)),
                             tz = "GMT")) %>% #need to put as GMT because otherwise excel will screw it up
  mutate(Sock=NA,jackSock=NA,Coho=NA,BTDV=NA,Steelhead=NA,Rainbow=NA,
         Whitefish=NA, Sucker=NA,Chin=NA,jackChin=NA,PK=NA,Other=NA,Comments=NA,
         Analyzer.signoff=NA, chute.open=NA) 
str(videodata)

#filter for continuous videos only
videodata.c <- data.frame(files) %>% 
  mutate(trap = substr(files,1,7), date = substr(files,9,18),
         type= ifelse(grepl("\\M",files),"M","C"),
         hour = substr(files,31,32)) %>% 
  filter(type %in% "C", trap != "Sbear c") 
str(videodata.c)

range(videodata.c$date)
write_csv(videodata.c,
          "C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/fishcam/videodata.c_5-Oct_to_4-Nov.csv")

#reading in completed analysis sheet

completed <- read_csv("2021VideoData.csv",col_types=list(Sock=col_integer(),
                                                         jackSock=col_integer(),
                                                         Coho=col_integer(),
                                                         BTDV=col_integer(),
                                                         Steelhead=col_integer(),
                                                         Rainbow=col_integer(),
                                                         Whitefish=col_integer(), 
                                                         Sucker=col_integer(),
                                                         Chin=col_integer(),
                                                         jackChin=col_integer(),
                                                         PK=col_integer(),
                                                         Other=col_integer())) %>%
  mutate(date=dmy(date)) %>% 
  filter(!is.na(Analyzer.signoff)) #make sure this field is filled out!!!

uniq <- unique(completed$files)

not.completed <- videodata %>% 
  filter(!(files %in% uniq)) #this is filtering for any new files not already analyzed

#append old rows to empty rows
new.file <- rbind(completed, not.completed)

write_csv(new.file, "2021VideoData.csv",na = "")

