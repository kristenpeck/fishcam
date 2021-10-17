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

#extract file names and info from video files (anything with .m4v extension)
files <- list.files(path = ".",pattern="*.m4v",recursive = T)

videodata <- data.frame(files) %>% 
  mutate(trap = substr(files,1,7), date = substr(files,9,18),
         type= ifelse(grepl("\\M",files),"M","C")) %>% 
  filter(type %in% "M", trap != "Sbear c") %>% 
  mutate(starttime = ymd_hms(paste(date,substr(files,31,38)),
                             tz = "GMT")) %>% #need to put as GMT because otherwise excel will screw it up
  mutate(Sock=NA,jackSock=NA,Coho=NA,BTDV=NA,Steelhead=NA,Rainbow=NA,
         Whitefish=NA, Sucker=NA,Chin=NA,jackChin=NA,Other=NA,Comments=NA,
         Analyzer.signoff=NA) 

#reading in completed analysis sheet

completed <- read_excel("2021-10-12data.dump.xlsx") %>% 
  filter(!is.na(Analyzer.signoff))
# double-check that this field (Analyzer.signoff) is 
# filled out before running overwrite script!!!
uniq <- unique(completed$files)

addit.data <- videodata %>% 
  filter(!(files %in% uniq)) 
#this is filtering for any new files not already analyzed

rewrite <- rbind(completed, addit.data)

write_csv(rewrite, paste0(Sys.Date(),"testing.csv"),na = "")

