
# Copy selected video files over to another location

#Author: Kristen Peck, DFO

library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")

#select the files that you want to be copied
getwd()  
#setwd("C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/fishcam")

#read in camera data for first Fish Drive harddrive ####
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




#read in camera data for second Fish Drive harddrive ####
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
  mutate(chute.open = "Y")
str(cam.data2)


#select the data you want to extract. 
# Note that you have to do this one harddrive at a time

select_rows <- cam.data2 %>% 
  filter(Rainbow >= 1) %>% 
  select(files)

(select_files <- paste0("F:/",select_rows$files))

#select_files <- paste0("F:/",c("Chute 3/2021-11-04/04-11-2021 09-15-48 M Chute 3.m4v",
#                  "Chute 3/2021-11-04/04-11-2021 09-16-29 M Chute 3.m4v"))




# copy to folder on a USB stick 

file.copy(from=select_files,
          to="E:/copiedvideo-RBCT")


