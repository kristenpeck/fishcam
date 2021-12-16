
# Copy selected video files over to another location

#Author: Kristen Peck, DFO

library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")

#select the files that you want to be copied
getwd()  
setwd("C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/fishcam")

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

select_rows <- cam.data1 %>% 
  filter(BTDV >= 1) %>% 
  select(files)

#paste the drive letter in front of the filepaths in the source drive
(select_files <- paste0("F:/",select_rows$files))



# copy to folder on a USB stick 

file.copy(from=select_files,
          to="E:/copiedvideo-BTDV")



#randomly select videos for QA - need to do this one harddrive at a time

sample <- sample(nrow(cam.data2),round(nrow(cam.data2)*0.01,0), 
                 replace = F)

select_rows <- cam.data2[sample,]

(select_files <- paste0("E:/",select_rows$files))

file.copy(from=select_files,
          to="C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/fishcam/QA/QA2",
          copy.date = T)

write_csv(select_rows, "QA/QA1.csv",na = "")

#
# cam.data2$filenames <- substr(cam.data2$files,20,60)
# setwd("C:/Users/Peckk/OneDrive - DFO-MPO/Documents/R/fishcam/QA/QA2")
# files <- list.files(path = ".",pattern="*.m4v",recursive = T) 
# files <- data.frame(filenames=files) %>% 
#   left_join(cam.data2)
# 
# write_csv(files, "QA2.csv",na = "")
# getwd()
