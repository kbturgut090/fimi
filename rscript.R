
#httr::set_config(httr::config(http_version = 0))
library(telegram.bot)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(grid)

df = readxl::read_excel('last_down.xlsx') %>% 
  select(ATM_ID,Address,everything()) %>% 
  select(ATM_ID,Address,Device,Elapsed_time)


#print(Sys.getenv('BOT_ID'))
# Initialize bot
bot <- Bot(token = Sys.getenv('BOT_ID'))

# Chat id
Baki_texniki <- -1001509736694

RIM_Texniki <- -1001639372523

zones = data.frame(zones=c('Baku','RIM'),id=c(Baki_texniki,RIM_Texniki))

for ( i in 1:nrow(zones)) {
  cur_zone = zones$zones[i]
  if(cur_zone=='Baku') {
    df_ = df %>% filter(stringr::str_starts(ATM_ID,'A0100'))
  } else {
    df_ = df %>% filter(!stringr::str_starts(ATM_ID,'A0100'))
  }
  
  png(paste(cur_zone,'.png',sep = ''), height = 30*nrow(df_), width = 200*ncol(df_))
  
  myTable <- tableGrob(df_, rows = NULL)
  
  
  
  grid.draw(myTable)
  dev.off()
  
  # Send photo
  bot$sendPhoto(zones$id[i],
                photo = paste(cur_zone,'.png',sep = '')
  )
  print(i)
  
}





