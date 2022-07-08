
#httr::set_config(httr::config(http_version = 0))
library(telegram.bot)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(grid)

df = readxl::read_excel('last_down.xlsx') %>% 
  select(ATM_ID,Address,everything()) %>% 
  select(ATM_ID,Address,Device,Elapsed_time) %>% 
  filter(Elapsed_time>=15)


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

qruplar = readxl::read_excel('chat_id.xlsx')

unique_chats = unique(qruplar$Responsible_ID)

options(scipen = 9999)

for (j in 1:length(unique_chats)) {
  df_ = semi_join(df,qruplar %>% filter(Responsible_ID==unique_chats[j])) 
  cur_zone = as.numeric(unique_chats[j])
  reg_name = paste(letters[1:5],collapse = '')
  print(cur_zone)
  if(nrow(df_)>0){
    if(nrow(df_)<=3) {
      png(paste(reg_name,'.png',sep = ''), height = 100*nrow(df_), width = 200*ncol(df_))
    } else {
      png(paste(reg_name,'.png',sep = ''), height = 30*nrow(df_), width = 200*ncol(df_))
    }
    
    myTable <- tableGrob(df_, rows = NULL)
    
    
    
    grid.draw(myTable)
    dev.off()
    
    # Send photo
    bot$sendPhoto(cur_zone,
                  photo = paste(reg_name,'.png',sep = '')
    )
    print(j)
  }
}


terminal_ = readxl::read_excel('\\\\10.0.6.184\\nt\\Daily_FIMI\\terminal\\last_problem.xlsx')

terminal_ %>% transmute(ATM_ID = stringr::str_extract(Point,'\\(.*\\)') %>% 
                          gsub('\\(|\\)','',.),
                        Address = trimws(gsub('\\(.*\\)','',Point)),
                        Bill_acceptor_status,
                        Printer_status,
                        Card_module,
                        color,
                        status,
                        
                        Elapsed_time = Elapsed_time) %>% 
  tidyr::gather(-ATM_ID,-Address, -Elapsed_time,key=Device,value=value) %>% 
  filter(!value %in% c('OK','Works'), !is.na(value)) %>% arrange(ATM_ID) #-> terminal_



