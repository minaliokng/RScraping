library(taskscheduleR)

setwd("C:/scraping")
scraping.schedule = file.path('C:/scraping/scraping.R')

## 지금으로부터 1일뒤마다 매일 같은 시간에 실행
taskscheduler_create(taskname = 'scraping_task', rscript = scraping.schedule,
                     schedule = 'DAILY',
                     starttime = format(Sys.time() + 1, '%H:%M'),
                     startdate = format(Sys.time(), '%Y/%d/%m'),
                     modifier = 1)

taskscheduler_delete('scraping_task') # 스케쥴 삭제
