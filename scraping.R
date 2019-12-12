require(rvest)
# 날짜를 다루는 패키지
require(lubridate)

url="https://www.twitchmetrics.net/channels/"
page=1:4

folder='C:/scraping/data' #웹페이지 데이터 저장될 폴더 경로
if(!dir.exists(folder)) dir.create(folder) #없으면 생성

setwd(folder) #워킹 디렉토리 변경

# 오늘 날짜
date <- Sys.Date()

# 현재 시간
h <- hour(Sys.time())
m <- minute(Sys.time())

# '현재 시간' 폴더 이름 
now <- paste(date, h, m, sep='-')
now.folder <- paste(folder, now, sep='/')

thema = c('viewership', 'growth', 'peak', 'popularity', 'follower')

# 폴더 생성   
if(!dir.exists(now.folder)) dir.create(now.folder)
subF=paste(now.folder, thema, sep='/')
for(i in 1:5){
  if(!dir.exists(subF[i])) dir.create(subF[i])
}

setwd(now.folder) #시간별로 폴더 관리하기 위함

file.name <- paste0('page', page, '.html')

for(i in 1:5){
  subUrl=paste(url, thema[i], sep='/')
  subUrl=paste(subUrl, 'page=', sep='?')
  pages=paste0(subUrl, page, sep='')
  
  setwd(subF[i])
  
  for(j in page){
    file=read_html(pages[j])
    write_xml(file, file=file.name[j])
  }
}

#---------------파일 준비 끝------------------

library(RSelenium)

mwName = c()
mwVh = c()

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4446L,
  browserName = "chrome"
)

url = now.folder

remDr$open()

sn=list(c(), c(), c(), c(), c())
sf=list(c(), c(), c(), c(), c())
sg=list(c(), c(), c(), c(), c())
ss=list(c(), c(), c(), c(), c())

for(i in 1:5){ #i는 테마
  url.now=paste(now.folder,thema[i], sep='/')
  filename=paste(url.now, file.name, sep="/")
  
  for(j in page){ #j는 페이지수
    remDr$navigate(filename[j])
    
    nameL=remDr$findElements(using="css selector", "h5.mr-2.mb-0")
    nameList=unlist(lapply(nameL, function(x) {x$getElementText()}))
    
    fromL=remDr$findElements(using="css selector", "div.mr-3")
    fL=unlist(lapply(fromL, function(x) {x$getElementText()}))
    fromList=fL[seq(from = 1, to = 100, by = 2)]
    genreList=fL[seq(from=2, to=100, by=2)]
    
    scoreL=remDr$findElements(using="css selector", "samp")
    scoreList=unlist(lapply(scoreL, function(x) {x$getElementText()}))
    
    sn[[i]]=c(sn[[i]], nameList)
    sf[[i]]=c(sf[[i]], fromList)
    sg[[i]]=c(sg[[i]], genreList)
    ss[[i]]=c(ss[[i]], scoreList)
  }
}
#for문 다 돌고나면 sn,sf,ss에 데이터 저장이 끝남

remDr$close()

#숫자표현에 포함된 컴마 삭제하고 숫자로 바꾸어 저장
for(i in 1:5){
  ss[[i]]=gsub(",", "", ss[[i]])
  ss[[i]]=as.numeric(ss[[1]])
}

#cbind(sn[[1]],sf[[1]], sg[[1]],ss[[1]]) #viewership
#cbind(sn[[2]],sf[[2]], sg[[2]],ss[[2]]) #growth
#cbind(sn[[3]],sf[[3]], sg[[3]],ss[[3]]) #peak
#cbind(sn[[4]],sf[[4]], sg[[4]],ss[[4]]) #popularity
#cbind(sn[[5]],sf[[5]], sg[[5]],ss[[5]]) #follower

library(ggplot2)

#그래프에 나타나는 숫자 표현 옵션을 변경
options("scipen" = 100)

#총 시청 시간 상위 5명
top.vs=data.frame(sn[[1]][1:5], ss[[1]][1:5])
colnames(top.vs)=c('name', 'viewer_hours')
ggplot(top.vs, aes(x=name, y=viewer_hours))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c(sn[[1]][1],sn[[1]][2],sn[[1]][3],sn[[1]][4],sn[[1]][5]))+
  ggtitle("시청 시간 Top 5")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))+
  geom_text(aes(label=viewer_hours), vjust=-0.2)

#급성장 상위 5명
top.gr=data.frame(sn[[2]][1:5], ss[[2]][1:5])
colnames(top.gr)=c('name', 'followe_change')
ggplot(top.gr, aes(x=name, y=followe_change))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c(sn[[2]][1],sn[[2]][2],sn[[2]][3],sn[[2]][4],sn[[2]][5]))+
  ggtitle("급성장 Top 5")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))+
  geom_text(aes(label=followe_change), vjust=-0.2)

#순간 최고 시청자 상위 5명
top.pk=data.frame(sn[[3]][1:5], ss[[3]][1:5])
colnames(top.pk)=c('name', 'peak_viewers')
ggplot(top.pk, aes(x=name, y=peak_viewers))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c(sn[[3]][1],sn[[3]][2],sn[[3]][3],sn[[3]][4],sn[[3]][5]))+
  ggtitle("순간 최고 시청자 Top 5")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))+
  geom_text(aes(label=peak_viewers), vjust=-0.2)

#평균 동시 시청자 상위 5명
top.pp=data.frame(sn[[4]][1:5], ss[[4]][1:5])
colnames(top.pp)=c('name', 'popularity')
ggplot(top.pp, aes(x=name, y=popularity))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c(sn[[4]][1],sn[[4]][2],sn[[4]][3],sn[[4]][4],sn[[4]][5]))+
  ggtitle("평균 동시 시청자 Top 5")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))+
  geom_text(aes(label=popularity), vjust=-0.2)

#전체 팔로워 상위 5명
top.fl=data.frame(sn[[5]][1:5], ss[[5]][1:5])
colnames(top.fl)=c('name', 'follower')
ggplot(top.fl, aes(x=name, y=follower))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c(sn[[5]][1],sn[[5]][2],sn[[5]][3],sn[[5]][4],sn[[5]][5]))+
  ggtitle("전체 팔로워 Top 5")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))+
  geom_text(aes(label=follower), vjust=-0.2)

#install.packages("plotly")
library(plotly)

#시청 순위별 원형 그래프

#viewership 국가 분ㅍ
c1=data.frame(table(sf[[1]]))
colnames(c1)=c('contry', 'freq')

plot_ly(c1, labels=~c1$contry, values=~c1$freq, type='pie', textinfo='label+percent') %>%
  layout(title='시청시간 200위 국가 분포',
         xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
         yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

#growth 국가 분포
c2=data.frame(table(sf[[2]]))
colnames(c2)=c('contry', 'freq')

plot_ly(c2, labels=~c2$contry, values=~c2$freq, type='pie', textinfo='label+percent') %>%
  layout(title='급성장 200위 국가 분포',
         xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
         yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

#peak
c3=data.frame(table(sf[[3]]))
colnames(c3)=c('contry', 'freq')

plot_ly(c3, labels=~c3$contry, values=~c3$freq, type='pie', textinfo='label+percent') %>%
  layout(title='순간 시청 200위 국가 분포',
         xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
         yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

#popularity
c4=data.frame(table(sf[[4]]))
colnames(c4)=c('contry', 'freq')

plot_ly(c4, labels=~c4$contry, values=~c4$freq, type='pie', textinfo='label+percent') %>%
  layout(title='평균 동시 시청 200위 국가 분포',
         xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
         yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

#follower
c5=data.frame(table(sf[[5]]))
colnames(c5)=c('contry', 'freq')

plot_ly(c5, labels=~c5$contry, values=~c5$freq, type='pie', textinfo='label+percent') %>%
  layout(title='팔로워수 200위 국가 분포',
         xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
         yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))



vc=c()
lc=c()
fc=c()
#테마별 방송 장르 순위그래프 비교
for(i in 1:5){
  dat=data.frame(sort(table(sg[[i]]), decreasing = T))
  
  vc=c(vc, dat$Freq[which(dat$Var1=='Variety')])
  lc=c(lc, dat$Freq[which(dat$Var1=='League of Legends')])
  fc=c(fc, dat$Freq[which(dat$Var1=='Fortnite')])
}

genre=data.frame(thema, vc, lc, fc)

ggplot(genre, aes(x=thema, y=genre$fc, group=1))+
  geom_line(color='red', size=1)+
  scale_x_discrete(limits=thema)+
  geom_line(aes(x=thema, y=genre$vc), color='blue')+
  geom_line(aes(x=thema, y=genre$lc), color='green')+
  labs(title="인기 방송 태그 분포",
       subtitle="파란색 = 버라이어티
빨간색 = 포트나이트
연두색 = 리그 오브 레전드")

setwd(now.folder)

#현재 읽은 데이터를 이전 데이터로 덮어쓰기 해둠
write.csv(cbind(sn[[1]],sf[[1]],ss[[1]]), file="viewership.csv")
write.csv(cbind(sn[[2]],sf[[2]],ss[[2]]), file="growth.csv")
write.csv(cbind(sn[[3]],sf[[3]],ss[[3]]), file="peak.csv")
write.csv(cbind(sn[[4]],sf[[4]],ss[[4]]), file="popularity.csv")
write.csv(cbind(sn[[5]],sf[[5]],ss[[5]]), file="follower.csv")