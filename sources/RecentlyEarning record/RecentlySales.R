#Related Libraries
library(plyr)
library(ggplot2)
library(ggthemr)  #devtools::install_github('cttobin/ggthemr')
library(extrafont)
loadfonts(device = "win")
ggthemr("earth")
library(ggrepel)

#Related Functions

#Select  latest sales data until "curNum". 
#The data is about specific goods ("goods_code")

latestBroadData<- function(df_orderList,df_broadMedia,goods_code,curNum){
  library(plyr)
  goods_data <- subset(df_orderList,GOODS_CODE==goods_code)
  deepbroadMedia<- subset(df_broadMedia,GOODS_CODE==goods_code)
  if(curNum>nrow(deepbroadMedia))
  {
    curNum <- nrow(ordered_data)
  }
  
  latestbroad<- deepbroadMedia[order(deepbroadMedia$dt,deepbroadMedia$BD_BTIME,decreasing = TRUE),][1:curNum,] #nrow(deepbroadMedia) range
  latestbroad <-subset(latestbroad,select=c('BD_BTIME','dt','GOODS_CODE')) 
  broadorder <- join(latestbroad,goods_data,type="left")
  broadorder<- subset(broadorder,select=c('BD_BTIME','GOODS_CODE'))
  latestbroad_order<- as.data.frame(table(broadorder))
  latestbroad_order$BD_BTIME <- as.POSIXct(latestbroad_order$BD_BTIME)
  latestbroad_order
}


# Draw the line plot with data 

latestplotline <- function(c){
  c<- c[order(c$BD_BTIME),]
  c$rec <- 1:dim(c)[1]
  
  plot_data <<- c
  
  p <- ggplot(plot_data,aes(x=rec,y=Freq,colour=factor(GOODS_CODE)))
  p<- p+geom_line(size=1.3)+geom_point()+xlab("OLD --------------------------------> LATEST")+ ylab("Count")+ggtitle("Latest Sales Record")
  p<- p+scale_colour_discrete(name="GOODS_CODE")
  #p<- p+scale_colour_brewer(palette = "Dark2")
  p<- p+geom_label_repel(fontface="bold",aes(label=format(plot_data$BD_BTIME,"%Y-%m-%d \n %H:%M" ),colour=factor(GOODS_CODE)),
                         size=4,nudge_x = 0.15, segment.color = "grey")
  p <-p+theme(axis.title.x=element_text(margin = margin(10,0,10,0),family = "NanumGothic",size=18,face="bold"),
              axis.title.y=element_text(margin = margin(0,10,0,10),family="NanumGothic",size=18,face="bold"),axis.text.y= element_text(size=18,family = "NanumGothic"))
  p <-p+theme(legend.title=element_text(size=13,family='NanumGothic'),legend.text=element_text(size=13,family = "NanumGothic",face="bold"))
  p <-p+theme(plot.title=element_text(size=23,face="bold",family="NanumGothic",margin = margin(10,0,10,0)))
  p <-p+theme(panel.grid.major.y=element_line(size=0.3,linetype = "solid",colour = "grey"))
  p
}


# Add line on original plot 

adlatestplotline <- function(c){
  c<- c[order(c$BD_BTIME),]
  c$rec <- 1:dim(c)[1]
  
  plot_data <<- rbind(plot_data,c)
  
  p <- ggplot(plot_data,aes(x=rec,y=Freq,colour=factor(GOODS_CODE)))
  p<- p+geom_line(size=1.3)+geom_point()+xlab("OLD --------------------------------> LATEST")+ ylab("Count")+ggtitle("Latest Sales Record")
  p<- p+scale_colour_discrete(name="GOODS_CODE")
  #p<- p+scale_colour_brewer(palette = "Dark2")
  p<- p+geom_label_repel(fontface="bold",aes(label=format(plot_data$BD_BTIME,"%Y-%m-%d \n %H:%M" ),colour=factor(GOODS_CODE)),
                         size=4,nudge_x = 0.15, segment.color = "grey")
  p <-p+theme(axis.title.x=element_text(margin = margin(10,0,10,0),family = "NanumGothic",size=18,face="bold"),
              axis.title.y=element_text(margin = margin(0,10,0,10),family="NanumGothic",size=18,face="bold"),axis.text.y= element_text(size=18,family = "NanumGothic"))
  p <-p+theme(legend.title=element_text(size=13,family='NanumGothic'),legend.text=element_text(size=13,family = "NanumGothic",face="bold"))
  p <-p+theme(plot.title=element_text(size=23,face="bold",family="NanumGothic",margin = margin(10,0,10,0)))
  p <-p+theme(panel.grid.major.y=element_line(size=0.3,linetype = "solid",colour = "grey"))
  p
}

#Function for web Publishing server 

ForshinylatestLineFirst<- function(df_orderList,df_broadMedia,goods_code,curNum){
  c<-latestBroadData(df_orderList,df_broadMedia,goods_code,curNum)
  latestplotline(c)
}
ForshinylatestLineAdd<- function(df_orderList,df_broadMedia,goods_code,curNum){
  c<-latestBroadData(df_orderList,df_broadMedia,goods_code,curNum)
  adlatestplotline(c)
}