#related library
library(ggplot2)
library(ggthemr)
library(extrafont)
library(plyr)
loadfonts(device = "pdf")
ggthemr("earth")
ggthemr_reset()

#related function
#Sumation sales by weekdays 
#the data is selected by specific goodscode 

daymeanSum <- function(broad_dpGoods,order_dpGoods){
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="월요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("days"="월요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  day_mean_sale_dpGoods<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  
  
  
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="화요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("days"="화요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  x<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  day_mean_sale_dpGoods<- rbind(day_mean_sale_dpGoods,x)
  
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="수요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("days"="수요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  x<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  day_mean_sale_dpGoods<- rbind(day_mean_sale_dpGoods,x)
  
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="목요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("days"="목요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  x<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  day_mean_sale_dpGoods<- rbind(day_mean_sale_dpGoods,x)
  
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="금요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("days"="금요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  x<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  day_mean_sale_dpGoods<- rbind(day_mean_sale_dpGoods,x)
  
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="토요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("day"="토요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  x<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  day_mean_sale_dpGoods<- rbind(day_mean_sale_dpGoods,x)
  
  day_broard_dpGoods<- broad_dpGoods[broad_dpGoods$days=="일요일",]
  day_sale_dpGoods.temp<-join(day_broard_dpGoods,order_dpGoods,type="inner") 
  
  s<-colSums(day_sale_dpGoods.temp[,5:16])/nrow(day_sale_dpGoods.temp)
  a<-list("days"=day_sale_dpGoods.temp$days[1],"GOODS_CODE"=day_sale_dpGoods.temp$GOODS_CODE[1])
  if(is.na(a$days))
  {
    a <- list("days"="일요일","GOODS_CODE"=order_dpGoods$GOODS_CODE[1])
    s[1:12] <-0
  }
  x<- cbind(as.data.frame.list(a),as.data.frame.list(s))
  day_mean_sale_dpGoods<- rbind(day_mean_sale_dpGoods,x)
  
  day_mean_sale_dpGoods
}

# group by weekdays and apply mean 

weekday_mean_data <- function(df_broadMedia,df_orderGoods,goods_code){
  df_broadMedia <- subset(df_broadMedia,select=c('BD_DATE','days','GATHER_DATE','GOODS_CODE'))
  broad_dpGoods<- df_broadMedia[df_broadMedia$GOODS_CODE==goods_code,] 
  
  df_orderGoods<- subset(df_orderGoods,select = c("GATHER_DATE","ORDER_AMT","ORDER_QTY","CANCEL_AMT","CANCEL_QTY","CLAIM_AMT","CLAIM_QTY",
                                                  "CLAIM_CAN_AMT","CLAIM_CAN_QTY","NETORDER_AMT","NETORDER_QTY","NETSALE_AMT","NETSALE_QTY",'GOODS_CODE'))
  order_dpGoods<- df_orderGoods[df_orderGoods$GOODS_CODE==goods_code,]
  head(order_dpGoods)
  weekdaysales<-daymeanSum(broad_dpGoods,order_dpGoods)
  
  na <- colnames(weekdaysales)[3:14]
  bna<- c("ORDER","ORDER","CANCEL","CANCEL","CLAIM","CLAIM","CLAIM_CAN","CLAIM_CAN","NETORDER","NETORDER","NETSALE","NETSALE")
  len<- dim(weekdaysales)[1]
  co <- dim(weekdaysales)[2]
  
  for(i in 1:(co-2)){
    if(i == 1){
      cate <- data.frame(WEEK_DAY =weekdaysales$days,cate=rep(na[1],len),sale=as.integer(weekdaysales[,i+2]/1000),bcate=rep(bna[1],len),scate=rep("AMT",len))
      rownames(cate)<- 1:len
    }
    else{
      if(i%%2==0)
      {
        temp <- data.frame(WEEK_DAY =weekdaysales$days,cate=rep(na[i],len),sale=as.integer(weekdaysales[,i+2]),bcate=rep(bna[i],len),scate=rep("QTY",len))
        rownames(temp)<-1:len
        
      }else{
        temp <- data.frame(WEEK_DAY =weekdaysales$days,cate=rep(na[i],len),sale=as.integer(weekdaysales[,i+2]/1000),bcate=rep(bna[i],len),scate=rep("AMT",len))
        rownames(temp)<-1:len
      }
      
      cate <- rbind(cate,temp)
    }
  }
  cate
}

# Draw Bar plot about money of each category(forexample, AMT.SALES,REFUND etc...)
weekdayplot<- function(z,cate){
  #Enday<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Sunday")
  c <- z[z$bcate==cate,]
  
  p<- ggplot(c,aes(x=WEEK_DAY,y=sale))
  p<- p+geom_bar(width = 0.8, stat="identity")#,position = "dodge"
  p<- p+ggtitle("Weekdays Sales")+theme(plot.title=element_text(size=25,face="bold",family = "NanumGothic",margin=margin(10,0,10,0)))
  p<-p+ylab("Sales(1,000 WON)")+theme(axis.title.x=element_blank(),axis.text.x = element_text(size=15,face="bold",family = "NanumGothic"))
  p<- p+theme(axis.title.y = element_text(size=13,face="bold",family = "NanumGothic"),axis.text.y=element_text(size=13,face="bold",family = "NanumGothic"))
  p <- p+geom_text(aes(label=round(sale,2),vjust=-0.5),position=position_dodge(width=0.8),family="NanumGothic",fontface="bold",size=4)
  p<- p+theme(legend.title = element_blank(),legend.text=element_text(size=12,face="bold",family = "NanumGothic"))
  p
  
}

# Draw Bar plot about count of each category(forexample, AMT.SALES,REFUND etc...)
weekdayQTYplot<- function(z,cate){
  #Enday<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Sunday")
  c <- z[z$bcate==cate,]
  
  p<- ggplot(c,aes(x=WEEK_DAY,y=sale))
  p<- p+geom_bar(width = 0.8, stat="identity")#,position = "dodge"
  p<- p+ggtitle("Weekdays Sales")+theme(plot.title=element_text(size=25,face="bold",family = "NanumGothic",margin=margin(10,0,10,0)))
  p<-p+ylab("Order count")+theme(axis.title.x=element_blank(),axis.text.x = element_text(size=15,face="bold",family = "NanumGothic"))
  p<- p+theme(axis.title.y = element_text(size=13,face="bold",family = "NanumGothic"),axis.text.y=element_text(size=13,face="bold",family = "NanumGothic"))
  p <- p+geom_text(aes(label=round(sale,2),vjust=-0.5),position=position_dodge(width=0.8),family="NanumGothic",fontface="bold",size=4)
  p<- p+theme(legend.title = element_blank(),legend.text=element_text(size=12,face="bold",family = "NanumGothic"))
  p
  
}

#FOR web publishing(shiny) sever 
Forshinyweekplot <- function(df_broadMedia,df_orderGoods,goods_code,cate){
  z<- weekday_mean_data(df_broadMedia,df_orderGoods,goods_code)
  z <- z[z$scate=="AMT",]
  weekdayplot(z,cate)
}
ForshinyQTYweekplot <- function(df_broadMedia,df_orderGoods,goods_code,cate){
  z<- weekday_mean_data(df_broadMedia,df_orderGoods,goods_code)
  z <- z[z$scate=="QTY",]
  weekdayQTYplot(z,cate)
}