#Related Libraries
library(plyr)
library(ggplot2)
library(extrafont)
loadfonts(device = "pdf")
ggthemr("earth")

#Rleated Function

#select data Specific month and Specific Age group 

ForshinyMonthAgeGroupGoods<- function(df_orderList,df_goods,month,ag){
  df_monthorder<- df_orderList[format(df_orderList$ORDER_DATE,"%Y-%m")==month,]
  
  c_year <- as.integer(format(Sys.Date(),"%Y"))
  older<-c_year-as.integer(substring(df_monthorder$BIRTHDAY,1,4))
  df_monthorder$age <- older
  df_monthorder <- within(df_monthorder,{
    agegp=character(0)
    agegp[age>=10]='10s'
    agegp[age>=20]='20s'
    agegp[age>=30]='30s'
    agegp[age>=40]='40s'
    agegp[age>=50]='50s'
    agegp[age>=60]='60s'
    agegp[age>=70]='over 70s'
    agegp[age<10] ='less 10s'
    agegp[is.na(age)|age>=100]='unKnown'
  })
  
  df_spci_ag<- df_monthorder[df_monthorder$agegp==ag,]
  z<-data.frame(table(df_spci_ag$GOODS_CODE))
  colnames(z) <- c("GOODS_CODE","Freq")
  z$GOODS_CODE<- factor(z$GOODS_CODE,levels =z[order(z$Freq),]$GOODS_CODE)
  z<-z[order(z$Freq,decreasing = TRUE),][c(1:10),]
  
  # Draw the bar plot head(10)
  p <- ggplot(z,aes(x=GOODS_CODE,y=Freq))
  p<-p+geom_bar(stat="identity",width = 0.8,fill="steelblue")
  p<- p+ylab("ORDER Count")
  p<- p+ggtitle("Goods Ordercount Top10")
  p<- p+theme(plot.title=element_text(size=20,face="bold", margin = margin(10,0,10,0),family = "NanumGothic")) 
  p<- p+theme(axis.title.y=element_blank(),axis.title.x=element_text(family = "NanumGothic",size = 15),axis.text.x=element_text(size=13,family = "NanumGothic"))
  p<- p+geom_text(aes(label=Freq,hjust=-0.2),position = position_dodge(width = 1),family="NanumGothic")
  p<- p+coord_flip()
  p
}