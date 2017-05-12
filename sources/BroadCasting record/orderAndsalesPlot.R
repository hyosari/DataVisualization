
#Related Functions
#number of the order in specific day when broadcasting 

dailyGoodsNUM <- function(df_orderList,df_goods,broad_day){
  
  orderList_WOsale<-subset(df_orderList,format(df_orderList$BD_DATE,"%Y-%m-%d")==broad_day,
                           select = c('GOODS_CODE','BD_DATE'))
  dayGoodsN_plot <- as.data.frame(table(orderList_WOsale))
  dayGoodsN_plot<- join(dayGoodsN_plot,df_goods,type="left")
  dayGoodsN_plot <- subset(dayGoodsN_plot,select = c('GOODS_CODE','Freq','GOODS_NAME'))
  dayGoodsN_plot$GOODS_CODE <-factor(dayGoodsN_plot$GOODS_CODE,levels = dayGoodsN_plot[order(dayGoodsN_plot$Freq),"GOODS_CODE"]) 
  dayGoodsN_plot
}

# amount of sales in specific day when broadcasting 
dailyGoodsSALE <- function(df_orderList,df_goods,broad_day){ 
  
  dayGoodsN_order<-subset(df_orderList,format(df_orderList$BD_DATE,"%Y-%m-%d")==broad_day,
                          select = c('GOODS_CODE','RSALE_AMT'))
  
  ## summary numbers no match. bucause custommer buy the goods that broad another day 
  x<-tapply(dayGoodsN_order$RSALE_AMT,dayGoodsN_order$GOODS_CODE,FUN=sum)
  dayGoodsSALE <- data.frame(GOODS_CODE=names(x),SUM_SALES=x)
  n <- dim(dayGoodsSALE)[1]
  id<- 1:n
  rownames(dayGoodsSALE)<-id
  dayGoodsSALE <- join(dayGoodsSALE,df_goods,type="left")
  dayGoodsSALE$GOODS_CODE <-factor(dayGoodsSALE$GOODS_CODE,levels = dayGoodsSALE[order(dayGoodsSALE$SUM_SALES),"GOODS_CODE"])
  dayGoodsSALE
}

#Plot of orderCount
dayBarplot_count <- function(dailydataNum){
  
  
  p<- ggplot(dailydataNum,aes(x=GOODS_CODE,y=Freq))
  p<- p+geom_bar(width = 0.8, stat="identity",fill="steelblue")
  p<- p+ylab("ORDER COUNT")
  p<- p+ggtitle("DAY ORDER COUNT")
  p<- p+theme(plot.title=element_text(size=20,face="bold", margin = margin(10,0,10,0),family = "NanumGothic")) 
  p<- p+theme(axis.title.y=element_blank(),axis.title.x=element_text(family = "NanumGothic",size = 15),axis.text.x=element_text(size=13,family = "NanumGothic"))
  p<- p+geom_text(aes(label=Freq,hjust=-0.2),position = position_dodge(width = 1),family="NanumGothic")
  p<- p+coord_flip()
  p
  
}

#Plot of Sales
dayBarplot_sale <- function(dailydataSALE){
  
  p<- ggplot(dailydataSALE,aes(x=GOODS_CODE,y=SUM_SALES/1000))
  p<- p+geom_bar(width = 0.8, stat="identity",fill="steelblue")
  p<- p+ylab("ORDER SALES( 1000 WON )")
  p<- p+ggtitle("DAY ORDER SALES")
  p<- p+theme(plot.title=element_text(size=20,face="bold", margin = margin(10,0,10,0),family = "NanumGothic")) 
  p<- p+theme(axis.title.y=element_blank(),axis.title.x=element_text(family = "NanumGothic",size = 15),axis.text.x=element_text(size=13,family = "NanumGothic"))
  p<- p+geom_text(aes(label=SUM_SALES/1000,hjust=-0.2),position = position_dodge(width = 1),family="NanumGothic")
  p<- p+coord_flip()
  p
}

#Used by Web Publishing(Shiny) Server 
ForshinyBroadsales_count <- function(df_orderList,df_goods,broad_day){
  dailydataNum <- dailyGoodsNUM(df_orderList,df_goods,broad_day)
  dayBarplot_count(dailydataNum)
}

ForshinyBroadsales_sales <- function(df_orderList,df_goods,broad_day){
  dailydataSALE<-dailyGoodsSALE(df_orderList,df_goods,broad_day)
  dayBarplot_sale(dailydataSALE)
}
