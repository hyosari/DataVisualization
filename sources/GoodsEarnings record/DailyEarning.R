
# Whole sales amount in sepcific day 
dailySale <- function(df_orderGoods,date){
  x_total <- df_orderGoods[format(df_orderGoods$GATHER_DATE,"%Y-%m-%d")==date,]
  x_order <- subset(x_total,select=c('GOODS_CODE','ORDER_AMT','ORDER_QTY'))
  x_cancel <-subset(x_total,select=c('GOODS_CODE','CANCEL_AMT','CANCEL_QTY'))
  x_claim <- subset(x_total,select = c('GOODS_CODE','CLAIM_AMT','CLAIM_QTY'))
  x_claim_can <- subset(x_total,select=c('GOODS_CODE','CLAIM_CAN_AMT','CLAIM_CAN_QTY'))
  x_netorder <- subset(x_total,select = c('GOODS_CODE','NETORDER_AMT','NETORDER_QTY'))
  x_netsale <- subset(x_total,select=c('GOODS_CODE','NETSALE_AMT','NETSALE_QTY'))
  
  x_total_sum<- as.data.frame.list(colSums(x_total[,-1]))
  x_total_sum
  SUM <- t(x_total_sum[2:13])
  d <- dim(SUM)[1]
  n <- 1:d
  rownames(SUM)<- n
  SALE <- c('ORDER','ORDER','CANCEL','CANCEL','CLAIM','CLAIM','CLAIM_CAN','CLAIM_CAN','NETORDER','NETORDER','NETSALE','NETSALE')
  MENU <- c('AMT','QTY','AMT','QTY','AMT','QTY','AMT','QTY','AMT','QTY','AMT','QTY')
  
  d_sum <- data.frame(Menu=MENU,Sale=SALE,X1=SUM)
  d_sum$Sale <- factor(d_sum$Sale,levels = c('ORDER','CANCEL','CLAIM','CLAIM_CAN','NETORDER','NETSALE'))
  d_sum
}

# draw plot of sales amount 
dailyplot <- function(dateSale){
  p <- ggplot(dateSale,aes(x=Sale,y=X1/1000000, fill=factor(Menu)))
  p<- p+ geom_bar(width=0.8, stat="identity",position="dodge")
  p <- p+ggtitle("Daily sales")+theme(plot.title=element_text(size=25,face="bold",family = "NanumGothic",margin=margin(10,0,10,0)))
  p<-p+ylab("Sales(1,000,000 WON)")+theme(axis.title.x=element_blank(),axis.text.x = element_text(size=12,face="bold",family = "NanumGothic"))
  p<- p+theme(axis.title.y = element_text(size=13,face="bold",family = "NanumGothic"),axis.text.y=element_text(size=13,face="bold",family = "NanumGothic"))
  p <- p+geom_text(aes(label=round(X1/1000000,2),vjust=-0.5),position=position_dodge(width=0.8),family="NanumGothic",fontface="bold")
  p<- p+theme(legend.title = element_blank(),legend.text=element_text(size=12,face="bold",family = "NanumGothic"))
  p
}
# draw plot of order count 
dailyQTYplot <- function(dateSale){
  p <- ggplot(dateSale,aes(x=Sale,y=X1, fill=factor(Menu)))
  p<- p+ geom_bar(width=0.8, stat="identity",position="dodge")
  p <- p+ggtitle("Daily sales")+theme(plot.title=element_text(size=25,face="bold",family = "NanumGothic",margin=margin(10,0,10,0)))
  p<-p+ylab("Count")+theme(axis.title.x=element_blank(),axis.text.x = element_text(size=12,face="bold",family = "NanumGothic"))
  p<- p+theme(axis.title.y = element_text(size=13,face="bold",family = "NanumGothic"),axis.text.y=element_text(size=13,face="bold",family = "NanumGothic"))
  p <- p+geom_text(aes(label=round(X1,2),vjust=-0.5),position=position_dodge(width=0.8),family="NanumGothic",fontface="bold")
  p<- p+theme(legend.title = element_blank(),legend.text=element_text(size=12,face="bold",family = "NanumGothic"))
  p
}

#For web publishing server

Forshinydailyplot <- function(df_orderGoods,date){
  dateSale<-dailySale(df_orderGoods,date)
  dateSale<-dateSale[dateSale$Menu=="AMT",]
  dailyplot(dateSale)
}
ForshinydailyQTYplot <- function(df_orderGoods,date){
  dateSale<-dailySale(df_orderGoods,date)
  dateSale<-dateSale[dateSale$Menu=="QTY",]
  dailyQTYplot(dateSale)
}