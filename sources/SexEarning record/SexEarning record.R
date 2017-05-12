ForshinyMonthSex <- function(df_orderList,month){
  df_monthorder<- df_orderList[format(df_orderList$ORDER_DATE,"%Y-%m")==month,]
  
  z<-data.frame(table(df_monthorder$SEX))
  colnames(z) <- c("SEX","Freq")
  z
  total <- sum(z$Freq)
  y_val <- z$Freq
  
  p <- ggplot(z,aes(x=factor(1),y=Freq,fill=SEX))
  p <- p+geom_bar(stat="identity",width = 1,colour="black")
  p<- p+coord_polar(theta='y')
  p <- p+geom_label_repel(aes(y=y_val/2+c(0,cumsum(y_val)[-length(y_val)]),
                              label=paste(round((y_val/total)*100,2),"%",sep="")),size=7,colour="white",nudge_x = ifelse(round((y_val/total)*100,2)<5,0.6,0))
  p <-p+xlab("Percentage(%)")+ylab("SEX")
  p <- p+theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank(),
               axis.line.y=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.line.x=element_blank())
  p <- p+theme(legend.title=element_text(size=17,family = "NanumGothic",face="bold"),
               legend.text=element_text(size=15,family = "NanumGothic",face="bold"))
  p <- p+theme(axis.title.y=element_text(size=20,family = "NanumGothic",margin = margin(0,10,0,10),face="bold"),
               axis.title.x=element_text(size = 20,family = "NanumGothic",margin = margin(10,0,10,0),face="bold"))
  p <- p+ggtitle("SEX Percentage")+theme(plot.title=element_text(size=28,family = "NanumGothic",margin=margin(10,0,10,0),face="bold"))
  p <- p+ scale_fill_manual(values=c("#4575b4","#d73027","#1a9850","#8073ac","#e08214","#01665e",
                                     "#878787","#b3de69","#c51b7d","#9970ab","#1a1a1a","#67001f","#ffff33"),labels=c('Male','Female'))
  p
}