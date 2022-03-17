getwd()
setwd("/Users/Frank/Desktop/R program- Delayed material")

#install.packages("lubridate")
#install.packages("ggthemes")
#install.packages("purrr")
#install.packages("data.table")
#install.packages("RColorBrewer")

library("readxl")
library("dplyr")
library("ggplot2")
library("lubridate")
library("forcats")
library("ggthemes")
library("purrr")
library("data.table")
library("RColorBrewer")

#----------------------------DATA IMPORTING----------------------------------------
inline_6600_0717 <- read_xlsx("6600_2020_0717.XLSX",na="")

#----------------------------DATA CLEANING----------------------------------------


colnames(inline_6600_0717) <- gsub(" ","",colnames(inline_6600_0717))
colnames(inline_6600_0717)[which(colnames(inline_6600_0717) == "DeliveryDate(RTA)")] <- "RTA"
colnames(inline_6600_0717)[which(colnames(inline_6600_0717) == "DeliveryCompleted")] <- "Status"
inline_6600_0717$RTDNotEnoughLeadTime[is.na(inline_6600_0717$RTDNotEnoughLeadTime)] <-0
inline_6600_0717$Status[which(inline_6600_0717$Status!="X"|is.na(inline_6600_0717$Status)==TRUE)] <-0
which(is.na(inline_6600_0717$InDate))


#task:Vectorisation
#insert 1 new column: "EnoughLeadTime" to indicate whether having enough lead time
inline_6600_0717 <- inline_6600_0717 %>%
  mutate(EnoughLeadTime= ifelse(inline_6600_0717$RTDNotEnoughLeadTime == "0","Enough LT","Not Enough LT"))

which(is.na(inline_6600_0717$EnoughLeadTime))

#Check whether value inside status is what we expect "Expired" & "Delivered"

inline_6600_0717 <- inline_6600_0717 %>%
  mutate(Status_1= ifelse(inline_6600_0717$Status == "0","Expired",
                          ifelse(inline_6600_0717$Status == "X","Delivered",NA)))

which(is.na(inline_6600_0717$Status_1))
inline_6600_0717$Status_1


#Remove space from vendor's name
inline_6600_0717$VendorName <- gsub(", ",",",inline_6600_0717$VendorName)

#Insert a column for determining delivery status: late, early or on-time

inline_6600_0717 <- inline_6600_0717 %>%
  mutate(DeliveryStatus= ifelse(is.na(as.Date(InDate)-as.Date(RTA))==TRUE,NA,as.Date(InDate)-as.Date(RTA)))
which(is.na(inline_6600_0717$DeliveryStatus))

inline_6600_0717$DeliveryStatus <- gsub(",","",inline_6600_0717$DeliveryStatus)
which(is.na(inline_6600_0717$DeliveryStatus))
inline_6600_0717$DeliveryStatus[37333]
inline_6600_0717$DeliveryStatus <- as.numeric(as.character(inline_6600_0717$DeliveryStatus))
which(is.na(inline_6600_0717$DeliveryStatus))


#New column for delivery performance: Late, early, on-time

inline_6600_0717 <- inline_6600_0717 %>%
  mutate(DeliveryStatus_1= ifelse(DeliveryStatus<= 3 & DeliveryStatus >= -3,"On Time Delivery",
                                  ifelse(DeliveryStatus < -3, "Early Delivery", 
                                         ifelse(DeliveryStatus >3,"Late Delivery","Expired"))))

names(inline_6600_0717)
length(which(inline_6600_0717$DifferenceQty!=0)) #partial 5888
length(which(is.na(inline_6600_0717$RTA))) #0
length(which(is.na(inline_6600_0717$InDate))) #5772
length(which(is.na(inline_6600_0717$DeliveryStatus))) #5772
length(which(is.na(inline_6600_0717$DeliveryStatus_1))) #5772
length(which(inline_6600_0717$Status_1=="Expired" & is.na(inline_6600_0717$InDate) == FALSE)) #8024

#Add one column for material not enough lead time but should be following up
inline_6600_0717 <- inline_6600_0717 %>%
  mutate(Upddate= ifelse((as.Date(PODate)+LeadTime+`TransitTime(TT)`)< today(), "Follow up","Waiting for deliver"))

#----------------------------VISUALIZATION----------------------------------------
#Enough lead time
inline_6600_0717_1 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE, RTA < as.Date("2020-07-17")) %>%
  group_by(EnoughLeadTime,DeliveryStatus_1)%>%
  summarize(n=n())%>%
  mutate(perc=paste0(round(100*n/sum(n),1),"%"))
  

ggplot(inline_6600_0717_1,aes(EnoughLeadTime,n,fill=DeliveryStatus_1))+
  geom_col(position = "fill")+
  geom_text(position = position_fill(vjust = 0.5),aes(label=n))+
  theme_economist()+
  ggtitle("JV2 Delivery status with/without enough Lead Time as of July 17th")+
  xlab("Lead Time")+
  ylab("Percentage(%)")+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank())+
  scale_fill_manual(values=c("yellow1", "red2", "green3"))
  

#filter date from inline_6000
inline_6600_0717_2 <- inline_6600_0717 %>%
  filter(RTA < as.Date("2020-07-17")) %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  select(VendorName,RTA,EnoughLeadTime,Status_1,MaterialType)

ggplot(inline_6600_0717_2,aes(x=Status_1))+
  geom_bar()+
  facet_wrap(~MaterialType, ncol=3,)+
  ggtitle("JV2 Number of RTA expired cross different material type (all Orders)",
          "As of July 17th, 2020")+
  coord_cartesian(ylim=c(0,20000))+
  geom_text(stat = 'count',aes(label=..count..,color=factor(Status_1)),,vjust=-1,size=4,fontface="bold")+
  ylab("Number of PO")+
  xlab("Status")+
  theme_economist()+
  theme(strip.text = element_text(face="bold", size=9, lineheight = 5),
        strip.background = element_rect(fill="white",color="black"),
        plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        legend.title = element_blank(),
        legend.text = element_text(size=13,face="bold"))



#Vendor lists of all expired orders
inline_6600_0717_3 <- inline_6600_0717 %>%
  filter(Status_1=="Expired",RTA < as.Date("2020-07-17")) %>%
  select(VendorName,RTA,EnoughLeadTime,Status_1) %>%
  group_by(VendorName) %>%
  count

ggplot(inline_6600_0717_3,aes(x=reorder(VendorName,n),y=n))+
  geom_bar(stat='identity')+
  coord_flip()+
  ggtitle("JV2 Number of POs expired by Vendors (all orders) as of July 17th")+
  xlab("Vendor")+
  ylab("Number of POs expired")+
  geom_text(aes(label=n),color="red",size=3, hjust=-0.1)+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3,hjust = 1),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 8))



#Example of as.data.table
DF = data.frame(x=rep(c("x","y","z"),each=2), y=c(1,3,6), row.names=LETTERS[1:6])
as.data.table(DF)
as.data.table(DF, keep.rownames=TRUE)
as.data.table(DF, keep.rownames="rownames")

#Divide RTA expired by month
inline_6600_0717_4 <- inline_6600_0717 %>%
  filter(Status_1=="Expired",RTA < as.Date("2020-07-17"),RTA > as.Date("2020-01-01")) %>%
  mutate(month = month(RTA)) %>%
  group_by(month,EnoughLeadTime) %>%
  count()
ggplot(inline_6600_0717_4, aes(x= factor(month),y=n,fill=EnoughLeadTime))+
  geom_col(position = "dodge")+
  geom_text(position= position_dodge(width = 0.9),aes(label=n),size=4,fontface="bold")+
  scale_x_discrete(labels=c("Jan","Feb","March","April", "May","June","July"))+
  xlab("Month")+
  ylab("# of expired PO")+
  ggtitle("JV2 Number of PO RTA expired by month", 
          "As of July 17th, 2020")+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 8))

#Early, late, on-time delivery % by month
#inline_6000_0703_5 <- inline_6000_0703 %>%
  #filter(is.na(InDate)==FALSE,Status_1=="Expired",RTA < as.Date("2020-07-03") & RTA > as.Date("2020-01-01")) %>%
  #mutate(month = month(RTA)) %>%
  #count(Mo=factor(month),De=factor(DeliveryStatus_1)) %>%
  #mutate(pct=prop.table(n))
#Early, late, on-time delivery % by month
#Same result with different approach (NEED STUDY AGAIN)
inline_6600_0717_5 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01")) %>%
  mutate(month = month(RTA)) %>%
  group_by(month,DeliveryStatus_1) %>%
  summarize(n=n())%>%
  mutate(freq= paste0(round(100*n/sum(n),1),"%"))
  

ggplot(inline_6600_0717_5,aes(x=factor(month),y=n,fill= DeliveryStatus_1))+
  geom_col(position = "fill",stat="identity")+
  theme_economist()+
  geom_text(position=position_fill(vjust = 0.5),aes(x=month,y=n,label=freq),size=4,fontface="bold")+ #geom_text position should be same with geom_bar's
  scale_x_discrete(labels=c("Jan","Feb","March","April","May","June","July"))+
  ggtitle("JV2 Material delivery performance over month", 
          "As of July 17th, 2020")+
  ylab("Percentage(%)")+
  xlab("Month")+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 10,face="bold"),
        axis.text.x = element_text(size=10,face="bold"),
        legend.title = element_blank())+
  scale_fill_manual(values=c("yellow1", "red2", "green3"))+
  coord_flip()


#Average delivery days by suppliers (Late delivery)
inline_6600_0717_6 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01")) %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  select(MaterialType,DeliveryStatus, VendorName)%>%
  group_by(VendorName) %>%
  summarize(n=round(mean(DeliveryStatus),2),na.rm=T)%>%
  filter(n>3)

ggplot(inline_6600_0717_6, aes(reorder(VendorName,n),n))+
  geom_col()+
  coord_flip()+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 8))+ 
  ggtitle("JV2 Vendors with Late delivery")+
  xlab("Vendor")+
  ylab("Late/Early delivery days")
  
#Average delivery days by suppliers (Early delivery)
inline_6600_0717_7 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01")) %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  select(MaterialType,DeliveryStatus, VendorName)%>%
  group_by(VendorName) %>%
  summarize(n=round(mean(DeliveryStatus),2),na.rm=T)%>%
  filter(n< -3)

ggplot(inline_6600_0717_7, aes(reorder(VendorName,n),n))+
  geom_col()+
  coord_flip()+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 8))+ 
  ggtitle("JV2 Vendors with Early delivery")+
  xlab("Vendor")+
  ylab("Late/Early delivery days")

#Average delivery days by suppliers (On time delivery)
inline_6600_0717_8 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01")) %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  select(MaterialType,DeliveryStatus, VendorName)%>%
  group_by(VendorName) %>%
  summarize(n=round(mean(DeliveryStatus),2),na.rm=T)%>%
  filter(n> -3 & n<3)

ggplot(inline_6600_0717_8, aes(reorder(VendorName,n),n))+
  geom_col()+
  coord_flip()+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 8))+ 
  ggtitle("JV2 Vendors with On-time delivery")+
  xlab("Vendor")+
  ylab("Late/Early delivery days")

#Early, late, on-time delivery % by material type
inline_6600_0717_9 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",DeliveryStatus != "Not yet delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01")) %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  group_by(MaterialType,DeliveryStatus_1) %>%
  dplyr::summarize(n=n())%>%
  mutate(freq= paste0(round(100*n/sum(n),1),"%"))
which((is.na(inline_6600_0703_9$DeliveryStatus_1)))
inline_6600_0717_9$DeliveryStatus_1

ggplot(inline_6600_0717_9,aes(x=factor(MaterialType),y=n,fill= DeliveryStatus_1))+
  geom_col(position = "fill")+
  theme_economist()+
  geom_text(position=position_fill(vjust = 0.5),aes(label=freq),size=4,fontface="bold")+ #geom_text position should be same with geom_bar's
  ggtitle("JV2 Material delivery performance by material type", "As of July 17th, 2020")+
  ylab("Percentage(%)")+
  xlab("Material Type")+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 10,face = "bold"),
        axis.text.x = element_text(size=10,face = "bold"),
        legend.title = element_blank())+
  scale_fill_manual(values=c("yellow1", "red2", "green3"))+
  coord_flip()

#Average delivery days by material type all order with delayed delivery
inline_6600_0717_10 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01"),DeliveryStatus_1=="Late Delivery") %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  select(MaterialType,DeliveryStatus, EnoughLeadTime) %>%
  group_by(MaterialType, EnoughLeadTime)%>%
  summarise(n=round(mean(DeliveryStatus),2),na.rm=T)

ggplot(inline_6600_0717_10,aes(MaterialType, n,fill=EnoughLeadTime))+
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width = 1),size=4,fontface="bold",aes(x=MaterialType, label=n))+
  theme_economist()+
  scale_fill_brewer(palette = "RdGy")+
  ggtitle("JV2 Average late delivery days by material type","As of July 17th,2020")+
  ylab("Average late delivery days")+
  xlab("Material Type")+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 10,face="bold"),
        axis.text.x = element_text(size = 10,face="bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size=10,face="bold"),
        legend.title = element_blank())+
  coord_flip()

#Average delivery days by material type all order with Early delivery
inline_6600_0717_11 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01"),DeliveryStatus_1=="Early Delivery") %>%
  mutate(MaterialType = substr(Material, 1,2)) %>%
  select(MaterialType,DeliveryStatus, EnoughLeadTime) %>%
  group_by(MaterialType, EnoughLeadTime)%>%
  summarise(n=round(mean(DeliveryStatus),1),na.rm=T)

ggplot(inline_6600_0717_11,aes(MaterialType, n,fill=EnoughLeadTime))+
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width = 1),size=4,fontface="bold",aes(x=MaterialType, label=n))+
  theme_economist()+
  scale_fill_brewer(palette = "RdGy")+
  ggtitle("JV2 Average early delivery days by material type","As of July 17th,2020")+
  ylab("Average early delivery days")+
  xlab("Material Type")+
  theme(plot.title = element_text(vjust = 3),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size = 10, face="bold"),
        axis.text.x = element_text(size=10,face="bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size=10,face="bold"),
        legend.title = element_blank())+
  coord_flip()
# performance ranking (all supplier)

inline_6600_0717_12 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Delivered",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01"),EnoughLeadTime=="Enough LT") %>%
  select(VendorName,DeliveryStatus_1)%>%
  group_by(VendorName, DeliveryStatus_1)%>%
  summarise(n=n())%>%
  mutate(pct=n/sum(n))%>%
  arrange(factor(DeliveryStatus_1,levels = c("On Time Delivery","Early Delivery","Late Delivery")), desc(pct))


inline_6600_0717_12 <- arrange(inline_6600_0717_12, DeliveryStatus_1, desc(pct))

inline_6600_0710_17$VendorName <- factor(inline_6600_0717_12$VendorName, levels = unique(inline_6600_0717_12$VendorName))  
install.packages('ggrepel')
library("ggrepel")
ggplot(inline_6600_0717_12,
       aes(x=VendorName,
           pct, 
           fill=DeliveryStatus_1))+
  geom_col(position = "fill")+
  coord_flip()+
  geom_text(position= position_fill(vjust=0.5),size=4,fontface="bold",aes(label= paste0(round(100*pct,2),"%")))+
  ggtitle("JV2 vendors performance as of July 17th, 2020")+
  ylab("Percentage(%)")+
  xlab("Vendors")+
  theme_economist()+
  theme(plot.title = element_text(vjust = 3,hjust = 0.5),
        plot.subtitle = element_text(hjust=-1.5),
        axis.title.x = element_text(size=12,face="bold",vjust = -2),
        axis.title.y = element_text(size=12,face="bold",vjust = 4),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=8),
        legend.title = element_blank())+scale_fill_manual(values=c("yellow1", "red2", "green3"))


#How many PO should be following up inside expired

inline_6600_0717_13 <- inline_6600_0717 %>%
  filter(is.na(InDate)==FALSE,Status_1=="Expired",RTA < as.Date("2020-07-17") & RTA > as.Date("2020-01-01"), EnoughLeadTime == "Not Enough LT") %>%
  mutate(month= month(RTA)) %>%
  group_by(month, Upddate) %>%
  summarize(n=n())


