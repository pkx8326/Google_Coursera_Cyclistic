library(tidyverse)
library(ggmap)
library(lubridate)
#--------------------------------------------------------------
alldata <- read_csv("alldata.csv")

#Create a polygon map of Chicago
Illinois <- map_data(map="county",region="illinois")
chicago <- Illinois %>%
  filter(subregion=="cook")

ggplot(data=chicago)+
  geom_polygon(mapping=aes(x=long,y=lat,group=group),
       color="gray",fill="beige")+
  coord_map()+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata,
             mapping=aes(x=start_lng,y=start_lat,
                         color=member_casual),alpha=0.6)+
  ggtitle("Bike Trip Starting Locations",
          subtitle="Source: Cyclistic")+
  scale_color_discrete(name="Customer Type")+
  theme(legend.key=element_blank())


#Create a toner-lite map of Chicago (starting location)
chg <-geocode("Chicago, IL")
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata,
             mapping=aes(x=start_lng,y=start_lat,
                         color=member_casual),alpha=0.6)+
  ggtitle("Bike Trip Starting Locations",
          subtitle="Source: Cyclistic")+
  scale_color_discrete(name="Customer Type")+
  theme(legend.key=element_blank())

#Create a toner-lite map of Chicago (ending location)
chg <-geocode("Chicago, IL")
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata,
             mapping=aes(x=end_lng,y=end_lat,
                         color=member_casual),alpha=0.6)+
  ggtitle("Bike Trip Ending Locations",
          subtitle="Source: Cyclistic")+
  scale_color_discrete(name="Customer Type")+
  theme(legend.key=element_blank())

#Which type of customer ride longer?----------------------
#Create a box plot for time duration
ggplot(data=alldata)+
  geom_boxplot(mapping=aes(x=member_casual,y=ride_duration_min))+
  ggtitle("Bike Trips Duration (mins)",
          subtitle="Source: Cyclistic")+
  scale_x_discrete(name=element_blank(),
                   labels=c("Casual Riders","Member Riders"))+
  ylab("Ride Duration (mins)")

#Check the stats of the column ride_duration_mins
summary(alldata$ride_duration_min)

#install statistical packages to deal with outliers
install.packages("ggstatplot")

#Creating temporary separated datasets for casual and member:
casualdata <- alldata %>%
  filter(member_casual=="casual")

memberdata <- alldata %>%
  filter(member_casual=="member")

#Having 0s in the time duration column doesn't make sense.
#REMOVING 0s in the time duration column from both datasets:
casualdata <- casualdata %>%
  filter(!(ride_duration_min==0))

memberdata <- memberdata %>%
  filter(!(ride_duration_min==0))

#REMOVE TIME DURATION OUTLIERS from each dataset with IQR techqnique
#FOR CASUAL DATA
#find the quantile range for casualdata:
Q_casual <- quantile(casualdata$ride_duration_min,probs=c(.25,.75),na.rm=F)
#find the IQR of casualdata
iqr_casual <- IQR(casualdata$ride_duration_min)
#find the upper cut-off value:
casual_up <- Q_casual[2]+1.5*iqr_casual
#find the lower cut-off value:
casual_low <- Q_casual[1]-1.5*iqr_casual
#removing the outliers from casualdata:
casualdata_adj <- casualdata %>%
  mutate(ride_duration_min=ifelse(ride_duration_min>casual_up,NA,
                                  ride_duration_min)) %>%
  mutate(ride_duration_min=ifelse(ride_duration_min<casual_low,NA,
                                  ride_duration_min))

#plotting a boxplot for casual data (adjusted)
ggplot(data=casualdata_adj)+
  geom_boxplot(mapping=aes(x=member_casual,y=ride_duration_min))

#FOR MEMBER DATA
#find the quantile range for memberdata:
Q_member <- quantile(memberdata$ride_duration_min,probs=c(.25,.75),na.rm=F)
#find the IQR of memberdata
iqr_member <- IQR(memberdata$ride_duration_min)
#find the upper cut-off value:
member_up <- Q_member[2]+1.5*iqr_member
#find the lower cut-off value:
member_low <- Q_member[1]-1.5*iqr_member
#removing the outliers from memberdata:
memberdata_adj <- memberdata %>%
  mutate(ride_duration_min=ifelse(ride_duration_min>member_up,NA,
                                  ride_duration_min)) %>%
  mutate(ride_duration_min=ifelse(ride_duration_min<member_low,NA,
                                  ride_duration_min))

#plotting a boxplot for member data (adjusted)
ggplot(data=memberdata_adj)+
  geom_boxplot(mapping=aes(x=member_casual,y=ride_duration_min))

#merging the outlier-free datasets:
alldata_adj <- bind_rows(casualdata_adj,memberdata_adj)

#Sorting the data by time back to its original order
#This will affect the map plot
alldata_adj <- alldata_adj[rev(order(alldata_adj$started_at)),
                           decreasing=TRUE,na.last=FALSE]

#CHECKING STATS FOR BOTH DATASETS:
boxplot(casualdata_adj$ride_duration_min)$stats
boxplot(memberdata_adj$ride_duration_min)$stats

#creating box plot of both data sets together:
ggplot(data=alldata_adj)+
  geom_boxplot(mapping=aes(x=member_casual,y=ride_duration_min))+
  ggtitle("Bike Trips Duration (mins)--adjusted",
          subtitle="Source: Cyclistic")+
  scale_x_discrete(name=element_blank(),labels=c("Casual Riders",
                                                 "Member Riders"))+
  ylab("Ride Duration (mins)")+
  annotate("text",label="0.02",x=1.1,y=1.5)+
  annotate("text",label="8.90",x=1.1,y=10.9)+
  annotate("text",label="15.12",x=1.1,y=17)+
  annotate("text",label="25.53",x=1.1,y=27.5)+
  annotate("text",label="50.47",x=1.1,y=52)+
  annotate("segment",x=1.05,xend=1,y=0,yend=0)+
  annotate("segment",x=1.05,xend=1,y=50.47,yend=50.47)+
  annotate("text",label="0.02",x=2.1,y=1.5)+
  annotate("text",label="5.67",x=2.1,y=7.5)+
  annotate("text",label="9.55",x=2.1,y=11.5)+
  annotate("text",label="15.75",x=2.1,y=17.5)+
  annotate("text",label="30.87",x=2.1,y=32)+
  annotate("segment",x=2.05,xend=2,y=0,yend=0)+
  annotate("segment",x=2.05,xend=2,y=30.87,yend=30.87)

#What kind of bike is the most popular among the customers?
ggplot(data=alldata_adj)+
  geom_bar(mapping=aes(x=rideable_type,fill=member_casual),
           position="dodge")+
  scale_x_discrete(name=element_blank(),labels=c("Classic Bike","Docked Bike",
                                                   "Electric Bike"))+
  scale_y_continuous(name=expression(Number ~ of ~ Rides ~ (x10^5)),
                     labels=function(x) x / 100000,
                     limits=c(0,1700000))+
  labs(fill="Customer Type")+
  annotate("text",label="1,120,625",x=0.75,y=1180000)+
  annotate("text",label="1,630,033",x=1.2,y=1700000)+
  annotate("text",label="407,763",x=1.75,y=480000)+
  annotate("text",label="270,169",x=2.25,y=350000)+
  annotate("text",label="829,649",x=2.75,y=905000)+
  annotate("text",label="877,563",x=3.25,y=957563)+
  ggtitle("Number of Rides of Different Bike Types",
          subtitle="Source: Cyclistic")

#On what day most customer visit?-------------------------------
#Finding out about statistics from member and casual customers
alldata_adj %>% group_by(weekdays(alldata_adj$started_at)) %>%
  filter(member_casual=="casual") %>% tally() %>% summary()

alldata_adj %>% group_by(weekdays(alldata_adj$started_at)) %>%
  filter(member_casual=="member") %>% tally() %>% summary()

#Visualize with grouped bar chart
ggplot(data=alldata_adj)+
  geom_bar(mapping=aes(x=weekdays(alldata_adj$started_at),
                                  fill=member_casual),
           position="dodge")+
  scale_x_discrete(name=element_blank())+
  scale_y_continuous(name=expression(Number ~ of ~ Rides ~ (x10^5)),
                     labels=function(x) x / 100000,
                     limits=c(0,700000))+
  labs(fill="Customer Type")+
  annotate("text",label="casual riders",
           x=4,y=700000)+
  annotate("text",label="max = 523,829 (SAT)",
           x=4,y=650000)+
  annotate("text",label="min = 251,483 (TUE)",
           x=4,y=600000)+
  annotate("text",label="avg = 336,862",
           x=4,y=550000)+
  annotate("text",label="member riders",
           x=6,y=700000)+
  annotate("text",label="max = 423,109 (WED)",
           x=6,y=650000)+
  annotate("text",label="min = 345,273 (SUN)",
           x=6,y=600000)+
  annotate("text",label="avg = 396,824",
           x=6,y=550000)+
  ggtitle("Number of Rides on Different Days of Week",
          subtitle="Source: Cyclistic")


#NEW MAP CHARTS-------------------------------------------
chg <-geocode("Chicago, IL")
#plot starting locations
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj,
             mapping=aes(x=start_lng,y=start_lat,
                         color=member_casual),alpha=0.6)+
  ggtitle("Bike Trip Starting Locations",
          subtitle="Source: Cyclistic")+
  scale_color_discrete(name="Customer Type")+
  theme(legend.key=element_blank())

#plot ending locations
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj,
             mapping=aes(x=end_lng,y=end_lat,
                         color=member_casual),alpha=0.6)+
  ggtitle("Bike Trip Ending Locations",
          subtitle="Source: Cyclistic")+
  scale_color_discrete(name="Customer Type")+
  theme(legend.key=element_blank())

#creating filtered "weekdays" data for casual riders
#Monday
alldata_adj_MON_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Monday"&member_casual=="casual")
#Tuesday
alldata_adj_TUE_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Tuesday"&member_casual=="casual")
#Wednesday
alldata_adj_WED_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Wednesday"&member_casual=="casual")
#Thursday
alldata_adj_THU_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Thursday"&member_casual=="casual")
#Friday
alldata_adj_FRI_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Friday"&member_casual=="casual")
#Saturday
alldata_adj_SAT_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Saturday"&member_casual=="casual")
#Sunday
alldata_adj_SUN_casual <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Sunday"&member_casual=="casual")

#Plot map charts of starting positions of casual riders
#each day to see the variation
#Monday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_MON_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Mondays for Casual Riders",
          subtitle="Source: Cyclistic")

#Tuesday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_TUE_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Tuesdays for Casual Riders",
          subtitle="Source: Cyclistic")


#Wednesday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_WED_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Wednesdays for Casual Riders",
          subtitle="Source: Cyclistic")

#Thursday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_THU_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Thursdays for Casual Riders",
          subtitle="Source: Cyclistic")

#Friday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_FRI_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Fridays for Casual Riders",
          subtitle="Source: Cyclistic")

#Saturday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_SAT_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Saturdays for Casual Riders",
          subtitle="Source: Cyclistic")

#Sunday map for starting locations of casual riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_SUN_casual,
             mapping=aes(x=start_lng,y=start_lat),
             color="#F8766D",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Sundays for Casual Riders",
          subtitle="Source: Cyclistic")

#creating filtered "weekdays" data for member riders
#Monday
alldata_adj_MON_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Monday"&member_casual=="member")
#Tuesday
alldata_adj_TUE_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Tuesday"&member_casual=="member")
#Wednesday
alldata_adj_WED_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Wednesday"&member_casual=="member")
#Thursday
alldata_adj_THU_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Thursday"&member_casual=="member")
#Friday
alldata_adj_FRI_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Friday"&member_casual=="member")
#Saturday
alldata_adj_SAT_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Saturday"&member_casual=="member")
#Sunday
alldata_adj_SUN_member <- alldata_adj %>%
  filter(weekdays(alldata_adj$started_at)=="Sunday"&member_casual=="member")

#Plot map charts of starting positions of member riders
#each day to see the variation
#Monday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_MON_member,
             mapping=aes(x=start_lng,y=start_lat,),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Mondays for Member Riders",
          subtitle="Source: Cyclistic")

#Tuesday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_TUE_member,
             mapping=aes(x=start_lng,y=start_lat,),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Tuesdays for Member Riders",
          subtitle="Source: Cyclistic")

#Wednesday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_WED_member,
             mapping=aes(x=start_lng,y=start_lat,),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Wednesdays for Member Riders",
          subtitle="Source: Cyclistic")

#Thursday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_THU_member,
             mapping=aes(x=start_lng,y=start_lat,),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Thursdays for Member Riders",
          subtitle="Source: Cyclistic")

#Friday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_FRI_member,
             mapping=aes(x=start_lng,y=start_lat,),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Fridays for Member Riders",
          subtitle="Source: Cyclistic")

#Saturday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_SAT_member,
             mapping=aes(x=start_lng,y=start_lat,),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Saturdays for Member Riders",
          subtitle="Source: Cyclistic")

#Sunday map for starting locations of member riders
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=alldata_adj_SUN_member,
             mapping=aes(x=start_lng,y=start_lat),
             color="#00BFC4",alpha=0.6)+
  theme(legend.position="none")+
  ggtitle("Bike Trip Starting Locations on Sundays for Member Riders",
          subtitle="Source: Cyclistic")

#Which type of customer take or park bikes outside the
#stations more than the other?---------------------------
#create only NAs casual dataset
na_casual <- alldata_adj %>%
  filter(is.na(start_station_id)==T&is.na(end_station_id)==T&member_casual=="casual")
#create only NAs member dataset
na_member <- alldata_adj %>%
  filter(is.na(start_station_id)==T&is.na(end_station_id)==T&member_casual=="member")
#combind the NAs data
na_data <- bind_rows(na_casual,na_member)
#Sorting the data by time back to its original order
#This will affect the map plot
na_data <- na_data[rev(order(na_data$started_at)),
                           decreasing=TRUE,na.last=FALSE]

#Creating a pie chart for counting NAs among
#both types of customer
ggplot(data=na_data)+
  geom_bar(mapping=aes(x="",fill=member_casual))+
  coord_polar("y")+
  annotate("text",label="Member",x=0.9,y=60000,size=6)+
  annotate("text",label="48.6%",x=0.9,y=80000,size=6)+
  annotate("text",label="Casual",x=0.9,y=249000,size=6)+
  annotate("text",label="51.4%",x=0.9,y=229000,size=6)+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        legend.position="none")+
  ggtitle(label="Unidentified Starting or Ending Stations",
          subtitle="Source: Cyclistic")

#Create a map chart to compare casual rider on SAT(max)
#and TUE(min)

#First, create "casualdata_adjd" data containing only ride
#information only on Tuesdays and Saturdays.
casualdata_adjd <- casualdata_adj %>%
  filter(weekdays(started_at)=="Tuesday"|
           weekdays(started_at)=="Saturday")

#Then, create another column containing only "Tuesday"
#and "Saturday"
casualdata_adjd <- casualdata_adjd %>%
  mutate(days=weekdays(started_at))

#CHECK WITH "table()" FIRST!!!
#Switch positions of legend keys:
casualdata_adjd$days <- 
  factor(casualdata_adjd$days,
         levels = rev(levels(casualdata_adjd$days)))

#Create a map chart to compare casual rider on SAT(max)
#and TUE(min)--Starting locations
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=casualdata_adjd,
             mapping=aes(x=start_lng,y=start_lat,
                         color=days),alpha=0.5)+
  scale_color_manual(labels=c("Tuesday (min. number of riders)",
                              "Saturday (max. number of riders)"),
                     values=c("orange", "green"),
                     guide=guide_legend(title="Day"))+
  theme(legend.key=element_blank())+
  ggtitle("Bike Trip Starting Locations on Tuesday and Saturday Casual Riders",
          subtitle="Source: Cyclistic")


#Create a map chart to compare casual rider on SAT(max)
#and TUE(min)--ending locations
ggmap(get_map(chg,maptype="toner-lite"))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_point(data=casualdata_adjd,
             mapping=aes(x=end_lng,y=end_lat,
                         color=days),alpha=0.5)+
  scale_color_manual(labels=c("Tuesday (min. number of riders)",
                              "Saturday (max. number of riders)"),
                     values=c("orange", "green"),
                     guide=guide_legend(title="Day"))+
  theme(legend.key=element_blank())+
  ggtitle("Bike Trip Ending Locations on Tuesday and Saturday Casual Riders",
          subtitle="Source: Cyclistic")

#At what hour of the day do customers use Cyclistic 
#service the  most?
#Create a dataset with hourly data (Every 1 hour)
alldata_adjh <- alldata_adj %>%
  + mutate(hours=hour(alldata_adj$started_at))

#Plot a bar chart to show hourly customer for the entire year
#!!must create a set of hours in the day fist
hr <- c(0:23)
#Start plotting 
ggplot(data=alldata_adjh)+
  geom_bar(mapping=aes(x=hours,fill=member_casual),
           position="dodge")+
  scale_x_continuous(name="Hour of Day",
                     labels=as.character(hr),
                     breaks=hr)+
  scale_y_continuous(name=expression(Number ~ of ~ Rides ~ (x10^5)),
                     labels=function(x) x / 100000)+
  labs(fill="Customer Type")+
  theme(legend.position=c(0.18,0.7))+
  ggtitle("Number of Rides at Different Hour of Day",
          subtitle="Source: Cyclistic")

#Basic number of rides as a pie chart
ggplot(data=alldata)+
  geom_bar(mapping=aes(x="",fill=member_casual))+
  coord_polar("y")+
  annotate("text",label="Member",x=1,y=900000,size=6)+
  annotate("text",label="2,777,974",x=1,y=1200000,size=6)+
  annotate("text",label="54.1%",x=1,y=1500000,size=6)+
  annotate("text",label="Casual",x=1,y=4220000,size=6)+
  annotate("text",label="2,358,287",x=1,y=3950000,size=6)+
  annotate("text",label="45.9%",x=1,y=3650000,size=6)+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        legend.position="none")+
  ggtitle(label="Number of Rides from Each Customer Type",
          subtitle="Source: Cyclistic")
  