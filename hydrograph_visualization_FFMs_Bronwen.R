###Hydrograph visualization plots - Bronwen's adapted code

library(ffcAPIClient);library(ggplot2);library(lubridate);library(dplyr);library(dataRetrieval);library(stringr);library(tidyr);library(purrr);library(Cairo)


df<-get_predicted_flow_metrics("8285120", wyt="any"); gageid<-11476500  #sf eel miranda


#format gage data#### 
#Download data from USGS website and create dataframe (dat)
gage <- readNWISdv(gageid, startDate="", endDate="", parameterCd="00060")
colnames(gage)<-c("Agency", "Site Number", "date", "flow", "Approval Code")

#Omit NAs from data
dat<-na.omit(gage)

#Add water year column
dat<-dat %>% 
  mutate(date=ymd(date),
         month=month(date), 
         year=year(date),
         Water.year=ifelse(month > 9, year+1,year))

#Specify water years that have more than 360 discharge values
keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
dat <- dat[as.factor(dat$Water.year) %in% keep, ]


# Create summary data frame (dat3) for each water month
dat3<-dat%>%
  group_by(Water.year)%>% 
  summarise(Mean=mean(flow))

# Rank yearly discharge values from largest to smallest for entire period of record
dat3$Rank.wy<-rank(-dat3$Mean, ties.method="first")

dat3<-dat3[order(dat3$Rank.wy),]# Sort rows by the "Rank.wyt" column

dat3<- dat3%>% 
  mutate(Exceedance.wy=(Rank.wy/length(Mean+1))*100,
         wyt=case_when(Exceedance.wy<=30 ~ "wet",
                       Exceedance.wy<=70 ~ "moderate",
                       TRUE ~ "dry"))%>%
  select(Water.year,wyt, Exceedance.wy)

# Assign water year types from dat3 to dat and add water year day
dat<-dat %>% 
  left_join(dat3)%>%
  filter(!is.na(wyt)) %>% 
  group_by(Water.year)%>% 
  arrange(date, .by_group= TRUE) %>% # in case the entries are not in order
  mutate(Day = 1:n()) #assign water year day to each flow

#find median year within each wyt based on exceedance
gage3<-dat %>%
  group_by(wyt) %>% 
  filter(Exceedance.wy==quantile(Exceedance.wy,p=.5, type=1)) 



# manipulating functional flow predictions to create a flow regime by wyt#####

#create a tibble with a row for each wyt for fall duration
df.fa1<- df %>%
  filter(metric=="FA_Dur")%>%   
  mutate(wyt="dry")
df.fa2<- df %>%
  filter(metric=="FA_Dur")%>%
  mutate(wyt="moderate")
df.fa3<- df %>%
  filter(metric=="FA_Dur")%>%
  mutate(wyt="wet")

df.fa<-rbind(df.fa1,df.fa2,df.fa3) %>%
  select(wyt, p10,p50, p90)%>%
  pivot_longer(-c(wyt),names_to="percentile") %>%
  mutate(wyt=factor(wyt, levels=c("wet","moderate","dry"))) #change order for faceting


#subset df to data by water year type
df.crit<-df %>% 
  filter(wyt != "all")%>% 
  mutate(wyt=droplevels(factor(wyt, levels=c("wet","moderate","dry")))) %>% #change order for faceting
  select(wyt, p10,p50, p90,metric)%>%
  pivot_longer(-c(wyt,metric),names_to="percentile")


#select all timing rows
tim.df<- df.crit%>%
  filter(str_detect(metric,'_Tim')) %>%#select timing rows
  rename(timing=value,name=metric)%>%
  mutate(metric=substr(name,1,2))


#set flow at end of fall pulse to dry season baseflow
tim.df1<-tim.df %>% filter(metric=="FA")%>%
  left_join(df.fa)%>% #set timing of end of pulse
  mutate(timing=value+timing,metric="DS", name="FA_End")%>%
  select(wyt,metric,percentile,timing,name)


#set 1 day before fall pulse to dry season baseflow
tim.df2<-tim.df %>% 
  filter(metric=="FA")%>% #set timing of before the pulse to dS
  mutate(timing=timing-1, metric="DS",name="FA_Start")


#set end of water year to dry season baseflow
tim.df3<-tim.df %>%
  filter(metric=="DS")%>% #set end of water year to dry season
  mutate(timing=365,name="Year_End")


#set beginning of water year to dry season baseflow
tim.df4<-tim.df %>%
  filter(metric=="DS")%>% 
  mutate(timing=1,name="Year_Start") #set beginning of water year to dry season


#set end of dry season to 1 day before start of wet season (alternately, could use dry season duration metric)
tim.df5<-tim.df %>%
  filter(metric=="We")%>% 
  mutate(timing=timing-1, metric="DS",name="DS_End") #set to ds throughout dry season


#calculate timing of end of wet season using wet season duration + wet season start
df.wet<-df.crit %>%
  filter(metric=="Wet_BFL_Dur")%>%select(-metric) #pull wet season duration

tim.df6<-tim.df %>%
  filter(metric=="We")%>% 
  left_join(df.wet)%>%
  mutate(timing=timing+value,name="Wet_End")%>% #set to wet throughout wet season based on wet season duration
  select(wyt,metric,percentile,timing,name)%>%
  left_join(filter(select(tim.df,-metric),name=="SP_Tim"),by=c("wyt","percentile"))%>% #join spring timing to compare dates
  mutate(timing.x=ifelse(timing.y<timing.x,timing.y-1,timing.x))%>% #wet season must end at least 1 day before spring start, otherwise, end of wet season is based on wet season duration
  select(wyt,metric,percentile,timing.x,name.x)%>%
  rename(timing=timing.x,name=name.x)

#combine all timing tibbles
tim.all<-rbind(tim.df,tim.df1,tim.df2,tim.df3, tim.df4, tim.df5,tim.df6)


#select all magnitude rows (only use Mag_50 for summer and winter baseflow for now)
mag.df<- df.crit %>% group_by(wyt)%>%
  filter(metric != "Wet_BFL_Mag_10" & metric !="DS_Mag_90")%>% 
  filter(grepl('_Mag',metric)) %>% #select mag rows
  rename(magnitude=value,source.mag=metric)%>%
  mutate(metric=substr(source.mag,1,2))#cut for easier joining


#join all timing and magnitude criteria
criteria.df1<-full_join(tim.all, mag.df) %>% pivot_wider(names_from=percentile, values_from=c(magnitude,timing))
#Note: the end of summer is based on the start of the wet season, not the duration of summer



#spring slope#####

ROC<-df$p50[df$metric=="SP_ROC"]

#pull out key metrics for use below by water year type
constants<-df %>%
  group_by(wyt)%>%
  filter(wyt!="all")%>%
  summarize(duration=p50[metric=="SP_Dur"],
            start=p50[metric=="SP_Tim"],
            mag=p50[metric=="SP_Mag"],
            mag90=p90[metric=="SP_Mag"],
            mag10=p10[metric=="SP_Mag"],
            mag.ds=p50[metric=="DS_Mag_50"])


# create a dataframe with one row for each day of the spring recession. Calculate magnitude for each day using ROC and # days since the beginning of the recession. This creates a curve that slopes down from the spring start to the dry season baseflow magnitude.

spring<-function(constants1){
  output<- data.frame(number=(1:constants1$duration),#number of days since the beginning of the recession
                      magnitude1=constants1$mag,    # start magnitude for each percentile
                      magnitude10=constants1$mag10,
                      magnitude90=constants1$mag90,
                      "metric"="SP","name"="SP_Slope","source.mag"=NA, "timing_p10"=NA,"timing_p90"=NA)%>% #create other needed column names to support the rbind
    mutate(magnitude_p50=magnitude1*(1-ROC)^number, #calculate magnitude for each day using rate of change and number of days since beginning of recession
           timing_p50=number+constants1$start, #convert days since beginning of spring to water year day
           magnitude_p50=ifelse(magnitude_p50<constants1$mag.ds, constants1$mag.ds, magnitude_p50), #set minimum flow at the dry season 50th percentile magnitude, to prevent creation of a graph that dips down and then comes back up for the summer
           magnitude_p10=magnitude10*(1-ROC)^number, #apply same ROC to 10th and 90th percentile as p50
           magnitude_p90=magnitude90*(1-ROC)^number)%>%
    select(-c(number,magnitude1,magnitude10,magnitude90))
}



springdat<-constants %>%
  nest(-wyt) %>%
  group_by(wyt) %>%
  summarize(purrr::map(data, spring))%>%  #runs the function by water year type
  unnest()
#https://community.rstudio.com/t/using-a-self-written-function-inside-group-by-and-mutate/1650

criteria.df2<-bind_rows(criteria.df1, springdat)


# assigning wet season timing and duration to peak flow magnitude metric
peakflow50<-with(df, p50[metric=="Peak_2"])
peakflow10<-with(df, p10[metric=="Peak_2"])
peakflow90<-with(df, p90[metric=="Peak_2"])

peak<-criteria.df1 %>% 
  filter(name %in% c("Wet_Tim", "Wet_End")) %>% #apply median wet season start and end timing to peak flows
  select(name, wyt,timing_p50) %>%
  mutate(magnitude_p50=peakflow50, magnitude_p10=peakflow10, magnitude_p90=peakflow90)



#plot with ribbon####

colors1<-c("chocolate2","chartreuse3","deepskyblue2") #set colors in the plot
Cairo(type="png", units="in",width=5,height=6,dpi=300,file="FuncFlowsCriteria.png") #include this line if you want to output a plot

plot1 <- ggplot(data=criteria.df2, aes(x=timing_p50, y=magnitude_p50))+theme_classic()+
  
  #hydrographs
  geom_line(data=dat, aes(x=Day, y=(flow)),color="grey30",alpha=.2,size=.2)+  
  geom_line(data=gage3, aes(x=Day, y=(flow)),color="black",size=.2)+
  facet_wrap(~wyt, ncol=1)+
  #theme(strip.background=element_rect(color=NA),strip.text.x=element_blank())+ #removes facet labels
  
  geom_ribbon(data=criteria.df2, aes(ymin=magnitude_p10, ymax=magnitude_p90, fill=wyt),color=NA,alpha=.4)+
  geom_line(data=criteria.df2, aes(x=timing_p50, y=magnitude_p50, color=wyt), size=1.5)+ 
  #geom_point(data=filter(criteria.df2, name!="SP_Slope"),aes(x=timing_p50, y=magnitude_p50, color=wyt), size=2) + 
  
  geom_line(data=peak, aes(y=magnitude_p50,x=timing_p50,color=wyt), lty=2, size=1.5)+
  
  scale_color_manual(values=colors1)+  scale_fill_manual(values=colors1)+
  ylab("Discharge (cfs)") + 
  theme(axis.title.x=element_blank())+
  scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240,271,302,333,366),
                     labels = paste0(c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct")),"Month")+
  
  theme(legend.position = "none")+
  coord_cartesian(ylim=c(0,10000))+
  ggtitle(paste("usgs gage",toString(gageid)))+ theme(plot.title = element_text(size=8))

print(plot1)

dev.off()
