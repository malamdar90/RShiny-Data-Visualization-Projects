#####EH PART START############

read.csv("HOSP_ProvidersPaidByEHR_06_2018.csv")->EH

####2016EH######
EH2016<-EH[EH$PROGRAM.YEAR==2016,]
nrow(EH2016)
EH2016_Stage1<-EH2016[EH2016$STAGE.NUMBER %in% "Stage 1",]
EH2016_Stage2<-EH2016[EH2016$STAGE.NUMBER %in% "Stage 2",]
EH2016_Stage3<-EH2016[EH2016$STAGE.NUMBER %in% "Stage 3",]
p <- ggplot(rel_by_region, 
            aes(x = bigregion, y = N, fill = religion))
library(ggplot2)
na.omit(EH2016)->EH2016
EH2016_groupby<- EH2016%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EH2016_groupby
p<-ggplot(EH2016_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2016EH Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

####2015EH####
EH2015<-EH[EH$PROGRAM.YEAR==2015,]
na.omit(EH2015)->EH2015
EH2015_groupby<- EH2015%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EH2015_groupby
p<-ggplot(EH2015_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2015EH Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

####2014EH####
EH2014<-EH[EH$PROGRAM.YEAR==2014,]
na.omit(EH2014)->EH2014
EH2014_groupby<- EH2014%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EH2014_groupby
p<-ggplot(EH2014_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2014EH Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p


####2013EH####
EH2013<-EH[EH$PROGRAM.YEAR==2013,]
na.omit(EH2013)->EH2013
EH2013_groupby<- EH2013%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EH2013_groupby
p<-ggplot(EH2013_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2013EH Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

####2012EH####
EH2012<-EH[EH$PROGRAM.YEAR==2012,]
na.omit(EH2012)->EH2012
EH2012_groupby<- EH2012%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EH2012_groupby
p<-ggplot(EH2012_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2012EH Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

####2011EH####
EH2011<-EH[EH$PROGRAM.YEAR==2011,]
na.omit(EH2011)->EH2011
EH2011_groupby<- EH2011%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EH2011_groupby
p<-ggplot(EH2011_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2011EH Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

###EHtotl CHART###
EHstage<-EH%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
na.omit(EHstage)->EHstage
ggplot(EHstage, aes(x=PROGRAM.YEAR, y=N, color=STAGE.NUMBER,shape=STAGE.NUMBER)) + geom_line() +geom_point(size=4)

#####EH PART END#######


#####EP PART START#######
###ep2011###
read.csv("2011_EP_ProvidersPaidByEHR_06_2018.csv")->EP2011
na.omit(EP2011)->EP2011
EP2011_groupby<- EP2011%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EP2011_groupby
p<-ggplot(EP2011_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2011EP Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

###ep2012###
read.csv("2012_EP_ProvidersPaidByEHR_06_2018.csv")->EP2012
na.omit(EP2012)->EP2012
EP2012_groupby<- EP2012%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
EP2012_groupby
p<-ggplot(EP2012_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2012EP Stage",x="Provider State",y="Count")
p
p  + coord_flip() 
p

###ep2013###
read.csv("2013_EP_ProvidersPaidByEHR_06_2018.csv")->EP2013
na.omit(EP2013)->EP2013
EP2013_groupby<- EP2013%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
p<-ggplot(EP2013_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2013EP Stage",x="Provider State",y="Count")
p
p  + coord_flip() 


###ep2014###
read.csv("2014_EP_ProvidersPaidByEHR_06_2018.csv")->EP2014
na.omit(EP2014)->EP2014
EP2014_groupby<- EP2014%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
p<-ggplot(EP2014_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2014EP Stage",x="Provider State",y="Count")
p
p  + coord_flip() 


###ep2015###
read.csv("2015_EP_ProvidersPaidByEHR_06_2018.csv")->EP2015
na.omit(EP2015)->EP2015
EP2015_groupby<- EP2015%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
p<-ggplot(EP2015_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2015EP Stage",x="Provider State",y="Count")
p
p  + coord_flip() 

###ep2016###
read.csv("2016_EP_ProvidersPaidByEHR_06_2018.csv")->EP2016
na.omit(EP2016)->EP2016
EP2016_groupby<- EP2016%>%
  group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
p<-ggplot(EP2016_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
p = p+geom_col(position = "stack")+labs(title="2016EP Stage",x="Provider State",y="Count")
p
p  + coord_flip() 


rbind(EP2011,EP2012,EP2013,EP2014,EP2015,EP2016)->EP
write_csv(EP,"EPdata.csv")

###eptotl CHART###
ep11<-EP2011%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
ep12<-EP2012%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
ep13<-EP2013%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
ep14<-EP2014%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
ep15<-EP2015%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
ep16<-EP2016%>%
  group_by(PROGRAM.YEAR,STAGE.NUMBER) %>% summarize(N = n())
rbind(ep11,ep12,ep13,ep14,ep15,ep16)->EPstage


na.omit(EPstage)->EPstage
ggplot(EPstage, aes(x=PROGRAM.YEAR, y=N, color=STAGE.NUMBER,shape=STAGE.NUMBER)) + geom_line() +geom_point(size=4)


#####EP PART END########



###eh FUNCTION#######
eh<-function(x){
  read.csv("HOSP_ProvidersPaidByEHR_06_2018.csv")->EH
  EHx<-EH[EH$PROGRAM.YEAR==x,]
  na.omit(EHx)->EHx
  EHx_groupby<- EHx%>%
    group_by(STAGE.NUMBER,PROVIDER.STATE) %>% summarize(N = n())
  EHx_groupby
  p<-ggplot(EHx_groupby, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
  p = p+geom_col(position = "stack")+labs(title="EH Stage",x="Provider State",y="Count")
  p
  p  + coord_flip()->ploteh
  return(ploteh)
}




eh<-function(year){
  input$year1->year
  read.csv(paste0(year,"EHgroupby.csv"))->EH
  p<-ggplot(EH, aes(x=PROVIDER.STATE, y=N, fill=STAGE.NUMBER))
  p = p+geom_col(position = "stack")+labs(title=paste0(year," EH Stage"),x="Provider State",y="Count")
  p
  p  + coord_flip()
}

##########SAVE DATA#######
rbind(EH2016_groupby,EH2015_groupby,EH2014_groupby,EH2013_groupby,EH2012_groupby,EH2011_groupby)->EHgroup_by
write_csv(EHgroup_by,"EHdata.csv")



