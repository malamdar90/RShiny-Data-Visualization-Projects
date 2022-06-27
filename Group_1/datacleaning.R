setwd("/Users/fanggepiao/Dropbox/Summer/Data Visualization/Project")

mydata <- read.csv("DC_Properties.csv")

mydata <- data.frame(mydata)

mydata <- mydata[mydata$SOURCE=="Residential", ]

mydata$ADDRESS <- paste(mydata$FULLADDRESS, mydata$CITY, mydata$STATE, mydata$ZIPCODE, sep = " ")

mydata$YR <- pmax(mydata$AYB, mydata$YR_RMDL, mydata$EYB, na.rm=TRUE)

mydata <- subset(mydata, select = -c(STORIES,USECODE,AYB,YR_RMDL,EYB,X.1,SQUARE,FULLADDRESS, CITY, STATE, ZIPCODE, NATIONALGRID,WARD,X,Y,CMPLX_NUM, LIVING_GBA, SOURCE, CENSUS_TRACT, CENSUS_BLOCK, BLDG_NUM, GIS_LAST_MOD_DTTM))

data <- subset(mydata, !is.na(mydata$PRICE))

data2 <- subset(data, data$BATHRM != 0)
data2 <- subset(data2, data2$ROOMS != 0)
data2 <- subset(data2, data2$BEDRM != 0)
data2 <- subset(data2, data2$NUM_UNITS != 0)
data2 <- subset(data2, data2$KITCHENS != 0)
data2 <- subset(data2, data2$LANDAREA != 0)

library(stringr)

data2$STORIES <- str_split_fixed(data2$STYLE, " ", 2)
data2 <- subset(data2, select = -c(STYLE))

write.csv(data2, "cleanedDC.csv")

df <- read.csv("cleanedDC.csv")
df$STORIES <- df$STORIES.1
df <- subset(df, select = -c(X,STORIES.1,STORIES.2))

df <- as.data.frame(df)
df$YEAR <- substr(df$SALEDATE,1,4)

# 在数据框X 中有a,b,c,d,e5列，顺序排列，现在将d移动到a，b之间。
# X[,c('a','d','b','c','e')]







             
write.csv(df, "cleanedDC.csv")
