library(dplyr)
datainfo = read.csv("com2017.csv",stringsAsFactors = F)
map  = read.csv("mapdata.csv",stringsAsFactors = F)
info = read.csv("competemodel.csv",stringsAsFactors = F)
x0 = filter(info,addnum == 0)
x1 = filter(info,addnum == 1)[c(T,T,T,T,F),]
x2 = filter(info,addnum == 2)[c(T,T,T,F,F),]
x3 = filter(info,addnum == 3)[c(T,T,F,F,F),]
x4 = filter(info,addnum == 4)[c(T,T,T,T,F),]
xx = rbind(x0,x1,x2,x3,x4)
t1 = xx$ModelIdx[!duplicated(xx$ModelIdx)]


for(i in 1:length(t1))
{
or = filter(datainfo,modelIdx ==  t1[i])
temdata = datainfo %>% na.omit()
temdata$ps = 1/(abs(log(temdata$meanprice/or$meanprice,10))+1)
#make similarity of price 

segnum =filter(temdata,Segment == or$Segment)$amo %>% sum()
temdata$segper = 0
temdata$segper[temdata$Segment == or$Segment] = temdata$amo[temdata$Segment == or$Segment]/segnum
#make market share 
temdata$segs = 0
for(j in 1:nrow(temdata))
{
  if(temdata[j,]$Segment == or$Segment)
  {
  temdata$segs = 1/(abs(log(temdata$segper/(or$amo/segnum),10))+1)
  }
}
#make similarity of marker share

temdata$y = 0
ids = filter(xx,ModelIdx == t1[i])$competeIdx
temdata$y[temdata$modelIdx %in% ids] = 1
#make y


contains = filter(datainfo,modelIdx %in% ids)$Subsegment
dataex = filter(temdata, (Subsegment %in% contains)|(y==1))
#first choose

mp = or$highprice
lp = or$lowprice
sigma = -0.3
#scale parameter
dataex = filter(dataex,!(lowprice > (1+sigma)*mp | highprice <(1-sigma)*lp))
#secondchoose
if(i == 1){
  finadata = dataex
}else{
  finadata = rbind(finadata,dataex)
}
print(i)
}





