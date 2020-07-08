setwd("C:/Users/甘岱融/Desktop/event_song")
rm(list = ls())
load('save.RData')
require(glmnet)
require(e1071)
require(pls)
require(rpart)
require(class)
require(randomForest)
require(xgboost)
library(gbm)

data = read.csv("event_by_artist_location_df_rmna_20190924_origin.csv", header = TRUE, na.strings = "NULL")
new.data = read.csv("output.csv", header = TRUE, na.strings = "NULL")
ptt.saledate = read.csv("event_SaleDate.csv", header = TRUE, na.strings = "NULL")
ptt.saledate.year = read.csv("event_SaleDate(year).csv", header = TRUE, na.strings = "NULL")
write.table(del.data,file="onlydelNULL.csv",sep=",",row.names=F, na = "NA")
target = 13
data$sale_prop_ticket #predict
#刪除不合理資料-------
#歌手跟演唱會對不起來
data = data[-which(data$event_id %in% c(56245, 58036, 56036, 49754, 55903)),]
#售出票數>售票數
data = data[-which(data['total_quantity']>data['ticket_amount']),]

#不同歌手在不同大小演唱會的表現--------
table(table(data$artist_name[data$ticket_amount<500])&table(data$artist_name[data$ticket_amount>=500]))
threshold = 500
temp = table(data$artist_name[data$ticket_amount<threshold])&table(data$artist_name[data$ticket_amount>=threshold])
temp = temp[temp==TRUE]
singer.sale.compare = matrix(NA, nrow = nrow(temp), ncol = 3)
colnames(singer.sale.compare) = c('演唱會數','小演唱會平均售票率', '大演唱會平均售票率')
rownames(singer.sale.compare) = names(temp)
for( i in 1:nrow(temp)){
  singer.sale.compare[i,1] = sum(data$artist_name==names(temp)[i])   
  singer.sale.compare[i,2] = mean(data$sale_prop_ticket[data$artist_name==names(temp)[i] & data$ticket_amount<threshold])
  singer.sale.compare[i,3] = mean(data$sale_prop_ticket[data$artist_name==names(temp)[i] & data$ticket_amount>=threshold])
}
dim(singer.sale.compare)
plot(singer.sale.compare[,2]~singer.sale.compare[,3], xlab = colnames(singer.sale.compare)[3], ylab = colnames(singer.sale.compare)[2])
singer.sale.compare
#PTT data analysis-----
#city
plot(-10:7, colSums(ptt.saledate[,3:ncol(ptt.saledate)]), xlab='date', ylab = 'count', type='l', col='blue')
plot(log10(data$overall_mean_value_overall_pc.daily[which(data$ticket_amount>0)]), log10(data$ticket_amount[which(data$ticket_amount>0)]))
data$city[1:15]
data$city[which(data$city!='台北市')]='其他'
temp = data$city
boxplot(data$sale_prop_ticket~temp)
mean(data$sale_prop_ticket[which(data$city!='台北市')])

#開售前一年每日討論量
which(data$artist_name=='楊丞琳')
plot(-365:-1, colSums(ptt.saledate.year[which(data$artist_name!='楊丞琳 (Rainie Yang)'),3:ncol(ptt.saledate.year)]), xlab='date', ylab = 'count', type='l')
plot(-365:-1, colSums(ptt.saledate.year[which(ptt.saledate.year$total<100),3:ncol(ptt.saledate.year)]), xlab='date', ylab = 'count', type='l')

#divide into 12 months
ptt.saledate.year.month = matrix(NA, nrow = nrow(data), ncol = 14)
for(i in 1:12) { #ptt col 8~367
      ptt.saledate.year.month[,i+2] = rowSums(ptt.saledate.year[,(8+(30*(i-1))):(7+(30*i))])
      print(length(rowSums(ptt.saledate.year[,(8+(30*(i-1))):(7+(30*i))])))
      cat('(',(8+(30*(i-1))),' , ',(7+(30*i)),')\n')
}
ptt.saledate.year.month[,1] = ptt.saledate.year[,1]
ptt.saledate.year.month[,2] = ptt.saledate.year[,2]
colnames(ptt.saledate.year.month) = c('event_id', 'total', -12:-1)
sale.prop.threshold = 0.8
plot(-12:-1, colSums(ptt.saledate.year.month[which(data$sale_prop_ticket>sale.prop.threshold),3:ncol(ptt.saledate.year.month)]), xlab='related month', ylab = 'count', type='l', col= 'blue', ylim = c(500, 3500))
lines(-12:-1,colSums(ptt.saledate.year.month[ which(data$sale_prop_ticket<=sale.prop.threshold),3:ncol(ptt.saledate.year.month)]), col = "red")
legend("bottomright",legend = c(paste(">", sale.prop.threshold, sep=' '), paste("<=", sale.prop.threshold, sep=' ')), 
       col = c('red','blue'), pch = c(1,1))

#divide into 52 weeks
ptt.saledate.year.week = matrix(NA, nrow = nrow(data), ncol = 54)
dim(ptt.saledate.year.week)
for(i in 1:52) { #ptt col 8~367
  ptt.saledate.year.week[,i+2] = rowSums(ptt.saledate.year[,(4+(7*(i-1))):(3+(7*i))])
#  print(length(rowSums(ptt.saledate.year[,(4+(7*(i-1))):(3+(7*i))])))
  cat('(',(4+(7*(i-1))),' , ',(3+(7*i)),')\n')
}
ptt.saledate.year.week[,1] = ptt.saledate.year[,1]
ptt.saledate.year.week[,2] = ptt.saledate.year[,2]
colnames(ptt.saledate.year.week) = c('event_id', 'total', -52:-1)
sale.prop.threshold = 0.8
plot(-52:-1, colSums(ptt.saledate.year.week[which(data$sale_prop_ticket>sale.prop.threshold),3:ncol(ptt.saledate.year.week)])/length(which(data$sale_prop_ticket>sale.prop.threshold)), xlab='related week', ylab = 'count', type='l', col= 'blue', ylim = c(0, 5.5))
lines(-52:-1,colSums(ptt.saledate.year.week[which(data$sale_prop_ticket<=sale.prop.threshold),3:ncol(ptt.saledate.year.week)])/length(which(data$sale_prop_ticket<=sale.prop.threshold)), col = "red")
legend("topleft",legend = c(paste(">", sale.prop.threshold, sep=' '), paste("<=", sale.prop.threshold, sep=' ')), 
       col = c('blue','red'), pch = c(1,1))
write.table(ptt.saledate.year.week,file="PTT_saledate_year_week.csv",sep=",",row.names=F, na = "NA")

#前11個月的討論 與 前1個月的討論 比較
#前11個月的討論中位數
write.table(ptt.saledate.year.month,file="PTT_saledate_year_month.csv",sep=",",row.names=F, na = "NA")
temp = ptt.saledate.year.month[,14]/(median(ptt.saledate.year.month[,3:13])+1)
plot(data$sale_prop_ticket, temp)
plot(log10(data$total_sale), temp)

#設定 threshold(看要以售票率多少當標準值來砍)
sale.prop.threshold = 0.7
#售票率>threshold的演唱會
length(data$sale_prop_ticket[which(data$sale_prop_ticket>=sale.prop.threshold)])
mean(ptt.saledate$total[which(data$sale_prop_ticket>=sale.prop.threshold)])
mean(ptt.eventdate$total[which(data$sale_prop_ticket>=sale.prop.threshold)])
plot(-10:7, colSums(ptt.saledate[which(data$sale_prop_ticket>=sale.prop.threshold),3:ncol(ptt.saledate)]), xlab='date', ylab = 'count', type='l', col='blue')
plot(-10:-1, colSums(ptt.eventdate[which(data$sale_prop_ticket>=sale.prop.threshold),3:ncol(ptt.eventdate)]), xlab='date', ylab = 'count', type='l', col='blue')
plot(data$sale_prop_ticket[which(ptt.saledate$total<50)]~ptt.saledate$total[which(ptt.saledate$total<50)])


#開售前一年每日PTT PUSH, DOWN
ptt.saledate.push.year = read.csv("Ptt_singer_eventPUSH.csv", header = TRUE, na.strings = "NULL")
ptt.saledate.down.year = read.csv("Ptt_singer_eventDOWN.csv", header = TRUE, na.strings = "NULL")
dim(ptt.saledate.push.year)
dim(ptt.saledate.down.year)
plot(-365:-1, colSums(ptt.saledate.push.year[,3:ncol(ptt.saledate.push.year)]/(ptt.saledate.year[,3:ncol(ptt.saledate.year)]+1)), xlab='date', ylab = 'count', type='l')
plot(-365:-1, colSums(ptt.saledate.down.year[,3:ncol(ptt.saledate.push.year)]/(ptt.saledate.year[,3:ncol(ptt.saledate.year)]+1)), xlab='date', ylab = 'count', type='l')
#切成月為單位
ptt.saledate.push.year.month = matrix(NA, nrow = nrow(data), ncol = 14)
ptt.saledate.down.year.month = matrix(NA, nrow = nrow(data), ncol = 14)
for(i in 1:12) { #ptt col 8~367
  ptt.saledate.push.year.month[,i+2] = rowSums(ptt.saledate.push.year[,(8+(30*(i-1))):(7+(30*i))])
  ptt.saledate.down.year.month[,i+2] = rowSums(ptt.saledate.down.year[,(8+(30*(i-1))):(7+(30*i))])
  print(length(rowSums(ptt.saledate.push.year[,(8+(30*(i-1))):(7+(30*i))])))
  cat('(',(8+(30*(i-1))),' , ',(7+(30*i)),')\n')
}
ptt.saledate.push.year.month[,1] = ptt.saledate.push.year[,1] ; ptt.saledate.push.year.month[,2] = ptt.saledate.push.year[,2]
ptt.saledate.down.year.month[,1] = ptt.saledate.down.year[,1] ; ptt.saledate.down.year.month[,2] = ptt.saledate.down.year[,2]
colnames(ptt.saledate.push.year.month) = c('event_id', 'total', -12:-1)
colnames(ptt.saledate.down.year.month) = c('event_id', 'total', -12:-1)
write.table(ptt.saledate.push.year.month,file="PTT_saledate_push_year_month.csv",sep=",",row.names=F, na = "NA")
write.table(ptt.saledate.down.year.month,file="PTT_saledate_down_year_month.csv",sep=",",row.names=F, na = "NA")
#切成周圍單位
ptt.saledate.push.year.week = matrix(NA, nrow = nrow(data), ncol = 54)
ptt.saledate.down.year.week = matrix(NA, nrow = nrow(data), ncol = 54)
for(i in 1:52) { #ptt col 8~367
  ptt.saledate.push.year.week[,i+2] = rowSums(ptt.saledate.push.year[,(4+(7*(i-1))):(3+(7*i))])
  ptt.saledate.down.year.week[,i+2] = rowSums(ptt.saledate.down.year[,(4+(7*(i-1))):(3+(7*i))])
  print(length(rowSums(ptt.saledate.push.year[,(4+(7*(i-1))):(3+(7*i))])))
  cat('(',(4+(7*(i-1))),' , ',(3+(7*i)),')\n')
}
ptt.saledate.push.year.week[,1] = ptt.saledate.push.year[,1] ; ptt.saledate.push.year.week[,2] = ptt.saledate.push.year[,2]
ptt.saledate.down.year.week[,1] = ptt.saledate.down.year[,1] ; ptt.saledate.down.year.week[,2] = ptt.saledate.down.year[,2]
colnames(ptt.saledate.push.year.week) = c('event_id', 'total', -52:-1)
colnames(ptt.saledate.down.year.week) = c('event_id', 'total', -52:-1)
write.table(ptt.saledate.push.year.week,file="PTT_saledate_push_year_week.csv",sep=",",row.names=F, na = "NA")
write.table(ptt.saledate.down.year.week,file="PTT_saledate_down_year_week.csv",sep=",",row.names=F, na = "NA")

#每篇文章平均推數、每篇文章平均噓數
plot(data$sale_prop_ticket[which(ptt.saledate.year$total>0)]~ log(ptt.saledate.push.year$total[which(ptt.saledate.year$total>0)]/ptt.saledate.year$total[which(ptt.saledate.year$total>0)]), xlab='文章平均推文數', ylab = '售票率') #平均推數不高的文章，通常售票率都不好
plot(data$sale_prop_ticket~ log(ptt.saledate.down.year$total/ptt.saledate.year$total), xlab='文章平均噓文數', ylab = '售票率')

#前12個月，售票率在threshold上/下 平均文章推噓數
sale.prop.threshold = 0.8
plot(-12:-1, colSums(ptt.saledate.push.year.month[ which(data$sale_prop_ticket>sale.prop.threshold),3:ncol(ptt.saledate.push.year.month)]/(ptt.saledate.year.month[ which(data$sale_prop_ticket>sale.prop.threshold),3:ncol(ptt.saledate.year.month)]+1))/length( which(data$sale_prop_ticket>sale.prop.threshold))
            , xlab='related month', ylab = 'count', type='l', col= 'blue')
lines(-12:-1,colSums(ptt.saledate.push.year.month[ which(data$sale_prop_ticket<=sale.prop.threshold),3:ncol(ptt.saledate.push.year.month)]/(ptt.saledate.year.month[ which(data$sale_prop_ticket<=sale.prop.threshold),3:ncol(ptt.saledate.year.month)]+1))/length( which(data$sale_prop_ticket<=sale.prop.threshold)), col = "red")
legend("bottomright",legend = c(paste(">", sale.prop.threshold, sep=' '), paste("<=", sale.prop.threshold, sep=' ')), 
       col = c('blue','red'), pch = c(1,1))

plot(-12:-1, (colSums(ptt.saledate.push.year.month[which(data$sale_prop_ticket<=sale.prop.threshold),3:ncol(ptt.saledate.push.year.month)]/
                       (ptt.saledate.year.month[ which(data$sale_prop_ticket<=sale.prop.threshold),3:ncol(ptt.saledate.year.month)]+1)))/length( which(data$sale_prop_ticket<=sale.prop.threshold))
     , xlab='related month', ylab = 'count', type='l', col= 'blue')

#售票率低於threshold的演唱會
length(data$sale_prop_ticket[which(data$sale_prop_ticket<sale.prop.threshold)])
mean(ptt.saledate$total[which(data$sale_prop_ticket<sale.prop.threshold)])
data$event_id[which(ptt.saledate$total==max(ptt.saledate$total[which(data$sale_prop_ticket<sale.prop.threshold)]))]

mean(ptt.eventdate$total[which(data$sale_prop_ticket<sale.prop.threshold)])
plot(-10:7, colSums(ptt.saledate[which(data$sale_prop_ticket<sale.prop.threshold),3:ncol(ptt.saledate)]), xlab='date', ylab = 'count', type='l', col='blue')
plot(-10:-1, colSums(ptt.eventdate[which(data$sale_prop_ticket<sale.prop.threshold),3:ncol(ptt.eventdate)]), xlab='date', ylab = 'count', type='l', col='blue')

#DCARD data analysis-------
setwd("C:/Users/甘岱融/Desktop/event_song/Dcard")
dcard.saledate.year = read.csv("Dcard_everyday.csv", header = TRUE, na.strings = "NULL")
dcard.saledate.like.year = read.csv("Dcard_likeCount.csv", header = TRUE, na.strings = "NULL")
plot(-365:-1, colSums(dcard.saledate.year[140:nrow(dcard.saledate.year),3:ncol(dcard.saledate.year)]), xlab='related day', ylab = 'count', type='l', col= 'blue')
plot(-365:-1, colSums(dcard.saledate.like.year[,3:ncol(dcard.saledate.like.year)]), xlab='related day', ylab = 'count', type='l', col= 'blue')

#divide into 12 months
dcard.saledate.year.month = matrix(NA, nrow = nrow(data), ncol = 14)
dcard.saledate.like.year.month = matrix(NA, nrow = nrow(data), ncol = 14)
for(i in 1:12) { #dcard col 8~367
  dcard.saledate.year.month[,i+2] = rowSums(dcard.saledate.year[,(8+(30*(i-1))):(7+(30*i))])
  dcard.saledate.like.year.month[,i+2] = rowSums(dcard.saledate.like.year[,(8+(30*(i-1))):(7+(30*i))])
  print(length(rowSums(dcard.saledate.year[,(8+(30*(i-1))):(7+(30*i))])))
  cat('(',(8+(30*(i-1))),' , ',(7+(30*i)),')\n')
}
dcard.saledate.year.month[,1] = dcard.saledate.year[,1]
dcard.saledate.year.month[,2] = dcard.saledate.year[,2]
dcard.saledate.like.year.month[,1] = dcard.saledate.like.year[,1]
dcard.saledate.like.year.month[,2] = dcard.saledate.like.year[,2]
colnames(dcard.saledate.year.month) = c('event_id', 'total', -12:-1)
colnames(dcard.saledate.like.year.month) = c('event_id', 'total', -12:-1)
write.table(dcard.saledate.year.month,file="DCARD_saledate_year_month.csv",sep=",",row.names=F, na = "NA")
write.table(dcard.saledate.like.year.month,file="DCARD_saledate_like_year_month.csv",sep=",",row.names=F, na = "NA")

plot(-12:-1, colSums(dcard.saledate.year.month[140:nrow(dcard.saledate.year),3:ncol(dcard.saledate.year.month)]), xlab='related month', ylab = 'count', type='l', col= 'blue')
plot(-12:-1, colSums(dcard.saledate.like.year.month[,3:ncol(dcard.saledate.like.year.month)]), xlab='related month', ylab = 'count', type='l', col= 'blue')

#divide into 52 weeks
dcard.saledate.year.week = matrix(NA, nrow = nrow(data), ncol = 54)
dcard.saledate.like.year.week = matrix(NA, nrow = nrow(data), ncol = 54)
for(i in 1:52) { #ptt col 8~367
  dcard.saledate.year.week[,i+2] = rowSums(dcard.saledate.year[,(4+(7*(i-1))):(3+(7*i))])
  dcard.saledate.like.year.week[,i+2] = rowSums(dcard.saledate.like.year[,(4+(7*(i-1))):(3+(7*i))])
  cat('(',(4+(7*(i-1))),' , ',(3+(7*i)),')\n')
}
dcard.saledate.year.week[,1] = dcard.saledate.year[,1]
dcard.saledate.year.week[,2] = dcard.saledate.year[,2]
dcard.saledate.like.year.week[,1] = dcard.saledate.like.year[,1]
dcard.saledate.like.year.week[,2] = dcard.saledate.like.year[,2]
colnames(dcard.saledate.year.week) = c('event_id', 'total', -52:-1)
colnames(dcard.saledate.like.year.week) = c('event_id', 'total', -52:-1)
write.table(dcard.saledate.year.week,file="DCARD_saledate_year_week.csv",sep=",",row.names=F, na = "NA")
write.table(dcard.saledate.like.year.week,file="DCARD_saledate_like_year_week.csv",sep=",",row.names=F, na = "NA")

plot(-52:-1, colSums(dcard.saledate.year.week[,3:ncol(ptt.saledate.year.week)]), xlab='related week', ylab = 'count', type='l', col= 'blue')
plot(-52:-1, colSums(dcard.saledate.like.year.week[,3:ncol(dcard.saledate.like.year.week)]), xlab='related week', ylab = 'count', type='l', col= 'blue')



#反應變數跟解釋變數的plot------
for(i in 1:ncol(data))  
  if(length(which(data[,i]==-99))>0)
    cat(i,'\t',length(which(data[,i]==-99)),'\n')
library(keypress)
for(i in c(6:11,18:ncol(data))){
  if(max(data[,i])==Inf)
    print(max(data[which(data[,i]!=Inf),i]))
}
for(i in c(1:5,12:17))
  if(length(which(data[,i]==''))>0)
    print(length(which(data[,i]=='')))
#Corelation coefficient------
count = 1

temp = rep(0,length(c(6:11,18:ncol(data))))
temp = rep(0, length(c(1:6,11:ncol(del.data))))
#for(i in c(6:11,18:ncol(data))){
names(del.data)[c(1:6,11:ncol(del.data))]
ncol(del.data)
for(i in c(1:6,11:ncol(del.data))){
    # temp[count]=cor(data$sale_prop_ticket[which(data[,i]!=-99&data[,i]!=Inf&data[,i]!=-Inf)],
    #              data[which(data[,i]!=-99&data[,i]!=Inf&data[,i]!=-Inf),i])
    temp[count]=cor(del.data$sale_prop_ticket, del.data[,i])
    
    # temp[count]= cor.test(data$sale_prop_ticket[which(data[,i]!=-99&data[,i]!=Inf&data[,i]!=-Inf)],
    #                       data[which(data[,i]!=-99&data[,i]!=Inf&data[,i]!=-Inf),i])$p.value
    if(is.na(temp[count])==TRUE)
      cat(i,'\t',max(del.data[,i]),'\t',min(del.data[,i]),'\n')
    count = count+1
}
length(temp)
for(i in 1:10)
  cat((i-1)/10, '~',i/10,'\t',length( which( abs(temp)>=(i-1)/10 & abs(temp)<i/10) ),'\n')

#除了revenue之外，其他的變數跟反應變數的相關係數都很低
cor(data$sale_prop_revenue, data$sale_prop_ticket)
for(j in 1:57){
  dev.off()
  par(mfrow = c(3,3))
  for(i in (9*j+27):(9*j+35)){
    if(i<=ncol(data)){
      plot(data$sale_prop_ticket[which(data[,i]!=-99)]~data[which(data[,i]!=-99),i])
    }
  }
  cat((9*j+27),':',(9*j+35),'\n')
  readline(prompt='')
}

#other----------
#-Inf
count = 0
cat(length(which(data[,498]==-99)),'\t',length(which(data[,498]==Inf)),'\n')
for(i in 1:ncol(data))
  cat(length(which(data[171,])))

for(i in 1:nrow(data))
  if(length(which(data[i,]==-Inf))>0)
    print(i)

data[171,498] #-inf
names(data)[498]
cat(498,'\t',(cor(data$sale_prop_ticket[which(data[-171,498]!=-99)],data[which(data[-171,498]!=-99),498])),'\n')
cat(498,'\t',(cor(data$sale_prop_ticket[which(data[-171,498]!=-99&data[-171,498]!=Inf)],
                  data[which(data[-171,498]!=-99&data[-171,498]!=Inf),498])),'\n')


(cor(data$sale_prop_ticket[which(data[,498]!=-99&data[,498]!=Inf)],
                   data[which(data[,498]!=-99&data[,498]!=Inf),498]))


#c.v. set & iteration------
k = 5 ; iter = 5
cv = c(1)
for( i in 1:(k-1))
  cv = c(cv, i*floor(nrow(del.data)/k), i*floor(nrow(del.data)/5)+1)
cv = c(cv, nrow(del.data))

#Test--------
new.data = cbind(new.data, ptt.saledate[,-1])
iter = 10
for(m in 11){ #different models
  set.seed(314)
  rmse.result = mape.result = mae.result = c()
  for(j in 1:iter){ #看要做幾次
    #cat('#iter = ', j, '\n')
    test.data = new.data[sample(1:nrow(new.data), nrow(new.data)),] #把資料打亂
    #  test.data = test.data[,c(17,22:ncol(new.data))]
    cross.result = c()
    if(m>6 &&m<11){ #classification
      temp = test.data$sale_prop_ticket
      test.data$sale_prop_ticket[which(test.data$sale_prop_ticket==1)]=1
      test.data$sale_prop_ticket[which(test.data$sale_prop_ticket!=1)]=0
      test.data$sale_prop_ticket = factor(test.data$sale_prop_ticket)
    }
    for( i in 1:k){ #k-fold cross validation
      #    print(i)
      train = test.data[-(cv[2*i-1]:cv[2*i]),]
      test = test.data[cv[2*i-1]:cv[2*i],]
      #    cat('train : ', dim(train), '\t', 'test : ', dim(test), '\n')
      tryCatch({
        if(m==1){
          if(j==1 & i==1)
            print('linear regression')
          model = lm(train$sale_prop_ticket~., data = train)
          model.result = predict(model, test)
        } #simple linear regression
        else if(m==2){ 
          if(j==1 & i==1)
            print('lasso regression')
          model =  cv.glmnet(x = as.matrix(train[, -17]), y = train[, 17], alpha = 1,family = "gaussian")
          model.result = predict(model, s = model$lambda.min, newx = as.matrix(test[,-17]))
        }#lasso
        else if(m==3){ 
          if(j==1 & i==1)
            print('ridge regression')
          model =  glmnet(x = as.matrix(train[, -target]), y = train[, target], alpha = 0,family = "gaussian")
          bestlam = model$lambda.min
          model =  cv.glmnet(x = as.matrix(train[, -target]), y = train[, target], alpha = 0,family = "gaussian", lambda = bestlam)
          model.result = predict(model, s = model$lambda.min, newx = as.matrix(test[,-target]))
        }#ridge
        else if(m==4){ 
          if(j==1 & i==1)
            print('support vector regression')
          model = svm(train$sale_prop_ticket~., data = train)
          model.result = predict(model, test)
        } #SVR
        else if(m==5){ 
          if(j==1 & i==1)
            print('Principal Component Regression')
          model = pcr(train$sale_prop_ticket~., data = train, scale = TRUE, validation = 'CV', ncomp = 59)
          model.result = predict(model, test, ncomp = 59) #48個component就可以解釋95%的變異
          #這種方法會爛掉
          # model = pcr(train$sale_prop_ticket~., data = train, scale = TRUE, validation = 'CV')
          # bestcomp = model$ncomp
          # model = pcr(train$sale_prop_ticket~., data = train, scale = TRUE, validation = 'CV', ncomp = bestcomp)
          # model.result = predict(model, test, ncomp = bestcomp)
        }#PCR
        else if(m==6){ 
          if(j==1 & i==1)
            print('Partial Least Squares Regression')
          model = plsr(train$sale_prop_ticket~., data = train, scale = TRUE, validation = 'CV', ncomp = 95)
          model.result = predict(model, test, ncomp = 95) #95個component就可以解釋95%的變異
        }#pls
        #Classification
        else if(m==7){ 
          if(j==1 & i==1)
            print('logistic regression')
          model = glm(train$sale_prop_ticket~., family = "binomial", data = train)
          model.result = predict(model, type = 'response',newdata = test)
        }#logistic
        else if(m==8){ 
          if(j==1 & i==1)
            print('decision tree')
          model = rpart(train$sale_prop_ticket~., data = train)
          model.result = predict(model, newdata = test)[,2]
        }#decision tree
        else if(m==9) { 
          if(j==1 & i==1)
            print('Support Vector Machine')
          model = svm(train$sale_prop_ticket~., data = train, kernel = "radial", probability = TRUE)
          model.result = predict(model, newdata = test, probability = TRUE)
          model.result = attr(model.result, "probabilities")[,2]
        }#SVM
        else if(m==10){ 
          if(j==1 & i==1)
            print('Random Forest')
          model = randomForest(train$sale_prop_ticket~., data = train, mtry = 3, ntree = 100, probability = TRUE)
          model.result = predict(model, newdata = test, type = "prob")[,2]
        }#random forest
        #advanced model
        else if(m==11){
          if(j==1 & i==1)
            print('Gradient Boosting Machine')
          gbm.model = gbm(train$sale_prop_ticket~., data = train,  distribution='gaussian', n.trees = 1000, cv.folds = 5,
                          shrinkage = 0.1, interaction.depth = 3)
          best.iter = gbm.perf(gbm.model,method='cv')
          temp$rel.inf = temp$rel.inf + summary(gbm.model, order = F)$rel.inf
          model.result = predict(gbm.model, test, n.trees = best.iter)
        }#gbm
        else if(m==12){
          if(j==1 & i==1)
            print('eXtreme Gradient Boosting')
          model =  xgboost(data = as.matrix(train[, -target]), label = train[,target], 
                           booster = 'gbtree',
                           objective = 'reg:linear',
                           gamma = 0.1,
                           max_depth = 6, 
                           lambda = 2,
                           subsample = 0.8,
                           colsample_bytree = 0.8,
                           eta = 0.01,
                           nround=10000, 
                           eval_metric = "rmse",
                           nthread = 3,
                           verbose = 0, #
                           early_stopping_rounds = 5 #
          )
          print(xgb.importance(model = model))
          model.result = predict(model, newdata = as.matrix(test[,-target]) )
        }#xgboost
      },
      error = function(msg){
        message(msg)
        return(NA)
      })
      cross.result = c(cross.result, model.result)
    }
    cross.result[which(cross.result>1)]=1
    cross.result[which(cross.result<0)]=0
    if(m>6 && m<11){
      rmse.result = c(rmse.result, rmse(temp, cross.result))
      mape.result = c(mape.result, mape(temp, cross.result))
      mae.result  = c(mae.result,  mae(temp, cross.result))
    }
    else {
      rmse.result = c(rmse.result, rmse(test.data$sale_prop_ticket, cross.result))
      mape.result = c(mape.result, mape(test.data$sale_prop_ticket, cross.result))
      mae.result  = c(mae.result,  mae(test.data$sale_prop_ticket, cross.result))
    }      
  }
  cat('RMSE:',mean(rmse.result),'\n')
  cat('MAPE:',mean(mape.result),'\n')
  cat('MAE :',mean(mae.result),'\n')
}
show_temp = temp[order(temp$rel.inf, decreasing = TRUE), ]
show_temp[1:10,]
for(i in 1:10)
  cat(rownames(show_temp)[i],'\t\t\t\t', show_temp$rel.inf[i], '\n')

plot(new.data$sale_prop_ticket[which(ptt.saledate$total<100)], ptt.saledate$total[which(ptt.saledate$total<100)])
#preprocessing-----
#年月要當作類別資料
#7 8 9 10

for(i in 7:10){
  new.data[,i] = factor(new.data[,i])
  print(head(new.data[,i]))  
  #  del.data[,i] = factor(del.data[,i])
#  print(head(del.data[,i]))  
}

#features 0~1
new.data = del.data
for(i in c(1:6,11:ncol(new.data))){
  if(max(new.data[,i])>1 | min(new.data[,i])<0){
    print(names(new.data)[i])
    new.data[,i] = (new.data[,i]-min(new.data[,i]))/(max(new.data[,i])-min(new.data[,i]))
  }
}
#compare-----
compare = data.frame(DNN=python.predict$DNN, GBM=cross.result, ANS=data$sale_prop_ticket)
length(cross.result)
length(python.predict$DNN)
length(data$sale_prop_ticket)
hist(compare$DNN)
boxplot(compare)
hist(compare$DNN, main = 'DNN')
hist(compare$GBM, main = 'GBM')
hist(compare$ANS, main ='TRUE')
hist(abs(compare$ANS-compare$GBM), main = 'abs(GBM-TRUE)')
sum(data$sale_prop_ticket[which(data$ticket_amount>0)] - (data$total_quantity[which(data$ticket_amount>0)]/data$ticket_amount[which(data$ticket_amount>0)]))

gbm_predict_diff = del.data$sale_prop_ticket - cross.result
temp.data = data

temp.data = cbind(temp.data, gbm_predict_diff)
order(temp.data$gbm_predict_diff)
temp.data$artist_name[order(temp.data$gbm_predict_diff)[1]]
temp.data$artist_name[temp.data$artist_name[order(temp.data$gbm_predict_diff)]]
for(i in 1:10){
  cat(temp.data$artist_name[order(temp.data$gbm_predict_diff)[i]])
}
hist(compare$DNN[which(abs(temp.data$gbm_predict_diff)>0.8)], main='不準確的場次實際數值')
hist(compare$DNN, main='所有實際數值')
hist(temp.data$ticket_amount[which(abs(temp.data$gbm_predict_diff)>0.8)], main='不準確場次座位分布')
hist(temp.data$ticket_amount, main = '母體座位分布')
temp.data$location[which(abs(temp.data$gbm_predict_diff)>0.8)]
temp.data$gbm_predict_diff[order(-abs(temp.data$gbm_predict_diff))[1:10]]
for(i in 1:10){
  cat(order(-abs(temp.data$gbm_predict_diff))[i], '\t',
      temp.data$gbm_predict_diff[order(-abs(temp.data$gbm_predict_diff))[i]],'\n')
}
cross.result[order(-abs(temp.data$gbm_predict_diff))[i]]

#create del.data------
#del.data = data[,-c(1:5, 14, 17, 18, 22, 26)]#delete id, string etc.
del.data = data
del = c()
for( i in 1:ncol(del.data)){
  if( length(del.data[which(del.data[,i]==-99),i])+
      length(del.data[which(del.data[,i]==Inf),i])+
      length(del.data[which(del.data[,i]==-Inf),i])>0){
    del = c(del, i)
    print(names(del.data)[i])    
  }
}
del.data = del.data[,-del]
del.data = del.data[,-which(names(del.data)=='sale_prop_revenue')]
new.data = del.data
hist(data$overall_mean_value_overall_pc.daily[which(data$overall_mean_value_overall_pc.daily<1e+3)])
length(which(data$overall_mean_value_overall_pc.daily>1e+4 | data$ticket_amount>1e+3))
length(which(data$overall_mean_value_overall_pc.daily<1e+3 & data$ticket_amount<300))

#create new data------
new.data = data[,-c(1:5, 14, 17, 18, 22, 26)]#delete id, string etc.
new.data = data
for(i in 1:ncol(new.data)){ #填補遺失值
  new.data[which(new.data[,i]==-99),i]=0
  new.data[which(new.data[,i]==-Inf),i]=0
  if(length(which(new.data[,i]==Inf)>0)){
    print(names(new.data)[i])
    new.data[which(new.data[,i]==Inf),i]=mean(new.data[which(new.data[,i]!=Inf),i])
  }
}
data = read.csv("event_by_artist_location_df_rmna_20190924_origin.csv", header = TRUE, na.strings = "NULL")

data[ncol(data)]
target = 17

#估計------
rmse = function(actual, predict){
  return(sqrt(mean((actual - predict)^2))) 
}
mae = function(actual, predict){
  return(mean(abs(actual-predict)))
}
mape = function(actual, predict){
  return(100*mean(abs(actual-predict)/actual))
}

