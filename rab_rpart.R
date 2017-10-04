library(rpart)

### import dataset
rab <- read.delim("D:/Dropbox/Ongoing/tree_roundabout/data/rab_approach_tree.txt")
View(rab)

### get training set and test set
#75% of the sample size
smp_size <- floor(0.75 * nrow(rab))
#set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(rab)), size = smp_size)
#training set and test set
train <- rab[train_ind, ]
test <- rab[-train_ind, ]

### Crash Type Model
## get tree with training set
trn <- rpart(crash.type~AADTMaj+AADTMin+RABRad+LnWdth+DefAngl+FlareLngth+entryRadius
             +YieldOnLeft+SawtoothYieldMark+YieldPvtMark+sharktoothORYIELD+PedCrossMark+ContRTLn
             +ramp,data=train,method="class")

# general roundabout variables
# urbRur+PSTDSPD+AppLn+CircLn+ConsecRAB+LandscapingOnCntrlIsl+numApp

#1. plot tree
plot(trn,compress=T)
text(trn,use.n=F,all=F,cex=.75,xpd=T)

#2. print and plot cp
printcp(trn,digits=4)
plotcp(trn)

#3. evaluate the full tree using test data


#test.pred <- predict(trn,test)
#RMSE.t <- sqrt(mean((test.pred-test$crash.type)^2))
#RMSE.t
#MAE.t <- mean(abs(test.pred-test$crash.type))
#MAE.t

#4. prune tree using the best cp
bestcp <- trn$cptable[which.min(trn$cptable[,"xerror"]),"CP"]
trn.p <- prune(trn, cp = bestcp)
plot(trn.p,compress=T)
text(trn.p,use.n=F,all=F,cex=.75,xpd=T)

#5. evaluate the pruned tree using test data
#test.pred.p <- predict(trn.p,test)
#RMSE.t.p <- sqrt(mean((test.pred.p-test$kabcmv_l)^2))
#RMSE.t.p
#MAE.t.p <- mean(abs(test.pred.p-test$kabcmv_l))
#MAE.t.p
