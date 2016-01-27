## Emilio Zagheni
## Sept 16, 2013
## Analyze MTUS data for Descriptive Findings series of Demographic Research
## The publication can be found here: http://demographic-research.org/volumes/vol29/35/default.htm
## README, IMPORTANT: this code requires the MTUS W53 data set ("MTUS_W53.sav") that you could download for free from the Multinational Time Use Study webpage (http://www.timeuse.org/mtus/download). The size of the file is about 86 MB.
##The only other data input required is the file "datapopsize.csv", extracted from the UN World Population Prospects 2010. You can find that file in this folder.
##The code will generate other datasets from the two data files. The datasets will be use as input for some parts of the analysis
##The code will also generate a number of figures in pdf format
## The code was tested on the R version 3.0.1 


rm(list=ls())
library("foreign")
data<- read.spss("MTUS_W53.sav", to.data.frame=TRUE)

#variables for time use activities
#av6: cook wash up
#av7 housework
#av8: other domestic work
#av9 gardening
#av10 shopping
#av11 childcare
#av12 domestic travel
data$tot_domestic <- data$av6+ data$av7+ data$av8+ data$av9+ data$av10+ data$av11+ data$av12



#paesi are the selected countries
paesi<- c("Italy","Spain","Germany","France")
#eta is a variable that takes on single years of age considered
eta<- c(1:80)
#sesso" is sex of indiviudals
sesso<- c("Man","Woman")


#list of surveys in the MTUS dataset:
# Italy: 1988, 1989, 2002, 2003
# USA: 1965, 1966, 1975, 1976, 1985, 1992, 1993, 1994, 1995, 2003, 2004, 2005, 2006, 2007
# Germany 1991, 1992, 2001, 2002
# France 1998, 1999
# Netherlands 1975, 1980, 1985, 1990, 2000, 2005 no hh id
# Spain 2002, 2003
# United Kingdom (UK) 1974, 1975, 1983, 1984, 1995, 2000, 20001, 2005 2005 No household id
# Austria 1992
# Australia 1974
# Denmark 1964, 1987
# Israel 1991, 1992
####################





### PART I - No household structure ####
## For each country and sex, generate a profile of time production by age and store it in an array

Man <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Man)[[2]]<- paesi

Woman <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Woman)[[2]]<- paesi

time.prod<-list(Man=Man,Woman=Woman)
time.sample<- list(Man=Man,Woman=Woman)

for (iicountry in paesi){
for (iisex in sesso){
profilo<-NULL
campione<- NULL
for (iiage in eta){
 indagine<- max(unique(data$survey[data$countrya==c(iicountry)]))
profilo<- c(profilo,mean(data$tot_domestic[data$countrya==iicountry & data$age==iiage & data$sex==iisex & data$survey== indagine]))
campione<- c(campione,length(data$tot_domestic[data$countrya==iicountry & data$age==iiage & data$sex==iisex & data$survey== indagine]))
}
time.prod[[iisex]][is.na(profilo)==FALSE,iicountry]<- (supsmu(1:length(profilo[is.na(profilo)==FALSE]),profilo[is.na(profilo)==FALSE]))$y
time.sample[[iisex]][is.na(campione)==FALSE,iicountry]<- campione[is.na(campione)==FALSE]
}
}





### Figures for production profiles

cou<- c("Italy","Spain","Germany","France")
par(mfrow=c(2,2))
for (ii in cou){
plot(((time.prod[[c("Man")]][,ii])/60), type="l", ylim=c(0,9),lwd=2,col="blue", main=ii,ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(((time.prod[[c("Woman")]][,ii])/60) ,lwd=2,col="red")
legend("topleft",c("Female","Male"),lty=c(1,1),col=c("red","blue"),lwd=c(2,2))
}



##total sample size
time.sample.tot<- time.sample[["Man"]] + time.sample[["Woman"]]
apply(time.sample.tot,2,sum)




##############
## CONSUMPTION PROFILES
##############

####
## CREATE THE DATASETS WITH DUMMIES. NEEDS TO BE DONE ONLY THE FIRST TIME YOU RUN THE CODE
###


## Choose the countries for which you would like to create dummy variables. Running this loop may take a little while. This is a good time for a coffee break or a nap 

for (paese.analisi in c("France","Germany","Italy","Spain")){
#paese.analisi<-c("France")
	
indagine<- max(unique(data$survey[data$countrya==paese.analisi]))
data.cou.survey<- subset(data, countrya==paese.analisi & survey==indagine)
datatemp<-subset(data.cou.survey,select=c("age","hldid","tot_domestic","hhldsize"))


agg.ages<- aggregate(datatemp$age, by=list(datatemp$hldid), c)

fun_lunghezza<- function(input){
	result<-length(unlist(input))
    return(result)
}

resp.size<- sapply(agg.ages[,2],fun_lunghezza)

agg.hh.size<- aggregate(datatemp$hhldsize, by=list(datatemp$hldid), unique)


select.hh<-cbind(agg.hh.size,resp.size)
names(select.hh)<- c("hhid","hhsize","nresp")
select.hh$discrep<- select.hh$hhsize- select.hh$nresp
if (paese.analisi=="Germany"){select.hh$discrep<- (select.hh$nresp/select.hh$hhsize) - 3 }

#select.hh$hhid[select.hh$discrep==0]


datasub<- subset(datatemp, hldid %in% select.hh$hhid[select.hh$discrep==0])
if (paese.analisi=="USA") {datasub<- datatemp}

datahh <-as.data.frame(matrix(rep(0,length(unique(datasub$hldid))*84),length(unique(datasub$hldid)),84))

names(datahh)<- c(as.character(c(0:80)),"country", "hhid", "time_dom")

datahh$hhid<- unique(datasub$hldid)

agg.dom.time<- aggregate(datasub$tot_domestic, by=list(datasub$hldid), sum)
agg.hh.size<- aggregate(datasub$hhldsize, by=list(datasub$hldid), unique)

datahh$time_dom<- agg.dom.time[2]

agg.ages<- aggregate(datasub$age, by=list(datasub$hldid), c)




for (ii in 1:length(datahh$hhid)){
##for (ii in 1:500){
tabella <- table(agg.ages[ii,2])
indici<- as.numeric(names(tabella))
valori <- as.numeric(tabella)
datahh[ii,(indici+1)]<- valori
}


nomi<- NULL
for (ii in 0:80){
nomi<- c(nomi,paste("V",ii, sep=""))
}
names(datahh)[1:81] <- nomi
datahh$ydata<- (as.numeric(as.vector(unlist(datahh[,84]))))
datahh$country<- paese.analisi


write.csv(as.matrix(datahh[,c(1:81,85)]),paste("datahh_",paese.analisi,".csv",sep=""),row.names=FALSE)

} ## end paese.analisi loop


##creation of datapopsize for WPP2010 data
#dataUN<- read.csv("WPP2010_INT_F3_POPULATION_BY_AGE_ANNUAL_SINGLE_MEDIUM.CSV")
#paesi.sel<- paesi
#paesi.sel[11]<- c("United States of America")
#datapopsize <- subset(dataUN, Location %in% paesi.sel)
#write.csv(datapopsize,"datapopsize.csv")

########################
## One-time run ends
#######################


time.cons.prelim <- as.data.frame(matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi)))
dimnames(time.cons.prelim)[[2]]<- paesi

time.cons <- as.data.frame(matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi)))
dimnames(time.cons)[[2]]<- paesi

average.ages <- as.data.frame(matrix(rep(NA,3*length(paesi)),3,length(paesi)))
dimnames(average.ages)[[2]]<- paesi
rownames(average.ages)<- c("male.prod","female.prod","cons")

popsize<-list(Man=Man,Woman=Woman)

datapopsize <- read.csv("datapopsize.csv")



#### Generate consumption profiles for selected countries #####


for (country.sel in c("Italy", "France","Spain","Germany")){
#for (country.sel in c("Germany")){

datacou.sel<- read.csv(paste("datahh_",country.sel,".csv",sep=""))

model.fit <- lm(ydata ~ -1+V0+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+V58+V59+V60+V61+V62+V63+V64+V65+V66+V67+V68+V69+V70+V71+V72+V73+V74+V75+V76+V77+V78+V79+V80 , datacou.sel)


coeff<- as.numeric(model.fit$coeff)


time.cons.prelim[supsmu(c(0:80),coeff)$x,country.sel]<- supsmu(c(0:80),coeff)$y


country.location<- country.sel
if (country.sel==c("USA")){ country.location<- c("United States of America") }
popsize[["Man"]][,country.sel] <- as.numeric(subset(datapopsize, Location==country.location & Time==2002 & Sex== c("Male"))[c(10:88,90)])

popsize[["Woman"]][,country.sel] <- as.numeric(subset(datapopsize, Location==country.location & Time==2002 & Sex== c("Female"))[c(10:88,90)])


pop.time.prod <- (time.prod[["Man"]][,country.sel] * popsize[["Man"]][,country.sel]) + (time.prod[["Woman"]][,country.sel] * popsize[["Woman"]][,country.sel])

tot.time.prod<- sum(na.omit(pop.time.prod))

pop.time.cons<- time.cons.prelim[,country.sel] * (popsize[["Man"]][,country.sel]+popsize[["Woman"]][,country.sel])
tot.time.cons<- sum(na.omit(pop.time.cons))

correction_factor <- tot.time.prod/tot.time.cons

##if no correction
#correction_factor<-1

time.cons[,country.sel]<- correction_factor * time.cons.prelim[,country.sel]


#plot(time.cons[,country.sel]/60,ylim=c(0,10))
#lines(time.prod[["Man"]][,country.sel]/60)
#lines(time.prod[["Woman"]][,country.sel]/60)


## Life cycle deficit

LCDmale<- time.cons[,country.sel]-time.prod[["Man"]][,country.sel]
LCDfemale<- time.cons[,country.sel]-time.prod[["Woman"]][,country.sel]


#plot(LCDmale/60,ylim=c(-5,5))
#lines(LCDfemale/60,ylim=c(-5,5))

### mean age at production and consumption

num.male<-sum(na.omit(c(1:80)*time.prod[["Man"]][,country.sel]*popsize[["Man"]][,country.sel]))
den.male<- sum(na.omit(time.prod[["Man"]][,country.sel]*popsize[["Man"]][,country.sel]))

male.mean.age.prod<- num.male/den.male
average.ages[1,country.sel]<-male.mean.age.prod

num.female<-sum(na.omit(c(1:80)*time.prod[["Woman"]][,country.sel]*popsize[["Woman"]][,country.sel]))
den.female<- sum(na.omit(time.prod[["Woman"]][,country.sel]*popsize[["Woman"]][,country.sel]))

female.mean.ageprod <- num.female/den.female
average.ages[2,country.sel]<-female.mean.ageprod


num.cons<- sum(na.omit(c(1:80)*time.cons[,country.sel]*(popsize[["Man"]][,country.sel]+popsize[["Woman"]][,country.sel])))

den.cons<- sum(na.omit(time.cons[,country.sel]*(popsize[["Man"]][,country.sel]+popsize[["Woman"]][,country.sel])))

mean.age.cons <- num.cons/den.cons
average.ages[3,country.sel]<-mean.age.cons


} #of country loop
######



time.cons
time.prod
average.ages


#cou<- c("Germany")
for (cou in c("Italy", "France","Spain","Germany")){

pdf(paste(cou,"_prod_cons.pdf",sep=""))
plot(((time.prod[[c("Man")]][,cou])/60), type="l", ylim=c(0,9),lwd=2.5,col="blue", main=cou,ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2,lty=2)
lines(((time.prod[[c("Woman")]][,cou])/60) ,lwd=2.5,col="red",lty=4)
lines(((time.cons[,cou])/60) ,lwd=2,col="black")
legend("topleft",c("Female Time Production","Male Time Production","Time Consumption"),lty=c(4,2,1),col=c("red","blue","black"),lwd=c(2.5,2.5,2))
dev.off()



pdf(paste(cou,"_life_cycle_deficit.pdf",sep=""))
plot(-((time.prod[[c("Man")]][,cou])/60)+((time.cons[,cou])/60), type="l", ylim=c(-5,5),lwd=2,col="blue", main=paste(cou, " - Time Life Cycle Deficit", sep=""),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(-((time.prod[[c("Woman")]][,cou])/60)+((time.cons[,cou])/60) ,lwd=2,col="red")
abline(h=0, lty=c(3))
legend("topleft",c("Female","Male"),lty=c(1,1),col=c("red","blue"),lwd=c(2,2))
dev.off()
}

pdf("men_life_cycle_deficit.pdf")
plot(-((time.prod[[c("Man")]][,"Italy"])/60)+((time.cons[,"Italy"])/60), type="l", ylim=c(-5,5),lwd=2,col="blue", main=c("Men - Time Life Cycle Deficit"),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(-((time.prod[[c("Man")]][,"Spain"])/60)+((time.cons[,"Spain"])/60) ,lwd=2,col="red")
lines(-((time.prod[[c("Man")]][,"France"])/60)+((time.cons[,"France"])/60) ,lwd=2,col="black")
#lines(-((time.prod[[c("Man")]][,"Austria"])/60)+((time.cons[,"Austria"])/60) ,lwd=2,col="black")
lines(-((time.prod[[c("Man")]][,"Germany"])/60)+((time.cons[,"Germany"])/60) ,lwd=2,col="green")
abline(h=0, lty=c(3))
legend("bottomright",c("Italy","Spain","France","Germany"),lty=c(1,1,1,1),col=c("blue","red","black","green"),lwd=c(2,2,2,2))
dev.off()

pdf("women_life_cycle_deficit.pdf")
plot(-((time.prod[[c("Woman")]][,"Italy"])/60)+((time.cons[,"Italy"])/60), type="l", ylim=c(-5,5),lwd=2,col="blue", main=c("Women - Time Life Cycle Deficit"),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(-((time.prod[[c("Woman")]][,"Spain"])/60)+((time.cons[,"Spain"])/60) ,lwd=2,col="red")
lines(-((time.prod[[c("Woman")]][,"France"])/60)+((time.cons[,"France"])/60) ,lwd=2,col="black")
#lines(-((time.prod[[c("Woman")]][,"Austria"])/60)+((time.cons[,"Austria"])/60) ,lwd=2,col="black")
lines(-((time.prod[[c("Woman")]][,"Germany"])/60)+((time.cons[,"Germany"])/60) ,lwd=2,col="green")
abline(h=0, lty=c(3))
legend("topright",c("Italy","Spain","France","Germany"),lty=c(1,1,1,1),col=c("blue","red","black","green"),lwd=c(2,2,2,2))
dev.off()



###########################################
#### Part II - time production by household structure  
###########################################
#### disaggregate by number of children in the hh


Man <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Man)[[2]]<- paesi

Woman <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Woman)[[2]]<- paesi

time.prod0K<-list(Man=Man,Woman=Woman)
time.prod1K<-list(Man=Man,Woman=Woman)
time.prod1olderK<-list(Man=Man,Woman=Woman)
#time.prod3K<-list(Man=Man,Woman=Woman)
#time.prod[[c("Man")]][,c("Austria")]
sample0K<-list(Man=Man,Woman=Woman)
sample1K<-list(Man=Man,Woman=Woman)
sample1olderK<-list(Man=Man,Woman=Woman)





for (iicountry in paesi){
indagine<- max(unique(data$survey[data$countrya==c(iicountry)]))
data_subset<-subset(data, country==iicountry & (famstat==c("Aged 18 to 39 with no coresident children <18") | famstat==c("Aged 40+ with no coresident children <18")) & survey==indagine)
for (iisex in sesso){
profilo<-NULL
campione<-NULL
for (iiage in eta){
profilo<- c(profilo,mean(data_subset$tot_domestic[data_subset$age==iiage & data_subset$sex==iisex]))
campione<- c(campione,length(data_subset$tot_domestic[data_subset$age==iiage & data_subset$sex==iisex]))
}
time.prod0K[[iisex]][,iicountry]<- profilo
sample0K[[iisex]][,iicountry]<- campione
}
}


for (iicountry in paesi){
indagine<- max(unique(data$survey[data$countrya==c(iicountry)]))
data_subset<-subset(data, countrya==iicountry & (famstat==c("Aged 18+ living with 1+ coresident children aged <5") ) & survey==indagine)
for (iisex in sesso){
profilo<-NULL
campione<-NULL
for (iiage in eta){
profilo<- c(profilo,mean(data_subset$tot_domestic[data_subset$age==iiage & data_subset$sex==iisex]))
campione<- c(campione,length(data_subset$tot_domestic[data_subset$age==iiage & data_subset$sex==iisex]))
}
time.prod1K[[iisex]][,iicountry]<- profilo
sample1K[[iisex]][,iicountry]<- campione

}
}


for (iicountry in paesi){
indagine<- max(unique(data$survey[data$countrya==c(iicountry)]))
data_subset<-subset(data, countrya==iicountry & (famstat==c("Aged 18+ living with 1+ coresident children 5-17, none <5") ) & survey==indagine)
for (iisex in sesso){
profilo<-NULL
campione<-NULL
for (iiage in eta){
profilo<- c(profilo,mean(data_subset$tot_domestic[data_subset$age==iiage & data_subset$sex==iisex]))
campione<- c(campione,length(data_subset$tot_domestic[data_subset$age==iiage & data_subset$sex==iisex]))
}
time.prod1olderK[[iisex]][,iicountry]<- profilo
sample1olderK[[iisex]][,iicountry]<- campione

}
}


##sanity check
#apply(sample0K[["Woman"]] + sample0K[["Man"]] + sample1K[["Woman"]] + sample1K[["Man"]] + sample1olderK[["Woman"]] + sample1olderK[["Man"]],2,sum)


cou<- c("France", "Italy","Spain","Germany")

#par(mfrow=c(3,2))
for (ii in cou){
pdf(paste(ii,"_women_time_production_by_hh_structure.pdf",sep=""))
plot(supsmu(c(1:80),time.prod0K[[c("Woman")]][,ii])$x,(supsmu(c(1:80),time.prod0K[[c("Woman")]][,ii])$y)/60, type="l", ylim=c(0,10),xlim=c(18,50),lwd=2,ylab=c("Hours per day"),xlab=c("Age"),cex.axis=1.2,cex.lab=1.2,main= paste(ii," - Women",sep="")   )
 lines(supsmu(c(1:80),time.prod1K[[c("Woman")]][,ii])$x,(supsmu(c(1:80),time.prod1K[[c("Woman")]][,ii])$y)/60, lwd=2.5, col="red",lty=2)
lines(supsmu(c(1:80),time.prod1olderK[[c("Woman")]][,ii])$x,(supsmu(c(1:80),time.prod1olderK[[c("Woman")]][,ii])$y)/60,lwd=2.5, col="blue",lty=4)
legend("bottomright",c("No children <18 in hh","1+ child <5 in hh","1+ child 5-17 in hh"),col=c("black","red","blue"),lwd=c(2,2.5,2.5),lty=c(1,2,4),bty="n")
dev.off()
}


for (ii in cou){
pdf(paste(ii,"_men_time_production_by_hh_structure.pdf",sep=""))
plot(supsmu(c(1:80),time.prod0K[[c("Man")]][,ii])$x,(supsmu(c(1:80),time.prod0K[[c("Man")]][,ii])$y)/60, type="l", ylim=c(0,10),xlim=c(18,50),lwd=2,ylab=c("Hours per day"),xlab=c("Age"),cex.axis=1.2,cex.lab=1.2,main=paste(ii," - Men",sep="")  )
 lines(supsmu(c(1:80),time.prod1K[[c("Man")]][,ii])$x,(supsmu(c(1:80),time.prod1K[[c("Man")]][,ii])$y)/60, lwd=2.5, col="red",lty=2)
lines(supsmu(c(1:80),time.prod1olderK[[c("Man")]][,ii])$x,(supsmu(c(1:80),time.prod1olderK[[c("Man")]][,ii])$y)/60,lwd=2.5, col="blue",lty=4)
legend("topright",c("No children <18 in hh","1+ child <5 in hh","1+ child 5-17 in hh"),col=c("black","red","blue"),lwd=c(2,2.5,2.5),lty=c(1,2,4),bty="n")
dev.off()
}


###########
##########
##Extra graphs on time production for specific activities, etc.
############
###########

Man <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Man)[[2]]<- paesi

Woman <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Woman)[[2]]<- paesi

time.prod.childcare<-list(Man=Man,Woman=Woman)

for (iicountry in paesi){
for (iisex in sesso){
profilo<-NULL
for (iiage in eta){
 indagine<- max(unique(data$survey[data$countrya==c(iicountry)]))
profilo<- c(profilo,mean(data$av11[data$countrya==iicountry & data$age==iiage & data$sex==iisex & data$survey== indagine]))
}
time.prod.childcare[[iisex]][is.na(profilo)==FALSE,iicountry]<- (supsmu(1:length(profilo[is.na(profilo)==FALSE]),profilo[is.na(profilo)==FALSE]))$y
}
}

pdf("men_childcare.pdf")
plot(((time.prod.childcare[[c("Man")]][,"Italy"])/60), type="l", ylim=c(0,2.5),lwd=2,col="blue", main=c("Men - Childcare"),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(((time.prod.childcare[[c("Man")]][,"Spain"])/60) ,lwd=2.5,lty=2,col="red")
lines(((time.prod.childcare[[c("Man")]][,"France"])/60) ,lwd=2.5,lty=4,col="black")
lines(((time.prod.childcare[[c("Man")]][,"Germany"])/60) ,lwd=2.5,lty=5,col="green")
legend("topright",c("Italy","Spain","France","Germany"),lty=c(1,2,4,5),col=c("blue","red","black","green"),lwd=c(2,2.5,2.5,2.5))
dev.off()

pdf("women_childcare.pdf")
plot(((time.prod.childcare[[c("Woman")]][,"Italy"])/60), type="l", ylim=c(0,2.5),lwd=2,col="blue", main=c("Women - Childcare"),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(((time.prod.childcare[[c("Woman")]][,"Spain"])/60) ,lty=2,lwd=2.5,col="red")
lines(((time.prod.childcare[[c("Woman")]][,"France"])/60) ,lty=4,lwd=2.5,col="black")
lines(((time.prod.childcare[[c("Woman")]][,"Germany"])/60) ,lty=5,lwd=2.5,col="green")
legend("topright",c("Italy","Spain","France","Germany"),lty=c(1,2,4,5),col=c("blue","red","black","green"),lwd=c(2,2.5,2.5,2.5))
dev.off()


Man <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Man)[[2]]<- paesi

Woman <- matrix(rep(NA,length(eta)*length(paesi)),length(eta),length(paesi))
dimnames(Woman)[[2]]<- paesi

time.prod.housework<-list(Man=Man,Woman=Woman)

for (iicountry in paesi){
for (iisex in sesso){
profilo<-NULL
for (iiage in eta){
 indagine<- max(unique(data$survey[data$countrya==c(iicountry)]))
profilo<- c(profilo,mean(data$av7[data$countrya==iicountry & data$age==iiage & data$sex==iisex & data$survey== indagine]))
}
time.prod.housework[[iisex]][is.na(profilo)==FALSE,iicountry]<- (supsmu(1:length(profilo[is.na(profilo)==FALSE]),profilo[is.na(profilo)==FALSE]))$y
}
}



pdf("men_housework.pdf")
plot(((time.prod.housework[[c("Man")]][,"Italy"])/60), type="l", ylim=c(0,2.5),lwd=2,col="blue", main=c("Men - Housework"),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(((time.prod.housework[[c("Man")]][,"Spain"])/60) ,lwd=2.5,lty=2, col="red")
lines(((time.prod.housework[[c("Man")]][,"France"])/60) ,lwd=2.5,lty=4,col="black") 
lines(((time.prod.housework[[c("Man")]][,"Germany"])/60) ,lwd=2.5, lty=5, col="green")
legend("topright",c("Italy","Spain","France","Germany"),lty=c(1,2,4,5),col=c("blue","red","black","green"),lwd=c(2,2.5,2.5,2.5))
dev.off()

pdf("women_housework.pdf")
plot(((time.prod.housework[[c("Woman")]][,"Italy"])/60), type="l", ylim=c(0,2.5),lwd=2,col="blue", main=c("Women - Housework"),ylab="Hours per day", xlab="Age",cex.lab=1.2,cex.axis=1.2)
lines(((time.prod.housework[[c("Woman")]][,"Spain"])/60) ,lwd=2.5,lty=2,col="red")
lines(((time.prod.housework[[c("Woman")]][,"France"])/60) ,lwd=2.5,lty=4,col="black")
lines(((time.prod.housework[[c("Woman")]][,"Germany"])/60) ,lwd=2.5,lty=5,col="green")
legend("bottomright",c("Italy","Spain","France","Germany"),lty=c(1,2,4,5),col=c("blue","red","black","green"),lwd=c(2,2.5,2.5,2.5))
dev.off()

############


#############
### END
############






