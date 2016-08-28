library(devtools)


#set.seed(294832)


myF=function(dat)
{
        K=table(dat$sub,dat$cond)
        N=sum(K)
        I=dim(K)[1]
        J=dim(K)[2]
        kstar=K[,1]*K[,2]/(K[,1]+K[,2])
        condMean=tapply(dat$y,list(dat$sub,dat$cond),mean)
        d=condMean[,2]-condMean[,1]
        dstar=d*sqrt(kstar)
        s1=var(dstar)
        ss=sum((K-1)*tapply(dat$y,list(dat$sub,dat$cond),var))
        s2=ss/(N-I*J)  
        return(s1/s2)
}


##Get data sets

SourceURL <- "https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/contexteffects/FlankerStroopSimon/cleaning.R"
source_url(SourceURL)

# ##Stroop Data (Data Set 1)
# filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/contexteffects/FlankerStroopSimon/LEF_stroop.csv")
# stroop <- read.csv2(filename, header=TRUE, dec=".")
# 
# stroop$cond <- as.numeric(stroop$congruency)  #congruent -> 1, incongruent -> 2, neutral -> 3
# ntrial <- length(stroop[stroop$ID == stroop$ID[1], 1])
# nsub <- length(unique(stroop$ID))
# stroop$trial <- rep(1:ntrial, nsub)
# stroop$rt <- stroop$RT/1000 #rt data in seconds
# 
# stroop <- stroop[stroop$rt > .2 & stroop$rt < 2, ]
# stroop <- subset(stroop, accuracy == 1 & cond != 3)

dat=stroop

##############################
#rename dat$rt as dat$y
dat$y=dat$rt

f.stat=myF(dat)

##############################
#

#check myF with simulation.
#under the null or otherwise

I=100
J=2
K=50

#######################
#######################
R=10000
fstat=1:R

for (r in 1:R)
{
t.sig=.3
t.base=rnorm(I,.9,.2)
t.effect=rnorm(I,.65,0)
t.mu=matrix(nrow=I,ncol=J)
t.mu[,1]=t.base
t.mu[,2]=t.base+t.effect

sub=rep(1:I,each=J*K)
cond=rep(rep(1:J,each=K),I)
t.m=t.mu[cbind(sub,cond)]
y=rnorm(I*J*K,t.m,t.sig)

dat=data.frame(sub,cond,y)

f.stat[r]=myF(dat)
}


#plot theoretical f and histogram!

df1=I-1
df2=I*J*(K-1)


f=seq(.01,2,.01)
png('checkF.png')
hist(f.stat,breaks=50,xlim=c(0,2),prob=T,main="Check Derived F\n balanced design")
lines(typ='l',f,df(f,df1,df2),col='red')
dev.off()