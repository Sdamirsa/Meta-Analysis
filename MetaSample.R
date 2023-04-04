if(!require('writexl')) {
  install.packages('writexl')
  library('writexl')
}

if(!require('readxl')) {
  install.packages('readxl')
  library('readxl')
}

if(!require('dplyr')) {
  install.packages('readxl')
  library('readxl')
}

if(!require('metafor')) {
  install.packages('readxl')
  library('readxl')
}

if(!require('robumeta')) {
  install.packages('writexl')
  library('writexl')
}

#for rma.glmn effect model
#install.packages("lme4")
library(lme4)

#vignette("metafor")   #this command will open metafor help
#vignette("diagram")   #this command will open metafor diagram
#https://cran.r-project.org/web/packages/metafor/metafor.pdf
#look to page 71 of metafor package for escals vairables
#handy videos to see #https://www.youtube.com/watch?v=d1pYHfCKhyA&t=328s
#handy videos to see #https://www.youtube.com/watch?v=IkduL5iRdqo&t=62s

#mean standard deviation group size    #Quantitative Variables:
#group 1 m1i sd1i n1i
#group 2 m2i sd2i n2i
#measure of effect mitone MD, SMD, SMDH, SMD1, ROM

#outcome 1 outcome 2 total        #Dichotomous variable
#group 1 ai bi n1i
#group 2 ci di n2i
#measure of effect mitone RR, OR, RD, AS, PETO
 

#number of events total person-time   # Event Counts
#group 1 x1i t1i
#group 2 x2i t2i
#measure of effect mitone IRR, IRD, IRSD

#Variable Association Two Quantitative Variables
#ri correlation coeefieint
#ni sample size
#measure of effect mitone COR, UCOR,ZCOR bashe

#Outcome Measures for Individual Groups:
# Quantitative Variables
#mi, sdi, ni
#measure of effect mitone MN, MNLN, CVLN, SDLN

#Dichotomous Variables
#xi, ni / xi, mi
#measure of effect mitone RP, PLN, PLO, PAS, PFT

#Event Counts
#xi ti
#measure of effect mitone IR, IRLN, IRS, IRFT

directory="D:\\MY WORK\\MaGHALE\\BMI and gastric cancer\\Meta\\data.xlsx"
sheetname= "Obesity-T"
dat=read_xlsx(directory, sheet= sheetname )
dat



#load data to metafor using escals
dat <- escalc(measure="RR", ci=control_event, ai=case_event,
              n2i= control_total, n1i=case_total,
              data=dat, slab=paste(Id ,Author, Year, sep=", ")) 
dat

#randoom effect
res <-  rma(yi, vi, data=dat)
res

#prediced pooled risk ratio and CI
predict(res, transf = exp, digits= 3)

#forrest
forest(res, atransf=exp, addpred=FALSE, header = TRUE, xlim=c(-2,2.4), at=log(c(0.5,1,2,3)),
       main = "Obesity")
text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)



#funnel
funnel(res, main = "Obesity")
regtest(res) 
ranktest(res) 

#leave1out
l1o <-leave1out(res)

forest(l1o$estimate, sei=l1o$se, header=TRUE, atransf=exp,main="Leave One Out Estimate", 
       refline=coef(res), at=c(coef(res),log(1),log(1.5)))
l1o


#outliers
baujat(res)
inf <- influence(res)
print(inf)
plot(inf)

# Moderating effect of age
#res.modage <- rma(yi, vi, mods = ~ Race, data=dat) #for numerical
res.mes <- rma(yi, vi, mods = ~ factor(Race), data=dat)  #for categorical
#res.modq <- rma(yi, vi, mods = ~ quality, data=dat) 
res.mes

#plot all in one
plot(res, atransf=transf.ztor)
#report of model
confint(res)  

#gosh plot for hetrogenicity
gosh <- gosh(res, subset=2000)
plot(gosh)

#final beautiful forrest
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

ci=control_event, ai=case_event,
n2i= control_total, n1i=case_total

forest(res, atransf=exp, addpred=FALSE, header = TRUE, 
       xlim=c(-6,2.4), #in taiin mikone kol shkle az koja ta koja bashe arzesh
       ylim=c(-1, 14), #in taiin mikone kol skel az koja ta koja bashe ertefash
       ilab=cbind(dat$case_event, dat$case_total, dat$control_event, dat$control_total), #data haro neshon mide
       ilab.xpos=c(-4,-3,-2,-1), #in mahale har ilab ro taiin mikone
       cex=0.90, #position ilab ro taain mikone
       at=log(c(0.5,1,2,3)), #in tik mark haye khatkesho taiin mikone
       main = "Obesity", 
       mlab=mlabfun("RE Model for All Studies", res))

# cumulative meta-analyis
cum <- cumul(res, order=dat$Year)
cum
forest(cum,atransf=exp,  header=TRUE, main="Cummulative meta-analysis")


#gererate method and result
reporter (res)
#reporter (res, format-"pdf"?.
#reporter (res, format-"word*)

