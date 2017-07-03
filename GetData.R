# THIS SCRIPT USES SMATERPOLAND TO RIP NATIONAL ACCOUNTS DATA FROM EUROSTAT
# AND CONVERT IT TO THE LONG DATA FORMAT

#Set your working directory
setwd("/Users/tomokeeffe/Desktop/R")

## Create output directory if it doesn't already exist
dir.create(file.path("data-eurostat","raw"), recursive = TRUE)
dir.create(file.path("data-eurostat","long"), recursive = TRUE)

library(reshape2)
library(SmarterPoland)
library(xlsx)
library(devtools)
library(dplyr)
library(zoo)
library(gdata)
library(gridExtra)
library(pdfetch)
library(ecb)
library(lubridate)
library(plyr)

nasa_10_nf_tr<-getEurostatRaw("nasa_10_nf_tr")
nasa_10_f_tr<-getEurostatRaw("nasa_10_f_tr")
prc_hicp_aind <- getEurostatRaw("prc_hicp_aind")

##################################################################################
#SAVING RAW DATA, MAKE SURE PROPER WORKING DIRECTORY
##################################################################################

#raw esa 95
save(nasa_10_nf_tr, file="data-eurostat/raw/nasa_10_nf_tr.RData")
save(nasa_10_f_tr, file="data-eurostat/raw/nasa_10_f_tr.RData")
save(prc_hicp_aind, file="data-eurostat/raw/prc_hicp_aind.RData")

##################################################################################
#transformation function
##################################################################################

#This transformation function converts the raw data to long format
#it assumes you've already loaded the data
trans<-function(df){
  data<-melt(df)
  colsplit<-colsplit(data[,1],",", as.vector(strsplit(names(data)[1],split=",")[[1]]))
  output<-data.frame(colsplit,data[2:length(data)])
  names<-names(output)
  names<-gsub("variable","time",names)
  names(output)<-names
  return(output)
}
##################################################################################
#TRANSFORMING DATA
##################################################################################
#now we transform the raw data into long format

#trans esa 95
nasa_10_nf_tr<-trans(nasa_10_nf_tr)
nasa_10_f_tr<-trans(nasa_10_f_tr)
prc_hicp_aind<-trans(prc_hicp_aind)

##################################################################################
#SAVING LONG DATA, MAKE SURE PROPER WORKING DIRECTORY
##################################################################################
#long esa 95

#long esa 95
save(nasa_10_nf_tr, file="data-eurostat/long/nasa_10_nf_tr.RData")
save(nasa_10_f_tr, file="data-eurostat/long/nasa_10_f_tr.RData")
save(prc_hicp_aind, file="data-eurostat/long/prc_hicp_aind.RData")
###################################################################################
#Filtering Data
###################################################################################

#Load from my wd() to save time
#load("/Users/tomokeeffe/data-eurostat/long/nasa_10_f_tr.RData")
#load("/Users/tomokeeffe/data-eurostat/long/nasa_10_nf_tr.RData")
#load("/Users/tomokeeffe/data-eurostat/long/prc_hicp_aind.RData")

nasa_10_f_tr$time <-  as.Date(nasa_10_f_tr$time, format("%Y"))
nasa_10_f_tr$time <- year(nasa_10_f_tr$time)

nasa_10_nf_tr$time <-  as.Date(nasa_10_nf_tr$time, format("%Y"))
nasa_10_nf_tr$time <- year(nasa_10_nf_tr$time)

prc_hicp_aind$time <-  as.Date(prc_hicp_aind$time, format("%Y"))
prc_hicp_aind$time <- year(prc_hicp_aind$time)

nasa_10_f_tr <- nasa_10_f_tr[order(as.Date(nasa_10_f_tr$time, format="%Y")),]
nasa_10_nf_tr <- nasa_10_nf_tr[order(as.Date(nasa_10_nf_tr$time, format="%Y")),]
prc_hicp_aind <- prc_hicp_aind[order(as.Date(prc_hicp_aind$time, format="%Y")),]


hh_nl_prv <- filter(nasa_10_f_tr, finpos == "LIAB", sector == "S14_S15", co_nco == "CO", unit == "MIO_EUR", na_item == "B9F")
#NFC NET LENDING
nfc_nl_prv<- filter(nasa_10_f_tr,unit == "MIO_EUR",na_item == "B9F", finpos == "LIAB", sector == "S11", co_nco == "CO")
#FC NET LENDING
fc_nl_prv<- filter(nasa_10_f_tr, unit == "MIO_EUR",na_item == "B9F", finpos == "LIAB", sector == "S12", co_nco == "CO")

tmp2<-filter(nasa_10_nf_tr, unit=="CP_MEUR")
tmp2<-filter(NonFinData,unit =="CP_MEUR")
tmp21<-filter(prc_hicp_aind, coicop=="CP00",unit=="INX_A_AVG")

hh_nl_prv <- filter(hh_nl_prv,  finpos == "LIAB", sector == "S14_S15", co_nco == "CO", unit == "MIO_EUR", na_item == "B9F")
#hh_nl_prv <- hh_nl_prv[order(as.Date(hh_nl_prv$time, format="%Y-%m-%d")),]

#NFC NET LENDING
nfc_nl_prv<- filter(nfc_nl_prv,unit == "MIO_EUR",na_item == "B9F",finpos == "LIAB", sector == "S11", co_nco == "CO")
# nfc_nl_prv <- nfc_nl_prv[order(as.Date(nfc_nl_prv$time, format="%Y-%m-%d")),]

#FC NET LENDING
fc_nl_prv<- filter(fc_nl_prv,unit == "MIO_EUR",na_item == "B9F", finpos == "LIAB", sector == "S12", co_nco == "CO")
#fc_nl_prv <- fc_nl_prv[order(as.Date(fc_nl_prv$time, format="%Y-%m-%d")),]

gdp<-filter(tmp2,na_item=="B1GQ", direct=="PAID")
DF <- subset(gdp,select = c("geo.time","time", "value"))
DF <- rename(DF, c("value" = "gdp"))

#general govt balance
gen_gov_balance<-filter(tmp2,na_item=="B9",direct=="PAID", sector=="S13")
gen_gov_balance <- subset(gen_gov_balance,select = c("geo.time","time", "value") )
DF <- merge(DF,gen_gov_balance, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "gen_gov_balance"))
DF$gen_gov_balance_pc<-as.numeric(as.character(DF$gen_gov_balance))/as.numeric(as.character(DF$gdp))*100

DF$gdp_d <- (DF$gdp/lag(DF$gdp, 1) -1)*100

gov_exp<-filter(tmp2,na_item=="OTE", direct=="PAID", sector=="S13")
gov_exp <- subset(gov_exp,select = c("geo.time","time", "value"))
DF <- merge(DF,gov_exp, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "gov_exp"))

#government tax revenue
gov_trev <- filter(tmp2,na_item == "OTR",sector == "S13", direct == "RECV")
gov_trev <- subset(gov_trev,select = c("geo.time","time", "value"))
DF <- merge(DF,gov_trev, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "gov_trev"))    

#theta - tax share
DF$theta<-as.numeric(as.character(DF$gov_trev))/as.numeric(as.character(DF$gdp))

#Adjusted fiscal ratio
DF$AFR<-as.numeric(as.character(DF$gov_exp))/as.numeric(as.character(DF$theta))
DF$AFR_d <- (DF$AFR/lag(DF$AFR,1) -1)*100

#current extrnal balance - BOP - ROW view # REVERSE IN PLOT
bop<-filter(tmp2,na_item=="B12",direct=="PAID",sector=="S2")
bop$value<-bop$value*-1   #ok it's reversed here and a pc of gdp value is done
bop <- subset(bop,select = c("geo.time","time", "value"))
DF <- merge(DF,bop, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "bop"))
DF$bop_pc<-as.numeric(as.character(DF$bop))/as.numeric(as.character(DF$gdp))*100

#exports in goods
bop_gexp<-filter(tmp2,na_item=="P61",direct=="PAID",sector=="S2")
bop_gexp <- subset(bop_gexp,select = c("geo.time","time", "value"))
DF <- merge(DF,bop_gexp, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "bop_gexp"))

#imports in goods
bop_gimp<-filter(tmp2,na_item=="P71",direct=="RECV",sector=="S2")
bop_gimp <- subset(bop_gimp,select = c("geo.time","time", "value"))
DF <- merge(DF,bop_gimp, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "bop_gimp"))    

#balance of trade in goods
DF$bop_goods<-as.numeric(as.character(DF$bop_gexp))-as.numeric(as.character(DF$bop_gimp))

#balance of trade in goods (pc of gdp)
DF$bop_goods_pc<-as.numeric(as.character(DF$bop_goods))/as.numeric(as.character(DF$gdp))*100

#exports in services
bop_sexp<-filter(tmp2,na_item=="P62",direct=="PAID",sector=="S2")
bop_sexp <- subset(bop_sexp,select = c("geo.time","time", "value"))
DF <- merge(DF,bop_sexp, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "bop_sexp")) 

#imports in services
bop_simp<-filter(tmp2,na_item=="P72",direct=="RECV",sector=="S2")
bop_simp <- subset(bop_simp,select = c("geo.time","time", "value"))
DF <- merge(DF,bop_simp, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "bop_simp")) 

#balance of trade in services
DF$bop_soods<-as.numeric(as.character(DF$bop_sexp))-as.numeric(as.character(DF$bop_simp))
#balance of trade in services (pc of gdp)
DF$bop_soods_pc<-as.numeric(as.character(DF$bop_soods))/as.numeric(as.character(DF$gdp))*100

#primary/secondary balances
pb_bal1<-filter(tmp2, na_item %in% c("IN1","IN21"),direct=="PAID",sector=="S2")
pb_bal1<-aggregate(pb_bal1$value, by=list(Category= pb_bal1$unit, pb_bal1$direct, pb_bal1$sector,pb_bal1$geo.time,pb_bal1$time), FUN=sum)
pb_bal1 <- rename(pb_bal1, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
pb_bal1 <- subset(pb_bal1,select = c("geo.time","time", "value"))
DF <- merge(DF,pb_bal1, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "pb_bal1"))  

pb_bal2<-filter(tmp2,na_item%in%c("IN1","IN21"),direct=="RECV",sector=="S2")
pb_bal2<-aggregate(pb_bal2$value, by=list(Category= pb_bal2$unit, pb_bal2$direct, pb_bal2$sector,pb_bal2$geo.time,pb_bal2$time), FUN=sum)
pb_bal2 <- rename(pb_bal2, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
pb_bal2 <- subset(pb_bal2,select = c("geo.time","time", "value"))
DF <- merge(DF,pb_bal2, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "pb_bal2"))

DF$pb_bal<-(as.numeric(as.character(DF$pb_bal1)))-(as.numeric(as.character(DF$pb_bal2)))
DF$pb_bal_pc<- (as.numeric(as.character(DF$pb_bal))/as.numeric(as.character(DF$gdp)))*100

#Import propensity
DF$mu<-(DF$bop_simp+DF$bop_gimp+as.numeric(DF$pb_bal2))/DF$gdp

#Adjusted Trade Ratio 
DF$ATR<-(DF$bop_sexp+DF$bop_gexp+as.numeric(DF$pb_bal1))/DF$mu

DF$ATR_d <- (DF$ATR/lag(DF$ATR,1) -1)*100

#CFTR
DF$CFTR<-(DF$bop_sexp+DF$bop_gexp+DF$pb_bal1+DF$gov_exp)/(DF$mu+DF$theta)
DF$CFTR_d <- (DF$CFTR/lag(DF$CFTR,1) -1)*100

#FINANCIAL BALANCES
#Saving 
S <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "B9", direct == "PAID")
S<-aggregate(S$value, by=list(Category= S$unit, S$direct, S$na_item,S$geo.time,S$time), FUN=sum)
S <- rename(S, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
S <- subset(S,select = c("geo.time","time", "value"))
DF <- merge(DF,S, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "S"))     

DF$S_pc<-(DF$S/DF$gdp)*100

#government Balance
GDEF<-subset(gen_gov_balance, select = c( "geo.time","time", "value"))
DF$GDEF <- DF$gen_gov_balance*-1
DF$GDEF_pc<-(DF$GDEF/DF$gdp)*100
#Current Account
CA<-filter(tmp2,na_item=="B9",direct=="PAID",sector=="S2")
CA <- subset(CA,select = c("geo.time","time", "value"))
DF <- merge(DF,CA, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "CA"))  
DF$CA_pc<-(DF$CA/DF$gdp)*100

hh_saving<-filter(tmp2,na_item=="B8G",direct=="RECV",sector=="S14_S15")
hh_saving <- subset(hh_saving,select = c("geo.time","time", "value"))
DF <- merge(DF,hh_saving, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "hh_saving"))

hh_inv<-filter(tmp2,na_item=="P5G",direct=="PAID",sector=="S14_S15")
hh_inv <- subset(hh_inv,select = c("geo.time","time", "value"))
DF <- merge(DF,hh_inv, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "hh_inv"))

DF$hh_sav_m_inv<-as.numeric(DF$hh_saving)-as.numeric(DF$hh_inv)
DF$hh_sav_m_inv_pc<-(DF$hh_sav_m_inv/DF$gdp)*100

nfc_saving<-filter(tmp2,na_item=="B8G",direct=="RECV",sector=="S11")
nfc_saving <- subset(nfc_saving,select = c("geo.time","time", "value"))
DF <- merge(DF,nfc_saving, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "nfc_saving"))

nfc_inv<-filter(tmp2,na_item=="P5G",direct=="PAID",sector=="S11")
nfc_inv <- subset(nfc_inv,select = c("geo.time","time", "value"))
DF <- merge(DF,nfc_inv, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "nfc_inv"))

DF$nfc_sav_m_inv<-as.numeric(DF$nfc_saving)-as.numeric(DF$nfc_inv)
DF$nfc_sav_m_inv_pc<-(DF$nfc_sav_m_inv/DF$gdp)*100

#Non_Fin Saving
S_nonfin <- filter(tmp2,sector %in% c("S14_S15","S11"),na_item == "B9", direct == "PAID")
S_nonfin<-aggregate(S_nonfin$value, by=list(Category= S_nonfin$unit, S_nonfin$direct, S_nonfin$na_item,S_nonfin$geo.time,S_nonfin$time), FUN=sum)
S_nonfin <- rename(S_nonfin, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
S_nonfin <- subset(S_nonfin,select = c("geo.time","time", "value"))
DF <- merge(DF,S_nonfin, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "S_nonfin")) 

DF$S_pc2<-(DF$S_nonfin/DF$gdp)*100

#real private disposable income
disp_inc_nonfin<- filter(tmp2,sector %in% c("S14_S15","S11"),na_item == "B6G", direct == "RECV")
disp_inc_nonfin<-aggregate(disp_inc_nonfin$value, by=list(Category= disp_inc_nonfin$unit, disp_inc_nonfin$direct, disp_inc_nonfin$na_item,disp_inc_nonfin$geo.time,disp_inc_nonfin$time), FUN=sum)

disp_inc_nonfin <- rename(disp_inc_nonfin, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
disp_inc_nonfin <- subset(disp_inc_nonfin,select = c("geo.time","time", "value"))
DF <- merge(DF,disp_inc_nonfin, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "disp_inc_nonfin"))

cpi <- subset(tmp21, select = c("geo.time","time","value"))
DF<- merge(DF, cpi, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "cpi"))

#real private disposable income
disp_inc <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "B6G", direct == "RECV")
disp_inc<-aggregate(disp_inc$value, by=list(Category= disp_inc$unit, disp_inc$direct, disp_inc$na_item,disp_inc$geo.time,disp_inc$time), FUN=sum)
disp_inc <- rename(disp_inc, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
disp_inc <- subset(disp_inc,select = c("geo.time","time", "value"))
DF <- merge(DF,disp_inc, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "disp_inc"))    

DF$disp_inc<-(DF$disp_inc/DF$cpi)*100
DF$disp_inc2_d <- (DF$disp_inc/lag(DF$disp_inc,1) -1)*100

#real private expenditure
prv_exp <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "P3", direct == "PAID")
prv_exp<-aggregate(prv_exp$value, by=list(Category= prv_exp$unit, prv_exp$direct, prv_exp$na_item, prv_exp$geo.time, prv_exp$time), FUN=sum)
prv_exp <- rename(prv_exp, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
prv_exp <- subset(prv_exp,select = c("geo.time","time", "value"))
DF <- merge(DF,prv_exp, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "prv_exp"))     

DF$prv_exp<-(DF$prv_exp/DF$cpi)*100
DF$prv_exp_d <- (DF$prv_exp/lag(DF$prv_exp,1) -1)*100

nl_prv <- subset(hh_nl_prv, select = c("geo.time","time", "value"))
DF<- merge(DF, nl_prv, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "nl_prv"))

nfc_nl_prv <- subset(nfc_nl_prv, select = c("geo.time","time", "value"))
DF<- merge(DF, nfc_nl_prv, all = TRUE, by =  c("geo.time","time"))
DF <- rename(DF, c("value" = "nfc_nl_prv"))

DF$nl_prv<- DF$nl_prv +as.numeric(as.character(DF$nfc_nl_prv)) 
DF$nl_prv_pc<-(as.numeric(DF$nl_prv))/(DF$disp_inc_nonfin)*-100


DF$S_pc_disp<-(DF$S_nonfin/DF$disp_inc_nonfin)*100

#DF <- filter(DF, geo.time %in% list("US", "EA","EA18","EEA","EU","HR","ME","RS"))
DF <- filter(DF, geo.time != "US")
DF <- filter(DF, geo.time != "EA")
DF <- filter(DF, geo.time != "EA18")
DF <- filter(DF, geo.time != "EEA")
DF <- filter(DF, geo.time != "EU")
DF <- filter(DF, geo.time != "HR")
DF <- filter(DF, geo.time != "ME")
DF <- filter(DF, geo.time != "RS")


#DF$gdp_d <- (DF$gdp/lag(DF$gdp, 1) -1)*100
save(DF, file = "/Users/tomokeeffe/Desktop/R/7Processes/AllData.RData")

