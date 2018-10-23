# Manual Trades
# One account in Redis is supported. 
# Other accounts should be in csv as the script allows for a single args[2] + args[3] pair. 

timer.start=Sys.time()
library(RTrade)
library(log4r)
library(tableHTML)
library(gmailr)

options(scipen=999)

#### PARAMETERS ####
#### PARAMETERS ####
options(scipen = 999)
#options(warn=2)

args.commandline = commandArgs(trailingOnly = TRUE)
if (length(args.commandline) > 0) {
        args <- args.commandline
}

### Read Parameters ###
if (length(args) > 1) {
        static <- readRDS(paste(tolower(args[2]),".rds",sep=""))
} else{
        static <- readRDS("manual01.rds")
        args<-c(1,tolower(static$core$kStrategy))
}

static$core$kBackTestEndDate = strftime(adjust("India", as.Date(static$core$kBackTestEndDate, tz = kTimeZone), bdc = 2), "%Y-%m-%d")
static$core$kBackTestStartDate = strftime(adjust("India", as.Date(static$core$kBackTestStartDate, tz = kTimeZone), bdc = 0), "%Y-%m-%d")

today=strftime(Sys.Date(),tz=kTimeZone,format="%Y-%m-%d")
bod<-paste(today, "09:08:00 IST",sep=" ")
eod<-paste(today, "15:30:00 IST",sep=" ")
bizdate=adjust('India',Sys.Date(),bdc=2) #PRECEDING

if(Sys.time()<bod){
        args[1]=1
}else if(Sys.time()<eod){
        args[1]=2
}else{
        args[1]=3
}

logger <- create.logger()
logfile(logger) <- static$core$kLogFile
level(logger) <- 'INFO'
levellog(logger, "INFO", "Starting EOD Scan")

holidays=readRDS(paste(datafolder,"static/holidays.rds",sep=""))
RQuantLib::addHolidays("India",holidays)

if(get_os()=="windows"){
        setwd(static$core$kHomeDirectoryWindows)
}else if(get_os()=="linux"){
        setwd(static$core$kHomeDirectoryLinux)        
}

#### FUNCTIONS ####
#### GENERATE SYMBOLS ####
#### GENERATE SIGNALS ####
#### GENERATE TRADES ####
for(s in seq_len(nrow(static$core$kSubscribers))){
        useForRedis=! is.na(static$core$kSubscribers$account[s])
        if(useForRedis){
                redisdb=static$core$kSubscribers$redisdb[s]
                pattern=paste("*trades*",tolower(trimws(static$core$kStrategy)),"*","Order",sep="")
                trades=createPNLSummary(redisdb,pattern,static$core$kBackTestStartDate,static$core$kBackTestEndDate)
        }else{
                externalfile=static$core$kSubscribers$externalfile[s]
                trades=read.csv(externalfile,header = TRUE,stringsAsFactors = FALSE)
                trades$entrytime=as.POSIXct(trades$entrytime,format="%d-%m-%Y")
                trades$exittime=as.POSIXct(trades$exittime,format="%d-%m-%Y")
                trades$netposition=trades$size
        }
        md=loadSymbol("NSENIFTY_IND___",days=1000000,realtime=TRUE)
        bizdays=md[md$date>=min(trades$entrytime) & md$date<static$core$kBackTestEndDate,c("date")]
        subscriber=static$core$kSubscribers[s,]
        generateExecutionSummary(trades,bizdays,static$core$kBackTestStartDate,static$core$kBackTestEndDate,static$core$kStrategy,kSubscribers = subscriber,static$core$kBrokerage,kCommittedCapital=static$core$kCommittedCapital,kMarginOnUnrealized = FALSE,kInvestmentReturn=static$core$kInvestmentReturn,kOverdraftPenalty=static$core$kOverdraftPenalty,realtime=TRUE)
}
