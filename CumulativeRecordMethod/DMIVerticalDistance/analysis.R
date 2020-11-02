# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/25


files <- list.files('CumulativeRecordMethod/DMIVerticalDistance/data',full.names=T)
f <- length(files)

si <- gsub(".*(..)DATE.*","\\1", files)
n <- length(table(si))
usi <- unique(si)

# Load data and store
dat <- read.csv(files[1], stringsAsFactors = F)
dat$sub <- si[1]
dat$sn <- 1
for (i in 2:f) {
  d <- read.csv(files[[i]], stringsAsFactors = F)
  d$sub <- si[i]
  d$sn <- i
  dat <- rbind(dat, d)
}

temp <- aggregate(x=dat$cdt, by=dat[c('trial', 'sub')], FUN=mean)
for (i in 1:n) {
  camp <- subset(temp, temp$sub==usi[i])
  plot(camp$trial, camp$x, ylim=c(0, 15), main=usi[i])
}
