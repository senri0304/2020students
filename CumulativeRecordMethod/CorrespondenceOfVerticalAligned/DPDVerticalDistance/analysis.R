# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/05
# List up files

files <- list.files('ConstantStimuliMethod/latency/data',full.names=T)
f <- length(files)

si <- gsub(".*(..)DATE.*","\\1", files)
n <- length(table(si))
usi <- unique(si)

# Load data and store
temp <- read.csv(files[1], stringsAsFactors = F)
temp$sub <- si[1]
temp$sn <- 1
for (i in 2:f) {
  d <- read.csv(files[[i]], stringsAsFactors = F)
  d$sub <- si[i]
  d$sn <- i
  temp <- rbind(temp, d)
}

camp <- aggregate(x=temp$response, by=temp[c('cnd', 'sub')], FUN=mean)

b1 <- subset(camp, sub=='b1')
b2 <- subset(camp, sub=='b2')

plot(x=b1$cnd, y=b1$x, ylim=c(0, 1))
plot(x=b2$cnd, y=b2$x, ylim=c(0, 1))

b3 <- subset(camp, sub=='b3')
b4 <- subset(camp, sub=='b4')

plot(x=b3$cnd, y=b3$x, ylim=c(0, 1))
plot(x=b4$cnd, y=b4$x, ylim=c(0, 1))

yh <- subset(camp, sub=='yh')

plot(x=yh$cnd, y=yh$x, ylim=c(0, 1))


a <- subset(temp, temp$sub=='b1')
a <- subset(a, select=c('response', 'cnd'))
table(a)