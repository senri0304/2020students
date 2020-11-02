# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/05
# List up files

library(ggplot2)

files <- list.files('disparityGradient/SLofRelativeDisparity/data',full.names=T)
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
cntl <- subset(camp, cnd==2000)
cntl_v <- rep(cntl$x, each=nrow(cntl)/n)


#camp <- subset(temp, cnd!=2000, select=c('cnd', 'response'))
tar <- subset(camp, cnd!=2000)

tar$diff <- tar$x - cntl_v

plot(tar$cnd, tar$diff)

g <- ggplot(tar, aes(y=diff, x=cnd)) +
     stat_summary(aes(cnd), fun=mean, geom='line', size=1) +
     stat_summary(aes(cnd),#種類ごとに
                  fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                  geom="errorbar",
                  size=0.5,#線の太さ
                  width=0.1) + #ぴょんって横に出てるアイツの幅
     stat_summary(aes(cnd), #種類ごとに
                  fun=mean, #平均値を
                  geom="point",#点で
                  colour="black") +
     stat_summary(aes(cnd, color=sub), fun=mean,
                  geom='line', size=0.5, linetype='dashed')

g

for (i in usi){
  camp <- subset(temp, sub==i)
  # The y-axis indicates the visibility probability of the target

  #キャンバスを用意して、gに格納
  g <- ggplot(camp, aes(y=response, x=log(cnd), color=as.character(stimulated_eye))) +
       stat_summary(aes(log(cnd), color=as.character(stimulated_eye)),
                    fun=mean, geom="point") +
       stat_summary(aes(log(cnd)), #種類ごとに
                      fun=mean, #平均値を
                      geom="point",#点で
                      colour="black") +
       #エラーバーの追加
       stat_summary(aes(log(cnd)),#種類ごとに
                      fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                      geom="errorbar",
                      size=0.5,#線の太さ
                      width=0.1) + #ぴょんって横に出てるアイツの幅
       stat_smooth(method = "lm", formula = y~x, fullrange = T, se = T,alpha=0.1) +
       # 1が左眼、-1が右眼にターゲットを提示する
       geom_point(aes(color=as.character(stimulated_eye)),
                  position=position_jitter(width=0.3, height=0.06),
                  alpha=0.4, shape=21) +
       stat_summary(aes(y=response, x=log(cnd)), fun=mean,
                      geom="line", colour="black") +
       labs(color='stimulated eye')

  print(g)
}
