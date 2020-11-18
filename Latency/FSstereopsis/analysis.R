# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/05
# List up files

library('ggplot2')
library('patchwork')

files <- list.files('Latency/FSstereopsis/data',full.names=T)
# remove ty who binocular unbalanced and es16 who measured in uncorrect setting
files <- files[-c(2, 5)]
f <- length(files)

si <- gsub(".*(..)DATE.*","\\1", files)
n <- length(table(si))
usi <- toupper(unique(si))

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

mistake <- subset(temp, latency<=0)
dat <- subset(temp, latency>=0)


#gs <- ggplot()
for (i in usi){
  camp <- subset(dat, toupper(sub)==i)
  subcamp <- subset(dat, toupper(sub)==i)
  # 外れ値の除外
  # 平均値+-3*標準偏差より外の反応を外れ値とする
#  camp <- subset(camp, latency<=mean(subcamp$latency)+3*sd(subcamp$latency))
#  camp <- subset(camp, latency>=mean(subcamp$latency)-3*sd(subcamp$latency))
#  outliers <- subset(subcamp, latency<=mean(subcamp$latency)+3*sd(subcamp$latency)&latency>=mean(subcamp$latency)-3*sd(subcamp$latency))

  #キャンバスを用意して、gに格納
  g <- ggplot(camp, aes(y=latency, x=cnd, color=as.character(stimulated_eye))) +
       stat_summary(aes(cnd, color=as.character(stimulated_eye)),
                    fun=mean, geom="point") +
       stat_summary(aes(cnd, color=as.character(stimulated_eye)),
                    fun=mean, geom="line") +
       #エラーバーの追加
       stat_summary(aes(cnd),#種類ごとに
                      fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                      geom="errorbar",
                      size=0.5,#線の太さ
                      width=0.1) + #ぴょんって横に出てるアイツの幅
#       stat_smooth(method="loess", formula=y~x, fullrange=T, se=T,alpha=0.1) +
       # 1が左眼、-1が右眼にターゲットを提示する
       geom_point(aes(color=as.character(stimulated_eye)),
                  position=position_jitter(width=0.03, height=0),
                  alpha=0.4, shape=21) +
       labs(subtitle=i) +
       ylim(0, max(camp$latency+sd(camp$latency))) + xlab('SOA') +
       scale_color_hue(name = "test eye", labels = c('-1' = "left", '1' ="right"))
  print(g)#  gs <- gs + g
}
gs[-1]
print(g)

usi <- usi[-3]
library("gamlss")
for (i in usi) {
  camp <- subset(dat, toupper(dat$sub) == i, c("latency", "cnd", "stimulated_eye"))
  glm.out <- gamlss(data=camp, latency~cnd, family=exGAUS, mu.fix=FALSE, sigma.fix = FALSE, nu.fix = FALSE)
  assign(sprintf("glm.out1.%s", i), glm.out)
  glm.out <- gamlss(data=camp, latency~cnd+stimulated_eye, family=exGAUS, mu.fix=FALSE, sigma.fix = FALSE, nu.fix = FALSE)
  assign(sprintf("glm.out2.%s", i), glm.out)
  glm.out <- gamlss(data=camp, latency~cnd+stimulated_eye+cnd*stimulated_eye, family=exGAUS, mu.fix=FALSE, sigma.fix = FALSE, nu.fix = FALSE)
  assign(sprintf("glm.out3.%s", i), glm.out)
  glm.out <- gamlss(data=camp, latency~1, family=exGAUS, mu.fix=FALSE, sigma.fix = FALSE, nu.fix = FALSE)
  assign(sprintf("null.out.%s", i), glm.out)
}

library("fmsb")
# 適合度に関する指標
aics <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
fitness_x2 <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
fitness_p <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
NR2 <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
for (i in usi) {
  # AIC
  aics <- rbind(aics, c(AIC(get(paste('glm.out1.', i, sep=''))),
                        AIC(get(paste('glm.out2.', i, sep=''))),
                        AIC(get(paste('glm.out3.', i, sep=''))),
                        AIC(get(paste('null.out.', i, sep='')))))

  # null modelとx modelの逸脱度の差，χ^2値に一致
  delta <- get(paste('null.out.', i, sep=''))$deviance - get(paste('glm.out1.', i, sep=''))$P.deviance
  delta2 <- get(paste('null.out.', i, sep=''))$deviance - get(paste('glm.out2.', i, sep=''))$P.deviance
  delta3 <- get(paste('null.out.', i, sep=''))$deviance - get(paste('glm.out3.', i, sep=''))$P.deviance
  fitness_x2 <- rbind(fitness_x2, c(delta, delta2, delta3))

  # 適合度のχ^2乗検定, p値の計算
  p1 <- 1.0 - pchisq(delta, 1) #get(paste('glm.out1.', i, sep=''))$rank-1)
  p2 <- 1.0 - pchisq(delta2,2) #get(paste('glm.out2.', i, sep=''))$rank-1)
  p3 <- 1.0 - pchisq(delta3, 3) #get(paste('glm.out3.', i, sep=''))$rank-1)
  fitness_p <- rbind(fitness_p, c(p1, p2, p3))

  # 擬似R^2値 NagelkerkeのR^2
  nr21 <- Rsq(get(paste('glm.out1.', i, sep='')), type = "Cragg Uhler")
  nr22 <- Rsq(get(paste('glm.out2.', i, sep='')), type = "Cragg Uhler")
  nr23 <- Rsq(get(paste('glm.out3.', i, sep='')), type = "Cragg Uhler")
  NR2 <- rbind(NR2, c(nr21, nr22, nr23))
}
colnames(aics) <- c("glm.out1", "glm.out2", "glm.out3", "null.model")
colnames(fitness_x2) <- c("glm.out1", "glm.out2", "glm.out3")
colnames(fitness_p) <- c("glm.out1", "glm.out2", "glm.out3")
colnames(NR2) <- c("glm.out1", "glm.out2", "glm.out3")
