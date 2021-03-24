# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/05
# List up files

library(ggplot2)

files <- list.files('ConstantStimuliMethod/size_eccentricity/data',full.names=T)
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


for (i in usi){
  camp <- subset(temp, sub==i)
  # The y-axis indicates the visibility probability of the target

  #キャンバスを用意して、gに格納
  g <- ggplot(camp, aes(y=response, x=eccentricity, color=as.character(cnd))) +
       stat_summary(aes(eccentricity, color=as.character(cnd)),
                    fun=mean, geom="point") +
       #折れ線グラフを描き入れる
       stat_summary(aes(eccentricity), #種類ごとに
                      fun=mean, #平均値を
                      geom="point",#点で
                      colour="black") +
       #エラーバーの追加
       stat_summary(aes(eccentricity),#種類ごとに
                      fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                      geom="errorbar",
                      size=0.5,#線の太さ
                      width=0.1) + #ぴょんって横に出てるアイツの幅
       stat_smooth(method = "lm", formula = y~x, fullrange = T, se = T,alpha=0.1) +
       # 1が左眼、-1が右眼にターゲットを提示する
       geom_point(aes(color=as.character(cnd)),
                  position=position_jitter(width=0.3, height=0.06),
                  alpha=0.4, shape=21) +
       stat_summary(aes(y=response, x=eccentricity), fun=mean,
                      geom="line", colour="black") +
       labs(color='size', subtitle=i)

  print(g)
}

temp <- subset(temp, sub!='tt')
usi <- usi[-which(usi %in% "tt")]

# 全体平均
g <- ggplot(temp, aes(x=eccentricity, y=response, color=as.character(cnd))) +
            stat_summary(fun=mean, geom="line") +
            stat_summary(aes(eccentricity),#種類ごとに
                         fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                         geom="errorbar",
                         size=0.5,#線の太さ
                         width=0.1) +
            labs(color='size')

g

temp$eccentricity[temp$eccentricity!=1] <- 'double'
temp$eccentricity[temp$eccentricity==1] <- 'equal'

library(tidyr)
# ANOVA
df <- aggregate(temp$response, by=temp[c('sub', 'cnd', 'eccentricity')], FUN=mean)
#df$eccentricity[df$eccentricity!=1] <- 'double'
#df$eccentricity[df$eccentricity==1] <- 'equal'
df_shaped <- pivot_wider(df, names_from=c('cnd', 'eccentricity'), values_from=x)
df_shaped$sub <- NULL

#ANOVA <- aov(x~cnd*eccentricity, df)
#summary(ANOVA)

source('anovakun_485.txt')
ANOVA <- anovakun(df_shaped, 'sAB', 2, 2, peta=T)


# 条件間の変化率が大きい眼を検出する
means <- data.frame(matrix(rep(NA, 5), nrow=1))[numeric(0), ]
devs <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]

for (i in usi) {
  camp <- subset(temp, temp$sub == i, c("response", "cnd", "eccentricity","test_eye"))
  a <- aggregate(x=camp$response, by=camp[c('cnd', 'eccentricity', 'test_eye')], FUN=mean)
  a <- cbind(a, i)
  # 大きさの効果を均して左眼 - 右眼、マイナスならば右眼の方が偏心度の効果が強く現れている
  deviation_l <- sqrt((mean(a$x[a$test_eye!=1 & a$eccentricity!=1]) - mean(a$x[a$test_eye!=1 & a$eccentricity!=2]))^2)
  deviation_r <- sqrt((mean(a$x[a$test_eye!=-1 & a$eccentricity!=1]) - mean(a$x[a$test_eye!=-1 & a$eccentricity!=2]))^2)
  a <- cbind(a, deviation_l)
  a <- cbind(a, deviation_r)
  a$d <- a$deviation_l - a$deviation_r
  means <- rbind(means, a)
}

kpl <- subset(means, means$d >= 0)
kpl <- subset(kpl, kpl$test_eye!=1)
kpr <- subset(means, means$d < 0)
kpr <- subset(kpr, kpr$test_eye!=-1)

trimmed_means <- rbind(kpl, kpr)

# 全体平均
g <- ggplot(trimmed_means, aes(x=eccentricity, y=x, color=as.character(cnd), fill=as.character(cnd))) +
            stat_summary(fun=mean,position=position_dodge(width=0.95),geom="bar") +
            stat_summary(aes(eccentricity),#種類ごとに
                         fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                         geom="errorbar",
                         size=0.5,#線の太さ
                         width=0.1, position=position_dodge(width=0.95)) +
            labs(fill='size', color='size')

g

tm_wide <- pivot_wider(trimmed_means, names_from=c('cnd', 'eccentricity'), values_from=x)
tm_shaped <- subset(tm_wide, select=-c(i, test_eye, deviation_l, deviation_r, d))

source('anovakun_485.txt')
ANOVA <- anovakun(tm_shaped, 'sAB', 2, 2, peta=T)
# 利き目のデータを抽出して比較しても、交互作用なし
