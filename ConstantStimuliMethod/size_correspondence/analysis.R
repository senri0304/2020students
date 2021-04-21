# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/05
# List up files

library(ggplot2)

files <- list.files('ConstantStimuliMethod/size_correspondence/data',full.names=T)
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

# responseを反転
temp$response <- -(temp$response - 1)

for (i in usi){
  camp <- subset(temp, sub==i)
  # The y-axis indicates the visibility probability of the target

  #キャンバスを用意して、gに格納
  g <- ggplot(camp, aes(y=response, x=cnd, color=as.character(stimulated_eye))) +
       stat_summary(aes(cnd, color=as.character(stimulated_eye)),
                    fun=mean, geom="point") +
       #折れ線グラフを描き入れる
       stat_summary(aes(cnd), #種類ごとに
                      fun=mean, #平均値を
                      geom="point",#点で
                      colour="black") +
       #エラーバーの追加
       stat_summary(aes(cnd),#種類ごとに
                      fun.data=mean_se,#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                      geom="errorbar",
                      size=0.5,#線の太さ
                      width=0.1) + #ぴょんって横に出てるアイツの幅
#       stat_smooth(method = "lm", formula = y~x, fullrange = T, se = T,alpha=0.1) +
       # 1が左眼、-1が右眼にターゲットを提示する
       geom_point(aes(color=as.character(stimulated_eye)),
                  position=position_jitter(width=0.3, height=0.06),
                  alpha=0.4, shape=21) +
       stat_summary(aes(y=response, x=cnd), fun=mean,
                      geom="line", colour="black") +
       labs(color='stimulated eye', subtitle=i)
  print(g)
}

temp <- subset(temp, temp$sub!='tt')

# 全体平均
df <- aggregate(response ~ cnd + sub, temp, FUN=mean)
g <- ggplot(df, aes(x=cnd, y=response)) +
  stat_summary(fun=mean, geom="line") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2., fill = "black") +
  stat_summary(fun.data='mean_se',#mean_seで標準誤差、#mean_cl_normalで95%信頼区間(正規分布)
                  geom="errorbar", size=0.5,#線の太さ
                  width=0.1) + xlab('size')
g

library(tidyr)
# ANOVA
df_shaped <- pivot_wider(df, names_from=cnd, values_from=response)
df_shaped$sub <- NULL

#ANOVA <- aov(x~cnd*eccentricity, df)
#summary(ANOVA)

source('ConstantStimuliMethod/anovakun_485.txt')
#ANOVA <- anovakun(df_shaped, 'sA', 4, holm=T, peta=T)
capture.output(anovakun(df_shaped, "sA", 4, holm=T, peta=T), file = "ConstantStimuliMethod/size_correspondence/output.txt")
