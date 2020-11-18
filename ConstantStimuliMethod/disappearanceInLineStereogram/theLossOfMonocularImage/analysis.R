# Title     : TODO
# Objective : TODO
# Created by: Mac1
# Created on: 2020/08/05
# List up files

#List up files
files <- list.files('ConstantStimuliMethod/disappearanceInLineStereogram/theLossOfMonocularImage/data',full.names=T)
# remove ty who binocular unbalanced and es16 who measured in uncorrect setting
files <- files[-c(1, 6)]

f <- length(files)

si <- gsub(".*(..)DATE.*", "\\1", files)
usi <- toupper(unique(si))
n <- length(table(usi))

# Load data and store
temp <- read.csv(files[1])
temp$sub <- si[1]
m <- nrow(temp)
for (i in 2:f) {
  d <- read.csv(files[[i]])
  d$sub <- si[i]
  temp <- rbind(temp, d)
}
dat <- temp

for (i in usi) {assign(sprintf("dat_%s", i), subset(dat, sub==tolower(i)))}

for (i in usi) {
  camp <- subset(dat, toupper(dat$sub) == i, c("response", "cnd", "stimulated_eye"))
  glm.out <- glm(response ~ cnd, family=binomial(), data=camp)
  assign(sprintf("glm.out1.%s", i), glm.out)
  glm.out <- glm(response ~ cnd + stimulated_eye, family=binomial(), data=camp)
  assign(sprintf("glm.out2.%s", i), glm.out)
  glm.out <- glm(response ~ cnd + stimulated_eye + cnd*stimulated_eye, family=binomial(), data=camp)
  assign(sprintf("glm.out3.%s", i), glm.out)
  glm.out <- glm(response ~ 1, family=binomial(), data=camp)
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
  delta <- get(paste('glm.out1.', i, sep=''))$null.deviance - get(paste('glm.out1.', i, sep=''))$deviance
  delta2 <- get(paste('glm.out2.', i, sep=''))$null.deviance - get(paste('glm.out2.', i, sep=''))$deviance
  delta3 <- get(paste('glm.out3.', i, sep=''))$null.deviance - get(paste('glm.out3.', i, sep=''))$deviance
  fitness_x2 <- rbind(fitness_x2, c(delta, delta2, delta3))

  # 適合度のχ^2乗検定, p値の計算
  p1 <- 1.0 - pchisq(delta, get(paste('glm.out1.', i, sep=''))$rank-1)
  p2 <- 1.0 - pchisq(delta2, get(paste('glm.out2.', i, sep=''))$rank-1)
  p3 <- 1.0 - pchisq(delta3, get(paste('glm.out3.', i, sep=''))$rank-1)
  fitness_p <- rbind(fitness_p, c(p1, p2, p3))

  # 擬似R^2値 NagelkerkeのR^2
  nr21 <- NagelkerkeR2(get(paste('glm.out1.', i, sep='')))$R2
  nr22 <- NagelkerkeR2(get(paste('glm.out2.', i, sep='')))$R2
  nr23 <- NagelkerkeR2(get(paste('glm.out3.', i, sep='')))$R2
  NR2 <- rbind(NR2, c(nr21, nr22, nr23))

}
colnames(aics) <- c("glm.out1", "glm.out2", "glm.out3", "null.model")
colnames(fitness_x2) <- c("glm.out1", "glm.out2", "glm.out3")
colnames(fitness_p) <- c("glm.out1", "glm.out2", "glm.out3")
colnames(NR2) <- c("glm.out1", "glm.out2", "glm.out3")


library(ggplot2)
par(mfrow=c(2, 2))
for (i in usi) {
  g <- ggplot(data=get(paste('dat_', i, sep='')), aes(x=cnd, y=response, color=as.character(stimulated_eye)))
  g <- g + geom_point(aes(color=as.character(stimulated_eye)),
                      position=position_jitter(width=0.3, height=0.06), alpha=0.6, shape=21)
  g <- g + geom_smooth(formula=y~x, method="glm",
                     method.args=list(family=binomial(link="logit")), se = TRUE)
  g <- g + scale_color_hue(name = "test eye", labels = c('-1' = "left", '1' ="right"))
  g <- g + ylim(c(-0.1, 1.1)) + xlab('SOA') + labs(subtitle=i)
  print(g)
}
par(mfrow=c(1, 1))
