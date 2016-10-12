###############################################################################
### Divide and Co-Opt: Tribal Groups and Civilian Defense Forces 			###
### Goran Peic, Ph.D. 											 			###
###############################################################################

###############################################################################
### Run Models
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Projects\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))

dat$sq2cdfs <- sqrt(sqrt(dat$cdfs))
dat$lnrddens <- log(dat$rddens)
dat$lntpop <- log(dat$tpop/1000)
dat$lngdppc <- log(dat$gdppc)
dat$lnrebskrmsh10 <- log(dat$rebskrmsh10+1)
dat$phil <- 0; dat$phil[dat$ccode==840] <- 1
dat$sqdist <- sqrt(dat$dist)
dat$lntribpop <- log(1+dat$tribpop/1000)
dat$tribpopZero <- 1; dat$tribpopZero[dat$tribpop>0] <- 0
dat$tribpopNonZero <- 1; dat$tribpopNonZero[dat$tribpop==0] <- 0
dat$tribpopLow <- 0; dat$tribpopLow[dat$tribpop>0 & dat$tribpop<=mean(
  dat$tribpop)] <- 1;
dat$tribpopHigh <- 0; dat$tribpopHigh[dat$tribpop>mean(dat$tribpop)] <- 1;

cor(dat[, c("lntribpop", "lntpop", "lngdppc", "lnrebskrmsh10")])

nb.model.theta <- glm.nb(cdfs~lnrebskrmsh10+lnrddens+lntpop, data=dat)

nb.model1 <- glm(cdfs~tribpopNonZero+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
  family=negative.binomial(theta=nb.model.theta$theta, link=log), data=dat)
summary(nb.model1)

nb.model2 <- glm(cdfs~tribpopLow+tribpopHigh+lnrebskrmsh10+lnrddens+lngdppc+
  lntpop+phil, family=negative.binomial(theta=nb.model.theta$theta, link=log),
  data=dat)
summary(nb.model2)

stErrofDiff <- sqrt(vcov(nb.model2)["tribpopLow", "tribpopLow"]+
               vcov(nb.model2)["tribpopHigh", "tribpopHigh"]-
			   2*vcov(nb.model2)["tribpopHigh", "tribpopLow"])
pval <- pnorm(nb.model2$coefficients[3]-nb.model2$coefficients[2], mean=0,
  sd=stErrofDiff, lower.tail=F)
print(paste("Pr(Data | High-Low=0) = ", round(pval, 3), sep=""))

nb.model3 <- glm(cdfs~lntribpop+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
  family=negative.binomial(theta=nb.model.theta$theta, link=log), data=dat)
summary(nb.model3)

### Identify Outliers ###
dat$res <- dat$cdfs-nb.model3$fitted.values
# ggplot(dat, aes(res))+geom_histogram()

nb.model4 <- glm(cdfs~lntribpop+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
  family=negative.binomial(theta=nb.model.theta$theta, link=log),
  data=dat[dat$res>-2500 & dat$res<2500, ])
summary(nb.model4)

###############################################################################
### Figure 1: Histogram of CDFs
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))

dat$country <- "Philippines"; dat$country[dat$ccode==640] <- "Turkey"
histcdfs <- ggplot(data=dat, aes(x=cdfs, fill=as.factor(country))) + theme(text=element_text(
  family = "CM Roman")) + xlab("Number of CDF Personnel") +
  geom_histogram(binwidth=1000) + guides(fill=guide_legend(title="Country")) +
  ylab("Number of Provinces") + scale_fill_manual(values=c("gray25", "gray55"),
  name="Country") + spartan.theme + scale_x_continuous(breaks=seq(0, 15000, by=5000), labels=c("0",
  "5000", "10,000", "15,000"))

histcdfs
ggsave(paste(outputDir, "\\histogram_cdfs.pdf", sep=""), width=8, height=8); # dev.off()
dev.off()

###############################################################################
### Figure 2: Histogram of Tribal Population
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))

dat$country <- "Philippines"; dat$country[dat$ccode==640] <- "Turkey"
histtribpop <- ggplot(data=dat, aes(x=tribpop, fill=as.factor(country))) +
  theme(text=element_text(family="CM Roman")) + xlab("Tribal Population") +
  geom_histogram(binwidth=200000) + guides(fill=guide_legend(title="Country")) +
  ylab("Number of Provinces") + scale_fill_manual(values=c("gray25", "gray55"),
  name="Country") + spartan.theme + scale_x_continuous(breaks=seq(0, 1400000, by=
  200000), labels=c("0", "200K", "400K", "600K", "800K", "1,000K",
  "1,200K", "1,400K"))

histtribpop
ggsave(paste(outputDir, "\\histogram_tribpop.pdf", sep=""), width=8, height=8); # dev.off()
dev.off()

###############################################################################
### Figure 3: Scatterplot of Tribes vs. CDFs
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
dat$lntribpop <- log(1+dat$tribpop/1000)

ggplot(data=dat, aes(x=lntribpop, y=cdfs)) + geom_point(shape=1, size=2.5) + geom_smooth(
  method=lm, se=FALSE, color="black", size=1.2) + theme(text = element_text(family = "CM Roman")) +
  spartan.theme + ylab("Number of CDF Personnel") + xlab("Tribal Population (Log)") +
  scale_y_continuous(breaks=seq(0, 7500, length.out=4), labels=c("0", "2,500",  "5,000",
  "7,500"), limits=c(0, 7500))
ggsave(paste(outputDir, "\\scatterplot.pdf", sep=""), width=7, height=6); # dev.off()
dev.off()

###############################################################################
### Figure 4: Barplot of Tribes vs. CDFs
###############################################################################

# rm(list=ls(all=T))
# homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# # homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
# source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
# dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
# dat$tribes5 <- cut(dat$tribpop, quantile(dat$tribpop, 0:5/5), include.lowest=T,
  # labels=paste(1:5))

# dat.plot <- data.frame(cdfs=tapply(dat$cdfs, dat$tribes5, median), tribes5=as.factor(1:5))

# ggplot(data=dat.plot, aes(x=tribes5, y=cdfs, fill=tribes5)) + theme(text=
  # element_text(family = "CM Roman")) + geom_bar(stat="identity") + ylab(
  # "Median Number of CDF Personnel") + xlab("Tribal Population") + scale_x_discrete(
  # labels=c("Very Low", "Low", "Medium", "High", "Very High")) + scale_y_continuous(
  # breaks=seq(0, 1200, length.out=13), labels=c("0", "100", "200", "300", "400",
  # "500", "600", "700", "800", "900", "1,000", "1,100", "1,200"), limits=c(0, 1200)) +
  # spartan.theme + scale_fill_manual(values=c("gray75", "gray60", "gray45", "gray30",
  # "gray15"), name="Sample\nPercentile", labels=c("0% to 20%", "20% to 40%", "40% to 60%",
  # "60% to 80%", "80% to 100%"))

# ggsave(paste(outputDir, "\\barplot.pdf", sep=""), width=7, height=6); # dev.off()
# dev.off()

###############################################################################
### Figure 4: Barplot of Tribes vs. CDFs (ALTERNATIVE -- 0 + 2 BINS)
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
dat$tribes3 <- 1
dat$tribes3[dat$tribpop>0 & dat$tribpop<=mean(dat$tribpop)] <- 2
dat$tribes3[dat$tribpop>mean(dat$tribpop)] <- 3

dat.plot <- data.frame(cdfs=tapply(dat$cdfs, dat$tribes3, median),
				       tribes3=as.factor(1:length(unique(dat$tribes3))))

ggplot(data=dat.plot, aes(x=tribes3, y=cdfs, fill=tribes3)) + theme(text=
  element_text(family = "CM Roman")) + geom_bar(stat="identity") + ylab(
  "Median Number of CDF Personnel") + xlab("Tribal Population") +
  scale_x_discrete(labels=c("None\n0", "Low\n1K to 140K (Mean)",
  "High\n140K to 1.2M (Max)")) + scale_y_continuous(breaks=seq(0, 1000,
  length.out=11), labels=c("0", "100", "200", "300", "400", "500", "600",
  "700", "800", "900", "1,000"), limits=c(0, 1000)) + spartan.theme +
  scale_fill_manual(values=c("gray73", "gray43", "gray13")) +
  theme(legend.position="none")

ggsave(paste(outputDir, "\\barplot.pdf", sep=""), width=5.5, height=6); # dev.off()
dev.off()

###############################################################################
### Figure 5: Substantive Effects for Tribes (3 BINS)
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
dat$sq2cdfs <- sqrt(sqrt(dat$cdfs))
dat$lnrddens <- log(dat$rddens)
dat$lntpop <- log(dat$tpop/1000)
dat$lngdppc <- log(dat$gdppc)
dat$lnrebskrmsh10 <- log(dat$rebskrmsh10+1)
dat$phil <- 0; dat$phil[dat$ccode==840] <- 1
dat$sqdist <- sqrt(dat$dist)
dat$tribpopLow <- 0; dat$tribpopLow[dat$tribpop>0 & dat$tribpop<=mean(
  dat$tribpop)] <- 1
dat$tribpopHigh <- 0; dat$tribpopHigh[dat$tribpop>mean(dat$tribpop)] <- 1

nb.model.theta <- glm.nb(cdfs~lnrebskrmsh10+lnrddens+lntpop, data=dat)
summary(nb.model.theta)

nb.model1 <- glm(cdfs~tribpopLow+tribpopHigh+lnrebskrmsh10+lnrddens+
  lngdppc+lntpop+phil, family=negative.binomial(theta=nb.model.theta$theta,
  link=log), data=dat)
summary(nb.model1)

nPoints=3
newdat <- data.frame(tribpopLow=c(0, 1, 0),
					 tribpopHigh=c(0, 0, 1),
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)

point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates

### Bootstrap Procedure ###

nBoot <- 5000; pointBoot.est <- model2Boot <- datBoot <- sampleBoot <- NULL
lowerCutoff <- 0.025*nBoot; upperCutoff <- 0.975*nBoot
for (i in 1:nBoot) {
  sampleBoot <- sample(1:nrow(dat), nrow(dat), replace=T)
  datBoot <- dat[sampleBoot, ]
  model2Boot = glm(cdfs~tribpopLow+tribpopHigh+lnrebskrmsh10+lnrddens+
    lngdppc+lntpop+phil, family=negative.binomial(theta=nb.model.theta$theta,
	link=log), data=datBoot)
  pointBoot.est <- cbind(pointBoot.est, exp(predict(model2Boot, newdat)))
}

lowerBound <- upperBound <- tempDat <- NULL
for (i in 1:nPoints) {
  tempDat <- pointBoot.est[i, order(pointBoot.est[i, ])]
  lowerBound <- c(lowerBound, tempDat[lowerCutoff])
  upperBound <- c(upperBound, tempDat[upperCutoff])
}

### Generate Plot ###

dat.plot <- data.frame(tribes3=factor(1:nPoints, levels=1:nPoints, labels=c(
					     "None", "Low", "High")),
					   lowerBound=lowerBound,
					   pointEst=point.est, upperBound=upperBound)

zeroToLow <- round(100*(dat.plot$pointEst[2]/dat.plot$pointEst[1]-1), 2)
zeroToHigh <- round(100*(dat.plot$pointEst[3]/dat.plot$pointEst[1]-1), 2)
print(paste("Zero to Low Tribes: ", zeroToLow, "%", sep=""))
print(paste("Zero to High Tribes: ", zeroToHigh, "%", sep=""))

subsfx <- ggplot(data=dat.plot, aes(x=tribes3, y=pointEst, fill=tribes3)) +
  geom_bar(stat="identity") + theme(text = element_text(family = "CM Roman")) +
  ylab("Predicted Number of CDF Personnel") + xlab("Tribal Population") +
  scale_x_discrete(labels=c("None\n0", "Low\n1K to 140K (Mean)",
  "High\n140K to 1.2M (Max)")) + scale_fill_manual(values=c("gray73",
  "gray43", "gray13"), name="Substantive\nEffect", labels=c("Baseline", paste("+", zeroToLow,
  "%", sep=""), paste("+", zeroToHigh, "%", sep=""))) + spartan.theme + geom_errorbar(
  aes(ymin=lowerBound, ymax=upperBound)) + scale_y_continuous(breaks=seq(0, 900,
  by=100), labels=c("0", "100", "200", "300", "400", "500", "600", "700", "800",
  "900"), limits=c(0, 900))

subsfx
ggsave(paste(outputDir, "\\subsfx_barplot3.pdf", sep=""), width=8, height=8); # dev.off()
dev.off()

print(paste("Zero to Low Tribes: ", round(100*(dat.plot$pointEst[2]/dat.plot$pointEst[1]-
  1), 2), "%", sep=""))
print(paste("Zero to High Tribes: ", round(100*(dat.plot$pointEst[3]/dat.plot$pointEst[1]-
  1), 2), "%", sep=""))

###############################################################################
### Figure 5: Substantive Effects for Tribes (BINARY)
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
dat$sq2cdfs <- sqrt(sqrt(dat$cdfs))
dat$lnrddens <- log(dat$rddens)
dat$lntpop <- log(dat$tpop/1000)
dat$lngdppc <- log(dat$gdppc)
dat$lnrebskrmsh10 <- log(dat$rebskrmsh10+1)
dat$phil <- 0; dat$phil[dat$ccode==840] <- 1
dat$sqdist <- sqrt(dat$dist)
dat$tribpopNonZero <- 1; dat$tribpopNonZero[dat$tribpop==0] <- 0

nb.model.theta <- glm.nb(cdfs~lnrebskrmsh10+lnrddens+lntpop, data=dat)
summary(nb.model.theta)

nb.model2 <- glm(cdfs~tribpopNonZero+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
  family=negative.binomial(theta=nb.model.theta$theta, link=log), data=dat)
summary(nb.model2)

nPoints=length(unique(dat$tribpopNonZero))
newdat <- data.frame(tribpopNonZero=unique(dat$tribpopNonZero),
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)

point.est <- exp(predict(nb.model2, newdat)) # obtain point estimates

### Bootstrap Procedure ###

nBoot <- 10000; pointBoot.est <- model2Boot <- datBoot <- sampleBoot <- NULL
lowerCutoff <- 0.025*nBoot; upperCutoff <- 0.975*nBoot
for (i in 1:nBoot) {
  sampleBoot <- sample(1:nrow(dat), nrow(dat), replace=T)
  datBoot <- dat[sampleBoot, ]
  model2Boot = glm(cdfs~tribpopNonZero+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
    family=negative.binomial(theta=nb.model.theta$theta, link=log), data=datBoot)
  pointBoot.est <- cbind(pointBoot.est, exp(predict(model2Boot, newdat)))
}

lowerBound <- upperBound <- tempDat <- NULL
for (i in 1:nPoints) {
  tempDat <- pointBoot.est[i, order(pointBoot.est[i, ])]
  lowerBound <- c(lowerBound, tempDat[lowerCutoff])
  upperBound <- c(upperBound, tempDat[upperCutoff])
}

### Generate Plot ###

dat.plot <- data.frame(tribpopNonZero=as.factor(newdat$tribpopNonZero),
				       lowerBound=lowerBound,
					   pointEst=point.est, upperBound=upperBound)

subsfx <- ggplot(data=dat.plot, aes(x=tribpopNonZero, y=pointEst, fill=tribpopNonZero)) +
  geom_bar(stat="identity") + theme(text = element_text(family = "CM Roman")) +
  ylab("Predicted Number of CDF Personnel") + xlab("Tribal Population") + scale_x_discrete(
  labels=c("Tribal Groups Absent", "Tribal Groups Present")) + scale_fill_manual(
  values=c("gray60", "gray30")) + spartan.theme + geom_errorbar(aes(ymin=lowerBound, ymax=upperBound)) +
  scale_y_continuous(breaks=seq(0, 800, by=100), labels=c("0", "100", "200",
  "300", "400", "500", "600", "700", "800"), limits=c(0, 800)) + theme(legend.position="none")

subsfx
ggsave(paste(outputDir, "\\subsfx_barplot2.pdf", sep=""), width=8, height=8); # dev.off()
dev.off()

###############################################################################
### Figure 6: Substantive Effects for Tribes (LOG-TRANSFORMED CONTINUOUS)
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
dat$sq2cdfs <- sqrt(sqrt(dat$cdfs))
dat$lnrddens <- log(dat$rddens)
dat$lntpop <- log(dat$tpop/1000)
dat$lngdppc <- log(dat$gdppc)
dat$lnrebskrmsh10 <- log(dat$rebskrmsh10+1)
dat$phil <- 0; dat$phil[dat$ccode==840] <- 1
dat$lntribpop <- log(1+dat$tribpop/1000)

nb.model.theta <- glm.nb(cdfs~lnrebskrmsh10+lnrddens+lntpop,
  data=dat)
summary(nb.model.theta)

nb.model1 <- glm(cdfs~lntribpop+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
  family=negative.binomial(theta=nb.model.theta$theta, link=log), data=dat)
summary(nb.model1)

nPoints=100 # nrow(dat)
newdat <- data.frame(lntribpop=log(seq(1, 500, length.out=nPoints)),
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)

point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates

### Bootstrap Procedure ###

nBoot <- 10000; pointBoot.est <- model2Boot <- datBoot <- sampleBoot <- NULL
lowerCutoff <- 0.025*nBoot; upperCutoff <- 0.975*nBoot
for (i in 1:nBoot) {
  sampleBoot <- sample(1:nrow(dat), nrow(dat), replace=T)
  datBoot <- dat[sampleBoot, ]
  model2Boot = glm(cdfs~lntribpop+lnrebskrmsh10+lnrddens+lngdppc+lntpop+phil,
    family=negative.binomial(theta=nb.model.theta$theta, link=log), data=datBoot)
  pointBoot.est <- cbind(pointBoot.est, exp(predict(model2Boot, newdat)))
}

lowerBound <- upperBound <- tempDat <- NULL
for (i in 1:nPoints) {
  tempDat <- pointBoot.est[i, order(pointBoot.est[i, ])]
  lowerBound <- c(lowerBound, tempDat[lowerCutoff])
  upperBound <- c(upperBound, tempDat[upperCutoff])
}

### Generate Plot ###

dat.plot <- data.frame(tribpop=exp(newdat$lntribpop), lowerBound=lowerBound,
					   pointEst=point.est, upperBound=upperBound)

subsfx <- ggplot(data=dat.plot, aes(x=tribpop, y=pointEst)) + theme(text = element_text(
  family = "CM Roman")) + ylab("Number of CDF Personnel") + xlab("Tribal Population (in Thousands)") +
  scale_y_continuous(breaks=seq(200, 800, by=100), labels=c("200",
  "300", "400", "500", "600", "700", "800"), limits=c(150, 850)) +
  geom_segment(aes(x=tribpop, xend=tribpop, y=lowerBound, yend=upperBound),
  color="gray") + spartan.theme + geom_line(aes(x=tribpop, y=pointEst),
  color="black", size=1.2) + geom_line(aes(x=tribpop, y=lowerBound),
  color="black", size=1, linetype="dashed") + geom_line(aes(x=tribpop,
  y=upperBound), color="black", size=1, linetype="dashed")

subsfx <- subsfx + annotate("text", x=380, y=230, label="Point Estimate") +
  annotate("text", x=415, y=190, label="95% Confidence Interval") +
  geom_segment(aes(x=300, xend=320, y=190, yend=190), linetype="dashed", size=0.8) +
  geom_segment(aes(x=300, xend=320, y=230, yend=230), size=0.9)

subsfx
ggsave(paste(outputDir, "\\subsfx_lntribes.pdf", sep=""), width=8, height=8)
dev.off()


###############################################################################
### Table 3: Substantive Effects for All Variables
###############################################################################

rm(list=ls(all=T))
homeDir <- "C:\\Users\\Blasco\\Dropbox\\Tribal_Groups_and_CDFs"
# homeDir <- "C:\\Users\\peicgo01\\Dropbox\\Tribal_Groups_and_CDFs"
source(paste(homeDir, "\\RScripts\\setup.R", sep=""))
dat <- read.dta(paste(dataDir, "provdata.dta", sep="\\"))
dat$sq2cdfs <- sqrt(sqrt(dat$cdfs))
dat$lnrddens <- log(dat$rddens)
dat$lntpop <- log(dat$tpop/1000)
dat$lngdppc <- log(dat$gdppc)
dat$lnrebskrmsh10 <- log(dat$rebskrmsh10+1)
dat$phil <- 0; dat$phil[dat$ccode==840] <- 1
dat$sqdist <- sqrt(dat$dist)
dat$tribpopLow <- 0; dat$tribpopLow[dat$tribpop>0 & dat$tribpop<=mean(
  dat$tribpop)] <- 1;
dat$tribpopHigh <- 0; dat$tribpopHigh[dat$tribpop>mean(dat$tribpop)] <- 1;

nb.model.theta <- glm.nb(cdfs~lnrebskrmsh10+lnrddens+lntpop,
  data=dat)
summary(nb.model.theta)

nb.model1 <- glm(cdfs~tribpopLow+tribpopHigh+lnrebskrmsh10+lnrddens+lngdppc+
  lntpop+phil, family=negative.binomial(theta=nb.model.theta$theta, link=log), data=dat)
summary(nb.model1)

### TRIBAL POPULATION ###
nPoints=3
newdat <- data.frame(tribpopLow=c(0, 1, 0), tribpopHigh=c(0, 0, 1),
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)
point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates
delta <- round(100*(point.est[2]/point.est[1]-1), 2)
paste("Changing TRIBAL POPULATION from None to Low results in ", delta,
  "% increase in predicted CDF personnel", sep="")
delta <- round(100*(point.est[3]/point.est[1]-1), 2)
paste("Changing TRIBAL POPULATION from High to Low results in ", delta,
  "% increase in predicted CDF personnel", sep="")

### INSURGENT THREAT ###
nPoints=10
newdat <- data.frame(tribpopLow=1, tribpopHigh=0,
                     lnrebskrmsh10=seq(mean(dat$lnrebskrmsh10)-sd(dat$lnrebskrmsh10),
                       mean(dat$lnrebskrmsh10)+sd(dat$lnrebskrmsh10), length.out=nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     sqdist=rep(mean(dat$sqdist), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)
point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates
delta <- round(100*(point.est[nrow(newdat)]/point.est[1]-1), 2)
paste("Changing INSURGENT THREAT from -1SD to +1SD results in ", delta,
  "% increase in predicted CDF personnel", sep="")
print(point.est)

### ROAD DENSITY ###
nPoints=10
newdat <- data.frame(tribpopLow=1, tribpopHigh=0,
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=seq(mean(dat$lnrddens)-sd(dat$lnrddens),
                       mean(dat$lnrddens)+sd(dat$lnrddens), length.out=nPoints),
                     sqdist=rep(mean(dat$sqdist), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)
point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates
delta <- round(100*(point.est[nrow(newdat)]/point.est[1]-1), 2)
paste("Changing ROAD DENSITY from -1SD to +1SD results in ", delta,
  "% decrease in predicted CDF personnel", sep="")
print(point.est)

### DEVELOPMENT ###
nPoints=10
newdat <- data.frame(tribpopLow=1, tribpopHigh=0,
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     sqdist=rep(mean(dat$sqdist), nPoints),
                     lngdppc=seq(mean(dat$lngdppc)-sd(dat$lngdppc),
                       mean(dat$lngdppc)+sd(dat$lngdppc), length.out=nPoints),
					 lntpop=rep(mean(dat$lntpop), nPoints),
					 phil=1)
point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates
delta <- round(100*(point.est[nrow(newdat)]/point.est[1]-1), 2)
paste("Changing DEVELOPMENT from -1SD to +1SD results in ", delta,
  "% decrease in predicted CDF personnel", sep="")
print(point.est)

### POPULATION ###
nPoints=10
newdat <- data.frame(tribpopLow=1, tribpopHigh=0,
                     lnrebskrmsh10=rep(mean(dat$lnrebskrmsh10), nPoints),
                     lnrddens=rep(mean(dat$lnrddens), nPoints),
                     sqdist=rep(mean(dat$sqdist), nPoints),
                     lngdppc=rep(mean(dat$lngdppc), nPoints),
					 lntpop=seq(mean(dat$lntpop)-sd(dat$lntpop),
                       mean(dat$lntpop)+sd(dat$lntpop), length.out=nPoints),
					 phil=1)
point.est <- exp(predict(nb.model1, newdat)) # obtain point estimates
delta <- round(100*(point.est[nrow(newdat)]/point.est[1]-1), 2)
paste("Changing POPULATION from -1SD to +1SD results in ", delta,
  "% increase in predicted CDF personnel", sep="")
print(point.est)

