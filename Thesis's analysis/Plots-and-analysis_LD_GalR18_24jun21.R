

#-------------------------script para grafico diferencias de sueño : Substraction plot, LD Gal-R18, 24jun21





#changing the wd.

setwd("D:/Documents/CNEA/Lab_Moscas/Project-Females_fly_circadian_clock/Subproject-Opto_in_flies/Analysis")



# packages
library("ggplot2")
library("reshape")

# data
data <- read.table("Exp04_Monitorv0-expeEze-24jun21slp_th8_acum1800s.dat", sep = " ", header = T) # monitor 0
dim(data)
head(data)

data2 <- read.table("Exp04_Monitorv1-expeEze-24jun21slp_th8_acum1800s.dat", sep = " ", header = T) # monitor 1
dim(data2)
head(data2)

# how many days does it have the data?
length(data2$bin)/48 + 1

# av controls
merge.ctr <- cbind(data[c(2,4,6,8,10,12)], data2[c(2,4,6,8,10,12)])
dim(merge.ctr)
head(merge.ctr)

# av treatment
merge.tra <- cbind(data[c(3,5,7,9,11,13)], data2[c(3,5,7,9,11,13)])
dim(merge.tra)
head(merge.tra)



#-------------------- subtraction per each fly


# adding standard desviations

# substractions

bday <- 1                           # baseline day
tday <- 2                           # treatment day

subs.ctr <- as.matrix(merge.ctr[(48*(tday-1)+1):(48*tday), ]) - as.matrix(merge.ctr[(48*(bday-1)+1):(48*bday), ])  # trat day - base day
subs.tra <- as.matrix(merge.tra[(48*(tday-1)+1):(48*tday), ]) - as.matrix(merge.tra[(48*(bday-1)+1):(48*bday), ])

# mean and sd
ctr <- apply(subs.ctr, 1, mean)
sd.ctr <- apply(subs.ctr, 1, sd)

tra <- apply(subs.tra, 1, mean)
sd.tra <- apply(subs.tra, 1, sd)

# plot table
Bin <- 1:48
avs <- melt(as.data.frame(cbind(Bin, ctr, tra)), "Bin")
head(avs)

sds <- melt(as.data.frame(cbind(Bin, sd.ctr, sd.tra)), "Bin")
head(sds)

plot.t <- as.data.frame(cbind(avs, sds$value))
colnames(plot.t) <- c("bin", "gen", "mean", "sd")
plot.t$fbin <- as.factor(plot.t$bin)
plot.t$max <- plot.t$mean+plot.t$sd
plot.t$min <- plot.t$mean-plot.t$sd
head(plot.t)
str(plot.t)





# ------------------------------------------------------------------------------
# PLOTs
pd <- position_dodge(0.4)


# to check first plot
ggplot(data=plot.t, aes(x=bin, y=mean, col=gen)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = pd) +
  scale_x_discrete(name ="ZT", 
                   limits=c("","","","2", "", "", "", "4", "", "", "", "6", "", "", "", "8", "", "", "", "10", "", "", "", "12",
                            "","", "", "14", "", "", "", "16", "", "", "", "18", "","", "", "20", "", "", "", "22", "", "", "", "24"))  +
  xlab("BIN") +
  ylab("Diferencia en sueño") +
  #scale_color_manual(labels=c("+>UAS-Chrimson", "R18H11-Gal4>UAS-Chrimson"), values = c("red", "orange")) +
  labs(colour = "Genotipo") +
  theme(# Change axis line, ticks and text
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 19, color = "gray22"),
    axis.title.y = element_text(size = 19, color = "gray22", vjust = 2),
    # Legend parameters
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "Black"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.7, "cm"),
    legend.text = element_text(size = 15, color = "gray22"),
    # Remove panel background
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"))




# Creating the png file
png("Exp04_Subs-plot_LD_GalR18_24jun21.png", width = 1024, height = 690, res= 120) # res are the DPI values. 
# Plot
ggplot(data=plot.t, aes(x=bin, y=mean, col=gen)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = pd) +
  scale_x_discrete(name ="ZT", 
                   limits=c("","","","2", "", "", "", "4", "", "", "", "6", "", "", "", "8", "", "", "", "10", "", "", "", "12",
                            "","", "", "14", "", "", "", "16", "", "", "", "18", "","", "", "20", "", "", "", "22", "", "", "", "24"))  +
  xlab("BIN") +
  ylab("Diferencia en sueño") +
  scale_color_manual(labels=c("R18H11-Gal4>+ (sin retinal)", "R18H11-Gal4>+ (con retinal)"), values = c("red", "orange")) +
  labs(colour = "Genotipo") +
  theme(# Change axis line, ticks and text
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 19, color = "gray22"),
    axis.title.y = element_text(size = 19, color = "gray22", vjust = 2),
    # Legend parameters
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "Black"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.7, "cm"),
    legend.text = element_text(size = 15, color = "gray22"),
    # Remove panel background
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"))


# Closing the file
dev.off()




#-------------------------------------------------------------------------------

# FOLD CHANGE (Treat-Base)/Base

R18_Chr_LED <- as.matrix(merge.tra[((48*(tday-1)+1)+14):(48*tday), ])
dim(R18_Chr_LED)
R18_Chr_LED

R18_Chr_base <- as.matrix(merge.tra[((48*(bday-1)+1)+14):(48*bday), ])
dim(R18_Chr_base)
R18_Chr_base

UAS_Chr_LED <- as.matrix(merge.ctr[((48*(tday-1)+1)+14):(48*tday), ])
dim(UAS_Chr_LED)
UAS_Chr_LED

UAS_Chr_base <- as.matrix(merge.ctr[((48*(bday-1)+1)+14):(48*bday), ])
dim(UAS_Chr_base)
UAS_Chr_base



#### Average sleep of each fly during either led-day or baseline-day (zt7 - zt24)

### treated flies
av_R18_led <- apply(R18_Chr_LED, 1, mean)
av_R18_base <- apply(R18_Chr_base, 1, mean)
#sd_R18_led <- apply(R18_Chr_LED, 1, sd)
#sd_R18_base <- apply(R18_Chr_base, 1, sd)

### control flies
av_UAS_led <- apply(UAS_Chr_LED, 1, mean)
av_UAS_base <- apply(UAS_Chr_base, 1, mean)
#sd_UAS_led <- apply(UAS_Chr_LED, 1, sd)
#sd_UAS_base <- apply(UAS_Chr_base, 1, sd)


# treatment day FOLD CHANGE
fold_change_led <- log(av_R18_led/av_UAS_led, 2)
se_fold_change_led <- sd(fold_change_led)/sqrt(length(fold_change_led))

# Mean and desviations of fold change
av_fold_change_led <- mean(fold_change_led)
av_fold_change_led_max <- av_fold_change_led+se_fold_change_led
av_fold_change_led_min <- av_fold_change_led-se_fold_change_led

# baseline day FOLD CHANGE
fold_change_base <- log(av_R18_base/av_UAS_base, 2)
se_fold_change_base <- sd(fold_change_base)/sqrt(length(fold_change_base))

# Mean and desviations of fold change
av_fold_change_base <- mean(fold_change_base)
av_fold_change_base_max <- av_fold_change_base+se_fold_change_base
av_fold_change_base_min <- av_fold_change_base-se_fold_change_base




#--------# bar graphs

# plot table
averages <- rbind(av_fold_change_led, av_fold_change_base)
maxs <- rbind(av_fold_change_led_max, av_fold_change_base_max)
mins <- rbind(av_fold_change_led_min, av_fold_change_base_min)
lab <- rbind("Día led (7-24 ZT)", "Día base (7-24 ZT)")

plot.table <- as.data.frame (cbind(lab, averages, maxs, mins))
colnames(plot.table) <- c("días","average", "max", "min")
plot.table$días<-as.factor(plot.table$días)
plot.table$average <- as.numeric(plot.table$average)
plot.table$max <- as.numeric(plot.table$max)
plot.table$min <- as.numeric(plot.table$min)

plot.table


# bar plot

ggplot(plot.table) +
  geom_bar( aes(x=días, y=average), stat="identity", col="black", fill="orange", alpha=0.8, width= 0.6) +
  geom_errorbar( aes(x=días, ymin=max, ymax=min), width=0.4, colour="black", alpha=0.9, size=1.3) +
  xlab("") +
  ylab("Log2 Fold change en sueño") +
  ylim(-0.12, 0.1) +
  theme(
    # Change axis line, ticks and text
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 23, color = "gray22", vjust = 2),
    # Remove panel background
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "gray", size = 1.2),
    panel.border = element_rect(fill = NA, color = "black"),
    plot.margin = unit(c(1.5,1.5,1.5,1.5), "line"))



# Creating the png file
png("Exp04_bar-plot_LD_GalR18_24jun21.png", width = 815, height = 690, res= 120) # res are the DPI values. 
# Plot
ggplot(plot.table) +
  geom_bar( aes(x=días, y=average), stat="identity", col="black", fill="orange", alpha=0.8, width= 0.6) +
  geom_errorbar( aes(x=días, ymin=max, ymax=min), width=0.4, colour="black", alpha=0.9, size=1.3) +
  xlab("") +
  ylab("Log2 Fold change en sueño") +
  ylim(-0.12, 0.1) +
  theme(
    # Change axis line, ticks and text
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 23, color = "gray22", vjust = 2),
    # Remove panel background
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "gray", size = 1.2),
    panel.border = element_rect(fill = NA, color = "black"),
    plot.margin = unit(c(1.5,1.5,1.5,1.5), "line"))


# Closing the file
dev.off()




#--------------------------------------------------------------------------- 
# STATISTICAL ANALYSIS


######################################### BAR PLOT ###########################################
# normality test

shapiro.test(fold_change_base)
shapiro.test(fold_change_led)
# (dan normales)


# t-tests
t.test(fold_change_base, fold_change_led)
t.test(fold_change_base, fold_change_led, paired = T)



######################################### SUBS PLOT ##########################################


# slicing data

idxi <- 48*(bday-1) + 1
idxf <- 48*tday

subs.ctr <- merge.ctr[idxi:idxf, ]  # controls
subs.tra <- merge.tra[idxi:idxf, ]  # treated

head(subs.ctr)
head(subs.tra)


# reshaping data
day <- as.factor(rep(c("baseline", "treatment"), each=48)) # por si las flies.
subs.ctr$day <- day
subs.tra$day <- day

head(subs.ctr)
head(subs.tra)

subs.ctr$bin <- as.factor(rep(1:48, 2))
subs.tra$bin <- as.factor(rep(1:48, 2))

str(subs.ctr)

ctr.tab <- melt(subs.ctr, id.vars = c("day", "bin")) # apilo las moscas (todo, excepto day y bin)
tra.tab <- melt(subs.tra, id.vars = c("day", "bin")) 

head(ctr.tab, 100)
head(tra.tab, 100)

colnames(ctr.tab) <- c("Day", "Bin", "Fly", "Sleep")
colnames(tra.tab) <- c("Day", "Bin", "Fly", "Sleep")

str(tra.tab)
str(ctr.tab)




#### Statistical analysis ----

## models for controls

# glm
ctr.glm <- glm(Sleep~Day*Bin, family = 'gaussian', data = ctr.tab)

1-pchisq(ctr.glm$deviance, ctr.glm$df.residual)  
summary(ctr.glm)
Anova(ctr.glm, type = 3) 

# lm
ctr.lm <- lm(Sleep~Day*Bin, data = ctr.tab) # este modelo lineal da lo mismo que el glm
summary(ctr.lm)
anova(ctr.lm) # el anova si cambia respecto al anova tipo 3 sobre el glm
#TukeyHSD(aov(ctr.lm))


## models for treatment

# glm
tra.glm <- glm(Sleep~Day*Bin, family = 'gaussian', data = tra.tab)

1-pchisq(tra.glm$deviance, tra.glm$df.residual)  
summary(tra.glm)
Anova(tra.glm, type = 3) 

# lm
tra.lm <- lm(Sleep~Day*Bin, data = tra.tab)
summary(tra.lm)
anova(tra.lm)












