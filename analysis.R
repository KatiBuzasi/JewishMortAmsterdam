# This script recreates Figure 2 and Table 1 in
# Riswick, T., Buzasi, K., and Muurling, S. 
# Exploring the mortality advantage of Jewish neighbourhoods in mid-nineteenth century Amsterdam
# published in Demographic Research

# setting working library

#setwd("") # type or copy the path to your working directory where you saved DATA.csv and this script

######## necessary libraries

library(readxl)

# libraries for plotting
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

# libraries for regressions
library(lmtest)
# library for robust standard errors
library(sandwich)


# import data
data <- read.csv("DATA.csv", sep=";", dec = ",")


#######################################################################
#
# recreating figure 1
#
#######################################################################

# create a label for jewish neighborhoods which will be used as a marker above the bars
data$jewish2 <- ""
data$jewish2[data$jewish_neighb2==1] <- "J"

# rename wealth variable to have a prettier legend

names(data)[names(data) == 'wealth_Isr'] <- 'wealth'

# prepare subfigures

fig1 <- ggplot(data, aes(x = reorder(neighborhood, -av_cdr), y = av_cdr, fill=wealth)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=jewish2), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=4)+
  scale_fill_manual(values=c("red3", "salmon1", "palegreen3", "palegreen4"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=14), legend.position="bottom") +
  labs(title="A. average death rate, 1867-70",x="", y="per 10,000", color="wealth (Israëls)", caption = "J = Jewish neighbourhood")
fig1

fig2 <- ggplot(data, aes(x = reorder(neighborhood, -av_idr), y = av_idr, fill=wealth)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=jewish2), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=4)+
  scale_fill_manual(values=c("red3", "salmon1", "palegreen3", "palegreen4"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=14), legend.position="bottom") +
  labs(title="B. average infant death rate, 1867-70",x="", y="per 10,000", color="wealth (Israëls)", caption = "J = Jewish neighbourhood")
fig2

fig3 <- ggplot(data, aes(x = reorder(neighborhood, -av_resp_dr), y = av_resp_dr, fill=wealth)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=jewish2), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=4)+
  scale_fill_manual(values=c("red3", "salmon1", "palegreen3", "palegreen4"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=14), legend.position="bottom") +
  labs(title="C. average death rate - respiratory causes, 1867-70",x="", y="per 10,000", color="wealth (Israëls)", caption = "J = Jewish neighbourhood")
fig3

fig4 <- ggplot(data, aes(x = reorder(neighborhood, -av_diarrh_dr), y = av_diarrh_dr, fill=wealth)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=jewish2), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=4)+
  scale_fill_manual(values=c("red3", "salmon1", "palegreen3", "palegreen4"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=14), legend.position="bottom") +
  labs(title="D. average death rate - diarrhoeal causes, 1867-70",x="", y="per 10,000", color="wealth (Israëls)", caption = "J = Jewish neighbourhood")
fig4

fig5 <- ggplot(data, aes(x = reorder(neighborhood, -cholera_DR), y = cholera_DR, fill=wealth)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=jewish2), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=4)+
  scale_fill_manual(values=c("red3", "salmon1", "palegreen3", "palegreen4"))+
  theme_classic()+
  ylim(0,220)+
  theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=14), legend.position="bottom") +
  labs(title="E. cholera death rate, 1866",x="", y="per 10,000", color="wealth (Israëls)", caption = "J = Jewish neighbourhood")
fig5

fig6 <- ggplot(data, aes(x = reorder(neighborhood, -smallpox_DR), y = smallpox_DR, fill=wealth)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=jewish2), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=4)+
  scale_fill_manual(values=c("red3", "salmon1", "palegreen3", "palegreen4"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=14), legend.position="bottom") +
  labs(title="F. smallpox death rate, 1871",x="", y="per 10,000", color="wealth (Israëls)", caption = "J = Jewish neighbourhood")
fig6

# combine subfigures, this compiles Figure 1

figure1 <- plot_grid(fig1, fig2, fig3, fig4, fig5, fig6, nrow=3)
figure1

# manual saving: saving parameters: width: 1200, height:1500

#######################################################################
#
# regression results presented in Table 1
#
#######################################################################

# Model 1
lm1 = lm(av_cdr~jewish_neighb2, data = data)
summary(lm1)
# standard errors
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))

# Model 2
lm2 = lm(av_idr~jewish_neighb2, data = data)
summary(lm2)
# standard errors
coeftest(lm2, vcov = vcovHC(lm2, type = "HC3"))

# Model 3
lm3 = lm(av_resp_dr~jewish_neighb2, data = data)
summary(lm3)
# standard errors
coeftest(lm3, vcov = vcovHC(lm3, type = "HC3"))

# Model 4
lm4 = lm(av_diarrh_dr~jewish_neighb2, data = data)
summary(lm4)
# standard errors
coeftest(lm4, vcov = vcovHC(lm4, type = "HC3"))

# Model 5
lm5 = lm(cholera_DR~jewish_neighb2, data = data)
summary(lm5)
# standard errors
coeftest(lm5, vcov = vcovHC(lm5, type = "HC3"))

# Model 6
lm6 = lm(smallpox_DR~jewish_neighb2, data = data)
summary(lm6)
# standard errors
coeftest(lm6, vcov = vcovHC(lm6, type = "HC3"))

# Model 7
lm7 = lm(av_cdr~jewish_neighb2+rather_poor+rather_well+well+birth_rate_Isr+dens_1869+hospital, data = data)
summary(lm7)
# standard errors
coeftest(lm7, vcov = vcovHC(lm7, type = "HC3"))

# Model 8
lm8 = lm(av_idr~jewish_neighb2+rather_poor+rather_well+well+birth_rate_Isr+dens_1869+hospital, data = data)
summary(lm8)
# standard errors
coeftest(lm8, vcov = vcovHC(lm8, type = "HC3"))

# Model 9
lm9 = lm(av_resp_dr~jewish_neighb2+rather_poor+rather_well+well+birth_rate_Isr+dens_1869+hospital, data = data)
summary(lm9)
# standard errors
coeftest(lm9, vcov = vcovHC(lm9, type = "HC3"))

# Model 10
lm10 = lm(av_diarrh_dr~jewish_neighb2+rather_poor+rather_well+well+birth_rate_Isr+dens_1869+hospital, data = data)
summary(lm10)
# standard errors
coeftest(lm10, vcov = vcovHC(lm10, type = "HC3"))

# Model 11
lm11 = lm(cholera_DR~jewish_neighb2+rather_poor+rather_well+well+birth_rate_Isr+dens_1869+hospital, data = data)
summary(lm11)
# standard errors
coeftest(lm11, vcov = vcovHC(lm11, type = "HC3"))

# Model 12
lm12 = lm(smallpox_DR~jewish_neighb2+rather_poor+rather_well+well+birth_rate_Isr+dens_1869+hospital, data = data)
summary(lm12)
# standard errors
coeftest(lm12, vcov = vcovHC(lm12, type = "HC3"))


