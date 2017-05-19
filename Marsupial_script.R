#==================================
#
#          NHM Data Course
#         15-19th May 2017
#
#    Marsupial data exploration
#
#==================================


library(tidyverse)

marsupial <- read.csv("Marsupial_skins.csv", header = TRUE)
glimpse(marsupial)


marsupial <- marsupial[!is.na(marsupial$Sex), ]


## Linear Regression

mar_model <- lm(Tail_length ~ Head_body, data = marsupial)
autoplot(mar_model, smooth.colour = NA)


#ANOVA
anova(mar_model) #not significant

#LINEAR MODEL
summary(mar_model)

#PLOT
ggplot(marsupial, aes(Tail_length, Head_body)) +
  geom_point(colour="yellowgreen") +
  geom_smooth(method=lm, se=TRUE, alpha = 0.2, colour = "violetred2") #include a regression line



#BOXPLOTS - no sig diff between M and F in lengths

#no significant difference between M and F in total length
p1 <- ggplot(data=marsupial, aes(Sex, Head_body)) + 
  geom_boxplot(aes(fill = Sex)) +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size=10, hjust = 1),
        axis.text.y = element_text(size=14),
        axis.title = element_text(size=12, face="bold")) +
  labs(x = "", y = "Total Length (mm)")


#no significant difference between M and F in tail length
p2 <- ggplot(data=marsupial, aes(Sex, Tail_length)) + 
  geom_boxplot(aes(fill = Sex)) +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size=10, hjust = 1),
        axis.text.y = element_text(size=14),
        axis.title = element_text(size=12, face="bold")) +
  labs(x = "", y = "Head & Body Length (mm)")


#no significant difference between M and F in head+body length
p3 <- ggplot(data=marsupial, aes(Sex, Hind_foot)) + 
  geom_boxplot(aes(fill = Sex)) +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size=10, hjust = 1),
        axis.text.y = element_text(size=14),
        axis.title = element_text(size=12, face="bold")) +
  labs(x = "", y = "Hind foot Length (mm)")


p4 <- ggplot(data=marsupial, aes(Sex, Ear_length)) + 
  geom_boxplot(aes(fill = Sex)) +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE) +
  theme(panel.background = element_blank(),
                 panel.border = element_rect(colour = "black", fill = NA),
                 axis.text.x = element_text(size=10, hjust = 1),
                 axis.text.y = element_text(size=14),
                 axis.title = element_text(size=12, face="bold")) +
  labs(x = "", y = "Ear Length (mm)")



require(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2)


###



ggplot(data=marsupial, aes(Head_body, Hind_foot, colour = Species)) +
  geom_point() +
  scale_colour_manual(values=c("cornflowerblue", "chartreuse", "violetred", "yellow"))


ggplot(data=marsupial, aes(Species, Head_body)) +
  geom_boxplot(aes(fill = Species)) +
  scale_fill_brewer(palette="Blues")

ggplot(data=marsupial, aes(Species, Tail_length)) +
  geom_boxplot(aes(fill = Species)) +
  scale_fill_brewer(palette="Blues")


ggplot(marsupial, aes(Sex, Head_body)) +
  geom_boxplot(aes(fill = Sex)) +
  scale_fill_brewer(palette="Purples")
