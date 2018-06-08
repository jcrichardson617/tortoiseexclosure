library(dplyr)
library(ggplot2)

mydata <- read.table("selection.csv", header = TRUE, sep = ",")

mydata$consumed <- mydata$start - mydata$end
mydata$initial <- mydata$start

alphavals <- 
  mydata %>%
  group_by(tortoise) %>%
  transmute(
    alpha = selectapref::manlysalpha(initial, consumed)
  )
  
alphavals <- as.data.frame(alphavals)

alphavals$species <- mydata$species

pref <- alphavals %>%
  group_by(species) %>%
  summarise(
    mean = mean(alpha),
    sd = sd(alpha),
    CI = 1.96 * (sd/sqrt(8))
  )

levels(pref$species) <- c("Fc",
                          "Hp",
                          "Pp",
                          "Wi",
                          "Pa")

Fc <- pref[1,]
Hp <- pref[2,]
Pp <- pref[3,]
Wi <- pref[4,]
Pa <- pref[5,]
pref <- rbind(Wi,Pa,Fc,Pp,Hp)
pref <- rbind(Hp,Pp,Fc,Pa,Wi)
pref$species <- factor(pref$species, levels = pref$species)

(pref_plot <- ggplot(pref, aes(x= (species), y=mean)) + 
  geom_point(color = "black", size = 3) +
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI), color = "black", height = 0.5, width = 0.5) +
  ylab("Manly's alpha (mean ± 95% CI)") +
  xlab("Plant species") +
  theme(axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), plot.background = element_rect(fill = "transparent",color = NA), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), plot.title = element_text(color = "black")) +
        geom_hline(yintercept=c(0.2), linetype="dotted") +
  theme(legend.position="none") +
  #geom_jitter(data=alphavals, aes(x = species, y = alpha, color = tortoise), size = 3, width = 0.2) +
  coord_flip())
ggsave("diets.png")




p <- ggplot(pref, aes(y=mean)) + theme(axis.text.x=element_text(angle=90, hjust=1))
p + geom_point(aes(x=species), data=pref, stat="identity")
