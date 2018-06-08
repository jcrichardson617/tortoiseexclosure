#################################
#for plotting combined Cs and Gs#
#################################
#use plants.R for seperate plots#
#################################

diversitydfnew <- diversitydf
levels(diversitydfnew$plot)[3] <- "C"
#means for final month
march17 <- diversitydfnew[diversitydfnew$date == '3/1/2017',]
march17 <- march17[,-c(1:2)]
march17 <- march17[,-c(3:16)]
march17


totalend <- ddply(march17, .(plot), summarize,
                  mean = mean(numberindividuals),
                  se = sd(numberindividuals)/sqrt(length(numberindividuals)))

rnend <- ddply(march17, .(plot), summarize,
               mean = mean(richness),
               se = sd(richness)/sqrt(length(richness)))

cdend <- ddply(march17, .(plot), summarize,
               mean = mean(compdiversity),
               se = sd(compdiversity)/sqrt(length(compdiversity)))

evenend <- ddply(march17, .(plot), summarize,
                 mean = mean(evenness),
                 se = sd(evenness)/sqrt(length(evenness)))

dominanceend <- ddply(march17, .(plot), summarize,
                      mean = mean(dominance),
                      se = sd(dominance)/sqrt(length(dominance)))

niendplot <- diversityplotsend(totalend, tite = "") +
  scale_y_continuous(limits=c(110, 450)) +
  #theme_bw() +
  ylab("Number of individuals (mean ± 1 SE)") +
  xlab("") 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                     labels = c("Control","Exclosure"))
niendplot
#ggsave("niend.png",bg = "transparent")

rnendplot <- ggplot(rnend, aes(x=plot, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), color = "black") +
  geom_point(color = "black") +
  ylab("Richness (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(limits=c(4, 9)) +
  #ggtitle("End") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
rnendplot
#ggsave("rnend.png",bg = "transparent")

cdendplot <- diversityplotsend(cdend, tite = "") +
  scale_y_continuous(limits=c(0.3, 0.8)) +
  theme_bw() +
  ylab("Diversity (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
cdendplot
#ggsave("cdend.png",bg = "transparent")

dnendplot <- diversityplotsend(dominanceend, tite = "") +
  scale_y_continuous(limits=c(0.3, 0.9)) +
  theme_bw() +
  ylab("Dominance (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
dnendplot
#ggsave("dnend.png",bg = "transparent")

enendplot <- diversityplotsend(evenend, tite = "") +
  scale_y_continuous(limits=c(0.4, 0.9)) +
  theme_bw() +
  ylab("Evenness (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
enendplot
#ggsave("enend.png",bg = "transparent")

#multiplot(niendplot,rnendplot,cdendplot,dnendplot,enendplot, cols = 2)

#means for first month
march15 <- diversitydfnew[diversitydfnew$date == '3/1/2015',]
march15 <- march15[,-c(1:2)]
march15 <- march15[,-c(3:16)]
march15

totalstart <- ddply(march15, .(plot), summarize,
                    mean = mean(numberindividuals),
                    se = sd(numberindividuals)/sqrt(length(numberindividuals)))

rnstart <- ddply(march15, .(plot), summarize,
                 mean = mean(richness),
                 se = sd(richness)/sqrt(length(richness)))

cdstart <- ddply(march15, .(plot), summarize,
                 mean = mean(compdiversity),
                 se = sd(compdiversity)/sqrt(length(compdiversity)))

evenstart <- ddply(march15, .(plot), summarize,
                   mean = mean(evenness),
                   se = sd(evenness)/sqrt(length(evenness)))

dominancestart <- ddply(march15, .(plot), summarize,
                        mean = mean(dominance),
                        se = sd(dominance)/sqrt(length(dominance)))

nistartplot <- diversityplotsstart(totalstart, tite = "Start") +
  scale_y_continuous(limits=c(110, 450)) +
  theme_bw() +
  ylab("Number of individuals (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
nistartplot
#ggsave("nistart.png",bg = "transparent")

rnstartplot <- diversityplotsstart(rnstart, tite = "Start") +
  scale_y_continuous(limits=c(4, 9)) +
  theme_bw() +
  ylab("Richness (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
rnstartplot
#ggsave("rnstart.png",bg = "transparent")

cdstartplot <- diversityplotsstart(cdstart, tite = "Start") +
  scale_y_continuous(limits=c(0.3, 0.8)) +
  theme_bw() +
  ylab("Diversity (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
rnstartplot
#ggsave("cdstart.png",bg = "transparent")

dnstartplot <- diversityplotsstart(dominancestart, tite = "Start") +
  scale_y_continuous(limits=c(0.3, 0.9)) +
  theme_bw() +
  ylab("Dominance (mean ± 1 SE)") +
  xlab("") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
dnstartplot
#ggsave("dnstart.png",bg = "transparent")

enstartplot <- diversityplotsstart(evenstart, tite = "Start") +
  scale_y_continuous(limits=c(0.4, 0.9)) +
  ylab("Evenness (mean ± 1 SE)") +
  xlab("") +
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  scale_x_discrete(limit = c("C", "E"),
                   labels = c("Control","Exclosure"))
enstartplot
#ggsave("enstart.png",bg = "transparent")

#multiplot(nistartplot,rnstartplot,cdstartplot,dnstartplot,enstartplot, cols = 2)
#multiplot(nistartplot, rnstartplot, cdstartplot, dnstartplot, enstartplot,
#          niendplot, rnendplot, cdendplot, dnendplot, enendplot, cols = 2)

#multiplot(nistartplot, niendplot, cols = 2)
#multiplot(rnstartplot, rnendplot, cols = 2)
#multiplot(cdstartplot, cdendplot, cols = 2)
#multiplot(dnstartplot, dnendplot, cols = 2)
#multiplot(enstartplot, enendplot, cols = 2)

multiplot(rnendplot, dnendplot, cdendplot, enendplot, cols = 2)

totalstart$time <- c("2015","2015")
totalend$time <- c("2017","2017")

ni_startend <- rbind(totalstart,totalend)

rnstart$time <- c("2015","2015")
rnend$time <- c("2017","2017")

rn_startend <- rbind(rnstart,rnend)

cdstart$time <- c("2015","2015")
cdend$time <- c("2017","2017")

cd_startend <- rbind(cdstart,cdend)

dominancestart$time <- c("2015","2015")
dominanceend$time <- c("2017","2017")

dn_startend <- rbind(dominancestart,dominanceend)

evenstart$time <- c("2015","2015")
evenend$time <- c("2017","2017")

en_startend <- rbind(evenstart,evenend)

#E and C/G for end all species
endcounts <- read.table("endcounts.csv", header = TRUE, sep = ",")

endcounts <- ddply(endcounts, .(species,plot), summarize,
                              mean = mean(count),
                              se = sd(count)/sqrt(length(count)))

endcounts$species <- c("Catharanthus roseus","Catharanthus roseus",
                       "Cyperus ligularis","Cyperus ligularis",
                       "Fimbristylis cymosa","Fimbristylis cymosa",
                       "Heliotropium polyphyllum","Heliotropium polyphyllum",
                       "Mecardonia acuminata","Mecardonia acuminata",
                       "Oenothera simulans","Oenothera simulans",
                       "Polypremum procumbens","Polypremum procumbens",
                       "Phyllanthus abnormis","Phyllanthus abnormis",
                       "Paspalum setaceum","Paspalum setaceum",
                       "Stachytarpheta jamaicensis","Stachytarpheta jamaicensis",
                       "Waltheria indica","Waltheria indica")

endcounts$species <- c("Cr","Cr",
                       "Cl","Cl",
                       "Fc","Fc",
                       "Hp","Hp",
                       "Ma","Ma",
                       "Os","Os",
                       "Pp","Pp",
                       "Pa","Pa",
                       "Ps","Ps",
                       "Sj","Sj",
                       "Wi","Wi")

startcounts <- read.table("startcounts.csv", header = TRUE, sep = ",")

startcounts <- ddply(startcounts, .(species,plot), summarize,
                   mean = mean(count),
                   se = sd(count)/sqrt(length(count)))

startcounts$species <- c("Catharanthus roseus","Catharanthus roseus",
                       "Cyperus ligularis","Cyperus ligularis",
                       "Ernodea littoralis","Ernodea littoralis",
                       "Eragrostis ciliaris","Eragrostis ciliaris",
                       "Fimbristylis cymosa","Fimbristylis cymosa",
                       "Heliotropium polyphyllum","Heliotropium polyphyllum",
                       "Mecardonia acuminata","Mecardonia acuminata",
                       "Oenothera simulans","Oenothera simulans",
                       "Polypremum procumbens","Polypremum procumbens",
                       "Phyllanthus abnormis","Phyllanthus abnormis",
                       "Paspalum setaceum","Paspalum setaceum",
                       "Stachytarpheta jamaicensis","Stachytarpheta jamaicensis",
                       "Waltheria indica","Waltheria indica")

ggplot(startcounts, aes(x=species, y=mean, fill=plot)) + 
  ylab("Number of individuals (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,250,50)) +
  expand_limits(y = c(0,250)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1))
#ggsave("speciesstart.png",bg = "transparent")
#start species counts

#Figures for publishing#----

(speciesend <- ggplot(endcounts, aes(x= reorder(species, -mean), y=mean, fill=plot)) + 
  ylab("Number of individuals (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,250,50)) +
  expand_limits(y = c(0,250)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(1), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1)))
#ggsave("speciesend.png",bg = "transparent")
#end species counts

(ni_startend_plot <-ggplot(ni_startend, aes(x=time, y=mean, fill=plot)) +
  ylab("Number of individuals (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,350,50)) +
  expand_limits(y = c(0,350)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black", size = 10),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1)))
#number individuals

(rn_startend_plot <- ggplot(rn_startend, aes(x=time, y=mean, fill=plot)) +
  ylab("Number of species (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(3,9,3)) +
  expand_limits(y = c(3,9)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black", size = 8),
        axis.text = element_text(color = "black", size = 10),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1)))
rn_startend_plot + coord_cartesian(ylim=c(3,9))
#richness

(cd_startend_plot <- ggplot(cd_startend, aes(x=time, y=mean, fill=plot)) +
  ylab("Gini-Simpson Index (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,0.8,0.2)) +
  expand_limits(y = c(0,0.8)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black", size = 8),
        axis.text = element_text(color = "black", size = 10),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1)))
cd_startend_plot + coord_cartesian(ylim=c(0.2,0.8))
#diversity

(dn_startend_plot <- ggplot(dn_startend, aes(x=time, y=mean, fill=plot)) +
  ylab("Dominance (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,0.9,0.3)) +
  expand_limits(y = c(0,0.9)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black", size = 8),
        axis.text = element_text(color = "black", size = 10),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1)))
dn_startend_plot + coord_cartesian(ylim=c(0.3,0.9))
#dominance

(en_startend_plot <- ggplot(en_startend, aes(x=time, y=mean, fill=plot)) +
  ylab("Evenness (mean ± 1 SE)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,0.9,0.3)) +
  expand_limits(y = c(0,0.9)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black", size = 8),
        axis.text = element_text(color = "black", size = 10),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "black")) +
  geom_col(position=position_dodge(), color = "black") +
  scale_fill_manual(values = c("C" = "black", "E" = "white")) +
  geom_errorbar(aes(ymin=mean, ymax=mean+se),
                width=.8,                    # Width of the error bars
                position=position_dodge(1)))
en_startend_plot + coord_cartesian(ylim=c(0.3,0.9))
#evenness

multiplot(
rn_startend_plot + coord_cartesian(ylim=c(3,9)),
en_startend_plot + coord_cartesian(ylim=c(0.3,0.9)),
cd_startend_plot + coord_cartesian(ylim=c(0.2,0.8)),
dn_startend_plot + coord_cartesian(ylim=c(0.3,0.9)), cols = 2)
