#end species counts
endcounts$plot <- factor(endcounts$plot , 
                   levels=levels(endcounts$plot)[order(levels(endcounts$plot), decreasing = TRUE)])

(speciesend <- ggplot(endcounts, aes(x= reorder(species, mean), y=mean, fill=plot)) + 
   ylab("Number of individuals (mean ± 1 SE)") +
   xlab("") +
   scale_y_continuous(expand = c(0,0), breaks=seq(0,250,50)) +
   expand_limits(y = c(0,250)) +
   theme(axis.line = element_line(color = "white"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank(),
         legend.position="none",
         axis.text.x=element_text(angle=0, hjust=1),
         plot.background = element_rect(fill = "transparent",color = NA),
         axis.title.x = element_text(color = "white"),
         axis.title.y = element_text(color = "white"),
         axis.text = element_text(color = "white"),
         axis.ticks = element_line(color = "white"),
         plot.title = element_text(color = "white")) +
   geom_col(position=position_dodge(1), color = "white") +
   scale_fill_manual(values = c("C" = "black", "E" = "white")) +
   geom_errorbar(aes(ymin=mean, ymax=mean+se),
                 width=.8,                    # Width of the error bars
                 color = "white",
                 position=position_dodge(1)) + coord_flip())
ggsave("speciesend.png", height = 5.57, width = 8, bg = "transparent")

#number individuals
(ni_startend_plot <-ggplot(ni_startend, aes(x=time, y=mean, fill=plot)) +
    ylab("Number of individuals (mean ± 1 SE)") +
    xlab("") +
    scale_y_continuous(expand = c(0,0), breaks=seq(0,350,50)) +
    expand_limits(y = c(0,350)) +
    theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", size = 15),
          axis.text = element_text(color = "white", size = 15),
          axis.ticks = element_blank(),
          plot.title = element_text(color = "white")) +
    geom_col(position=position_dodge(), color = "white") +
    scale_fill_manual(values = c("C" = "black", "E" = "white")) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),
                  width=0.8,                    # Width of the error bars
                  color = "white",
                  position=position_dodge(0.9)))
ggsave("numbind.png",bg = "transparent")

#richness
(rn_startend_plot <- ggplot(rn_startend, aes(x=time, y=mean, fill=plot)) +
    ylab("Number of species (mean ± 1 SE)") +
    xlab("") +
    scale_y_continuous(expand = c(0,0), breaks=seq(3,9,3)) +
    expand_limits(y = c(3,9)) +
    theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", size = 15),
          axis.text = element_text(color = "white", size = 15),
          axis.ticks = element_blank(),
          plot.title = element_text(color = "white")) +
    geom_col(position=position_dodge(), color = "white") +
    scale_fill_manual(values = c("C" = "black", "E" = "white")) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),
                  width=.8,                    # Width of the error bars
                  color = "white",
                  position=position_dodge(0.9)))
rn_startend_plot + coord_cartesian(ylim=c(3,9))
ggsave("richness.png",bg = "transparent")

#diversity
(cd_startend_plot <- ggplot(cd_startend, aes(x=time, y=mean, fill=plot)) +
    ylab("Gini-Simpson Index (mean ± 1 SE)") +
    xlab("") +
    scale_y_continuous(expand = c(0,0), breaks=seq(0,0.8,0.2)) +
    expand_limits(y = c(0,0.8)) +
    theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", size = 15),
          axis.text = element_text(color = "white", size = 15),
          axis.ticks = element_blank(),
          plot.title = element_text(color = "white")) +
    geom_col(position=position_dodge(), color = "white") +
    scale_fill_manual(values = c("C" = "black", "E" = "white")) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),
                  width=.8,                    # Width of the error bars
                  color = "white",
                  position=position_dodge(0.9)))
cd_startend_plot + coord_cartesian(ylim=c(0.2,0.8))
ggsave("diversity.png",bg = "transparent")

#dominance
(dn_startend_plot <- ggplot(dn_startend, aes(x=time, y=mean, fill=plot)) +
    ylab("Dominance (mean ± 1 SE)") +
    xlab("") +
    scale_y_continuous(expand = c(0,0), breaks=seq(0,0.9,0.3)) +
    expand_limits(y = c(0,0.9)) +
    theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", size = 15),
          axis.text = element_text(color = "white", size = 15),
          axis.ticks = element_blank(),
          plot.title = element_text(color = "white")) +
    geom_col(position=position_dodge(), color = "white") +
    scale_fill_manual(values = c("C" = "black", "E" = "white")) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),
                  width=.8,                    # Width of the error bars
                  color = "white",
                  position=position_dodge(0.9)))
dn_startend_plot + coord_cartesian(ylim=c(0.3,0.9))
ggsave("dominance.png",bg = "transparent")

#evenness
(en_startend_plot <- ggplot(en_startend, aes(x=time, y=mean, fill=plot)) +
    ylab("Evenness (mean ± 1 SE)") +
    xlab("") +
    scale_y_continuous(expand = c(0,0), breaks=seq(0,0.9,0.3)) +
    expand_limits(y = c(0,0.9)) +
    theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", size = 15),
          axis.text = element_text(color = "white", size = 15),
          axis.ticks = element_blank(),
          plot.title = element_text(color = "white")) +
    geom_col(position=position_dodge(), color = "white") +
    scale_fill_manual(values = c("C" = "black", "E" = "white")) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),
                  width=.8,                    # Width of the error bars
                  color = "white",
                  position=position_dodge(0.9)))
en_startend_plot + coord_cartesian(ylim=c(0.3,0.9))
ggsave("evenness.png",bg = "transparent")

#diet
(pref_plot <- ggplot(pref, aes(x= (species), y=mean)) + 
    geom_point(color = "white", size = 3) +
    geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI), height = 0.5, width = 0.5, color = "white") +
    ylab("Manly's alpha (mean ± 95% CI)") +
    xlab("Plant species") +
    theme(axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), plot.background = element_rect(fill = "transparent",color = NA), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), plot.title = element_text(color = "black")) +
    geom_hline(yintercept=c(0.2), linetype="dotted", color = "white") +
    theme(legend.position="none") +
    theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", size = 15),
          axis.text = element_text(color = "white", size = 15),
          axis.ticks = element_blank(),
          plot.title = element_text(color = "white")) +
    #geom_jitter(data=alphavals, aes(x = species, y = alpha, color = tortoise), size = 3, width = 0.2) +
    coord_flip())
ggsave("diets.png",bg = "transparent")
