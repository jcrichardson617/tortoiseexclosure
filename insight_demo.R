#evenness calculation####

#load libraries
library(ggplot2)
library(plyr)
library(car)
library(betareg)

#import data
mydata <- read.table("communityall.csv", header = TRUE, sep = ",")

#function for doing species richness, diversity, dominance, and evenness in a given season####
diversityplants <- function(aDataFrame, season){
  
  #subsets season of desire
  currentseason <- subset(aDataFrame, date == season) 
  
  #counts individuals across all species
  numberindividuals <- rowSums(currentseason[, 5:ncol(currentseason)]) 
  
  #calculate richness
  richness <- apply(currentseason[, 5:ncol(currentseason)], 1, function(x)sum(x!=0)) 
  
  #calculate composite simpsons diversity from Spiller and Shoener (1998) and originally Lande (1996)
  compdiversity <- 1 - (rowSums(((currentseason[, 5:ncol(currentseason)])/(rowSums(currentseason[, 5:ncol(currentseason)])))^2)) 
  
  #proportion of most abundant contribution to overall
  dominance <- (apply(currentseason[, 5:ncol(currentseason)], 1, max))/(rowSums(currentseason[, 5:ncol(currentseason)])) 
  
  #calculates simpsons evenness, actual value over the value if all species were even
  evenness <- apply(currentseason[, 5:ncol(currentseason)], 1, function (x) (1- sum((x/sum(x))^2))/(1- sum((rep(((sum(x))/(sum(x > 0))), times = sum(x > 0))/sum(rep(((sum(x))/(sum(x > 0))), times = sum(x > 0))))^2)))
  
  #binds all measured things to community matrix
  season <- cbind(currentseason, numberindividuals, richness, compdiversity, dominance, evenness, row.names = NULL)
  
  return(season)
  
}

#gives diversity measures in a given season#####
#focus on start and end here for evenness
march15div <- diversityplants (mydata, "3/1/2015")
march17div <- diversityplants (mydata, "3/1/2017")


#Stats####
#Do C and G values differ in the beginning

#isolate the metric of interest
startevenness <- march15div[,c("plot","evenness")]

#get exclosure (E) out of the picture
(bigC <- subset(startevenness, startevenness$plot != "E"))

#eyeball any differences, tl;dr not much
(bigCmeansstart <- ddply(bigC, .(plot), summarize,
                   mean = mean(evenness),
                   se = sd(evenness)/sqrt(length(evenness))))

#jump into GLM based on previous work of the topic
#AIC model selection was conducted to determine best distribution, only best model was kept to avoid
#code cluttering

#beta regression
model.sen.beta.bigC <- betareg(evenness ~ plot, data = bigC)
summary(model.sen.beta.bigC)

Anova(model.sen.beta.bigC) 

#no difference between C and G, so collapse into single control

levels(startevenness$plot)[3] <- "C"

(meansstart <- ddply(startevenness, .(plot), summarize,
                         mean = mean(evenness),
                         se = sd(evenness)/sqrt(length(evenness))))

#then test collapsed controls vs exclosures
model.sen.beta <- betareg(evenness ~ plot, data = startevenness)
summary(model.sen.beta)

Anova(model.sen.beta) #no differences in beginning, cool!

#end evenness
march17div

endevenness <- march17div[,c("plot","evenness")]

#C and G again, then collapse and test bigC vs E
(bigC <- subset(endevenness, endevenness$plot != "E"))

(bigCmeansend <- ddply(bigC, .(plot), summarize,
                   mean = mean(evenness),
                   se = sd(evenness)/sqrt(length(evenness))))

model.een.beta <- betareg(evenness ~ plot, data = bigC)
summary(model.een.beta)

Anova(model.een.beta) #no difference C and G at end

levels(endevenness$plot)[3] <- "C"

(meansend <- ddply(endevenness, .(plot), summarize,
                     mean = mean(evenness),
                     se = sd(evenness)/sqrt(length(evenness))))

model.edv.gaussian <- betareg(evenness ~ plot, data = endevenness)
summary(model.edv.gaussian)
Anova(model.edv.gaussian)

#did similar pairwise comparisons for "Start vs End" for exclosures, and "Start vs End" for controls.

#graph it
evenness <- rbind(meansstart, meansend)
evenness$time <- c("2015", "2015", "2017", "2017")

(evenness_plot <- ggplot(evenness, aes(x=time, y=mean, fill=plot)) +
    ylab("Evenness (mean ± 1 SE)") +
    xlab("") +
    scale_y_continuous(expand = c(0,0), breaks=seq(0,0.9,0.3)) +
    expand_limits(y = c(0,0.9)) +
    theme(axis.line = element_line(color = "black"),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5, size = 15),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(size = 15, color = "black"),
          axis.ticks = element_blank(),
          plot.title = element_blank()) +
    geom_col(position=position_dodge(), color = "black") +
    scale_fill_manual(values = c("C" = "black", "E" = "white")) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),
                  width=.8,                    # Width of the error bars
                  position=position_dodge(0.9)))
#black is control, white exclosure