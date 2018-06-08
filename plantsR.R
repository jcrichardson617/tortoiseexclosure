########################
#Community data metrics#
########################
#run before ind metrics#
########################

library(ggplot2)
library(plyr)
library(vegan)
library(reshape)
library(MASS)
library(aod)
library(car)
library(multcomp)
library(betareg)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

speciesplots <- function(species, graphname) {
  pd <- position_dodge(20) # move them .05 to the left and right
  return(
    ggplot(species, aes(x=date, y=species[,5], colour=plot, shape=as.character(replicate))) + 
      geom_line(position=pd) +
      geom_point(position=pd) +
      ggtitle(graphname))
}

perspeciesplot <- function(species, graphtitle) {
  pd <- position_dodge(10) # move them .05 to the left and right
  return(ggplot(species, aes(x=date, y=mean, colour=plot)) + 
           geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
           geom_line(position=pd) +
           geom_point(position=pd) +
           ggtitle(graphtitle) +
           scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="24 months")))
}

diversityplots <- function(diversitymeasure, tite) {
  pd <- position_dodge(10) # move them .05 to the left and right
  return(ggplot(diversitymeasure, aes(x=date, y=mean, colour=plot)) + 
           geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
           geom_line(position=pd) +
           geom_point(position=pd) +
           ggtitle(tite)) + 
    scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"), labels=date_format("%Y-%m-%d"))
}

#function for doing species richness, diversity, dominance, and evenness in a given season
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

#function for making plot of overall (through time) stuffs.
diversityplotstotals <- function(diversitymeasure, tite) {
  return(ggplot(diversitymeasure, aes(x=plot, y=mean)) + 
           geom_errorbar(aes(ymin=mean-se, ymax=mean+se)) +
           geom_point() +
           ggtitle(tite))
}

#plots end values
diversityplotsend <- function(diversitymeasure, tite) {
  return(ggplot(diversitymeasure, aes(x=plot, y=mean)) + 
           geom_errorbar(aes(ymin=mean-se, ymax=mean+se), color = "white") + #white for esa talk
           geom_point(color = "white") + #white for esa talk
           ggtitle(tite))
}

#plots start values
diversityplotsstart <- function(diversitymeasure, tite) {
  return(ggplot(diversitymeasure, aes(x=plot, y=mean)) + 
           geom_errorbar(aes(ymin=mean-se, ymax=mean+se), color = "black") +
           geom_point(color = "black") +
           ggtitle(tite))
}

#import
mydata <- read.table("communityall.csv", header = TRUE, sep = ",")

#######diversity plots##########
#gives diversity measures per season
march15div <- diversityplants (mydata, "3/1/2015")
june15div <- diversityplants (mydata, "6/1/2015")
sept15div <- diversityplants (mydata, "9/1/2015")
dec15div <- diversityplants (mydata, "12/1/2015")
march16div <- diversityplants (mydata, "3/1/2016")
june16div <- diversityplants (mydata, "6/1/2016")
sept16div <- diversityplants (mydata, "9/1/2016")
dec16div <- diversityplants (mydata, "12/1/2016")
march17div <- diversityplants (mydata, "3/1/2017")

#binds season together into one large matrix
diversitydf <- rbind(march15div, june15div, sept15div, dec15div, march16div, june16div, sept16div, dec16div, march17div)
#log diversity
#diversitydf$compdiversity <- log(diversitydf$compdiversity)
#arcsin sqrt evenness
#diversitydf$evenness <- asin(sign(diversitydf$evenness) * sqrt(abs(diversitydf$evenness)))
#arcsin sqrt dominance
#diversitydf$dominance <- asin(sign(diversitydf$dominance) * sqrt(abs(diversitydf$dominance)))

#averages all plots per each season, BUT NOT CURRENTLY FOR PROPORTIONS
numberindividuals <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                 mean = mean(numberindividuals),
                 se = sd(numberindividuals)/sqrt(length(numberindividuals)))
colnames(numberindividuals) <- c("date", "plot","mean","se")

richness <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(richness),
                  se = sd(richness)/sqrt(length(richness)))
colnames(richness) <- c("date", "plot","mean","se")
#plot(richness$se~richness$mean)

compdiversity <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                   mean = mean(compdiversity),
                   se = sd(compdiversity)/sqrt(length(compdiversity)))
colnames(compdiversity) <- c("date", "plot","mean","se")
#plot(compdiversity$se~compdiversity$mean)

dominance <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                   mean = mean(dominance),
                   se = sd(dominance)/sqrt(length(dominance)))
colnames(dominance) <- c("date", "plot","mean","se")  

evenness <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(evenness),
                  se = sd(evenness)/sqrt(length(evenness)))
colnames(evenness) <- c("date", "plot","mean","se")

#Plots monthly avged values
ni <- diversityplots(numberindividuals, tite = "Number of individuals")
ni <- ni + scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"))
#ni
rn <- diversityplots(richness, tite = "Species richness")
rn <- rn + scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"))
#rn
cd <- diversityplots(compdiversity, tite = "Composite diversity")
cd <- cd + scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"))
#cd
dn <- diversityplots(dominance, tite = "Dominance")
dn <- dn + scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"))
#dn
en <- diversityplots(evenness, tite = "Evenness")
en <- en + scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"))
#en

#multiplot(ni,rn,cd,dn,en, cols = 2)

#gives raw diversity values which were plotted, for manual data inspection.
raw <- diversitydf[,-c(2:3)]
raw <- raw[,-c(3:16)]
raw

#gives overalls means THROUGH TIME
#totalind <- ddply(diversitydf, .(plot), summarize,
 #     mean = mean(numberindividuals),
  #    se = sd(numberindividuals)/sqrt(length(numberindividuals)))

#totalrichness <- ddply(diversitydf, .(plot), summarize,
 #                 mean = mean(richness),
  #                se = sd(richness)/sqrt(length(richness)))

#totalcd <- ddply(diversitydf, .(plot), summarize,
 #                mean = mean(compdiversity),
  #               se = sd(compdiversity)/sqrt(length(compdiversity)))

#totaleven <- ddply(diversitydf, .(plot), summarize,
 #                mean = mean(evenness),
  #               se = sd(evenness)/sqrt(length(evenness)))

#totaldom <- ddply(diversitydf, .(plot), summarize,
 #                mean = mean(dominance),
  #               se = sd(dominance)/sqrt(length(dominance)))

#niall <- diversityplotstotals(totalind, tite = "Number of individuals")
#rnall <- diversityplotstotals(totalrichness, tite = "Species richness")
#cdall <- diversityplotstotals(totalcd, tite = "Composite diversity")
#dnall <- diversityplotstotals(totaldom, tite = "Dominance")
#enall <- diversityplotstotals(totaleven, tite = "Evenness")

#means for final month
march17 <- diversitydf[diversitydf$date == '3/1/2017',]
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

niendplot <- diversityplotsend(totalend, tite = "Number of individuals at end") +
  scale_y_continuous(limits=c(110, 450))
rnendplot <- ggplot(rnend, aes(x=plot, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se)) +
  geom_point() +
  scale_y_continuous(limits=c(4, 9)) +
  ggtitle("Species richness at end")
cdendplot <- diversityplotsend(cdend, tite = "Composite diversity at end") +
  scale_y_continuous(limits=c(0.3, 0.8))
dnendplot <- diversityplotsend(dominanceend, tite = "Dominance at end") +
  scale_y_continuous(limits=c(0.3, 0.9))
enendplot <- diversityplotsend(evenend, tite = "Evenness at end") +
  scale_y_continuous(limits=c(0.4, 0.9))
  
#multiplot(niendplot,rnendplot,cdendplot,dnendplot,enendplot, cols = 2)

#means for first month
march15 <- diversitydf[diversitydf$date == '3/1/2015',]
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

nistartplot <- diversityplotsstart(totalstart, tite = "Number of individuals at start") +
  scale_y_continuous(limits=c(110, 450))
rnstartplot <- diversityplotsstart(rnstart, tite = "Species richness at start") +
  scale_y_continuous(limits=c(4, 9))
cdstartplot <- diversityplotsstart(cdstart, tite = "Composite diversity at start") +
  scale_y_continuous(limits=c(0.3, 0.8))
dnstartplot <- diversityplotsstart(dominancestart, tite = "Dominance at start") +
  scale_y_continuous(limits=c(0.3, 0.9))
enstartplot <- diversityplotsstart(evenstart, tite = "Evenness at start") +
  scale_y_continuous(limits=c(0.4, 0.9))

#multiplot(nistartplot,rnstartplot,cdstartplot,dnstartplot,enstartplot, cols = 2)
#multiplot(nistartplot, rnstartplot, cdstartplot, dnstartplot, enstartplot,
#          niendplot, rnendplot, cdendplot, dnendplot, enendplot, cols = 2)

#multiplot(nistartplot, niendplot, cols = 2)
#multiplot(rnstartplot, rnendplot, cols = 2)
#multiplot(cdstartplot, cdendplot, cols = 2)
#multiplot(dnstartplot, dnendplot, cols = 2)
#multiplot(enstartplot, enendplot, cols = 2)

#######diversity statistics#######
#isolates measure of diversity for beginning month and each plot type
start <- diversitydf[diversitydf$date == '3/1/2015',]

startnumberindividuals <- start[,c("plot","numberindividuals")]
Estartnumberindividuals <- startnumberindividuals[startnumberindividuals$plot == 'E',]
Cstartnumberindividuals <- startnumberindividuals[startnumberindividuals$plot == 'C',]
Gstartnumberindividuals <- startnumberindividuals[startnumberindividuals$plot == 'G',]

startrichness <- start[,c("plot","richness")]
Estartrichness <- startrichness[startrichness$plot == 'E',]
Cstartrichness <- startrichness[startrichness$plot == 'C',]
Gstartrichness <- startrichness[startrichness$plot == 'G',]

startdiversity <- start[,c("plot","compdiversity")]
Estartdiversity <- startdiversity[startdiversity$plot == 'E',]
Cstartdiversity <- startdiversity[startdiversity$plot == 'C',]
Gstartdiversity <- startdiversity[startdiversity$plot == 'G',]

startdominance <- start[,c("plot","dominance")]
Estartdominance <- startdominance[startdominance$plot == 'E',]
Cstartdominance <- startdominance[startdominance$plot == 'C',]
Gstartdominance <- startdominance[startdominance$plot == 'G',]

startevenness <- start[,c("plot","evenness")]
Estartevenness <- startevenness[startevenness$plot == 'E',]
Cstartevenness <- startevenness[startevenness$plot == 'C',]
Gstartevenness <- startevenness[startevenness$plot == 'G',]

#isolates measure of diversity for end month
end <- diversitydf[diversitydf$date == '3/1/2017',]

endnumberindividuals <- end[,c("plot","numberindividuals")]
Eendnumberindividuals <- endnumberindividuals[endnumberindividuals$plot == 'E',]
Cendnumberindividuals <- endnumberindividuals[endnumberindividuals$plot == 'C',]
Gendnumberindividuals <- endnumberindividuals[endnumberindividuals$plot == 'G',]

endrichness <- end[,c("plot","richness")]
Eendrichness <- endrichness[endrichness$plot == 'E',]
Cendrichness <- endrichness[endrichness$plot == 'C',]
Gendrichness <- endrichness[endrichness$plot == 'G',]

enddiversity <- end[,c("plot","compdiversity")]
Eenddiversity <- enddiversity[enddiversity$plot == 'E',]
Cenddiversity <- enddiversity[enddiversity$plot == 'C',]
Genddiversity <- enddiversity[enddiversity$plot == 'G',]

enddominance <- end[,c("plot","dominance")]
Eenddominance <- enddominance[enddominance$plot == 'E',]
Cenddominance <- enddominance[enddominance$plot == 'C',]
Genddominance <- enddominance[enddominance$plot == 'G',]

endevenness <- end[,c("plot","evenness")]
Eendevenness <- endevenness[endevenness$plot == 'E',]
Cendevenness <- endevenness[endevenness$plot == 'C',]
Gendevenness <- endevenness[endevenness$plot == 'G',]

#####GLMs#####
#go to other files

#######Variation within plots#######
#isolate species
fimbristylis <- cbind(mydata[,c(1:4)], mydata[,"f_cymosa"]) 
fimbristylis[,1] <-(as.Date(fimbristylis[,1], format="%m/%d/%Y"))
polypremum <- cbind(mydata[,c(1:4)], mydata[,"p_procumbens"])
polypremum[,1] <-(as.Date(polypremum[,1], format="%m/%d/%Y"))
waltheria <- cbind(mydata[,c(1:4)], mydata[,"w_indica"])
waltheria[,1] <-(as.Date(waltheria[,1], format="%m/%d/%Y"))
heliotropum <- cbind(mydata[,c(1:4)], mydata[,"h_polyphyllum"])
heliotropum[,1] <-(as.Date(heliotropum[,1], format="%m/%d/%Y"))
po_set <- cbind(mydata[,c(1:4)], mydata[,"po_setaceum"])
po_set[,1] <-(as.Date(po_set[,1], format="%m/%d/%Y"))
eustachys <- cbind(mydata[,c(1:4)], mydata[,"e_neglecta"])
eustachys[,1] <-(as.Date(eustachys[,1], format="%m/%d/%Y"))
cyperus <- cbind(mydata[,c(1:4)], mydata[,"cyperus"])
cyperus[,1] <-(as.Date(cyperus[,1], format="%m/%d/%Y"))
stachytar <- cbind(mydata[,c(1:4)], mydata[,"s_jamaicensis"])
stachytar[,1] <-(as.Date(stachytar[,1], format="%m/%d/%Y"))
phyllanthus <- cbind(mydata[,c(1:4)], mydata[,"ph_abnormis"])
phyllanthus[,1] <-(as.Date(phyllanthus[,1], format="%m/%d/%Y"))
mecardonia <- cbind(mydata[,c(1:4)], mydata[,"m_acuminata"])
mecardonia[,1] <-(as.Date(mecardonia[,1], format="%m/%d/%Y"))
catharanthus <- cbind(mydata[,c(1:4)], mydata[,"c_roseus"])
catharanthus[,1] <-(as.Date(catharanthus[,1], format="%m/%d/%Y"))
oenothera <- cbind(mydata[,c(1:4)], mydata[,"o_simulans"])
oenothera[,1] <-(as.Date(oenothera[,1], format="%m/%d/%Y"))
ernodia <- cbind(mydata[,c(1:4)], mydata[,"e_littoralis"])
ernodia[,1] <-(as.Date(ernodia[,1], format="%m/%d/%Y"))
eragostris <- cbind(mydata[,c(1:4)], mydata[,"eragostris"])
eragostris[,1] <-(as.Date(eragostris[,1], format="%m/%d/%Y"))

#species through time not avged
fimb <- speciesplots(fimbristylis, "fimbristylis")
poly <- speciesplots(polypremum, "polypremum")
walt <- speciesplots(waltheria, "waltheria")
helio <- speciesplots(heliotropum, "heliotropum")
poset <- speciesplots(po_set, "po_set")
eust <- speciesplots(eustachys, "eustachys")
cyp <- speciesplots(cyperus, "cyperus")
stach <- speciesplots(stachytar, "stachytar")
phyll <- speciesplots(phyllanthus, "phyllanthus")
mecar <- speciesplots(mecardonia, "mecardonia")
cathar <- speciesplots(catharanthus, "catharanthus")
oeno <- speciesplots(oenothera, "oenothera")
ern <- speciesplots(ernodia, "ernodia")
erag <- speciesplots(eragostris, "eragostris")

fimb
poly
walt
helio
poset
eust
cyp
stach
phyll
mecar
cathar
oeno
ern
erag

#species counts through time, averaged
march15all <- subset(mydata, date == "3/1/2015")

march17all <- subset(mydata, date == "3/1/2017")

firstlast <- rbind(march15all,march17all)

f_cymosa <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                    mean = mean(f_cymosa),
                    se = sd(f_cymosa)/sqrt(length(f_cymosa)))
colnames(f_cymosa) <- c("date", "plot","mean","se")

p_procumbens <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(p_procumbens),
                  se = sd(p_procumbens)/sqrt(length(p_procumbens)))
colnames(p_procumbens) <- c("date", "plot","mean","se")

w_indica <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                      mean = mean(w_indica),
                      se = sd(w_indica)/sqrt(length(w_indica)))
colnames(w_indica) <- c("date", "plot","mean","se")

h_polyphyllum <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(h_polyphyllum),
                  se = sd(h_polyphyllum)/sqrt(length(h_polyphyllum)))
colnames(h_polyphyllum) <- c("date", "plot","mean","se")

po_setaceum <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                       mean = mean(po_setaceum),
                       se = sd(po_setaceum)/sqrt(length(po_setaceum)))
colnames(po_setaceum) <- c("date", "plot","mean","se")

e_neglecta <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                     mean = mean(e_neglecta),
                     se = sd(e_neglecta)/sqrt(length(e_neglecta)))
colnames(e_neglecta) <- c("date", "plot","mean","se")

cyperus <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                    mean = mean(cyperus),
                    se = sd(cyperus)/sqrt(length(cyperus)))
colnames(cyperus) <- c("date", "plot","mean","se")

s_jamaicensis <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                 mean = mean(s_jamaicensis),
                 se = sd(s_jamaicensis)/sqrt(length(s_jamaicensis)))
colnames(s_jamaicensis) <- c("date", "plot","mean","se")

ph_abnormis <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                       mean = mean(ph_abnormis),
                       se = sd(ph_abnormis)/sqrt(length(ph_abnormis)))
colnames(ph_abnormis) <- c("date", "plot","mean","se")

m_acuminata <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                     mean = mean(m_acuminata),
                     se = sd(m_acuminata)/sqrt(length(m_acuminata)))
colnames(m_acuminata) <- c("date", "plot","mean","se")

c_roseus <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                     mean = mean(c_roseus),
                     se = sd(c_roseus)/sqrt(length(c_roseus)))
colnames(c_roseus) <- c("date", "plot","mean","se")

o_simulans <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(o_simulans),
                  se = sd(o_simulans)/sqrt(length(o_simulans)))
colnames(o_simulans) <- c("date", "plot","mean","se")

e_littoralis <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                    mean = mean(e_littoralis),
                    se = sd(e_littoralis)/sqrt(length(e_littoralis)))
colnames(e_littoralis) <- c("date", "plot","mean","se")

eragostris <- ddply(mydata, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                      mean = mean(eragostris),
                      se = sd(eragostris)/sqrt(length(eragostris)))
colnames(eragostris) <- c("date", "plot","mean","se")

#through time
#multiplot(perspeciesplot(f_cymosa, "Fimbristylis cymosa"),    
#          perspeciesplot(p_procumbens, "Polypremum procumbens"),
#          perspeciesplot(w_indica, "Waltheria indica"),
#          perspeciesplot(h_polyphyllum, "Heliotropium polyphyllum"),
#          perspeciesplot(po_setaceum, "Paspalum setaceum"),
#          perspeciesplot(e_neglecta, "Eustachys neglecta"), cols = 2)
          
#multiplot(perspeciesplot(cyperus, "Cyperus ligularis"),
#          perspeciesplot(s_jamaicensis, "Stachytarpheta jamaicensis"),
#          perspeciesplot(ph_abnormis, "Phyllanthus abnormis"),
#          perspeciesplot(m_acuminata, "Mecardonia acuminata"),
#          perspeciesplot(c_roseus, "Catharanthus roseus"),
#          perspeciesplot(o_simulans, "Oenothera simulans"),
#          perspeciesplot(eragostris, "Eragrostis ciliaris"), cols = 2)

#plots for first and last
f_cymosa <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(f_cymosa),
                  se = sd(f_cymosa)/sqrt(length(f_cymosa)))
colnames(f_cymosa) <- c("date", "plot","mean","se")

p_procumbens <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                      mean = mean(p_procumbens),
                      se = sd(p_procumbens)/sqrt(length(p_procumbens)))
colnames(p_procumbens) <- c("date", "plot","mean","se")

w_indica <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(w_indica),
                  se = sd(w_indica)/sqrt(length(w_indica)))
colnames(w_indica) <- c("date", "plot","mean","se")

h_polyphyllum <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                       mean = mean(h_polyphyllum),
                       se = sd(h_polyphyllum)/sqrt(length(h_polyphyllum)))
colnames(h_polyphyllum) <- c("date", "plot","mean","se")

po_setaceum <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                     mean = mean(po_setaceum),
                     se = sd(po_setaceum)/sqrt(length(po_setaceum)))
colnames(po_setaceum) <- c("date", "plot","mean","se")

e_neglecta <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                    mean = mean(e_neglecta),
                    se = sd(e_neglecta)/sqrt(length(e_neglecta)))
colnames(e_neglecta) <- c("date", "plot","mean","se")

cyperus <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                 mean = mean(cyperus),
                 se = sd(cyperus)/sqrt(length(cyperus)))
colnames(cyperus) <- c("date", "plot","mean","se")

s_jamaicensis <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                       mean = mean(s_jamaicensis),
                       se = sd(s_jamaicensis)/sqrt(length(s_jamaicensis)))
colnames(s_jamaicensis) <- c("date", "plot","mean","se")

ph_abnormis <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                     mean = mean(ph_abnormis),
                     se = sd(ph_abnormis)/sqrt(length(ph_abnormis)))
colnames(ph_abnormis) <- c("date", "plot","mean","se")

m_acuminata <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                     mean = mean(m_acuminata),
                     se = sd(m_acuminata)/sqrt(length(m_acuminata)))
colnames(m_acuminata) <- c("date", "plot","mean","se")

c_roseus <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(c_roseus),
                  se = sd(c_roseus)/sqrt(length(c_roseus)))
colnames(c_roseus) <- c("date", "plot","mean","se")

o_simulans <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                    mean = mean(o_simulans),
                    se = sd(o_simulans)/sqrt(length(o_simulans)))
colnames(o_simulans) <- c("date", "plot","mean","se")

e_littoralis <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                      mean = mean(e_littoralis),
                      se = sd(e_littoralis)/sqrt(length(e_littoralis)))
colnames(e_littoralis) <- c("date", "plot","mean","se")

eragostris <- ddply(firstlast, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                    mean = mean(eragostris),
                    se = sd(eragostris)/sqrt(length(eragostris)))
colnames(eragostris) <- c("date", "plot","mean","se")

multiplot(perspeciesplot(f_cymosa, "Fimbristylis cymosa"),    
          perspeciesplot(h_polyphyllum, "Heliotropium polyphyllum"),
          perspeciesplot(po_setaceum, "Paspalum setaceum"),
          perspeciesplot(w_indica, "Waltheria indica"),
          perspeciesplot(s_jamaicensis, "Stachytarpheta jamaicensis"),
          perspeciesplot(c_roseus, "Catharanthus roseus"),
          perspeciesplot(eragostris, "Eragrostis ciliaris"),
          perspeciesplot(o_simulans, "Oenothera simulans"), cols = 2)

multiplot(perspeciesplot(cyperus, "Cyperus ligularis"),
          perspeciesplot(p_procumbens, "Polypremum procumbens"),
          perspeciesplot(ph_abnormis, "Phyllanthus abnormis"),
          perspeciesplot(m_acuminata, "Mecardonia acuminata"), cols = 2)


#Jaccard
matrix <- mydata[mydata$date == '3/1/2015',-c(1:4)]
jaccard <- vegdist(matrix, method = "jaccard", binary = T, diag = T)
attr(jaccard, "Labels") <- mydata[mydata$date == '3/1/2015',4]
jaccard
m <- as.matrix(jaccard)
jaccardlist <- melt(m)[melt(upper.tri(m))$value,]
names(jaccardlist) <- c("c1", "c2", "distance")
unique(jaccardlist)
jaccardlist[order(-jaccardlist$distance),]

#species per number individuals
diversitydf
ggplot(march15div, aes(x=numberindividuals, y=richness)) + 
  geom_point() +
  ggtitle("Number of individuals vs richness: START") +
  geom_smooth(method = 'glm')

m1 <- glm(richness ~ plot * numberindividuals, data = march15div, family = poisson)
summary(m1)

m4 <- update(m1, ~ . - plot:numberindividuals)
summary(m4)
anova(m1, m4, test="Chi")

AIC(m1); AIC(m4)
AIC(m1)-AIC(m4)

m5 <- update(m4, ~ . - plot)
anova(m4,m5, test = "Chi")
AIC(m4); AIC(m5)
AIC(m4)-AIC(m5)

Anova(m5)

ggplot(march17div, aes(x=numberindividuals, y=richness)) + 
  geom_point() +
  ggtitle("Number of individuals vs Richness: END") +
  geom_smooth(method = 'glm')

m1 <- glm(richness ~ plot * numberindividuals, data = march17div, family = poisson)
summary(m1)

m4 <- update(m1, ~ . - plot:numberindividuals)
summary(m4)
anova(m1, m4, test="Chi")

Anova(m4)

AIC(m1); AIC(m4)
AIC(m1)-AIC(m4)

m5 <- update(m4, ~ . - plot)
anova(m4,m5, test = "Chi")
AIC(m4); AIC(m5)
AIC(m4)-AIC(m5)

Anova(m5)

################################
#trends of metrics through time#
################################

douchetest <- function(diversitymeasure, tite) {
  pd <- position_dodge(10) # move them .05 to the left and right
  return(ggplot(diversitymeasure, aes(x=date, y=mean, colour=plot)) + 
           geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
           geom_line(position=pd) +
           geom_point(position=pd) +
           ggtitle(tite)) + 
           theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8), legend.position="none") +
           scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months"))
}

levels(diversitydf$plot)[3] <- "C"

numberindividuals <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                           mean = mean(numberindividuals),
                           se = sd(numberindividuals)/sqrt(length(numberindividuals)))
colnames(numberindividuals) <- c("date", "plot","mean","se")

richness <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(richness),
                  se = sd(richness)/sqrt(length(richness)))
colnames(richness) <- c("date", "plot","mean","se")

compdiversity <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                       mean = mean(compdiversity),
                       se = sd(compdiversity)/sqrt(length(compdiversity)))
colnames(compdiversity) <- c("date", "plot","mean","se")

dominance <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                   mean = mean(dominance),
                   se = sd(dominance)/sqrt(length(dominance)))

evenness <- ddply(diversitydf, .(as.Date(date, format="%m/%d/%Y"), plot), summarize,
                  mean = mean(evenness),
                  se = sd(evenness)/sqrt(length(evenness)))
colnames(evenness) <- c("date", "plot","mean","se")

pd <- position_dodge(10)

ni <- ggplot(numberindividuals, aes(x=date, y=mean, colour=plot)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ggtitle("Number of Individuals") +
  scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months")) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8), legend.position="none")
ni

rn <- ggplot(richness, aes(x=date, y=mean, colour=plot)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ggtitle("Richness") +
  scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months")) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8), legend.position="none")
rn

cd <- ggplot(compdiversity, aes(x=date, y=mean, colour=plot)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ggtitle("Diversity") +
  scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months")) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8), legend.position="none")
cd

dn <- ggplot(dominance, aes(x=date, y=mean, colour=plot)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ggtitle("Dominance") +
  scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months")) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8), legend.position="none")
dn

en <- ggplot(evenness, aes(x=date, y=mean, colour=plot)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ggtitle("Evenness") +
  scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months")) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8), legend.position="none")
en

legender <- ggplot(dominance, aes(x=date, y=mean, colour=plot)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ggtitle("Dominance") +
  scale_x_date(breaks = seq(as.Date("2015-03-01"), as.Date("2017-03-01"), by="3 months")) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8))
legender

multiplot(ni,rn,cd,dn,en,legender, cols = 2)

####fimbristylis maths#####
fimbristylis <- fimbristylis[fimbristylis$date == "2017-03-01",]
fimbristylis <- fimbristylis[,-c(1,2)]
fimbristylis <- fimbristylis[,-c(2)]
levels(fimbristylis$plot)[3] <- "C"
colnames(fimbristylis) <- c("plot","count")

model.fimbri.poisson <- glm(count ~ plot, poisson, data = fimbristylis)
summary(model.fimbri.poisson) #overdispersed

model.fimbri.quasipoisson <- glm(count ~ plot, poisson, data = fimbristylis)
summary(model.fimbri.quasipoisson) #overdispersed

model.fimbri.nb <- glm.nb(count ~ plot, data = fimbristylis)
summary(model.fimbri.nb) #lowest AIC

Anova(model.fimbri.nb)

