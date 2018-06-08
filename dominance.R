###########
#Dominance#
###########
#check beginning

(bigC <- subset(startdominance, startdominance$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(dominance),
                   se = sd(dominance)/sqrt(length(dominance)))

model.sdm.beta.bigC <- betareg(dominance ~ plot, data = bigC)
summary(model.sdm.beta.bigC)

Anova(model.sdm.beta.bigC) #no difference between C and G

levels(startdominance$plot)[3] <- "C"

model.sdm.beta <- betareg(dominance ~ plot, data = startdominance)
summary(model.sdm.beta)

Anova(model.sdm.beta) #no differences in beginning

#end dominance
enddominance
(bigC <- subset(enddominance, enddominance$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(dominance),
                   se = sd(dominance)/sqrt(length(dominance)))

model.edm.beta <- betareg(dominance ~ plot, data = bigC)
summary(model.edm.beta)

Anova(model.edm.beta) #no difference C and G at end

end_dm <- cbind(enddominance, enddominance$dominance)
colnames(end_dm)[3] <- "pre"
levels(end_dm$plot)[3] <- "C"
end_dm

model.edv.gaussian <- betareg(dominance ~ plot, data = end_dm)
summary(model.edv.gaussian)
Anova(model.edv.gaussian)

#E start vs end
levels(Estartdominance$plot)[2] <-"start"
levels(Eenddominance$plot)[2] <- "end"
Edominance <- rbind(Estartdominance,Eenddominance)
colnames(Edominance)[1] <- "date"

model.edom <- betareg(dominance ~ date, data = Edominance)
summary(model.edom)
Anova(model.edom)

#C start vs end
levels(Cstartdominance$plot)[1] <-"start"
levels(Cenddominance$plot)[1] <- "end"
levels(Gstartdominance$plot)[3] <-"start"
levels(Genddominance$plot)[3] <- "end"
Cdominance <- rbind(Cstartdominance,Cenddominance,Gstartdominance,Genddominance)
colnames(Cdominance)[1] <- "date"

model.cdom <- betareg(dominance ~ date, data = Cdominance)
#plot(model.cdiver.gaussian)
summary(model.cdom)
Anova(model.cdom)
