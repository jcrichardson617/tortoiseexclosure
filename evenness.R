##########
#Evenness#
##########
#check beginning
(bigC <- subset(startevenness, startevenness$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(evenness),
                   se = sd(evenness)/sqrt(length(evenness)))

model.sen.beta.bigC <- betareg(evenness ~ plot, data = bigC)
summary(model.sen.beta.bigC)

Anova(model.sen.beta.bigC) #no difference between C and G

levels(startevenness$plot)[3] <- "C"

model.sen.beta <- betareg(evenness ~ plot, data = startevenness)
summary(model.sen.beta)

Anova(model.sen.beta) #no differences in beginning

#end evenness
endevenness
(bigC <- subset(endevenness, endevenness$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(evenness),
                   se = sd(evenness)/sqrt(length(evenness)))

model.een.beta <- betareg(evenness ~ plot, data = bigC)
summary(model.een.beta)

Anova(model.een.beta) #no difference C and G at end

end_en <- cbind(endevenness, endevenness$evenness)
colnames(end_en)[3] <- "pre"
levels(end_en$plot)[3] <- "C"
end_en

model.edv.gaussian <- betareg(evenness ~ plot, data = end_en)
summary(model.edv.gaussian)
Anova(model.edv.gaussian)

#E start vs end
levels(Estartevenness$plot)[2] <-"start"
levels(Eendevenness$plot)[2] <- "end"
Eevenness <- rbind(Estartevenness,Eendevenness)
colnames(Eevenness)[1] <- "date"

model.eeven <- betareg(evenness ~ date, data = Eevenness)
summary(model.eeven)
Anova(model.eeven)

#C start vs end
levels(Cstartevenness$plot)[1] <-"start"
levels(Cendevenness$plot)[1] <- "end"
levels(Gstartevenness$plot)[3] <-"start"
levels(Gendevenness$plot)[3] <- "end"
Cevenness <- rbind(Cstartevenness,Cendevenness,Gstartevenness,Gendevenness)
colnames(Cevenness)[1] <- "date"

model.ceven <- betareg(evenness ~ date, data = Cevenness)
#plot(model.cdiver.gaussian)
summary(model.ceven)
Anova(model.ceven)
