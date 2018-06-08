##########
#Richness#
##########
#check beginning
(bigC <- subset(startrichness, startrichness$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(richness),
                   se = sd(richness)/sqrt(length(richness)))

model.srn.poisson.bigC <- glm(richness ~ plot, poisson, data = bigC)
summary(model.srn.poisson.bigC) #notoverdispersed, low AIC

Anova(model.srn.poisson.bigC) #no difference between start C and G

levels(startrichness$plot)[3] <- "C"

model.srn.poisson <- glm(richness ~ plot, poisson, data = startrichness)
summary(model.srn.poisson) #AIC 51.595

Anova(model.srn.poisson) #no differences in beginning

#end richness
endrichness
(bigC <- subset(endrichness, endrichness$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(richness),
                   se = sd(richness)/sqrt(length(richness)))

model.ern.poisson <- glm(richness ~ plot, poisson, data = bigC)
summary(model.ern.poisson) #AIC 36.212

Anova(model.ern.poisson) #no difference C and G at end

end_rn <- cbind(endrichness, startrichness$richness)
colnames(end_rn)[3] <- "pre"
levels(end_rn$plot)[3] <- "C"
end_rn

model.ern.poisson <- glm(richness ~ plot, poisson, data = end_rn)
summary(model.ern.poisson) #AIC 50.515

Anova(model.ern.poisson) #no diff between E and C end

#E start vs end
levels(Estartrichness$plot)[2] <-"start"
levels(Eendrichness$plot)[2] <- "end"
Erichness <- rbind(Estartrichness,Eendrichness)

model.erich.poisson <- glm(richness ~ plot, poisson, data = Erichness)
summary(model.erich.poisson) #AIC 34.511
Anova(model.erich.poisson) # no diff E start vs end

#C start vs end
levels(Cstartrichness$plot)[1] <-"start"
levels(Cendrichness$plot)[1] <- "end"
levels(Gstartrichness$plot)[3] <-"start"
levels(Gendrichness$plot)[3] <- "end"
Crichness <- rbind(Cstartrichness,Cendrichness,Gstartrichness,Gendrichness)

model.crich.poisson <- glm(richness ~ plot, poisson, data = Crichness)
summary(model.crich.poisson)
Anova(model.crich.poisson) #no diff C start vs end