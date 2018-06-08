#######################
#Number of individuals#
#######################
#check beginning
(bigC <- subset(startnumberindividuals, startnumberindividuals$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                  mean = mean(numberindividuals),
                  se = sd(numberindividuals)/sqrt(length(numberindividuals)))

model.sni.poisson.bigC <- glm(numberindividuals ~ plot, poisson, data = bigC)
summary(model.sni.poisson.bigC) #overdispersed

model.sni.quasipoisson.bigC <- glm(numberindividuals ~ plot, quasipoisson, data = bigC)
summary(model.sni.quasipoisson.bigC) #overdispersed

model.sni.negbinom.bigC <- glm.nb(numberindividuals ~ plot, data = bigC)
summary(model.sni.negbinom.bigC) #lowest AIC

Anova(model.sni.negbinom.bigC) #no difference between C and G in beginning

levels(startnumberindividuals$plot)[3] <- "C"

model.sni.poisson <- glm(numberindividuals ~ plot, poisson, data = startnumberindividuals)
summary(model.sni.poisson) #overdispersed

model.sni.quasipoisson <- glm(numberindividuals ~ plot, quasipoisson, data = startnumberindividuals)
summary(model.sni.quasipoisson) #stilloverdispersed

model.sni.negbinom <- glm.nb(numberindividuals ~ plot, data = startnumberindividuals)
summary(model.sni.negbinom) #good, lowest AIC
#plot(model.sni.negbinom)
Anova(model.sni.negbinom) #no difference between E and Cs in beginning

#end numberindividuals
endnumberindividuals
(bigC <- subset(endnumberindividuals, endnumberindividuals$plot != "E"))

bigCmeans <- ddply(bigC, .(plot), summarize,
                   mean = mean(numberindividuals),
                   se = sd(numberindividuals)/sqrt(length(numberindividuals)))

model.eni.poisson <- glm(numberindividuals ~ plot, poisson, data = bigC)
summary(model.eni.poisson) #overdispersed

model.eni.quasipoisson <- glm(numberindividuals ~ plot, quasipoisson, data = bigC)
summary(model.eni.quasipoisson) #still overdispersed

model.eni.negbinom <- glm.nb(numberindividuals ~ plot, data = bigC)
summary(model.eni.negbinom) #good
#plot(model.eni.negbinom)
Anova(model.eni.negbinom) #no difference between Cs in end

levels(endnumberindividuals$plot)[3] <- "C"

model.eni.poisson <- glm(numberindividuals ~ plot, poisson, data = endnumberindividuals)
summary(model.sni.poisson) #overdispersed

model.eni.quasipoisson <- glm(numberindividuals ~ plot, quasipoisson, data = endnumberindividuals)
summary(model.eni.quasipoisson) #stilloverdispersed

model.eni.negbinom <- glm.nb(numberindividuals ~ plot, data = endnumberindividuals)
summary(model.eni.negbinom) #good, lowest AIC
#plot(model.sni.negbinom)
Anova(model.eni.negbinom) #no difference between E and Cs in end

#E start  vs end
levels(Estartnumberindividuals$plot)[2] <-"start"
levels(Eendnumberindividuals$plot)[2] <- "end"
Enumberind <- rbind(Estartnumberindividuals,Eendnumberindividuals)
colnames(Enumberind)[1] <- "date"

model.enum.poisson <- glm(numberindividuals ~ date, poisson, data = Enumberind)
summary(model.enum.poisson) #overdispersed

model.enum.quasipoisson <- glm(numberindividuals ~ date, quasipoisson, data = Enumberind)
summary(model.enum.quasipoisson) #still overdispersed

model.enum.negbinom <- glm.nb(numberindividuals ~ date, data = Enumberind)
summary(model.enum.negbinom)
Anova(model.enum.negbinom) #no difference between exclosure start v end

#C start vs end
levels(Cstartnumberindividuals$plot)[1] <-"start"
levels(Cendnumberindividuals$plot)[1] <- "end"
levels(Gstartnumberindividuals$plot)[3] <-"start"
levels(Gendnumberindividuals$plot)[3] <- "end"
Cnumberind <- rbind(Cstartnumberindividuals,Cendnumberindividuals,Gstartnumberindividuals,Cendnumberindividuals)
colnames(Cnumberind)[1] <- "date"

model.cnum.poisson <- glm(numberindividuals ~ date, poisson, data = Cnumberind)
summary(model.cnum.poisson) #overdispersed

model.cnum.quasipoisson <- glm(numberindividuals ~ date, quasipoisson, data = Cnumberind)
summary(model.cnum.quasipoisson) #still overdispersed

model.cnum.negbinom <- glm.nb(numberindividuals ~ date, data = Cnumberind)
summary(model.cnum.negbinom)
Anova(model.cnum.negbinom) #no difference between controls start vs end
