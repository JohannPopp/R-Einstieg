# R-Skript zum Handout Lineare Regression

# Johann Popp
# 2016-02-6
#####################################

# Regressionsmodel erstellen
# lm(AV ~ UV, data = Daten)

rsales <- read.delim("http://studysites.uk.sagepub.com/dsur/study/DSUR%20Data%20Files/Chapter%207/Album%20Sales%202.dat")

summary(rsales)

# Welchen Einfluss haben die Werbeausgabe auf die Plattenverkäufe
plot(rsales$adverts, rsales$sales)

# lineares Modell (univariat)
rs1 <- lm(sales ~ adverts, data = rsales)
rs1

# Regressionslinie im Streudiagramm
abline(rs1)

# Koeffizienten mit SE, T und p-Wert, F-Statistik und R²
summary(rs1)
summary.aov(rs1)

# Inhalte eines lm-Objektes
str(rs1)
rs1$coefficients
rs1[1]

# Konfidenzintervalle
confint(rs1)

# Standardisierte Betas
rsStandard <- lm(scale(sales) ~ scale(adverts), data = rsales)
rsStandard



# multivariates lineares Modell
rs2 <- lm(sales ~ adverts + airplay + attract, data = rsales)
summary(rs2)

# update
rs2Upd <- update(rs1,. ~ . + airplay + attract)


# Interaktionen
rs3 <- lm(sales ~ adverts * airplay * attract, data = rsales)
summary(rs3)

rs4 <- update(rs3, ~ . - adverts:airplay:attract)
summary(rs4)

# Modelle Vergleichen
anova(rs4, rs3)
anova(rs4, rs3, test = "LRT")
AIC(rs4, rs3)

# Modellieren mit drop1
drop1(rs4, test = "Chisq")

# Annahmen prüfen
par(mfrow = c(2,2))
plot(rs2)

# Residuen extrahieren
# unstandardisiert
residuals(rs2)
# standardisiert
rstandard(rs2)
rstudent(rs2)
# influence measures
influence.measures(rs2)


# Linearität prüfen
scatter.smooth(rsales$sales ~ rsales$adverts)
abline(rs1, col = "red")
# multivariate fractional polynomials
library(mfp)
mfp(sales ~ fp(adverts, df = 4) + fp(airplay, df = 4, select = 0.05) + fp(attract, df = 4, select = 0.05), data = rsales)

# Normalverteilung prüfen
shapiro.test(rs2$residuals)

# Varianzhomogenität prüfen
gruppe <-  cut(rs2$fitted.values, breaks = quantile(rs2$fitted.values))
library(car)
leveneTest(rs2$residuals ~ gruppe)

# Multikolinearität
cor(rs2$model)
plot(rs2$model)

library(car)
vif(rs2)
mean(vif(rs2))

# Autokorrelation
library(car)
durbinWatsonTest(rs2)

# ANOVA
lowbwt <- read.table("http://www.umass.edu/statdata/statdata/data/lowbwt.dat", header = TRUE, skip = 5)
lowbwt$RACE <- factor(lowbwt$RACE)
boxplot(lowbwt$BWT ~ lowbwt$RACE)
tapply(lowbwt$BWT, lowbwt$RACE, mean)

lwtAnov1 <- lm(BWT ~ RACE, data = lowbwt)

summary.aov(lwtAnov1)
summary(lwtAnov1)

contrasts(lowbwt$RACE)
contr.treatment(3)
contr.treatment(3, base = 3)
contrasts(lowbwt$RACE) <- contr.treatment(3, base = 3)
lwtAnov2 <- lm(BWT ~ RACE, data = lowbwt)
summary(lwtAnov2)

contr.helmert(3)
contrasts(lowbwt$RACE) <- contr.helmert(levels(lowbwt$RACE))

# post-hoc Tests
pairwise.t.test(lowbwt$BWT, lowbwt$RACE, p.adjust.method = "bonferroni")

# interaction
lwtAnov3 <- update(lwtAnov1, . ~ . * HT)
summary(lwtAnov3)
interaction.plot(lowbwt$RACE, lowbwt$HT, lowbwt$BWT)
tapply(lowbwt$BWT, list(lowbwt$RACE, lowbwt$HT), mean)


# Logistische Regression
lr01 <- glm(LOW ~ SMOKE + RACE + LWT + HT + UI, data = lowbwt, family = "binomial")
summary(lr01)

exp(coef(lr01))
exp(confint(lr01))

library(epiDisplay)
logistic.display(lr01)

# Omnibus Test vs. Null-Model
lrNull <- glm(LOW ~ 1, data = lowbwt, family = "binomial")
anova(lr01, lrNull, test = "LRT")

# Pseudo-R²
library(descr)
LogRegR2(lr01)

# HL-Test
library(MKmisc)
HLgof.test(fitted(lr01), lr01$y)

# Cessie-van Houwelingen-Test
library(rms)
lr02 <- lrm(LOW ~ LWT + RACE + SMOKE + HT + UI, data = lowbwt, y = TRUE, x = TRUE)
residuals(lr02, "gof")

# Klassifikationstabellen
klassTab <- function(x, cutoff = 0.5){
  prob <- predict(x, type = "response")
  ta <- table(vorhergesagt = cut(prob, c(0, cutoff, 1)), beobachtet = x$y) / length(prob)
  correct <- sum(ta[1,1], ta[2,2])
  list(Klassifikationstabelle = round(ta,3), "Anteil korrekter Schätzungen" = round(correct, 3))
}
klassTab(lr01)

# ROC
library(pROC)
plot(roc(lr01$y, predict(lr01, type = "response")))
