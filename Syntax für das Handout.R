# Funktionen für das Handout "Einführung in R"

# Autor: Johann Popp
# Datum:            2015-10-01
# Letzte Änderung:  2016-02-04
###########################################


(24 + 54 + 30 + 19) / 4       # R als Taschenrechner

# Mittelwert Schritt für Schritt basteln
Alter <- c(24, 54, 30, 19)
Alter
ls()
Summe <- sum(Alter)
N <- length(Alter)
Summe / N

# ... und jetzt als Funktion
Mittelwert <- function(x){sum(x) / length(x)}
Mittelwert(Alter)
mean(Alter)

# Hilfe
?mean
mean(c(24, NA, 19))
mean(c(24, NA, 19), na.rm = TRUE)

# Grafik
plot(Alter)
#demo(graphics)
library(lattice)
#demo(lattice)
demo()


######
# Übung

VAS.A <- c(41, 63, 31, 28, 46, 24, 42)
VAS.B <- c(46, 33, 31, 20, 33, 08, 30)
t.test(VAS.A, VAS.B, paired = TRUE)

boxplot(VAS.A, VAS.B)
segments(rep(1,7), VAS.A, rep(2,7), VAS.B, col = "blue", lty = 2)

shapiro.test(VAS.A-VAS.B)
qqnorm(VAS.A-VAS.B)


#########
# Datenformate

x <- ordered(c("nie", "oft", "oft", "selten"), levels = c("nie", "selten", "oft"))
class(x)
x
y <- c(1,3,3,2,3,1)
summary(cbind(x,y)[,1])

groesse <- c(172, 168, 184, 175)
class(groesse)
is.numeric(groesse)

geschlecht <- c('w', 'm', 'm', 'w')
class(geschlecht)
as.numeric(geschlecht)

# data.frame
daten <- data.frame(Alter, groesse, geschlecht, x)
daten

daten2 <- data.frame(Alter = c(24, 54, 30, 19),
                     groesse = c(172, 168, 184, 175),
                     geschlecht = c("w", "m", "m", "w"),
                     x = c("nie", "oft", "oft", "selten"))
daten2




######################
# Daten einlesen

fix(daten2)

# Textdateien

frami <- read.table("/media/poppi/Windows7_OS/Austausch/Lehrauftrag/Sonstige/R-Einstieg/framingham.csv", header = TRUE, sep = ",", dec = ".")
head(frami)
# frami[,c(9,12:13,16:20,23:30)] <- frami[,c(9,12:13,16:20,23:30)] == 1
# summary(frami)
# write.csv(frami[,-1], "framingham.csv", row.names = FALSE)


read.delim("http://studysites.uk.sagepub.com/dsur/study/DSUR%20Data%20Files/Chapter%209/SpiderWide.dat")


# Paket foreign
#install.packages("foreign")
library(foreign)

daten3 <- read.spss("SpiderRM.SAV")
daten3 <- read.spss("/media/poppi/Windows7_OS/Austausch/Lehrauftrag/Sonstige/R-Einstieg/SpiderRM.SAV")
daten3
class(daten3)
spider <- as.data.frame(daten3)
spider

###############
# Daten aufbereiten

# head, str, summary
head(frami)
tail(frami)

str(frami)
summary(frami)
View(frami)

#
# Fehlende Werte

fehlende <- c(1,1,NA,3,4,1,2,NA,6,3)
sum(fehlende)
sum(fehlende, na.rm = TRUE)
table(fehlende)
table(fehlende, useNA = "ifany")
is.na(fehlende)
fehlende[is.na(fehlende)]
fehlende[!is.na(fehlende)]
fehlend2 <- data.frame(var1 = c(3,5,1,999, 2, 999),
                       var2 = c(999, 6, 23, 999, 999,8))
fehlend2
fehlend2[fehlend2 == 999] <- NA
fehlend2

0 / 0
apply(frami, 2, function(x) sum(is.na(x)))
apply(frami, 2, function(x) sum(is.na(x)) / length(x))
naPlot <- function(x){
  image(!is.na(t(x)), xlab = "Variablen", ylab = "statistische Einheiten", main = "Fehlende Werte im Datensatz", xaxt = "n", yaxt = "n")
  axis(1, at = seq(0, 1, length.out = ncol(x)), labels = names(x), cex.axis = 0.5, las = 3)
}
naPlot(frami)
plot(apply(frami, 2, function(x) sum(is.na(x)) / length(x)), ylab = "Anteil Missings pro Variable")

# Variablen anwählen
min(AGE)
min(frami$AGE)
with(frami, min(AGE))

attach(frami)
#min(AGE)
detach(frami)

# Navigieren im Datensatz
zahlen <- c("eins", "zwei", "drei", "vier", "fünf")
zahlen[3]

navi <- data.frame(zahlen, buchst = c("a", "b", "c", "d", "e"))
navi
navi[3,2]

navi[3, ]

navi[-3,]

navi[2:4,]

navi[c(1, 3),]

navi[5:1,2:1]

navi[zahlen == "zwei",]

navi[order(navi$zahlen),]
order(navi$zahlen)
navi[c(3,1,5,4,2),]

# Variablen berechnen
wert1 <- c(1,2,3,4,5)
cbind(navi, wert1)
navi <- cbind(navi, wert1)
navi$wert2 <- c(1, 10, 100, 1000, 10000)
navi
navi$wert1 * navi$wert2
navi$wert1 * 10
#navi$wert1 * c(1, 10)
navi$wert1mal2 <- navi$wert1 * navi$wert2

# umkodieren
navi$wertGr[navi$wert1 < 4] <- "< 4"
navi$wertGr[navi$wert1 >= 4] <- ">= 4"

write.csv2(navi, "navi.csv", row.names = FALSE)

cut(navi$wert1, 2)
cut(navi$wert1, c(0,4,6))
cut(navi$wert1, c(0,4,6), right = FALSE)
cut(navi$wert1, c(0,4,6), labels = c("0-4", ">4"))
cut(navi$wert1, quantile(navi$wert1), include.lowest = TRUE)

# Zeilen zufügen
navi2 <- data.frame(zahlen = "sechs", buchst = "a", wert1 = 6, wert2 = 100000, wert1mal2 = 600000, wertGr = ">= 4")
rbind(navi, navi2)
merge(navi, navi2, all = TRUE, sort = FALSE)


######
# Andere Datensatzfunktionen

# t()
t(navi)

# apply()
#apply(navi,2, sd)
apply(navi[,3:5],2,sd)
VK <- function(x){
  sd(x)/mean(x)
}
apply(navi[3:5], 2, VK)
apply(navi[3:5], 2, function(x){sd(x) / mean(x)})
apply(frami, 1, function(x) sum(is.na(x)))
plot(apply(frami, 1, function(x) sum(is.na(x))))
table(apply(frami, 1, function(x) sum(is.na(x))))
summeDerMissings <- function(x){
  fehlend <- is.na(x)
  sum(fehlend)
}
fehlendProZeile <- apply(frami, 1, summeDerMissings)
plot(fehlendProZeile)
rm(fehlendProZeile, summeDerMissings)
#rm(list = ls())

spider 
reshape(spider, varying = list(1:2), direction = "long", v.names = "VAS", timevar = "Type")


framiWide <- reshape(frami, idvar = "RANDID", timevar = "PERIOD", direction = "wide")
str(framiWide)

###############
# Datums- und Zeitfunktionen

Systemzeit <- Sys.time()
Systemzeit
Systemzeit <- as.POSIXlt(Systemzeit)

Systemzeit$hour
Systemzeit$mday

weekdays(Systemzeit)
months(Systemzeit)
quarters(Systemzeit)

difftime(Sys.time(), Systemzeit)
difftime(Sys.time(), Systemzeit, units = "days")

datum <- c("23.3.2004", "2.10.2015")
strptime(datum, format = "%d.%m.%Y")

####
# Textfunktionen

grep("e", navi$zahlen)
grep("e", navi$zahlen, value = TRUE)
grepl("e", navi$zahlen)
grep("^e", navi$zahlen, value = TRUE)
?regex
agrep("zwo", navi$zahlen, value = TRUE)

sub("f", "-GNRZ-", navi$zahlen)
gsub("f", "-GNRZ-", navi$zahlen)

paste("Die Zahl", navi$wert1, "ist", navi$wertGr)
paste("Die Zahl", navi$wert1, "ist", navi$wertGr, sep = "-")
paste("Die Zahl", navi$wert1, "ist", navi$wertGr, collapse  = " & ")

####################
# Übung

who <- read.csv("http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/WHOSIS_000004&profile=crosstable&filter=COUNTRY:*;REGION:*;SEX:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO;SEX", skip = 1)
str(who)
summary(who)
who[who$Country == "Germany",]
summary(who[who$Country == "Germany",])
whoWide <- reshape(who, timevar = "Year", idvar = "Country", direction = "wide")
head(whoWide)
names(whoWide)
whoWide$meanMortBoth <- apply(whoWide[c(5, 8, 11)], 1, mean)
whoWide[whoWide$meanMortBoth == min(whoWide$meanMortBoth),c(1,14)]
whoWide[whoWide$meanMortBoth == max(whoWide$meanMortBoth),c(1,14)]
rank(whoWide$meanMortBoth)[whoWide$Country == "Germany"]


###########################
# Deskriptive Statistiken

summary(frami$TOTCHOL)
summary(frami$SEX)
summary(frami$CURSMOKE)

# Häufigkeiten
table(frami$BPMEDS)
table(frami$BPMEDS, useNA = "always")
table(frami$SEX, frami$BPMEDS)
table(Geschlecht = frami$SEX, Blutdruckmedikamente = frami$BPMEDS)
table(Geschlecht = frami$SEX, Blutdruckmedikamente = frami$BPMEDS, Raucher = frami$CURSMOKE)

prop.table(table(frami$BPMEDS))
prop.table(table(frami$BPMEDS, frami$SEX), 1)
addmargins(prop.table(table(frami$BPMEDS, frami$SEX), 1))

table(spider$REAL)
cumsum(table(spider$REAL))
cumsum(prop.table(table(spider$REAL)))
prop.table(cumsum(table(spider$REAL)))

# Lagemaße
mean(frami$TOTCHOL)
median(frami$TOTCHOL)
mean(frami$TOTCHOL, na.rm = TRUE)
median(frami$TOTCHOL, na.rm = TRUE)
Modus <- function(x){
  names(table(x))[table(x) == max(table(x))]
}
Modus(frami$TOTCHOL)

# Streuungsmaße
min(frami$TOTCHOL, na.rm = TRUE)
max(frami$TOTCHOL, na.rm = TRUE)
var(frami$TOTCHOL, na.rm = TRUE)
sd(frami$TOTCHOL, na.rm = TRUE)
quantile(frami$TOTCHOL, na.rm = TRUE)
quantile(frami$TOTCHOL, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

seq(0,1,0.1)
seq(0,1, length.out = 11)

# Zwischenübung
quantile(frami$TOTCHOL, probs = seq(0,1,0.05), na.rm = TRUE)
EmpVar <- function(x, na.rm = FALSE){
  if(na.rm == TRUE){x <- x[!is.na(x)]}
  sum((mean(x)-x)^2) / length(x)
}
EmpVar(frami$TOTCHOL, na.rm = TRUE)

# tapply
tapply(frami$AGE, frami$SEX, mean)
tapply(frami$TOTCHOL, frami$SEX, function(x) mean(x, na.rm = TRUE))


#################
# Grafik - base

# Objektorientierte plots
layout(matrix(c(1,2,3,4,4,4), nrow = 2, byrow = TRUE))
plot(frami$SEX)
plot(who$X.Female)
plot(who$X.Female, who$X.Male)
plot(who)

# pch = 
plot(1:20, rep(1, 20), pch = 1:20, ylim = c(0.9, 1.2), xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", main = "pch =: Welche Zahl erzeugt welches Symbol?")
text(1:20, rep(1, 20) + 0.1, labels = 1:20, adj = c(0.5, 0), cex = 0.8)

plot(spider$PICTURE, spider$REAL, pch = "#")

# Farbauswahl
plot(1:8, rep(1, 8), col = cm.colors(8), pch = c("1", "2", "3", "4", "5", "6", "7", "8"), xlab = "", xaxt = "n", ylab = "", yaxt = "n", xlim = c(-2,8.5), main = "Farbkodes als Wörter, Zahlen und im RGB-Format")
text(1:8, rep(1.2, 8), col = c("black", "red", "green", "blue", "cyan", "magenta", "yellow", "grey"), labels = c("black", "red", "green", "blue", "cyan", "magenta", "yellow", "grey"), srt = 70)
text(1:8, rep(0.8, 8), col = c("#000000", "#FF0000", "#00FF00", "#0000FF", "#00FFFF", "#FF00FF", "#FFFF00", "#D0D0D0"), labels = c("#000000", "#FF0000", "#00FF00", "#0000FF", "#00FFFF", "#FF00FF", "#FFFF00", "#D0D0D0"), srt = 70)
text(rep(-2,3), c(1.2, 1, 0.8), labels = c("Wort-Kode:", "Zahlenkode:", "RGB-Kode:"), adj = c(0, 0.5))

# Farbpaletten
plot(1:8, rep(0.9, 8), col = rainbow(8), pch = 19, cex = 2, xlim = c(-3,8), ylim = c(-0.2, 1), xaxt = "n", xlab = "", yaxt = "n", ylab = "")
points(1:8, rep(0.8, 8), col = terrain.colors(8), pch = 19, cex = 2)
points(1:8, rep(0.7, 8), col = heat.colors(8), pch = 19, cex = 2)
points(1:8, rep(0.6, 8), col = topo.colors(8), pch = 19, cex = 2)
points(1:8, rep(0.5, 8), col = cm.colors(8), pch = 19, cex = 2)
points(1:8, rep(0.3, 8), col = grey(1:8 / 8), pch = 19, cex = 2)
library(RColorBrewer)
points(1:8, rep(0.1, 8), col = brewer.pal(8, "Purples"), pch = 19, cex = 2)
points(1:8, rep(0, 8), col = brewer.pal(8, "RdBu"), pch = 19, cex = 2)
points(1:8, rep(-0.1, 8), col = brewer.pal(8, "Dark2"), pch = 19, cex = 2)
text(-3, seq(0.9, -0.1, -0.1), labels = c("rainbow(8)", "terrain.colors(8)", "heat.colors(8)", "topo.colors(8)", "cm.colors(8)","",  "grey(1:8 / 8)", "", "RColorBrewer:", "brewer.pal(8, '...')", ""), adj = c(0, 0.5 ))

# Transparenz
plot.new()
text(0.5, 0.5, "Transparenz", font = 2, cex = 5.5, col = "grey")
text(seq(0.02, 0.98, length.out = 9), rep(0.5, 9), labels = paste(1:9 / 10), col = heat.colors(1, alpha = 1:9 / 10), cex = 2, font = 2)


# Linientepen
plot(c(0.05, 1), c(1,1), xlim = c(0,1), ylim = c(-0.05,1.05), main = "Linientypen", type = "l", lwd = 3, xaxt = "n", xlab = "", yaxt = "n", ylab = "")
segments(rep(0.05, 6), seq(1, 0, length.out = 6), rep(1, 6), lty = 1:6, cex = 3, lwd = 3)
text(rep(0, 6), seq(1, 0, length.out = 6), labels = 1:6)

# Plot-Statistiken abrufen
agePlot <- boxplot(frami$AGE)
agePlot
text(rep(1.22, 5), agePlot$stats, labels = agePlot$stats, adj = c(0, 0.5))

#################
# ggplot2

library(ggplot2)

qplot(TOTCHOL, data = frami)

agePlot <- ggplot(frami, aes(AGE))
agePlot + geom_histogram()
agePlot + geom_histogram(aes(fill = ANGINA))
agePlot + geom_histogram(aes(fill = ANGINA), position = "fill")
agePlot + geom_histogram(aes(fill = ANGINA), position = "fill") + facet_wrap(~SEX)
agePlot + geom_histogram(aes(fill = ANGINA), position = "fill") + facet_wrap(~SEX) + labs(title = "Altersabhängige Häufigkeit von Angina Pectoris getrennt nach Geschlecht", y = "Anteil") + theme_bw()

######
# Übung

lowbwt <- read.table("http://www.umass.edu/statdata/statdata/data/lowbwt.dat", header = TRUE, skip = 5)
str(lowbwt)
lowbwt$SMOKE <- lowbwt$SMOKE == 1

####################
# Verteilungsfunktionen

Bereich <- seq(0, 8, 0.01) # Wertebereich von 0 bis 8 mit Abständen von 0.01
plot(Bereich, dchisq(Bereich, df = 1), type = "l")


qchisq(0.05, df = 1, lower.tail = FALSE)

pchisq(3.84, df = 1, lower.tail = FALSE)

set.seed(23)
ZufallChi <- rchisq(100, 1)
hist(ZufallChi, freq = FALSE)
lines(Bereich, dchisq(Bereich, df = 1))

#########
# Annahmen

par(mfrow = c(1,2))
hist(lowbwt$BWT, freq = FALSE)
lines(500:5000, dnorm(500:5000, mean = mean(lowbwt$BWT), sd = sd(lowbwt$BWT)))
qqnorm(lowbwt$BWT)

shapiro.test(lowbwt$BWT)
with(lowbwt, ks.test(BWT, "pnorm", mean = mean(BWT), sd = sd(BWT)))

par(mfrow = c(1,1))
boxplot(lowbwt$BWT ~ lowbwt$SMOKE)

library(car)
leveneTest(lowbwt$BWT ~ lowbwt$SMOKE)

########
# tests

t.test(BWT ~ SMOKE, data = lowbwt)
wilcox.test(BWT ~ SMOKE, data = lowbwt)
chisq.test(lowbwt$SMOKE, lowbwt$LOW, correct = FALSE)
fisher.test(lowbwt$LOW, lowbwt$SMOKE)
summary(aov(BWT ~ RACE, data = lowbwt))
kruskal.test(lowbwt$BWT ~ lowbwt$RACE)


############
# Zusammenänge

cor(lowbwt$BWT, lowbwt$LWT, method = "spearman")
cor.test(lowbwt$BWT, lowbwt$LWT)
scatter.smooth(lowbwt$BWT ~ lowbwt$LWT)

library(vcd)
assocstats(table(lowbwt$LOW, lowbwt$SMOKE))

#############
# Epi
table(lowbwt$LOW, lowbwt$SMOKE)

library(epiR)
epi.2by2(table(lowbwt$LOW, lowbwt$SMOKE))

library(epiDisplay)
cs(lowbwt$LOW, lowbwt$SMOKE)
cc(lowbwt$LOW, lowbwt$SMOKE)
mhor(lowbwt$LOW, lowbwt$SMOKE, lowbwt$HT)
