
library(readr)
library(stats)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggthemes)
library(corrplot)
library(tidyverse)

setwd("C:/Users/oriol.gallemi/Desktop/R datascience/projectWB")

#data taken from https://data.worldbank.org/indicator/NY.GNP.PCAP.CD    fitxer mundial per capita (escollit) 
## NOTA : per fer-ho ben fet hauria de ser una nested tibble amb 
#  comuns els COUNTRY NAME i YEAR i 2 valors per casella (income i population)

dfFullIncome<-read_csv("API_NY.GNP.PCAP.CD_DS2_en_csv_v2_1218027.csv", col_names = T, guess_max = 64, na=c("","NA"), quoted_na=T, quote =  "\"",
                 skip = 4, progress=show_progress(), skip_empty_rows = T)
str(dfFullIncome)
view(dfFullIncome)
summary(dfFullIncome)

# vars <- ls() #neteja de mem?ria .... RESET
# vars <- vars[vars!="dfFull"]
# rm(list=vars)
# rm(vars)

dfFullPop<-read_csv("API_SP.POP.TOTL_DS2_en_csv_v2_1217749.csv", col_names = T, guess_max = 66, na=c("","NA"), quoted_na=T, quote =  "\"",
                       skip = 4, progress=show_progress(), skip_empty_rows = T)
str(dfFullPop)
view(dfFullPop)
summary(dfFullPop)

## ALERTA que hi ha files que no son paisos, sino REGIONS economiques, CAL ELIMINAR-LOS per al processat. Estan a les mateixes files als dos Tibbles

title = 'Lorenz curve of AVG household income / world 1962-2005'
label.threshold = 38000000

prepare.data <- function(Income, Population, Year) { ### segons any escollit busca els vectors a income i poblacio
  c(Country_name=select(Income,(1)) ,Yearincome=Income[,which(colnames(Income)==Year)], Yearpop=Population[, which(colnames(Population)==Year)])
}

printable<-prepare.data(dfFullIncome, dfFullPop, 1997) ####ALERTA que algunes vars no son numeriques !!!! cal transformar
printable<-as_tibble(printable)   ### RECICLATGE DE LA VARIABLE PRINTABLE
colnames(printable)<-c("CountryName", "AvgIncome", "AvgPop" )
# format(printable$AvgIncome, decimal.mark=".")    # DUBTO de si ja ho tinc en numeric o què passa, en principi és inocu
# printable$AvgIncome <- as.numeric(as.character(printable$AvgIncome))
# printable$AvgPop <- as.numeric(as.character(printable$AvgPop))

# elimino files inutils (agrupacions de regions, paradisos fiscals remenuts)
######### S'haurien d'eliminar també els NA de POBLACIO per si de cas ...
printable<-printable[-c(6, 7, 35, 60, 61, 62, 63, 64, 67, 72, 73, 94, 97, 101, 102, 103, 104, 106, 109, 127, 133, 134, 135, 138, 139, 141, 152, 155, 160, 169, 180,
                        182, 190, 195, 196, 197, 203, 214, 216, 217, 229, 230, 235, 237, 239, 240, 248, 258),]
str(printable)
view(printable)
summary(printable) # unsorted printable (bar chart), printables= SORTED printable

printables <- printable[order(printable$AvgIncome, na.last = NA),]  ## els [] son els apuntadors a files i order retorna apuntadors
# afegeixo noves columnes acumulatives
printables

printables$GDP <- printables$AvgIncome*printables$AvgPop
#printables[nrow(printables),] = c(NA)
printables$GDP

# gdp = printables$GDP[1:(nrow(printables)-1)] ### retallo darrer valor
gdp = printables$GDP[1:(nrow(printables)-1)]
gdp
# pop = as.numeric(printables$AvgPop[1:(nrow(printables)-1)]) ## retallo darrer valor
pop = as.numeric(printables$AvgPop[1:(nrow(printables)-1)])
pop
############## ES NORMALITZA A PERCENTATGES , per eliminar inflacions ##########
printables$gdp.perc <- c(0, cumsum(gdp) / sum(gdp))
printables$pop.perc <- c(0, cumsum(pop) / sum(pop))

# str(printables)
# view(printables)
# summary(printables) #### vector sorted ready to plot
pairs(printables[,-c(1)])


a = ggplot() + 
  scale_y_continuous(name = "cumulative share of annual GDP", limits=c(0, 1.05), breaks=seq(0, 1, by=0.1)) +
  scale_x_continuous(name = "cumulative share of population", breaks=seq(0, 1, by=0.1)) +
  ggtitle(title)    

colors = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

for (i in seq(1, nrow(printables))) {
  #polygons:
  d=data.frame(x=c(printables$pop.perc[i],printables$pop.perc[i],printables$pop.perc[i+1],printables$pop.perc[i+1]), 
               y=c(0,                printables$gdp.perc[i],printables$gdp.perc[i+1],0))
  color=colors[1 + i %% length(colors)]
  a = a + geom_polygon(data=d, mapping=aes(x=x, y=y), fill=color)
  
  #annotations:
  if (i < nrow(printables) && printables$AvgPop[i] > label.threshold) {
    x = (printables$pop.perc[i] + printables$pop.perc[i+1])/2
    y = (printables$gdp.perc[i] + printables$gdp.perc[i+1])/2 + 0.03
    a = a + annotate("text", x = x, y = y, label = printables$CountryName[i], color=color, lineheight=0.5, angle=90, hjust=0)
  }
}
a
# Correlograms, bastant caca
# corrplot(cor(printables[,-c(1)]),order = "hclust",addrect = 3,
#          mar = c(2, 1, 1, 1), cl.length = 5)
# ggcorr(printables[,-c(1)])
# ggcorr(printables[,-c(1)], geom="circle", nbreaks = 8, palette = "PuOr")
# 
# 
# # Scatter plot matrix ***************ELS MILLORS ##################
# pairs(printables[,-c(1)])
# 
# ggpairs(printables[,-c(1)], axisLabels = "none") + 
#   theme_few()
