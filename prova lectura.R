
library(readr)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/oriol.gallemi/Desktop/R datascience/projectWB")

#data taken from https://data.worldbank.org/indicator/NY.GNP.PCAP.CD    fitxer mundial per capita (escollit) 

dfFullIncome<-read_csv("API_NY.GNP.PCAP.CD_DS2_en_csv_v2_1218027.csv", col_names = T, guess_max = 64, na=c("","NA"), quoted_na=T, quote =  "\"",
                 skip = 4, progress=show_progress(), skip_empty_rows = T)
str(dfFullIncome)
view(dfFullIncome)
summary(dfFullIncome)

# dfFull <- read_lines("API_NY.GNP.PCAP.CD_DS2_en_csv_v2_1218027.csv", )
# str(dfFull)
# view(dfFull)
# dfFull[1:2] ### ensenya un parell de files per veure'n continguts. isualitza un parell de dades per tipus de fitxer
# # Identificador de caracters: ""cometes  separador columnes ,coma
# 
# dfFullList<-str_split(dfFull, sep=",", dec=".", quote = "\"") ### posa en taula tota la variable, separant per les comes i cada col separada
# dfFullList
# FullLength<-sapply(dfFullList, length) ## miro que totes tinguin iguals files!!! CAL FER-HO
# FullLength
# table(FullLength)
# 
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





######################################



### Clean the data ####
# The goal is to make sure that the data matches the definition of the variables
# in the table

# Unify codes for missing values
carsDF[carsDF==""] <- NA
sum(is.na(carsDF))

# Remove duplicated or irrelevant information
table(duplicated(carsDF$id))
table(is.na(carsDF$number))

# Improve data coding
summary(carsDF$category)
table(carsDF$category) 
table(carsDF$category, useNA = "ifany")
carsDF$category <- droplevels(carsDF$category)

table(carsDF$category, useNA = "ifany")
carsDF <- carsDF[!is.na(carsDF$category),]

carsDF[carsDF$category=="N1",c("man","variant","version",
                               "make","comName","number")]

carsDF[!is.na(carsDF$comName) & carsDF$comName=="Sprinter",
       c("man","variant","version",
         "make","comName","number", "category")] ###veure si es correspon el tipus M1 o N1
carsDF[!is.na(carsDF$comName) & carsDF$comName=="Transit",
       c("man","variant","version",
         "make","comName","number", "category")]
carsDF[!is.na(carsDF$comName) & carsDF$comName=="DUCATO",
       c("man","variant","version",
         "make","comName","number", "category")]

carsDF$category[carsDF$category=="N1"] <- "M1"

table(is.na(carsDF$man))
table(carsDF$man, useNA = "ifany")
# OUT OF SCOPE? UNKNOWN?
table(carsDF$man, useNA = "ifany")[c("OUT OF SCOPE","UNKNOWN")]
carsDF[carsDF$man=="OUT OF SCOPE" | carsDF$man=="UNKNOWN",
       c("man","man1", "man2","make")]

# Options for missing or miscoded value
# ... (1) reassign / impute --- PLEASE DOCUMENT ANY CHANGES TO THE DATASET
table(droplevels(carsDF$man[carsDF$man1=="RENAULT SAS"]))
table(droplevels(carsDF$man[carsDF$man1=="FCA US LLC"]))
table(droplevels(carsDF$man[carsDF$man1=="FCA ITALY SPA"]))
table(droplevels(carsDF$man[carsDF$man1=="FORD MOTOR COMPANY"]))
table(droplevels(carsDF$man[carsDF$man1=="AUTOMOBILE DACIA SA"]))
table(droplevels(carsDF$man[carsDF$man1=="MAGYAR SUZUKI CORPORATION LTD"]))

carsDF$man[carsDF$man1=="RENAULT SAS"] <- "RENAULT"
carsDF$man[carsDF$man1=="FCA US LLC"] <- "CHRYSLER"
carsDF$man[carsDF$man1=="FCA ITALY SPA"] <- "FIAT GROUP"
carsDF$man[carsDF$man1=="FORD MOTOR COMPANY"] <- "FORD MOTOR COMPANY"
carsDF$man[carsDF$man1=="AUTOMOBILE DACIA SA"] <- "DACIA"
carsDF$man[carsDF$man1=="MAGYAR SUZUKI CORPORATION LTD"] <- "MAGYAR SUZUKI"

# ... (2) analyze as is

# ... (3) remove
# carsDF <- carsDF[!carsDF$man=="OUT OF SCOPE",]

# Simplify columns
carsDF <- carsDF[,c("id","state","man", "variant","version",
                    "make","comName",
                    "number","emissionCO2","mass",
                    "wheelBase","axleWidth1","axleWidth2",
                    "fuelType","fuelMode","engineCap","enginePower",
                    "elecCons")]


#### EDA: EXPLORATORY DATA ANALYSIS ####
# It's the process of exploring the data to get insights out of it,
# and generate hypotheses for further testing. Questions usually
# emerge as we explore the data.

### ... for a qualitative variable (fuelType) ####
# Which type of fuel is preferred in the European market? And in Spain?
summary(carsDF$fuelType)
carsDF$fuelType <- droplevels(carsDF$fuelType)
table(carsDF$fuelType, useNA = "ifany")

# Start cleaning the data? Any miscoding?
carsDF$fuelType <- toupper(carsDF$fuelType) #### posa tot en majuscules
table(carsDF$fuelType, useNA = "ifany")

# Any outliers?
# Attention should made to infrequent values... Are they miscoded?
# If so, should we fix them, code them as NA...
# Should we group low frequency responses?

carsDF$fuelType2 <- carsDF$fuelType
carsDF$fuelType2[carsDF$fuelType %in% c("DIESEL-ELECTRIC","PETROL-ELECTRIC")] <- "HYBRID"
carsDF$fuelType2[carsDF$fuelType %in% c("BIODIESEL","NG-BIOMETHANE","E85")] <- "BIOFUEL"
table(carsDF$fuelType2, useNA = "ifany")

# Missing values? Should we impute them? If so, how?
# There many imputation procedures, but there isn't enough time to 
# discuss them now

# Present easy to read tables or visualizations
# ... for EU
fuelDF <- carsDF[,c("state","number","fuelType2")]
fuelDF <- fuelDF %>% group_by(fuelType2) %>% summarize(sales=sum(number))
fuelDF

fuelDF$pc <- fuelDF$sales / sum(fuelDF$sales) * 100
fuelDF

ggplot(fuelDF, aes(x=fuelType2,y=pc,fill=fuelType2)) +
  geom_bar(stat="identity") +
  coord_flip() + theme_bw() + theme(legend.position = "none")

ggplot(fuelDF, aes(x=1,y=pc,fill=fuelType2)) +
  geom_bar(stat="identity", position=position_fill()) +
  coord_flip() + theme_bw() + 
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "bottom",
        legend.title = element_blank())

fuelDF$pci <- floor(fuelDF$pc)
fuelDF <- fuelDF[order(fuelDF$pc-fuelDF$pci,decreasing = T),]
fuelDF$pci <- fuelDF$pci + ifelse(1:nrow(fuelDF)>100-sum(fuelDF$pci),0,1) 
fuelDF <- fuelDF[order(desc(fuelDF$pci)),]

df <- expand.grid(x=1:10,y=1:10)
df$fuelType <- rep(fuelDF$fuelType2, fuelDF$pci)

ggplot(df,aes(x=x,y=y,fill=fuelType))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  guides(fill=guide_legend(title="FUEL TYPE")) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


# ... for Spain (ES)
fuelDF <- carsDF[carsDF$state=="ES",c("state","number","fuelType2")]
fuelDF <- fuelDF %>% group_by(fuelType2) %>% summarize(sales=sum(number))
#fuelDF <- summarise(group_by(fuelDF, fuelType2), sales=sum(number)) #o la de sobre una o altra
view(fuelDF)

## PIPING: OK sempre que DATA sigui el primer argument 

fuelDF$pc <- fuelDF$sales / sum(fuelDF$sales) * 100
fuelDF

ggplot(fuelDF, aes(x=fuelType2,y=pc,fill=fuelType2)) +
  geom_bar(stat="identity") +
  coord_flip() + theme_bw() + theme(legend.position = "none")

ggplot(fuelDF, aes(x=1,y=pc,fill=fuelType2)) +
  geom_bar(stat="identity", position=position_fill()) +
  coord_flip() + theme_bw() + 
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "bottom",
        legend.title = element_blank())

fuelDF$pci <- floor(fuelDF$pc)
fuelDF <- fuelDF[order(fuelDF$pc-fuelDF$pci,decreasing = T),]
fuelDF$pci <- fuelDF$pci + ifelse(1:nrow(fuelDF)>100-sum(fuelDF$pci),0,1) 
fuelDF <- fuelDF[order(desc(fuelDF$pci)),]

df <- expand.grid(x=1:10,y=1:10)
df$fuelType <- rep(fuelDF$fuelType2, fuelDF$pci)

ggplot(df,aes(x=x,y=y,fill=fuelType))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  guides(fill=guide_legend(title="FUEL TYPE")) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


### ... for a quantitative variable (CO2 emissions) ####
# What are the emissions for cars sold in 2016?
str(carsDF)
EficDF <- carsDF[carsDF$fuelType2=="DIESEL" | carsDF$fuelType2=="PETROL",]
EficDF <- EficDF[,c("man", "emissionCO2", "mass", "wheelBase", "axleWidth1",
                    "axleWidth2", "engineCap", "enginePower","number")]
summary(EficDF)

valid <- !is.na(apply(EficDF[,-c(1,9)],1,sum))
table(valid)

EficDF <- EficDF[valid,]

# Only different cars
EficDF <- EficDF %>% group_by(man,emissionCO2,mass,
                              wheelBase,axleWidth1,axleWidth2,engineCap,
                              enginePower) %>% summarize(sales=sum(number))

# Any outliers?
summary(EficDF$emissionCO2)

ggplot(EficDF, aes(x=1, y=emissionCO2))+geom_boxplot()+
  coord_flip()+ theme_bw() + 
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot(EficDF, aes(x=1, y=emissionCO2))+
  geom_jitter(alpha=.1, size=1, shape=21, height=0, width=.3) +
  geom_boxplot(outlier.shape = NA,col="red",fill=NA,size=1)+
  coord_flip()+ theme_bw() + 
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

EficDF <- EficDF[order(EficDF$emissionCO2, decreasing = TRUE),]
head(EficDF)
tail(EficDF[,-(4:6)])

sel=carsDF$man=="VOLVO" & carsDF$emissionCO2==49
carsDF[sel, ]
# Should we remove them?

summary(EficDF$sales)
table(EficDF$sales>5000)

EficDF2 <- EficDF[EficDF$sales>5000,]


# Statistical description
ggplot(EficDF2, aes(x=1, y=emissionCO2))+
  geom_jitter(alpha=.1, size=1, shape=21, height=0, width=.3) +
  geom_boxplot(outlier.shape = NA,col="red",fill=NA,size=1)+
  coord_flip()+ theme_bw() + 
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

summary(EficDF2$emissionCO2)

ggplot(EficDF2, aes(x=emissionCO2))+
  geom_histogram(fill="#CCCCCC",col="black",binwidth=10, center=100) +
  theme_bw()

### ... relations among variables ####

# What features are related to the commercial success of a vehicle?

carsDF_DC <- carsDF %>% group_by(man,emissionCO2,mass,
                                 wheelBase,axleWidth1,axleWidth2,engineCap,
                                 enginePower) %>% slice(which.max(number))

modelsDF <- carsDF %>% group_by(man,emissionCO2,mass,
                                wheelBase,axleWidth1,axleWidth2,engineCap,
                                enginePower) %>% summarize(sales=sum(number))

carsDF_DC <- full_join(carsDF_DC, modelsDF, by=c("man","emissionCO2","mass",
                                                 "wheelBase","axleWidth1","axleWidth2","engineCap",
                                                 "enginePower"))

valid <- apply(carsDF_DC[,c("man","emissionCO2","mass",
                            "wheelBase","axleWidth1","axleWidth2","engineCap",
                            "enginePower","fuelType2")],1,function(x) sum(is.na(x)))==0
table(valid)

colnames(carsDF_DC)
carsDF_DC <- carsDF_DC[valid,c("man","emissionCO2","mass",
                               "wheelBase","axleWidth1","axleWidth2","engineCap",
                               "enginePower","fuelType2","sales")]


if(!require("GGally")) {
  install.packages("GGally")
  library("GGally")
}
if(!require("corrplot")) {
  install.packages("corrplot")
  library("corrplot")
}
if(!require("ggthemes")) {
  install.packages("ggthemes")
  library("ggthemes")
}

carsDF_DCs <- carsDF_DC[sample(1:nrow(carsDF_DC),1000),]
cor(carsDF_DCs[,-c(1,9)])

# Correlograms
corrplot(cor(carsDF_DCs[,-c(1,9)]),order = "hclust",addrect = 3,
         mar = c(2, 1, 1, 1), cl.length = 5)

ggcorr(carsDF_DCs[,-c(1,9)])
ggcorr(carsDF_DCs[,-c(1,9)], geom="circle", nbreaks = 8, palette = "PuOr")


# Scatter plot matrix ***************ELS MILLORS ##################
pairs(carsDF_DCs[,-c(1,9)])

ggpairs(carsDF_DCs[,-1], axisLabels = "none") + 
  theme_few()


# Small multiples
ggplot(carsDF_DC, aes(y=log10(sales), x=mass)) + 
  geom_point(alpha=.1, size=.5) +
  geom_smooth(se=FALSE, method="lm", formula=y ~ x) + 
  facet_grid(fuelType2~floor(emissionCO2/100)) +
  theme_bw()
