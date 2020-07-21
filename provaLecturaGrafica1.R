
library(readr)
library(stats)
library(dplyr)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/oriol.gallemi/Desktop/R datascience/projectWB")

#data taken from https://data.worldbank.org/indicator/NY.GNP.PCAP.CD    fitxer mundial per capita (escollit) 

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

title = 'Lorenz curve of AVG household income / world'

prepare.data <- function(Income, Population, Year) { ### segons any escollit busca els vectors a income i poblacio
  c(Country_name=select(Income,(1)) ,Yearincome=Income[,which(colnames(Income)==Year)], Yearpop=Population[, which(colnames(Population)==Year)])
}

printable<-prepare.data(dfFullIncome, dfFullPop, 1991) ####ALERTA que algunes vars no son numeriques !!!! cal transformar
printable<-as_tibble(printable)   ### RECICLATGE DE LA VARIABLE PRINTABLE
colnames(printable)<-c("CountryName", "AvgIncome", "AvgPop" )
# format(printable$AvgIncome, decimal.mark=".")    # DUBTO de si ja ho tinc en numeric o què passa, en principi és inocu
# printable$AvgIncome <- as.numeric(as.character(printable$AvgIncome))
# printable$AvgPop <- as.numeric(as.character(printable$AvgPop))

# elimino files inutils (agrupacions de regions, paradisos fiscals remenuts)
printable<-printable[-c(6, 7, 35, 60, 61, 62, 63, 64, 67, 72, 73, 97, 101, 102, 103, 104, 106, 109, 127, 133, 134, 135, 138, 139, 141, 160, 180,
                        182, 190, 195, 196, 197, 214, 216, 217, 229, 230, 235, 237, 239, 240, 248, 258),]
str(printable)
view(printable)
summary(printable) # unsorted printable (bar chart)

printables <- printable[order(printable$AvgIncome, na.last = NA),]  ## els [] son els apuntadors a files i order retorna apuntadors
str(printables)
view(printables)
summary(printables) #### vector sorted ready to plot






   ############################## 


  #########################
  #  set seed for reproducibility
  set.seed(123)
  df <- data.frame( col1 = sample(5,10,repl=T) , col2 = sample(5,10,repl=T) , col3 = sample(5,10,repl=T) )
  
  #  We want to sort by 'col3' then by 'col1'
  sort_list <- c("col3","col1")
  
  #  Use 'do.call' to call order. Second argument in do.call is a list of arguments
  #  to pass to the first argument, in this case 'order'.
  #  Since  a data.frame is really a list, we just subset the data.frame
  #  according to the columns we want to sort in, in that order
  df[ do.call( order , df[ , match( sort_list , names(df) ) ]  ) , ]
  # if (!is.null(continent)) {
  #   income = income[which(income$Continent==continent),]
  # }
  # income = income[with(income, order(gdp.capita)), ]
  # 
  # income$gdp = income$gdp.capita * income$population
  # income[nrow(income),] = c(NA)
  # gdp = income$gdp[1:(nrow(income)-1)]
  # pop = as.numeric(income$population[1:(nrow(income)-1)])
  # income$gdp.perc = c(0, cumsum(gdp) / sum(gdp))
  # income$pop.perc = c(0, cumsum(pop) / sum(pop))
  # return(income)
  ########################## 
  
} ### GENERA UNA  FUNCIO NOVA SIMPLIFICADA, per visualitzar nom?s lea dades reduides

input = prepare.data(input, continent)

a = ggplot() + 
  scale_y_continuous(name = "cumulative share of annual GDP", limits=c(0, 1.05), breaks=seq(0, 1, by=0.1)) +
  scale_x_continuous(name = "cumulative share of population", breaks=seq(0, 1, by=0.1)) +
  ggtitle(title)    


colors = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

for (i in seq(1, nrow(input))) {
  #polygons:
  d=data.frame(x=c(input$pop.perc[i],input$pop.perc[i],input$pop.perc[i+1],input$pop.perc[i+1]), 
               y=c(0,                input$gdp.perc[i],input$gdp.perc[i+1],0))
  color=colors[1 + i %% length(colors)]
  a = a + geom_polygon(data=d, mapping=aes(x=x, y=y), fill=color)
  
  #annotations:
  if (i < nrow(input) && input$population[i] > label.threshold) {
    x = (input$pop.perc[i] + input$pop.perc[i+1])/2
    y = (input$gdp.perc[i] + input$gdp.perc[i+1])/2 + 0.03
    a = a + annotate("text", x = x, y = y, label = input$Country.Name[i], color=color, lineheight=0.5, angle=90, hjust=0)
  }
}



######################################



### Clean the data ####

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
