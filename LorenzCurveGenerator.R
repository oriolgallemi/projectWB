library(ggplot2)

#continent filter:
continent = NULL
#continent = "E"

# show lables 
label.threshold = 38000000
#label.threshold = 5000000

title = 'Lorenz curve of world GDP'

input = gdp.and.pop <- read.csv("gdp-and-pop.csv")

prepare.data = function(input, continent=NULL) {
  if (!is.null(continent)) {
    input = input[which(input$Continent==continent),]
  }
  input = input[with(input, order(gdp.capita)), ]
  
  input$gdp = input$gdp.capita * input$population
  input[nrow(input),] = c(NA)
  gdp = input$gdp[1:(nrow(input)-1)]
  pop = as.numeric(input$population[1:(nrow(input)-1)])
  input$gdp.perc = c(0, cumsum(gdp) / sum(gdp))
  input$pop.perc = c(0, cumsum(pop) / sum(pop))
  return(input)
}  

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
