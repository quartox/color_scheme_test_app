library(plotrix)
library(ggplot2)

colour.list <- c("greyscale", "greenscale", "YellowRed", "YellowGreenBlue","Spectral", "matlabSpectral")

# function to choose a random colour scheme
choose.random.colour.scheme<-function(){
  sample(colour.list, 1, replace=FALSE)
}

#random number
random.number<- function(number, type) {
  if (type=="single") {
    sample.int(number, 1)
  } else if (type=="list") {
    sample.int(number)
  }
}

make.counter <- function(max) {
  i <- 0
  function() {
    i <<- if (i >= max) 1 else i + 1
    i
  }
}

question.properties <- function(...) {
  data$properties <- list(...)
  div(..., style="display:none")
}

#intervals out of 100
random.number.interval <- function(number) {
  round(seq(1, 100, length.out=number + 2)[-c(1, number + 2)])[sample(number)]
}

make.colour.linear.question<-function(scheme, ActualValue, include.legend=FALSE) {
  ncols <- 250
  ActualValue <- ActualValue / 100 * ncols
  colourscheme <- colour_schemes()[[scheme]]
  x <- colourscheme(ncols)
  op <- par(mar=c(0, 0, 0, 0))
  on.exit(par(op))
  if (include.legend) {
    ylim <- c(360, 450)
  } else {
    ylim <- c(400, 450)
  }

  plot(c(100, 175), ylim, type= "n", xlab = "", ylab = "", axes=FALSE)
  #   gradient.rect(xleft=100,xright=175,ybottom=400,ytop=450, col=x)
  rect(100, 400, 125, 450, col = x[1], border="white",lwd=10)
  rect(125, 400, 150, 450, col = x[ActualValue], border="white",lwd=10)
  rect(150, 400, 175, 450, col = x[ncols], border="white",lwd=10)
  if (include.legend) {
    text(c(112,137,162), 365,
         c("1","Middle square", "100"), cex=1.2)
    arrows(x0=c(112,137,162), 375, y1=390, lwd=2, length=0.1)
  }
}

make.gradient.rectangle<-function(scheme){
  ncols <- 250
  colourscheme <- colour_schemes()[[scheme]]
  x <- colourscheme(ncols)
  op <- par(mar=c(0, 0, 0, 0))
  on.exit(par(op))
  plot(c(0, 100), c(0, 100), type= "n", xlab = "", ylab = "", axes=FALSE)
  ylab <- strheight("1") * 2
  gradient.rect(xleft=1, xright=100, ybottom=ylab, ytop=100, col=x, border="white")
  text(x=c(1, 100), y=ylab/2, c(1, 100))
}


progress.bar <- function(max, length) {
  par(mar = rep(0, 4))
  plot(c(0,max), c(0, 50), type= "n", xlab = "", ylab = "", axes=FALSE)
  rect(xleft=0,ybottom=0,xright=max,ytop=50,col="lightblue", border=NA)
  rect(xleft=0,ybottom=0,xright=length,ytop=50,col="dodgerblue", border=NA)
}

#display volcano plot for aesthetic rating
display.plot <-function(scheme){
  colourscheme <- colour_schemes()[[scheme]]
  filled.contour(volcano, color.palette = colourscheme, asp = 1)
}

# display heat mat for aesthetic rating
display.heat.map<- function(scheme) {
  airdata=read.csv("dataForQuestions/airdata.csv")
  #set variables as factors
  airdata$Month <- factor(airdata$Month, c("January","February","March","April", "May", "June", "July","August", "September", "October",   "November",  "December"))
  airdata$Year <- factor(airdata$Year)
  colourschemefunction <- colour_schemes()[[scheme]]
  colourscheme<-colourschemefunction(250)
  myplot<-ggplot(airdata, aes(Year,Month,fill=value)) + geom_tile() + guides(fill=guide_colorbar(barheight=15, barwidth=2,nbin=100, label.theme=element_text(size=15, angle=0))) + scale_fill_gradientn(colours = colourscheme,limits=c(50,650), breaks=c(100,200,300,400,500,600)) +coord_equal() + ggtitle("Air Passengers") + labs(fill="Number of\nair passengers") +theme(legend.title=element_text(size=15),axis.text=element_text(size=15),axis.text.x=element_text(angle=90), plot.title=element_text(size=25))
  print(myplot)
}

display.map.for.aesthetics<- function(scheme) {
  gisborne=read.csv("dataForQuestions/gisborne_f.csv")
  colourschemefunction <- colour_schemes()[[scheme]]
  colourscheme<-colourschemefunction(250)
  myplot<-ggplot(gisborne, aes(long, lat, group = group, fill = MB06)) + geom_polygon()+coord_equal() + scale_fill_gradientn(colours = colourscheme) + theme_minimal()  + guides(fill=guide_colorbar(barheight=15, barwidth=2,nbin=100, label.theme=element_text(size=15, angle=0))) + theme(legend.title=element_blank())
  print(myplot)
}

# random coordinate generator for choropleth Q
random.coordinate.choropleth <- function(n){
  random.coordinate <- sample.int(33,n)
  return(random.coordinate)
}


actual.value.choropleth <- function(coordinate) {
  coords=read.csv("dataForQuestions/coords.csv")
  #return actual value of area
  return(coords[coordinate,"Partic_Per"])
}

#single point question-choropleth
display.single.marker.on.map <- function(scheme, areaID) {
  coords <- get_london()$coords
  lnd_f <- get_london()$shape
  colourschemefunction <- colour_schemes()[[scheme]]
  colourscheme<-colourschemefunction(250)

  map<-ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
    geom_polygon(colour="white", size=0.25) +
    coord_equal() +
    labs(x = NULL, y = NULL,fill = "Number of\ncats")+
    scale_fill_gradientn(colours = colourscheme, breaks=c(0,100)) +
    annotate("point", x=coords[areaID,3], y=coords[areaID,4], color="white", size=8)+
    annotate("text", x=coords[areaID,3], y=coords[areaID,4], label="A", color="black", size=5) +
    guides(fill=guide_colorbar(draw.ulim=FALSE,drawllim=FALSE,barheight=17, barwidth=2,nbin=100,label.theme=element_text(size=15, angle=0))) +
    theme(legend.title=element_text(size=15), panel.grid=element_blank(),panel.background = element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin=grid::unit(c(0, 0, 0, 0), "cm"))

  print(map)
}

load_london <- function() {
  london_coords <- read.csv("dataForQuestions/coords.csv")
  london_shape <- read.csv("dataForQuestions/new london data.csv")
  london_coords$Partic_Per[1:24] <- c(random.number.interval(12), random.number.interval(12))
  london_shape$Partic_Per <- london_coords$Partic_Per[match(london_shape$name, london_coords$name)]
  log_info("", london_true_values=london_coords$Partic_Per[1:24])
  list(coords=london_coords, shape=london_shape)
}

london_env <- new.env(parent=emptyenv())

reload_london <- function() {
  london_env$data <- load_london()
}
get_london <- function() {
  london_env$data
}
## ensure london data is always good to use.
reload_london()
