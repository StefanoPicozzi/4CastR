#!/usr/bin/Rscript

# Author: Stefano Picozzi
# Date: January 2015
# Name: 4CastR.R

# Batch control script
#Sys.setenv(NOAWT = "true")

library("lattice")
library("MASS")
library("ggplot2")
library("rjson")
library("plyr")
library("scales")
library("rgl")
library("xlsx")
require("grid")

setwd("/Users/stefanopicozzi/Google Drive/4Cast/")

# The top 20 ASX stocks
zLabel <- c("AMP", "ANZ", "BHP", "BXB",   "CBA", "CSL", "IAG",	"MQG", "NAB", "ORG",	"QBE", "RIO", "SCG",	"SUN", "TLS", "WBC",	"WES", "WFD", "WOW", "WPL")	
# Generate some random sample data
x <- sample(1:9, 20, replace=TRUE)
y <- sample(1:9, 20, replace=TRUE)
z <- sample(10000:500000, 20, replace=T)
data <- data.frame(y=y, x=x, z=z, zLabel=zLabel)

xAxisTitle <- "Business Case"
yAxisTitle <- "Competitive Score"
chartTitle <- "My Sales 4Cast"
xIntercept <- 5
yIntercept <- 5
xMin <- 0
yMin <- 0
xMax <- 10
yMax <- 10
config <- data.frame(xAxisTitle, yAxisTitle, chartTitle, 
                     xIntercept, yIntercept, xMin, yMin, xMax, yMax, stringsAsFactors=FALSE)

get4Cast <- function(config, data) {

   printMoney <- function(x) {
      gsub(" ", "", paste("$", format(x, digits = 10, nsmall = 0, decimal.mark = ".", big.mark = ","), sep = ""))
   }
   setZero <- function(x) { if (x > 0) {x} else {x = 0} }
   
   set.seed(123)
   N <- nrow(data)
   c00Label <- "Cover"
   c10Label <- "Convert"
   c01Label <- "Counsel"
   c11Label <- "Close"
      
   data <-cbind(data, yJitter=data[,"y"]+(rnorm(N)/2), xJitter=data[,"x"]+(rnorm(N)/2))

   data[,"z"] <- as.numeric(data[,"z"])

   maxValue <- max(as.numeric(data[,"z"]))
   data <- cbind(data, yLabel=data[,"yJitter"]+(data[,"z"]/maxValue/2), xLabel=data[,"xJitter"])
   data <- cbind(data, zMoney=as.character(printMoney(data[,"z"])))

   cData <- subset(data, x > config$xIntercept & y > config$yIntercept)
   c11Total <- sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x <= config$xIntercept & y <= config$yIntercept)
   c00Total <- sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x < config$xIntercept & y > config$yIntercept)
   c01Total <- sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x >= config$xIntercept & y <= config$yIntercept)
   c10Total <- sum(cData$z, na.rm=FALSE)

   c11Total <- setZero(c11Total)
   c10Total <- setZero(c10Total)
   c01Total <- setZero(c01Total)   
   c00Total <- setZero(c00Total)
   total <- sum(data$z)

   data$z <- as.numeric(lapply( data$z, function(x) { if (x == 0) { x = 1000 } else { x }} ))
   
   ggplot(data, aes(y=yJitter, x=xJitter)) +
   
      # Plotting
      geom_point(aes(size=z), colour="blue", fill="blue", shape=21, alpha=0.5) +   
      scale_size_area(max_size=70, guide=FALSE) +   
      geom_text(aes(y=yLabel, x=xLabel, label=zLabel), size=4, hjust=0, vjust=0, color="black", face="bold") +
      geom_text(aes(y=yJitter, x=xJitter+0.2, label=zMoney), size=3, hjust=1, vjust=1, color="white", face="bold") +
   
      # Axis Labels
      ylab(config$yAxisTitle) + theme(axis.title.y=element_text(angle=90, size=18, face="bold")) +
      xlab(config$xAxisTitle) + theme(axis.title.x=element_text(angle=0, size=18, face="bold")) +
   
      # Axis Tick Marks
      scale_y_continuous(name=config$yAxisTitle, breaks=seq(1,9,by=1), limits=c(0,10)) +
      theme(axis.text.x=element_text(size=12, color="black", angle=0, face="bold")) +
      scale_x_continuous(name=config$xAxisTitle, breaks=seq(1,9,by=1), limits=c(0,10)) +
      theme(axis.text.y=element_text(size=12, color="black", angle=0, face="bold")) +

      # Chart Settings
      theme(plot.background = element_rect(fill = 'grey')) +
      ggtitle(config$chartTitle) + theme(plot.title=element_text(size=24, face="bold")) +
      theme(legend.position=c(0.1,0.45)) + theme(legend.key.height=unit(1,"cm")) +

      # Grid Layout
      geom_hline(yintercept=config$yIntercept) +
      geom_vline(xintercept=config$xIntercept) +
      geom_hline(yintercept=7, linetype=2, alpha=1) +
      geom_hline(yintercept=3, linetype=2, alpha=1) +
      geom_vline(xintercept=7, linetype=2, alpha=1) +
      geom_vline(xintercept=3, linetype=2, alpha=1) +
            
      annotate("text", x=config$xMax, y=config$yMax, label=c11Label, size=6, fontface="bold.italic", hjust=1) +
      annotate("text", x=config$xMax, y=config$yMin, label=c10Label, size=6, fontface="bold.italic", hjust=1) +   
      annotate("text", x=config$xMin, y=config$yMin, label=c00Label, size=6, fontface="bold.italic", hjust=0) +
      annotate("text", x=config$xMin, y=config$yMax, label=c01Label, size=6, fontface="bold.italic", hjust=0) +

      annotate("text", x=config$xMax, y=config$yMax-0.25, label=printMoney(c11Total), size=4, fontface="bold", hjust=1) +
      annotate("text", x=config$xMax, y=config$yMin+0.25, label=printMoney(c10Total), size=4, fontface="bold", hjust=1) +
      annotate("text", x=config$xMin, y=config$yMin+0.25, label=printMoney(c00Total), size=4, fontface="bold", hjust=0) +
      annotate("text", x=config$xMin, y=config$yMax-0.25, label=printMoney(c01Total), size=4, fontface="bold", hjust=0) +

      annotate("text", x=config$xMin, y=config$yMin+1.5, label=paste("Grand Total=", printMoney(total), separator=""), size=6, fontface="bold", hjust=0)
   
   
}

get4Cast(config, data)


