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
#library("rgl")
library("xlsx")
require("grid")
library("png")

print("Building plots ...")
get4Cast <- function(config, data) {

   img <- readPNG("4CastR.png")
   g <- rasterGrob(img, interpolate=TRUE)
   
   printMoney <- function(x) {
      gsub(" ", "", paste("$", format(x, digits = 10, nsmall = 0, decimal.mark = ".", big.mark = ","), sep = ""))
   }
   setZero <- function(x) { if (x > 0) {x} else {x = 0} }
   
   yLabels = c("", "Token", "Beauty Pageant", "Visbility", "Parity", "Advantage", "Sponsored", "Favoured", "Proven", "Top Seed", "")
   xLabels <- c("", "Experimental", "Discovery", "Championed", "Disrupters", "Project", "Hard Benefit", "Strategic", "Executive", "CXO Visibility", "")
   
   set.seed(123)
   N <- nrow(data)
   c00Label <- "Cover"
   c10Label <- "Convert"
   c01Label <- "Counsel"
   c11Label <- "Close"
   subTotalOffset <- 0.35
   
   data <-cbind(data, yJitter=data[,"y"]+(rnorm(N)/2), xJitter=data[,"x"]+(rnorm(N)/2))
   
   data[,"z"] <- as.numeric(data[,"z"])
   
   maxValue <- max(as.numeric(data[,"z"]))
   minValue <- 999
   
   data <- cbind(data, yLabel=data[,"yJitter"]+(data[,"z"]/maxValue/2), xLabel=data[,"xJitter"])
   data <- cbind(data, zMoney=as.character(printMoney(data[,"z"])), stringsAsFactors=FALSE)
   
   cData <- subset(data, x == config$xIntercept & y == config$yIntercept)
   c11Total <- sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x == config$xIntercept & y > config$yIntercept)
   c11Total <- c11Total + sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x > config$xIntercept & y == config$yIntercept)
   c11Total <- c11Total + sum(cData$z, na.rm=FALSE)   
   cData <- subset(data, x > config$xIntercept & y > config$yIntercept)
   c11Total <- c11Total + sum(cData$z, na.rm=FALSE)   
   
   cData <- subset(data, x < config$xIntercept & y == config$yIntercept)
   c01Total <- sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x < config$xIntercept & y > config$yIntercept)
   c01Total <- c01Total + sum(cData$z, na.rm=FALSE)
   
   cData <- subset(data, x == config$xIntercept & y < config$yIntercept)
   c10Total <- sum(cData$z, na.rm=FALSE)
   cData <- subset(data, x > config$xIntercept & y < config$yIntercept)
   c10Total <- c10Total + sum(cData$z, na.rm=FALSE)
   
   cData <- subset(data, x < config$xIntercept & y < config$yIntercept)
   c00Total <- sum(cData$z, na.rm=FALSE)
      
   c11Total <- setZero(c11Total)
   c10Total <- setZero(c10Total)
   c01Total <- setZero(c01Total)   
   c00Total <- setZero(c00Total)
   total <- sum(data$z)
   
   data$z <- as.numeric(lapply( data$z, function(x) { if (x == 0) { x = minValue } else { x }} ))
   
   ggplot(data, aes(y=yJitter, x=xJitter, colour=d)) +
      
      scale_colour_brewer(palette="Set1") +
     
      # Plotting
      # geom_point(aes(size=z, colour=d), colour="blue", fill="blue", shape=21, alpha=0.5) +   
      geom_point(aes(size=z, colour=d, fill=d), shape=21, alpha=0.5) +   
      scale_size_area(max_size=config$size, guide=FALSE) +   
      geom_text(aes(y=yLabel, x=xLabel, label=zLabel), size=4, hjust=0, vjust=0, color="black", face="bold") +
      geom_text(aes(y=yJitter, x=xJitter+0.25, label=zMoney), size=3, hjust=1, vjust=1, color="white", face="bold") +
      
      # Legend
      #theme(legend.position=c(0,1), legend.justification=c(2,2), legend.title=element_blank()) +
      theme(legend.position=c(0.1, -5), legend.title=element_blank()) +
     
     
      # Axis Labels
      ylab(config$yAxisTitle) + theme(axis.title.y=element_text(angle=90, size=18, face="bold")) +
      xlab(config$xAxisTitle) + theme(axis.title.x=element_text(angle=0, size=18, face="bold")) +
      
      # Axis Tick Marks
      theme(axis.ticks=element_blank()) +
      theme(axis.text.x=element_text(size=12, color="darkgray", angle=0, face="bold")) +
      # scale_x_continuous(name=config$xAxisTitle, breaks=seq(xMin,xMax,by=xGap), limits=c(xMin,xMax)) +
      # theme(axis.text.y=element_text(size=12, color="black", angle=0, face="bold")) +
      scale_x_continuous(name=config$xAxisTitle, breaks=seq(xMin-1,xMax+1,by=xGap), labels=xLabels, limits=c(xMin-1,xMax+1)) +
      theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, size=12, color="black", face="bold")) +
      scale_y_continuous(name=config$yAxisTitle, breaks=seq(yMin-1,yMax+1,by=yGap), labels=yLabels, limits=c(yMin-1,yMax+1)) +
      theme(axis.text.y=element_text(angle=45, hjust=1, vjust=1, size=12, color="black", face="bold")) +
        
      # Chart Settings
      theme(plot.background = element_rect(fill = 'grey'), panel.background = element_rect(fill = "white")) +
      ggtitle(paste(config$chartTitle, " [ Total=", printMoney(total), " ]", sep="")) + theme(plot.title=element_text(size=15, face="bold")) +
      theme(legend.position=c(0.1,0.45)) + theme(legend.key.height=unit(1,"cm")) +
      # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      # annotation_custom(g, xmin=config$xMax-0.25, xmax=config$xMax+0.25, ymin=(config$yMax/2)-1, ymax=(config$yMax/2)+1) +    
      # annotation_custom(g, xmin=config$xMax-0.2, xmax=config$xMax+0.4, ymin=config$yIntercept-0.9, ymax=config$yIntercept-0.1) +
      annotation_custom(g, xmin=config$xMax+0.5, xmax=config$xMax+1.25, ymin=1, ymax=2) +
       
      # Grid Layout
      geom_hline(yintercept=config$yIntercept) +
      geom_vline(xintercept=config$xIntercept) +
      geom_hline(yintercept=as.integer(2*yMax/3)+1, linetype=2, alpha=1, color="darkgrey") +
      geom_hline(yintercept=as.integer(yMax/3), linetype=2, alpha=1, color="darkgrey") +
      geom_vline(xintercept=as.integer(2*xMax/3)+1, linetype=2, alpha=1, color="darkgrey") +
      geom_vline(xintercept=as.integer(xMax/3), linetype=2, alpha=1, color="darkgrey") +
      
      annotate("text", x=config$xMax+1, y=config$yMax+1, label=c11Label, size=6, fontface="bold.italic", hjust=1) +
      annotate("text", x=config$xMax+1, y=config$yMin-1, label=c10Label, size=6, fontface="bold.italic", hjust=1) +   
      annotate("text", x=config$xMin-1, y=config$yMin-1, label=c00Label, size=6, fontface="bold.italic", hjust=0) +
      annotate("text", x=config$xMin-1, y=config$yMax+1, label=c01Label, size=6, fontface="bold.italic", hjust=0) +
      
      annotate("text", x=config$xMax+1, y=config$yMax+1-subTotalOffset, label=printMoney(c11Total), size=4, fontface="bold", hjust=1) +
      annotate("text", x=config$xMax+1, y=config$yMin-1+subTotalOffset, label=printMoney(c10Total), size=4, fontface="bold", hjust=1) +
      annotate("text", x=config$xMin-1, y=config$yMin-1+subTotalOffset, label=printMoney(c00Total), size=4, fontface="bold", hjust=0) +
      annotate("text", x=config$xMin-1, y=config$yMax+1-subTotalOffset, label=printMoney(c01Total), size=4, fontface="bold", hjust=0)    
}



