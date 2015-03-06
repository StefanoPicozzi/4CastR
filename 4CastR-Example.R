#!/usr/bin/Rscript

# Author: Stefano Picozzi
# Date: January 2015
# Name: 4CastR.R

# Batch control script
#Sys.setenv(NOAWT = "true")

source("4CastR.R")
ppi <- 1000

# The top 20 ASX stocks
zLabel <- c("AMP", "ANZ", "BHP", "BXB", "CBA", "CSL", "IAG", "MQG", "NAB", "ORG",	
            "QBE", "RIO", "SCG", "SUN", "TLS", "WBC",	"WES", "WFD", "WOW", "WPL")	
d <-      c("NORTH", "SOUTH", "NORTH", "SOUTH", "NORTH", "SOUTH", "NORTH", "SOUTH","NORTH", "SOUTH",
            "NORTH", "SOUTH", "NORTH", "SOUTH", "NORTH", "SOUTH", "NORTH", "SOUTH","NORTH", "SOUTH")

# Generate some random sample data
x <- sample(1:9, 20, replace=TRUE)
y <- sample(1:9, 20, replace=TRUE)
z <- sample(0:500000, 20, replace=T)
data <- data.frame(y=y, x=x, z=z, zLabel=zLabel, d=d)

xAxisTitle <- "Business Case"
yAxisTitle <- "Competitive Score"
chartTitle <- "My Sales 4Cast"
xIntercept <- 5
yIntercept <- 5
xMin <- 1
yMin <- 1
yGap <- 1
xMax <- 9
yMax <- 9
xGap <- 1
size <- 50
config <- data.frame(xAxisTitle, yAxisTitle, chartTitle, 
                     xIntercept, yIntercept, xMin, yMin, xMax, yMax, size, stringsAsFactors=FALSE)

get4Cast(config, data)


