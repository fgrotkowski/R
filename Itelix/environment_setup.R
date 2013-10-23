### Set the proper path if you want to run this
routes.csv <- "routes.csv"
summary.csv <- "summary.csv"

################### GLOBAL CONSTS ########################
PDF <- TRUE
#PDF <- FALSE

#REMOVE_TRIMMED_ROUTES <- FALSE
REMOVE_TRIMMED_ROUTES <- TRUE

INFINITE_ROUTE_TASK_ID <- 0

bg.color <- "white"
#bg.color <- "lightgrey"
hist.buckets <- 20
blockers.buckets <- 20

report.date <- Sys.time()
report.date.filename <- format(report.date, "%Y%m%d-%H%M%S")
report.date.header <- format(report.date, "%Y/%m/%d %H:%M:%S")

if (require(RColorBrewer) == FALSE)
	stop("Package RColorBrewer missing")
if (require(gregmisc) == FALSE)
	stop("Package gregmisc missing")
if (require(ggplot2) == FALSE)
	stop("Package ggplot2 missing")
if (require(RSEIS) == FALSE)
	stop("Package RSEIS missing")
if (require(fields) == FALSE)
	stop("Package fields missing")
if (require(plotrix) == FALSE)
	stop("Package plotrix missing")

library(RColorBrewer)
library(gregmisc)
library(ggplot2)
library(RSEIS)
library(fields)
library(plotrix)


myRound <- function(d) {
	round(d, 2)
}

sec.to.tull.time <- function(sec) {
	h <- trunc(sec/3600)
	min <- trunc((sec - h*3600)/60)
	secs <- sec - h*3600 - min*60
	paste(h, "[h] ", min, "[min] ", myRound(secs), "[s]", sep = "")
}

resetPar <- function() {
	dev.new()
	op <- par(no.readonly = TRUE)
	dev.off()
	op
}

mvc.pos.to.id <- function(pos) {
#4  8 20 24 36 40 52 56 68 72
	if (pos == "4") ret <- "1"
	if (pos == "8") ret <- "2"
	if (pos == "20") ret <- "3"
	if (pos == "24") ret <- "4"
	if (pos == "36") ret <- "5"
	if (pos == "40") ret <- "6"
	if (pos == "52") ret <- "7"
	if (pos == "56") ret <- "8"
	if (pos == "68") ret <- "9"
	if (pos == "72") ret <- "10"
	ret
}

mvc.pos.to.mayor <- function(pos) {
	ret <- as.character(floor((as.numeric(mvc.pos.to.id(pos)) + 1)/2))
	ret
}

mvc.pos.to.type <- function(pos) {
	ret <- as.numeric(mvc.pos.to.id(pos))
	as.character(sum(ret > 6))
}

colors <- c(1,4,7,16)
#colors <- brewer.pal(6, "Blues")
colors <- brewer.pal(6, "Greens")

col.output <- brewer.pal(9, "GnBu")[8]
col.input <- brewer.pal(9, "Reds")[8]
col.trimmed <- brewer.pal(9, "YlOrRd")[4]
col.input.continue <- brewer.pal(9, "Reds")[5]
col.output.continue <- brewer.pal(9, "GnBu")[5]
col.output.return.case <- "white"
col.disable.bot <- "black"

mvc.cex <- 0.5
mvc.visual.cex <- 1.0
mvc.colors <- brewer.pal(6, "PuRd")

BOT.UTILITY.BOTS.PER.PAGE <- 40
MAPS.BOTS.PER.PAGE <- 21*2
THROUGHPUT.WINDOW.WIDTH <- 400	# in seconds
GRZEGORZ.CLUSTERIZATION <- FALSE
GIL.CONFIG <- FALSE

TIME.BETWEEN.PLATFORMS <- 5
MAX.TPH.PER.MVC <- 3600/TIME.BETWEEN.PLATFORMS
MIN.STAY.IN.STORAGE.INPUT <- 18
MIN.STAY.IN.STORAGE.OUTPUT <- 10
MIN.STAY.IN.MVC.INPUT <- 17
MIN.STAY.IN.MVC.OUTPUT <- 13

