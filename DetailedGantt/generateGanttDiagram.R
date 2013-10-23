#
#

PDF <- FALSE
BOT.UTILITY.BOTS.PER.PAGE <- 20
bots.per.level <- 6
simulation.time <- "60 minutes"
levels.num <- 18

TIME.BETWEEN.PLATFORMS <- 4.4
MAX.TPH.PER.MVC <- 3600/TIME.BETWEEN.PLATFORMS
THROUGHPUT.WINDOW.WIDTH <- 400

PLOT.OUTPUT <- TRUE
PLOT.INPUT <- TRUE

mvcs.output <- 4
mvcs.input <- 4
mvcs <- mvcs.output + mvcs.input

data.csv <- "ganttdata.txt"
dir <- "C:/Users/Filip/Documents/HMPC docs/DetailedGantt"
setwd(dir)

resetPar <- function() {
	dev.new()
	op <- par(no.readonly = TRUE)
	dev.off()
	op
}

myRound <- function(d) {
	round(d, 2)
}

if (require(plotrix) == FALSE)
	stop("Package plotrix missing")
if (require(RColorBrewer) == FALSE)
	stop("Package RColorBrewer missing")
colors <- c(1,4,7,16)
if (require(gregmisc) == FALSE)
	stop("Package gregmisc missing")
#colors <- brewer.pal(6, "Blues")
colors <- brewer.pal(6, "Greens")

col.output <- brewer.pal(9, "GnBu")[8]
col.input <- brewer.pal(9, "Reds")[8]
col.trimmed <- brewer.pal(9, "YlOrRd")[4]
col.input.continue <- brewer.pal(9, "Reds")[5]
col.output.continue <- brewer.pal(9, "GnBu")[5]
col.output.return.case <- "white"
col.disable.bot <- "black"
bg.color <- "white"

alpha <- 150
col.input.transparent <- col2rgb(col.input)
col.input.transparent <- rgb(col.input.transparent[1], col.input.transparent[2], col.input.transparent[3], max = 255, alpha = alpha)
col.output.transparent <- col2rgb(col.output)
col.output.transparent <- rgb(col.output.transparent[1], col.output.transparent[2], col.output.transparent[3], max = 255, alpha = alpha)


# PDF
if (PDF) {
  output.file <- "botUtilizationPlots.pdf"
  pdf(file = output.file,
      width = 11, height = 7,
      bg = bg.color,
      title = "Report"
  )
}


dataFull <- read.csv(data.csv, header = TRUE, sep = "\t", quote="\"", dec=".", fill = TRUE, comment.char="")
bots.per.level <- max(dataFull$Bot)
levels.num <- max(dataFull$Level)
dataFull <- cbind(dataFull, bot = bots.per.level*(dataFull$Level - 1) + dataFull$Bot)
#dataFull$bot <- bots.per.level*(dataFull$Level - 1) + dataFull$Bot


data <- dataFull[, c("bot", "Level", "Estimated.Start.Time", "Estimated.End.Time", "Estimated.Duration..milliseconds.", "Action.Type", "Marker", "Component")]
colnames(data) <- c("bot", "level", "startTime", "endTime", "duration", "actionType", "marker", "component")

#### Convert times in format "M:S.MS" to seconds
time.to.sec <- function(vec) {
	splt <- strsplit(vec, ":")
	minutes <- as.numeric(sapply(splt, function (x) if(length(x) == 2) x[1] else as.character(NA)))
	seconds <- as.numeric(sapply(splt, function (x) if(length(x) == 2) x[2] else as.character(NA)))
	minutes*60 + seconds
}

#### Converts startTime and endTime to seconds
fixTimes <- function(columnName) {
	t <- strptime(data[, columnName], "%M:%OS")
	t1 <- strptime(data[which(!is.na(t)), columnName], "%M:%OS")
	t2 <- strptime(data[which(is.na(t)), columnName], "%OS")

	#### Extract minutes + seconds
	t1 <- format(t1, "%M:%OS")
	t2 <- format(t2, "%M:%OS")

	t1 <- time.to.sec(t1)
	t2 <- time.to.sec(t2)

	###### Final set up
	data[, columnName] <<- as.numeric(0)
	data[which(!is.na(t)), columnName] <<- t1
	data[which(is.na(t)), columnName] <<- t2
}

fixTimes("startTime")
fixTimes("endTime")



######################### End of data preparation ##############################


data <- data[order(data$bot), ]
routes <- data
routes.no.trim <- routes

bots <- unique(data$bot)
bots <- sort(bots)
bots.num <- length(bots)
bots.per.page <- floor(BOT.UTILITY.BOTS.PER.PAGE/bots.per.level)*bots.per.level
pages <- ceiling(length(bots)/bots.per.page)




######################## NEW GANTT ########################
for (i in 1:pages) {

	par(mfrow = c(1, 1), oma=c(0,3,0,3), mar=c(0,0,0,3), mai=c(0,0,0,3))

	min.idx <- (i-1)*bots.per.page + 1
	max.idx <- i*bots.per.page
	bots.subset <- bots[which(bots >=min.idx)]
	bots.subset <- bots.subset[which(bots.subset <= max.idx)]
	data.subset <- data[is.element(data$bot, bots.subset), ]
	labels <- data.subset$bot
	starts <- data.subset$startTime

	ends <- data.subset$endTime
	gantt.data <- list(labels=labels, starts=starts, ends=ends)

	vgridlab <- seq(0, max(data$endTime, na.rm=T), by=60)
	vgridpos <- seq(0, max(data$endTime, na.rm=T), by=60)
	vgridlab <- round(vgridlab/60, 0)

	taskcolors <- factor(data.subset$taskType,
		levels=c("INPUT", "OUTPUT", "BLOCKER", "TRIMMED", "INPUT_CONTINUE", "OUTPUT_CONTINUE", "OUTPUT_RETURN_CASE", "DISABLE_BOT"))
	blocker.idx <- which(data.subset$routeType == "BLOCKER")
	taskcolors[blocker.idx] <- rep("BLOCKER", length(blocker.idx))
	levels(taskcolors) <- c(col.input, col.output, colors[3], col.trimmed, col.input.continue, col.output.continue, col.output.return.case, col.disable.bot)
	taskcolors <- as.character(taskcolors)

	xlim = c(min(data$endTime), max(data$endTime))

	label.cex <- 0.7
layout(rbind(1,2), heights=c(7,1))
	gantt.chart(gantt.data,
		vgridlab = vgridlab, vgridpos = vgridpos, label.cex = label.cex,
		xlab = "Time [min]", xlim = xlim,
		hgrid=T,
		main = paste("Bot utilization in time (levels ", min(data.subset$level),
			" - ", max(data.subset$level), ")", sep=""),
		taskcolors = taskcolors,
		border.col = "black"
	)

	#mtext("Bot id", side=2, line=2, cex.lab=0.8, las=3)
mtext("", side=3, line=2, las=3)
# setup for no margins on the legend
par(mar=c(0, 0, 0, 0))
par(oma=c(1,1,0,1))
#plot.new()
	legend('center', c("INPUT", "OUTPUT"),
		fill = c(col.input, col.output),
		title = "Task type",
		bg = "#FFFFFF", horiz = TRUE,  cex = 1.0, x.intersp=0.4, xjust=0.5, bty = "n")
	par(resetPar())
}



if (PDF) {
	dev.off()
}


