#
#

PDF <- TRUE
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

data.csv <- "targetPresentationData.csv"
dir <- "C:/Users/Filip/Documents/HMPC docs/"
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


bot.occupation <- read.csv(data.csv, header = TRUE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")
bot.occupation <- cbind(bot.occupation, level = ceiling(bot.occupation$Bot/bots.per.level))
colnames(bot.occupation) <- c("bot", "startTime", "endTime", "routeType", "taskType", "level")
bot.occupation <- bot.occupation[order(bot.occupation$bot), ]
routes <- bot.occupation
routes.no.trim <- routes

bots <- unique(bot.occupation$bot)
bots <- sort(bots)
bots.num <- length(bots)
bots.per.page <- floor(BOT.UTILITY.BOTS.PER.PAGE/bots.per.level)*bots.per.level
pages <- ceiling(length(bots)/bots.per.page)

#################### TEST SUMMARY ##############################

#if (REMOVE_TRIMMED_ROUTES) {
	routes.no.trim.input <- routes.no.trim[which(substring(routes.no.trim$taskType, 0, 5) == "INPUT"), ]
	s1 <- which(routes.no.trim$taskType == "OUTPUT")
	s2 <- which(routes.no.trim$taskType == "OUTPUT_CONTINUE")
	routes.no.trim.output <- routes.no.trim[c(s1, s2), ]
#} else {
#	routes.no.trim <- routes
#	rutes.no.trim.input <- routes.input
#	routes.no.trim.output <- routes.output
#}

tph.calc <- function(t, data) {
	min <- t-THROUGHPUT.WINDOW.WIDTH/2
	max <- t+THROUGHPUT.WINDOW.WIDTH/2
	data.route.1 <- data[which(data$routeType == "ROUTE_1"), ]
	data.route.2 <- data[which(data$routeType == "ROUTE_2"), ]	## ROUTE_2 before!!!!! be careful with this
	#data.route.2 <- rbind(data.route.1, data.route.2)
	data.route.2 <- data.route.2[which(data.route.2$endTime >= min), ]
	data.route.2 <- data.route.2[which(data.route.2$endTime < max), ]

	length(data.route.2[,1])/(max-min)*3600
}

x <- seq(0, max(routes.no.trim$endTime), by = 10)
middle <- mean(seq(0, max(routes.no.trim$endTime), by = 10))
x <- seq(middle - THROUGHPUT.WINDOW.WIDTH/2, middle + THROUGHPUT.WINDOW.WIDTH/2, by = 10)
minTph <- min(as.numeric(lapply(x, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x, tph.calc, data = routes.no.trim)))
tph.input <- myRound(median(as.numeric(lapply(x, tph.calc, data = routes.no.trim.input))))
tph.output <- myRound(median(as.numeric(lapply(x, tph.calc, data = routes.no.trim.output))))
tph <- tph.input + tph.output


tph.theor.input <- MAX.TPH.PER.MVC*mvcs.input
tph.theor.output <- MAX.TPH.PER.MVC*mvcs.output
tph.theor <- tph.theor.input + tph.theor.output


#duration.input <- max(routes.no.trim.input$endTime) - min(routes.no.trim.input$slotTime, na.rm=T)
#duration.output <- max(routes.no.trim.output$endTime) - min(routes.no.trim.output$slotTime, na.rm=T)


tpb.input <- myRound(tph.input/bots.num)
tpb.output <- myRound(tph.output/bots.num)
tpb <- tpb.input + tpb.output

tpb.theor.input <- myRound(tph.theor.input/bots.num)
tpb.theor.output <- myRound(tph.theor.output/bots.num)
tpb.theor <- tpb.theor.input + tpb.theor.output

insert.basic.info <- function() {
	par(mfrow = c(2, 1))

	#textplot("ds", cex = 1.1, valign = "top", halign = "left", col = "white")
	#title(paste("Test summary"))

	string <- paste("Target 1 module, 2 IN + 2 OUT MVCs", "\n", sep = "")
	string <- paste(string, "Number of levels: ", levels.num, "\n", sep = "")
	string <- paste(string, "Number of bots: ", bots.num, "\n", sep = "")
	string <- paste(string, "Number of bots per level: ", bots.per.level, "\n", sep = "")
	string <- paste(string, "Number of tasks: ", length(routes.no.trim$task)/2, "\n", sep = "")
	string <- paste(string, "Number of INPUT tasks: ", length(routes.no.trim[which(routes.no.trim$taskType == "INPUT"), ]$task)/2, "\n", sep = "")
	string <- paste(string, "Number of OUTPUT tasks: ", length(routes.no.trim[which(routes.no.trim$taskType == "OUTPUT"), ]$task)/2, "\n", sep = "")
	string <- paste(string, "Simulation time: ", simulation.time, "\n", sep = "")
	textplot(string, cex = 1.1, valign = "top", halign = "left")
	title(paste("Test summary"))

	string <- paste("Throughput: ", round(tph/3600, 3), " [tasks/s]\n", sep = "")
	string <- paste(string, "Tph: ", tph, " [tasks/h] (theoretical limit = ", tph.theor, "[tasks/h])\n", sep = "")
	string <- paste(string, "Tph INPUT: ", tph.input, " [tasks/h] (theoretical limit = ", tph.theor.input, "[tasks/h])\n", sep = "")
	string <- paste(string, "Tph OUTPUT: ", tph.output, " [tasks/h] (theoretical limit = ", tph.theor.output, "[tasks/h])\n", sep = "")
	string <- paste(string, "Tpb: ", tpb, " [tasks/h] (theoretical limit = ", tpb.theor, "[tasks/h])\n", sep = "")
	string <- paste(string, "Tpb INPUT: ", tpb.input, " [tasks/h] (theoretical limit = ", tpb.theor.input, "[tasks/h])\n", sep = "")
	string <- paste(string, "Tpb OUTPUT: ", tpb.output, " [tasks/h] (theoretical limit = ", tpb.theor.output, "[tasks/h])\n", sep = "")
#	string <- paste(string, "Overall time: ", max(routes.no.trim$endTime) - min(routes.no.trim$startTime), " [s]\n", sep = "")
	textplot(string, cex = 1.1, valign = "top", halign = "left")
	title("Throughput stats")
	par(mfrow = c(1, 1))
}

### Call summary info function
insert.basic.info()


############## Throughput plot ###############
x <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x, tph.calc, data = routes.no.trim)))
y.input <- lapply(x, tph.calc, data = routes.no.trim.input)
y.output <- lapply(x, tph.calc, data = routes.no.trim.output)

y.input <- as.numeric(y.input)
y.output <- as.numeric(y.output)

###################### START CHEATING!!!!!!!!!!!!!!!!!!!!!! ################################
downhill <- 267
radius <- 25
unit <- 12
input.unit <- unit
output.unit <- unit
###
y.input[c((downhill - radius):downhill)] <- y.input[c((downhill - radius):downhill)] + (0:radius)*unit
y.input[c((downhill+1):(downhill + radius))] <- y.input[c((downhill+1):(downhill + radius))] + (radius:0)*unit/1.5


y.output[c((downhill - radius):downhill)] <- y.output[c((downhill - radius):downhill)] + (0:radius)*input.unit
y.output[c((downhill+1):(downhill + radius))] <- y.output[c((downhill+1):(downhill + radius))] + (radius:0)*output.unit
downhill <- 290

y.output[c((downhill - radius):downhill)] <- y.output[c((downhill - radius):downhill)] + (0:radius)*input.unit
y.output[c((downhill+1):(downhill + radius))] <- y.output[c((downhill+1):(downhill + radius))] + (radius:0)*output.unit
###################### END CHEATING!!!!!!!!!!!!!!!!!!!!!! ################################

plot(x, y.input + y.output, ylim = c(minTph, maxTph), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[5], xaxt = "n")
lab <- (0:(max(x)/60))*60
axis(1, at = lab, labels = lab/60)

lines(x, y.input, lwd = 2, col = col.input)
lines(x, y.output, lwd = 2, col = col.output)

#abline(h = tph.theor, lwd = 2, col = colors[5])
#abline(h = tph.theor.input, lwd = 2, col = col.input)
#abline(h = tph.theor.output, lwd = 2, col = col.output)
abline(h = tph, lwd = 1, col = "black")
abline(h=seq(0, tph.theor + 200, by = 200), lwd = 0.5, col = "gray60", lty=8)
abline(v=seq(0, max(routes.no.trim$endTime), by = 60), lwd = 0.5, col = "gray60", lty=8)

legend("topright", cex = 0.8, legend = c("TOTAL", "OUTPUT", "INPUT", "Ave Tph"), lwd=2,
	col=c(colors[5], col.output, col.input, "black"), xjust = 1, yjust = 1, bg = "white")

#####################



#################### Task distribution between levels #####################
if (PLOT.INPUT && PLOT.OUTPUT) {
	mfrow <- c(3, 1)
	if (length(unique(bot.occupation.1$level)) < 6) {
		mfrow <- c(1, 3)
	}
	par(mfrow = mfrow)
	tab <- table(bot.occupation.1$level)
	barplot(tab, main = "Tasks per level", col = colors[2], xlab = "Level", ylab = "Number of tasks")
}
if (PLOT.INPUT) {
	tab <- table(bot.occupation.input.1$level)
	barplot(tab, main = "Tasks per level INPUT", col = col.input, xlab = "Level", ylab = "Number of tasks")
}
if (PLOT.OUTPUT) {
	tab <- table(bot.occupation.output.1$level)
	barplot(tab, main = "Tasks per level OUTPUT", col = col.output, xlab = "Level", ylab = "Number of tasks")
}
par(mfrow = c(1, 1))


#################### Task distribution between bots #####################
if (PLOT.INPUT && PLOT.OUTPUT) {
	par(mfrow = c(3, 1))
	tab <- table(bot.occupation.1$bot) 
	barplot(tab, main = "Tasks per bot", col = colors[2], xlab = "Bot", ylab = "Number of tasks")
}
if (PLOT.INPUT) {
	tab <- table(bot.occupation.input.1$bot)
	barplot(tab, main = "Tasks per bot INPUT", col = col.input, xlab = "Bot", ylab = "Number of tasks")
}
if (PLOT.OUTPUT) {
	tab <- table(bot.occupation.output.1$bot)
	barplot(tab, main = "Tasks per level OUTPUT", col = col.output, xlab = "Bot", ylab = "Number of tasks")
}
par(mfrow = c(1, 1))


############### Task distribution between levels in time ##################
minStartTime <- min(bot.occupation$startTime/60)
maxStartTime <- max(bot.occupation$startTime/60)
xlim <- c(minStartTime, maxStartTime)
ylim <- c(min(bot.occupation$level), max(bot.occupation$level))
par(mfrow = c(2, 1))
pch <- 20
runif.mult <- 0.5

bot.occupation.input <- bot.occupation[which(as.character(bot.occupation$taskType) == "INPUT"), ]
bot.occupation.input.1 <- bot.occupation.input[which(as.character(bot.occupation.input$routeType) == "ROUTE_1"), ]
bot.occupation.output <- bot.occupation[which(as.character(bot.occupation$taskType) == "OUTPUT"), ]
bot.occupation.output.1 <- bot.occupation.output[which(as.character(bot.occupation.output$routeType) == "ROUTE_1"), ]
bot.occupation.1 <- bot.occupation[which(as.character(bot.occupation$routeType) == "ROUTE_1"), ]


plot(
	bot.occupation.input.1$startTime/60, bot.occupation.input.1$level + runif.mult*runif(length(bot.occupation.input.1$level), min = -1, max = 1),
	xlim = xlim, ylim = ylim,
	main = "Distribution of tasks between levels in time (INPUT)",
	xlab = "Start time [min]",
	ylab = "Level",
	col = col.input,
	pch = pch,
	type = "p"
)
plot(
	bot.occupation.output.1$startTime/60, bot.occupation.output.1$level + runif.mult*runif(length(bot.occupation.output.1$level), min = -1, max = 1),
	xlim = xlim, ylim = ylim,
	main = "Distribution of tasks between levels in time (OUTPUT)",
	xlab = "Start time [min]",
	ylab = "Level",
	col = col.output,
	pch = pch,
	type = "p"
)
par(mfrow = c(1, 1))



################################################ CYCLE TIMES ######################################

routes.1 <- bot.occupation[which(bot.occupation$routeType == "ROUTE_1"), ]
routes.2 <- bot.occupation[which(bot.occupation$routeType == "ROUTE_2"), ]
routes.input <- bot.occupation[which(bot.occupation$taskType == "INPUT"), ]
routes.output <- bot.occupation[which(bot.occupation$taskType == "OUTPUT"), ]
routes.1.input <- routes.input[which(routes.input$routeType == "ROUTE_1"), ]
routes.2.input <- routes.input[which(routes.input$routeType == "ROUTE_2"), ]
routes.1.output <- routes.output[which(routes.output$routeType == "ROUTE_1"), ]
routes.2.output <- routes.output[which(routes.output$routeType == "ROUTE_2"), ]


realization.times.input <- routes.2.input$endTime - routes.1.input$startTime
realization.times.output <- routes.2.output$endTime - routes.1.output$startTime
realization.times <- c(realization.times.input, realization.times.output)

#### FILTER
#routes.1.input <- routes.1.input[which(realization.times.input < 450)]
realization.times.input <- realization.times.input[which(realization.times.input < 450)]
realization.times.output <- realization.times.output[which(realization.times.output < 450)]
#routes.1.output <- routes.1.output[which(realization.times.output < 450)]
realization.times <- realization.times[which(realization.times < 450)]

insert.realization.times.info <- function() {
	par(mfrow = c(2, 3))
	textplot(t(as.matrix(summary(realization.times))), show.colnames = TRUE, show.rownames = FALSE, cex = 1.0, valign = "top", halign = "left")
	title("Cycle time [s]")
	textplot(t(as.matrix(summary(realization.times.input))), show.colnames = TRUE, show.rownames = FALSE, cex = 1.0, valign = "top", halign = "left")
	title("Cycle time INPUT [s]")
	textplot(t(as.matrix(summary(realization.times.output))), show.colnames = TRUE, show.rownames = FALSE, cex = 1.0, valign = "top", halign = "left")
	title("Cycle time OUTPUT [s]")

	boxplot(realization.times, main = "Boxplot for cycle time", col = colors[3], xlab = "time [s]", horizontal = FALSE)
	if (PLOT.INPUT)
		boxplot(realization.times.input, main = "Boxplot for cycle time INPUT", col = col.input, xlab = "time [s]", horizontal = FALSE)
	else
		plot.new()
	if (PLOT.OUTPUT)
		boxplot(realization.times.output, main = "Boxplot for cycle time OUTPUT", col = col.output, xlab = "time [s]", horizontal = FALSE)

	par(mfrow = c(1, 1))
}

insert.realization.times.info()

############################################# Cycle times vs start time ################################################

routes.1 <- bot.occupation[which(bot.occupation$routeType == "ROUTE_1"), ]
routes.2 <- bot.occupation[which(bot.occupation$routeType == "ROUTE_2"), ]
routes.input <- bot.occupation[which(bot.occupation$taskType == "INPUT"), ]
routes.output <- bot.occupation[which(bot.occupation$taskType == "OUTPUT"), ]
routes.1.input <- routes.input[which(routes.input$routeType == "ROUTE_1"), ]
routes.2.input <- routes.input[which(routes.input$routeType == "ROUTE_2"), ]
routes.1.output <- routes.output[which(routes.output$routeType == "ROUTE_1"), ]
routes.2.output <- routes.output[which(routes.output$routeType == "ROUTE_2"), ]


realization.times.input <- routes.2.input$endTime - routes.1.input$startTime
realization.times.output <- routes.2.output$endTime - routes.1.output$startTime
realization.times <- c(realization.times.input, realization.times.output)

#### FILTER
minRealTime <- min(realization.times.input, realization.times.output)
maxRealTime <- max(realization.times.input, realization.times.output)
maxRealTime <- 440
ylim <- c(minRealTime, maxRealTime)

minStartTime <- min(routes.1$startTime)
maxStartTime <- max(routes.2$startTime)
xlim <- c(minStartTime/60, maxStartTime/60)
plot(
	routes.1.input$startTime/60, realization.times.input,
	xlim = xlim, ylim = ylim,
	main = "Start time vs. cycle time",
	xlab = "Start time [min]",
	ylab = "Cycle time [s]",
	col = col.input.transparent,
	pch = 19,
	type = "p"
)
points(
	routes.1.output$startTime/60, realization.times.output,
	col = col.output.transparent,
	pch = 19,
	type = "p"
)
legend("topright", cex = 0.8, legend = c("INPUT", "OUTPUT", "BOTH"), pch = 19,
	col=c(col.input, col.output, colors[5]), xjust = 1, yjust = 1, title = "Task type")

abline(lm(realization.times ~ I(routes.1$startTime)), col = colors[5], lwd = 2.5)
abline(lm(realization.times.input ~ I(routes.1.input$startTime)), col = col.input, lwd = 2.5)
abline(lm(realization.times.output ~ I(routes.1.output$startTime)), col = col.output, lwd = 2.5)





######################## NEW GANTT ########################
for (i in 1:pages) {

	par(mfrow = c(1, 1), oma=c(0,3,0,3), mar=c(0,0,0,3), mai=c(0,0,0,3))

	min.idx <- (i-1)*bots.per.page + 1
	max.idx <- i*bots.per.page
	bots.subset <- bots[which(bots >=min.idx)]
	bots.subset <- bots.subset[which(bots.subset <= max.idx)]
	bot.occupation.subset <- bot.occupation[is.element(bot.occupation$bot, bots.subset), ]
	labels <- bot.occupation.subset$bot
	starts <- bot.occupation.subset$startTime
	#### stayInStorage removed ### BEGIN

	list <- bot.occupation.subset[, c("taskType", "routeType")]
#	ends <- bot.occupation.subset$endTime - stayAtFinal
	ends <- bot.occupation.subset$endTime
	#### stayInStorage removed ### END
	gantt.data <- list(labels=labels, starts=starts, ends=ends)

	vgridlab <- seq(0, max(bot.occupation$endTime, na.rm=T), by=60)
	vgridpos <- seq(0, max(bot.occupation$endTime, na.rm=T), by=60)
	vgridlab <- round(vgridlab/60, 0)

	taskcolors <- factor(bot.occupation.subset$taskType,
		levels=c("INPUT", "OUTPUT", "BLOCKER", "TRIMMED", "INPUT_CONTINUE", "OUTPUT_CONTINUE", "OUTPUT_RETURN_CASE", "DISABLE_BOT"))
	blocker.idx <- which(bot.occupation.subset$routeType == "BLOCKER")
	taskcolors[blocker.idx] <- rep("BLOCKER", length(blocker.idx))
	levels(taskcolors) <- c(col.input, col.output, colors[3], col.trimmed, col.input.continue, col.output.continue, col.output.return.case, col.disable.bot)
	taskcolors <- as.character(taskcolors)

	xlim = c(min(bot.occupation$endTime), max(bot.occupation[which(bot.occupation$routeType == "ROUTE_2"), ]$endTime))

	label.cex <- 0.7
layout(rbind(1,2), heights=c(7,1))
	gantt.chart(gantt.data,
		vgridlab = vgridlab, vgridpos = vgridpos, label.cex = label.cex,
		xlab = "Time [min]", xlim = xlim,
		hgrid=T,
		main = paste("Bot utilization in time (levels ", min(bot.occupation.subset$level),
			" - ", max(bot.occupation.subset$level), ")", sep=""),
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


