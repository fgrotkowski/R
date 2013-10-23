#### Compute time when bot has task assigned
#assigned.time <- aggregate(cbind(routes.comb[, c("bot.1")], time = realization.times), by = list(bot = routes.comb$bot.1), FUN = sum)
#max.end.time <- max(routes.comb$endTime.2)
#min.start.time <-min(routes.comb$startTime.1)
#work.interval <- max.end.time - min.start.time
#not.assigned.time <- work.interval - assigned.time$time
#
#tab <- t(cbind(bot = assigned.time$bot, assigned.time = assigned.time$time, not.assigned.time))
#tab <- tab[-1,]
#colnames(tab) <- assigned.time$bot
#
#barplot(tab, main = "Bot utilization plot", col = colors[c(6,2)], xlab = "Bot id", ylab = "time [s]")
#legend('topleft', c("task assigned", "without a task"), fill = colors[c(6,2)], bg = "#FFFFFF", horiz = TRUE,  cex = 1)

#### With blockers
routes.util <- routes[which(routes$task != INFINITE_ROUTE_TASK_ID), ]
assigned.time <- aggregate(cbind(routes.util$bot, level = routes.util$level, time = realization.times.with.blockers/60), by = list(bot = routes.util$bot, level = routes.util$level), FUN = sum)
max.end.time <- max(routes.util$endTime/60)
min.start.time <-min(routes.util$startTime/60)
work.interval <- max.end.time - min.start.time
not.assigned.time <- work.interval - assigned.time$time

tab <- t(cbind(bot = assigned.time$bot, assigned.time = assigned.time$time, not.assigned.time, level = assigned.time$level))
tab <- tab[-1,]
colnames(tab) <- assigned.time$bot

bots <- sort(unique(routes.util$bot))
bots.per.page <- floor(BOT.UTILITY.BOTS.PER.PAGE/bots.per.level)*bots.per.level
if (GIL.CONFIG)
	bots.per.page <- length(bots)
pages <- ceiling(length(bots)/bots.per.page)
for (i in 1:pages) {
	min.idx <- (i-1)*bots.per.page + 1
	max.idx <- i*bots.per.page
	bots.subset <- bots[which(bots >=min.idx)]
	bots.subset <- bots.subset[which(bots.subset <= max.idx)]

	tab.subset <- tab[, is.element(colnames(tab), bots.subset)]
	main <- paste("Bot utilization in time (levels ", min(tab.subset["level", ]), " - ", max(tab.subset["level", ]), ")", sep="")
	barplot(tab.subset[1:2, ], main = main, col = colors[c(6,2)], xlab = "Bot id", ylab = "time [min]")
	legend('topleft', c("task assigned", "without a task"), fill = colors[c(6,2)], bg = "#FFFFFF", horiz = TRUE,  cex = 1)
}

#
##### Gantt diagram
#### Without blockers
bot.occupation <- routes.comb.with.trimmed[, c("bot.1", "startTime.1", "endTime.2", "level.1")]
colnames(bot.occupation) <- c("bot", "startTime", "endTime", "level")
### With blockers here
bot.occupation <- routes[, c("task", "bot", "startTime", "endTime", "level", "routeType", "taskType", "stayAtFinal", "isTrimmed")]
colnames(bot.occupation) <- c("task", "bot", "startTime", "endTime", "level", "routeType", "taskType", "stayAtFinal", "isTrimmed")
bot.occupation <- bot.occupation[order(bot.occupation$bot), ]

levels(bot.occupation[, "taskType"]) <- c(levels(bot.occupation[, "taskType"]), "TRIMMED")
bot.occupation[which(bot.occupation$isTrimmed == "true"), "taskType"] <- "TRIMMED"
#bot.occupation[which(bot.occupation$isTrimmed == "true"), "routeType"] <- "BLOCKER"

#theme_text = function(...)
#		ggplot2::theme_text(colour="black", ...)
#gantt.colors <- colorRampPalette(brewer.pal(9,"Blues")[5:9])(pages)
##gantt.colors <- colorRampPalette(brewer.pal(9,"RdBu")[1:9])(pages)
#for (i in 1:pages) {
#	min.idx <- (i-1)*bots.per.page + 1
#	max.idx <- i*bots.per.page
#	bots.subset <- bots[which(bots >=min.idx)]
#	bots.subset <- bots.subset[which(bots.subset <= max.idx)]
#	bot.occupation.subset <- bot.occupation[is.element(bot.occupation$bot, bots.subset), ]
#	bp <- ggplot(bot.occupation.subset) + 
#	    geom_segment(aes(x=startTime, xend=endTime, y=bot, yend=bot), size=1, colour=colors[5]) +
#	    xlab("Duration [s]") +
#		opts(
#			title = paste("Bot utilization in time (levels ",
#				min(bot.occupation.subset$level), " - ", max(bot.occupation.subset$level), ")", sep=""),
#			plot.background = theme_rect(fill=bg.color, colour=bg.color),
#			axis.text.x = theme_text(),
#	    		axis.text.y = theme_text()
#		)
#	plot(bp + opts(legend.position="none"))
#}
#

######################## NEW GANTT ########################
### Start with legend
plot.new()
legend('center', c("INPUT", "OUTPUT", "BLOCKER", "TRIMMED", "INPUT_CONTINUE", "OUTPUT_CONTINUE", "OUTPUT_RETURN_CASE", "DISABLE_BOT"),
	fill = c(col.input, col.output, colors[3], col.trimmed, col.input.continue, col.output.continue, col.output.return.case, col.disable.bot),
	title = "Gantt diagram legend. Task type:",
	bg = "#FFFFFF", horiz = FALSE,  cex = 1, x.intersp=0.4, xjust=0.5, bty = "n")

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
	getStayAtFinal <- function (x) {
		ret <- 0
		if (x[1] == "INPUT" && x[2] == "ROUTE_2")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.STORAGE.INPUT
		if (x[1] == "OUTPUT" && x[2] == "ROUTE_1")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.STORAGE.OUTPUT
		if (x[1] == "INPUT" && x[2] == "ROUTE_1")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.MVC.INPUT
		if (x[1] == "OUTPUT" && x[2] == "ROUTE_2")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.MVC.OUTPUT
		ret
	}
	list <- bot.occupation.subset[, c("taskType", "routeType", "stayAtFinal")]
	stayAtFinal <- apply(list, 1, FUN=getStayAtFinal)
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
	if (GIL.CONFIG)
		label.cex <- 0.2
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
	legend('center', c("INPUT", "OUTPUT", "BLOCKER", "TRIMMED", "INPUT_CONTINUE", "OUTPUT_CONTINUE", "OUTPUT_RETURN_CASE"),
		fill = c(col.input, col.output, colors[3], col.trimmed, col.input.continue, col.output.continue, col.output.return.case),
		title = "Task/route type",
		bg = "#FFFFFF", horiz = TRUE,  cex = 0.6, x.intersp=0.4, xjust=0.5, bty = "n")
	par(resetPar())
}


######################## BOTS UTILIZED IN TIME ########################
labels <- bot.occupation$bot
starts <- bot.occupation$startTime
#### stayInStorage removed ### BEGIN
getStayAtFinal <- function (x) {
		ret <- 0
		if (x[1] == "INPUT" && x[2] == "ROUTE_2")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.STORAGE.INPUT
		if (x[1] == "OUTPUT" && x[2] == "ROUTE_1")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.STORAGE.OUTPUT
		if (x[1] == "INPUT" && x[2] == "ROUTE_1")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.MVC.INPUT
		if (x[1] == "OUTPUT" && x[2] == "ROUTE_2")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.MVC.OUTPUT
		ret
}
list <- bot.occupation[, c("taskType", "routeType", "stayAtFinal")]
stayAtFinal <- apply(list, 1, FUN=getStayAtFinal)
ends <- bot.occupation$endTime - stayAtFinal
#### stayInStorage removed ### END

data <- cbind(level=bot.occupation$level, taskType = bot.occupation$taskType, start=starts, end=ends)
plot.data <- rbind(
	cbind(level = data[, "level"], taskType = data[, "taskType"], time = data[, "start"], val = 1),
	cbind(level = data[, "level"], taskType = data[, "taskType"], time = data[, "end"], val = -1)
)
plot.data <- plot.data[order(plot.data[, "time"]) ,]
plot.data <- cbind(
	plot.data[, c("time", "val")],
	timeWithOffset=plot.data[, "time"]+ifelse(plot.data[, "taskType"] == "OUTPUT", 1, -1)*plot.data[, "level"]*TIME.BETWEEN.PLATFORMS
)
plot.data <- aggregate(plot.data, by=list(time=plot.data[, "time"], timeWithOffset=plot.data[, "timeWithOffset"]), FUN=sum)

bar.size <- 0
bar.size.vec <- c()
for (val in plot.data[, "val"]) {
	bar.size <- bar.size + val
	bar.size.vec <- c(bar.size.vec, bar.size)
}
plot.data <- cbind(plot.data, bar.size = bar.size.vec)

xlim = c(min(bot.occupation$endTime), max(bot.occupation[which(bot.occupation$routeType == "ROUTE_2"), ]$endTime))/60
plot(
	plot.data[, "timeWithOffset"]/60, plot.data[, "bar.size"],
	xlab = "MVC Time [min]",
	ylab = "Number of bots",
	ylim = c(0, bots.num), xlim = xlim,
	type = "l",
	col = colors[5],
	lwd = 2,
	main = paste("Number of working bots in time (all levels)", sep="")
)
abline(h = bots.num, lwd = 2, col = colors[5])

###### Missed slots ######
ret <- c()
missed.slots.table <- function(mvcs, inOrOut, ids=1:length(mvcs), color) {
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		attach(one.mvc.data)
		missed <- c(as.numeric(slotNr)[1], as.numeric(slotNr))
		missed <- missed[-length(missed)]
		missed <- slotNr - missed
		missed[-1] <- missed[-1] - 1
		if (length(ret) == 0)
			ret <- cbind(missed=missed, slotNr=slotNr, slotTime=slotNr*TIME.BETWEEN.PLATFORMS)
		else {
			ret <- rbind(ret, cbind(missed=missed, slotNr=slotNr, slotTime=slotTime))
		}
		detach(one.mvc.data)
	}
	ret
}

if (PLOT.OUTPUT) {
	missed.slots.tab.output <- missed.slots.table(mvcs.output, "OUTPUT", ids.output, col.output)
}
if (PLOT.INPUT) {
	missed.slots.tab.input <- missed.slots.table(mvcs.input, "INPUT", ids.input, col.input)
}

MIN.MISSED.OUTPUT <- 4
MIN.MISSED.INPUT <- 2
missed.slots.tab.output <- missed.slots.tab.output[which(missed.slots.tab.output[, "missed"] > MIN.MISSED.OUTPUT) , ]
missed.slots.tab.input <- missed.slots.tab.input[which(missed.slots.tab.input[, "missed"] > MIN.MISSED.INPUT) , ]

abline(v = missed.slots.tab.output[, "slotTime"]/60, lwd = 1, col = col.output)
abline(v = missed.slots.tab.input[, "slotTime"]/60, lwd = 1, col = col.input)
legend('topright', c(paste(">", MIN.MISSED.OUTPUT," OUTPUT platform missed"), paste(">", MIN.MISSED.INPUT," INPUT platform missed")), fill = c(col.output, col.input), bg = "#FFFFFF", horiz = FALSE,  cex = 0.8)



bots.per.page <- bots.per.level
pages <- ceiling(length(bots)/bots.per.page)
######################## BOTS UTILIZED IN TIME ONE BY ONE ########################
par(mfrow = c(3, 1))
for (i in 1:pages) {
	min.idx <- (i-1)*bots.per.page + 1
	max.idx <- i*bots.per.page
	bots.subset <- bots[which(bots >=min.idx)]
	bots.subset <- bots.subset[which(bots.subset <= max.idx)]
	bot.occupation.subset <- bot.occupation[is.element(bot.occupation$bot, bots.subset), ]
	labels <- bot.occupation.subset$bot
	starts <- bot.occupation.subset$startTime
	#### stayInStorage removed ### BEGIN
	getStayAtFinal <- function (x) {
		ret <- 0
		if (x[1] == "INPUT" && x[2] == "ROUTE_2")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.STORAGE.INPUT
		if (x[1] == "OUTPUT" && x[2] == "ROUTE_1")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.STORAGE.OUTPUT
		if (x[1] == "INPUT" && x[2] == "ROUTE_1")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.MVC.INPUT
		if (x[1] == "OUTPUT" && x[2] == "ROUTE_2")
			ret <- as.numeric(x[3]) - MIN.STAY.IN.MVC.OUTPUT
		ret
	}
	list <- bot.occupation.subset[, c("taskType", "routeType", "stayAtFinal")]
	stayAtFinal <- apply(list, 1, FUN=getStayAtFinal)
	ends <- bot.occupation.subset$endTime - stayAtFinal
	#### stayInStorage removed ### END

	data <- cbind(level=bot.occupation.subset$level, start=starts, end=ends)
	plot.data <- rbind(
		cbind(level = data[, "level"], time = data[, "start"], val = 1),
		cbind(level = data[, "level"], time = data[, "end"], val = -1)
	)
	plot.data <- plot.data[order(plot.data[, "time"]) ,]
	plot.data <- aggregate(plot.data, by=list(time=plot.data[, "time"], level = plot.data[, "level"]), FUN=sum)

	bar.size <- 0
	bar.size.vec <- c()
	for (val in plot.data[, "val"]) {
		bar.size <- bar.size + val
		bar.size.vec <- c(bar.size.vec, bar.size)
	}
	plot.data <- cbind(plot.data, bar.size = bar.size.vec)
	
	plot(
		plot.data[, "time"]/60, plot.data[, "bar.size"],
		xlab = "Time [min]",
		ylab = "Number of bots",
		ylim = c(0, bots.per.level), xlim = xlim,
		type = "l",
		col = colors[5],
		lwd = 2,
		main = paste("Number of working bots in time (level ", max(bot.occupation.subset$level), ")", sep="")
	)
	abline(v = (missed.slots.tab.output[, "slotTime"]+plot.data[, "level"]*TIME.BETWEEN.PLATFORMS)/60, lwd = 1, col = col.output)
	abline(v = (missed.slots.tab.input[, "slotTime"]-plot.data[, "level"]*TIME.BETWEEN.PLATFORMS)/60, lwd = 1, col = col.input)
}
par(resetPar())


################################ DUAL-CYCLE RATIO #################################################

dual.cycle.ratio <- c()
b <- bots[1]
for (b in bots) {
	bot.data <- bot.occupation[which(bot.occupation$bot == b), ]
#	attach(bot.data)
	bot.data <- bot.data[which(bot.data$routeType == "ROUTE_1"), ]
	bot.data <- bot.data[order(bot.data$startTime), ]
	tt <- bot.data$taskType
	prevTT <- tt[1]
	cnt <- 0
	for (t in tt[-1]) {
		if (t != prevTT)
			cnt <- cnt + 1
		prevTT <- t
	}
	if (length(dual.cycle.ratio) == 0)
		dual.cycle.ratio <- c(b, cnt, cnt/(length(tt) - 1))
	else
		dual.cycle.ratio <- rbind(dual.cycle.ratio, c(b, cnt, cnt/(length(tt) - 1)))
	
}

colnames(dual.cycle.ratio) <- c("bot", "dual.cycles", "ratio")
dual.cycle.ratio[, "ratio"] <- dual.cycle.ratio[, "ratio"]*100

#plot(dual.cycle.ratio[, "bot"], dual.cycle.ratio[, "dual.cycles"], col = colors[5], pch = 19, xlab = "Bot", ylab = "Number of dual-cycles", main = "Number of dual cycles")
#abline(h = mean(dual.cycle.ratio[, "dual.cycles"]), col = colors[5], lwd = 2)

plot(dual.cycle.ratio[, "bot"], dual.cycle.ratio[, "ratio"], col = colors[5], pch = 19, xlab = "Bot", ylab = "Dual cycle %", main = "% of max dual-cycles done by each bot")
abline(h = mean(dual.cycle.ratio[, "ratio"]), col = colors[5], lwd = 2)


