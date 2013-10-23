############################################### BOT MOVEMENT #################################################

################# INCLUDE BLOCKER TO CYCLE TIME ###################
include.blockers.to.tasks <- function(bot.routes) {
	blocker.start <- -1
	blocker.end <- -1
	duration <- 0
	for (i in 1:length(bot.routes[,1])) {
		# cumulate blockers
		bot.routes[i, "routeType"]
		if (bot.routes[i, "routeType"] == "BLOCKER") {
			if (blocker.start < 0) blocker.start <- bot.routes[i, "startTime"]
			blocker.end <- max(blocker.end, bot.routes[i, "endTime"])
			duration <- duration + bot.routes[i, "endTime"]-bot.routes[i, "startTime"]
		}
		if (bot.routes[i, "routeType"] != "BLOCKER") {
			if (duration > 0) {
				if (bot.routes[i, "routeType"] == "ROUTE_1")
					bot.routes[i, "startTime"] <- min(blocker.start, bot.routes[i, "startTime"])
				if (bot.routes[i, "routeType"] == "ROUTE_2")
					bot.routes[i, "endTime"] <- min(blocker.end, bot.routes[i, "endTime"])
				bot.routes[i, "time"] <- bot.routes[i, "time"] + duration
			}
			blocker.start <- -1
			blocker.end <- -1
			duration <- 0
		}
	}
	bot.routes[which(bot.routes$routeType != "BLOCKER"), ]
}

## test
#include.blockers.to.tasks(bot.routes)


bots <- sort(unique(routes$bot))
fixed.routes <- c()
for (bot in bots) {
	bot.routes <- routes[which(routes$bot == bot), ]
	bot.routes <- bot.routes[order(bot.routes$startTime), ]
	bot.routes <- include.blockers.to.tasks(bot.routes)
	fixed.routes <- rbind(fixed.routes, bot.routes)
}

### Remove trimmed routes
fixed.routes <- fixed.routes[which(fixed.routes$isTrimmed == "false"), ]

routes.1 <- fixed.routes[which(fixed.routes$routeType == "ROUTE_1"), ]
routes.2 <- fixed.routes[which(fixed.routes$routeType == "ROUTE_2"), ]
routes.input <- fixed.routes[which(fixed.routes$taskType == "INPUT"), ]
routes.output <- fixed.routes[which(fixed.routes$taskType == "OUTPUT"), ]
routes.1.input <- routes.input[which(routes.input$routeType == "ROUTE_1"), ]
routes.2.input <- routes.input[which(routes.input$routeType == "ROUTE_2"), ]
routes.1.output <- routes.output[which(routes.output$routeType == "ROUTE_1"), ]
routes.2.output <- routes.output[which(routes.output$routeType == "ROUTE_2"), ]
by <- c("task", "bot", "level", "taskType", "mvc")
routes.comb <- merge(routes.1, routes.2, by = by, suffixes = c(".1", ".2"))
routes.comb.input <- merge(routes.1.input, routes.2.input, by = by, suffixes = c(".1", ".2"))
routes.comb.output <- merge(routes.1.output, routes.2.output, by = by, suffixes = c(".1", ".2"))



### Cycle time analysis
#realization.times <- routes.comb$endTime.2 - routes.comb$startTime.1
#realization.times.input <- routes.comb.input$endTime.2 - routes.comb.input$startTime.1
#realization.times.output <- routes.comb.output$endTime.2 - routes.comb.output$startTime.1
realization.times <- routes.comb$time.1 + routes.comb$time.2 + routes.comb$startTime.2 - routes.comb$endTime.1
realization.times.input <- routes.comb.input$time.1 + routes.comb.input$time.2
	+ routes.comb.input$startTime.2 - routes.comb.input$endTime.1
realization.times.output <- routes.comb.output$time.1 + routes.comb.output$time.2
	+ routes.comb.output$startTime.2 - routes.comb.output$endTime.1

### Cycle time with blockers
routes.cycle <- routes[which(routes$task != INFINITE_ROUTE_TASK_ID), ]
realization.times.with.blockers <- routes.cycle$endTime - routes.cycle$startTime

#### 
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
	list <- routes.cycle[, c("taskType", "routeType", "stayAtFinal")]
	stayAtFinal <- apply(list, 1, FUN=getStayAtFinal)
	realization.times.with.blockers <- routes.cycle$endTime - routes.cycle$startTime - stayAtFinal
	#### stayInStorage removed ### END

##### FUNCTIONS
generateHistogram <- function (data, subtitle = "", color = colors[3]) {
	title <- paste(paste("Cycle time (", length(data), " tasks)", sep = ""), subtitle, sep = "\n")
	hist(
		data, main = title,
		breaks = hist.buckets,
		xlab = "Realization time [s]", col = color
	)
}

generateDensity <- function (data, subtitle = "", color = colors[5]) {
	title <- paste(paste("Cycle time (", length(data), " tasks)", sep = ""), subtitle, sep = "\n")
	plot(
		density(data),
		main = title,
		xlab = "Realization time [s]",
		lwd = 3,
		col = color
	)
}

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

insert.route.segment.info <- function(data, reqType = "", r1.title = "", r2.title = "", mvc.title = "",
		stay.in.storage.title = "", color = colors[3]) {
	cmar = 0.8
	par(mfrow = c(2, 4))
	#r1.data <- data$endTime.1 - data$startTime.1 - (reqType == "INPUT")*data$stayAtFinal.1
	#r2.data <- data$endTime.2 - data$startTime.2 - (reqType == "OUTPUT")*data$stayAtFinal.2
	r1.data <- data$time.1 - (reqType == "INPUT")*data$stayAtFinal.1
	r2.data <- data$time.2 - (reqType == "OUTPUT")*data$stayAtFinal.2
	if (reqType == "INPUT") {
		mvc.data <- data$stayAtFinal.1
		stay.in.storage <- data$stayAtFinal.2
		r2.data <- data$time.2 - data$stayAtFinal.2
	}
	if (reqType == "OUTPUT") {
		mvc.data <- data$stayAtFinal.2
		stay.in.storage <- data$stayAtFinal.1
		r1.data <- data$time.1 - data$stayAtFinal.1
	}
	#mvc.data <- mvc.data + data$startTime.2 - data$endTime.1

	textplot(t(as.matrix(summary(r1.data))), show.colnames = TRUE, show.rownames = FALSE,
		cex = 1.0, valign = "top", halign = "left", cmar = cmar)
	title(paste(r1.title, reqType, "[s]"))

	if (reqType == "OUTPUT") {
		textplot(t(as.matrix(summary(stay.in.storage))), show.colnames = TRUE, show.rownames = FALSE,
			cex = 1.0, valign = "top", halign = "left", cmar = cmar)
		title(paste(stay.in.storage.title, reqType, "[s]"))
	}

	if (reqType == "INPUT") {
		textplot(t(as.matrix(summary(mvc.data))), show.colnames = TRUE, show.rownames = FALSE,
			cex = 1.0, valign = "top", halign = "left", cmar = cmar)
		title(paste(mvc.title, reqType, "[s]"))
	}
	textplot(t(as.matrix(summary(r2.data))), show.colnames = TRUE, show.rownames = FALSE,
			cex = 1.0, valign = "top", halign = "left", cmar = cmar)
	title(paste(r2.title, reqType, "[s]"))
	if (reqType == "OUTPUT") {
		textplot(t(as.matrix(summary(mvc.data))), show.colnames = TRUE, show.rownames = FALSE,
			cex = 1.0, valign = "top", halign = "left", cmar = cmar)
		title(paste(mvc.title, reqType, "[s]"))
	}

	if (reqType == "INPUT") {
		textplot(t(as.matrix(summary(stay.in.storage))), show.colnames = TRUE, show.rownames = FALSE,
			cex = 1.0, valign = "top", halign = "left", cmar = cmar)
		title(paste(stay.in.storage.title, reqType, "[s]"))
	}
	####
	boxplot(r1.data, main = "", col = color, xlab = "time [s]", horizontal = FALSE)
	title(paste(r1.title, reqType, "[s]"))
	if (reqType == "OUTPUT") {
		boxplot(stay.in.storage, main = "", col = color, xlab = "time [s]", horizontal = FALSE)
		title(paste(stay.in.storage.title, reqType, "[s]"))
	}
	if (reqType == "INPUT") {
		boxplot(mvc.data, main = "", col = color, xlab = "time [s]", horizontal = FALSE)
		title(paste(mvc.title, reqType, "[s]"))
	}
	boxplot(r2.data, main = "", col = color, xlab = "time [s]", horizontal = FALSE)
	title(paste(r2.title, reqType, "[s]"))
	if (reqType == "OUTPUT") {
		boxplot(mvc.data, main = "", col = color, xlab = "time [s]", horizontal = FALSE)
		title(paste(mvc.title, reqType, "[s]"))
	}

	if (reqType == "INPUT") {
		boxplot(stay.in.storage, main = "", col = color, xlab = "time [s]", horizontal = FALSE)
		title(paste(stay.in.storage.title, reqType, "[s]"))
	}

	par(mfrow = c(1, 1))
}

insert.realization.times.info()
if (PLOT.INPUT) insert.route.segment.info(routes.comb.input, "INPUT", "to MVC time", "to storage time", "@MVC time", "Stay in storage", color = col.input)
if (PLOT.OUTPUT) insert.route.segment.info(routes.comb.output, "OUTPUT", "to storage time", "to MVC time", "@MVC time", "Stay in storage", color = col.output)
###

#### We don't need summary of relialization times with blockers: better to analyse separate
#generateHistogram(times)
#generateDensity(times)

#### No outliers
#generateHistogram(no.blockers$time, subtitle = "No blockers escape routes")
#generateDensity(no.blockers$time, subtitle = "No blockers escape routes")
if (PLOT.INPUT && PLOT.OUTPUT) {
	par(mfrow = c(2, 1))
	data <- routes.comb$endTime.2-routes.comb$startTime.1
	#data <- (routes.comb$endTime.2-routes.comb$startTime.1)[1:3000]
	generateHistogram(data, subtitle = "")
	generateDensity(data, subtitle = "")
	par(mfrow = c(1, 1))
}
if (PLOT.INPUT) {
	par(mfrow = c(2, 1))
	data <- routes.comb.input$endTime.2-routes.comb.input$startTime.1
	#data <- (routes.comb.input$endTime.2-routes.comb.input$startTime.1)[1:1162]
	generateHistogram(data, subtitle = "INPUT", color = col.input)
	generateDensity(data, subtitle = "INPUT", color = col.input)
	par(mfrow = c(1, 1))
}
if (PLOT.OUTPUT) {
	par(mfrow = c(2, 1))
	data <- routes.comb.output$endTime.2-routes.comb.output$startTime.1
	generateHistogram(data, subtitle = "OUTPUT", color = col.output)
	generateDensity(data, subtitle = "OUTPUT", color = col.output)
	par(mfrow = c(1, 1))
}

#### Blockers
par(mfrow = c(2, 1))
hist(
	blockers$time, main = paste("Histogram of escape/blocker route duration (", length(blockers$time), " routes together)", sep = ""),
	breaks = blockers.buckets,
	xlab = "Realization time [s]", col = colors[3]
)
plot(
	density(blockers$time),
	main =  paste("Density of escape/blocker route duration (", length(blockers$time), " routes together)", sep = ""),
	xlab = "Realization time [s]",
	lwd = 3,
	col = colors[5]
)
par(mfrow = c(1, 1))

minTask <- min(routes.comb$task)
maxTask <- max(routes.comb$task)
xlim <- c(minTask, maxTask)
minRealTime <- min(realization.times.input, realization.times.output)
maxRealTime <- max(realization.times.input, realization.times.output)
ylim <- c(minRealTime, maxRealTime)

alpha <- 150
col.input.transparent <- col2rgb(col.input)
col.input.transparent <- rgb(col.input.transparent[1], col.input.transparent[2], col.input.transparent[3], max = 255, alpha = alpha)
col.output.transparent <- col2rgb(col.output)
col.output.transparent <- rgb(col.output.transparent[1], col.output.transparent[2], col.output.transparent[3], max = 255, alpha = alpha)

plot(
	routes.comb.input$task, realization.times.input,
	xlim = xlim, ylim = ylim,
	main = "Cycle time",
	xlab = "Task ID",
	ylab = "Cycle time [s]",
	col = col.input.transparent,
	pch = 19,
	type = "p"
)
points(
	routes.comb.output$task, realization.times.output,
	col = col.output.transparent,
	pch = 19,
	type = "p"
)
legend("topright", cex = 0.8, legend = c("INPUT", "OUTPUT"), pch = 19,
	col=c(col.input, col.output), xjust = 1, yjust = 1, title = "Task type")

minStartTime <- min(routes.comb$startTime.1/60)
maxStartTime <- max(routes.comb$startTime.1/60)
xlim <- c(minStartTime, maxStartTime)
plot(
	routes.comb.input$startTime.1/60, realization.times.input,
	xlim = xlim, ylim = ylim,
	main = "Start time vs. cycle time",
	xlab = "Start time [min]",
	ylab = "Cycle time [s]",
	col = col.input.transparent,
	pch = 19,
	type = "p"
)
points(
	routes.comb.output$startTime.1/60, realization.times.output,
	col = col.output.transparent,
	pch = 19,
	type = "p"
)
legend("topright", cex = 0.8, legend = c("INPUT", "OUTPUT", "BOTH"), pch = 19,
	col=c(col.input, col.output, colors[5]), xjust = 1, yjust = 1, title = "Task type")

abline(lm(realization.times ~ I(routes.comb$startTime.1/60)), col = colors[5], lwd = 2.5)
abline(lm(realization.times.input ~ I(routes.comb.input$startTime.1/60)), col = col.input, lwd = 2.5)
abline(lm(realization.times.output ~ I(routes.comb.output$startTime.1/60)), col = col.output, lwd = 2.5)


