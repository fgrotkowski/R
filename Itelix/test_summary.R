#if (REMOVE_TRIMMED_ROUTES) {
	routes.no.trim <- routes[which(routes$isTrimmed == "false"), ]
	routes.no.trim <- routes.no.trim[which(routes.no.trim$task != INFINITE_ROUTE_TASK_ID), ]
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
	data.route.1 <- rbind(data.route.1[which(data.route.1$taskType == "INPUT_CONTINUE"), ], data.route.1[which(data.route.1$taskType == "OUTPUT_CONTINUE"), ])
	data.route.2 <- data[which(data$routeType == "ROUTE_2"), ]	## ROUTE_2 before!!!!! be careful with this
	data.route.2 <- rbind(data.route.1, data.route.2)
	data.route.2 <- data.route.2[which(data.route.2$isTrimmed == "false"), ]
	data.route.2 <- data.route.2[which(data.route.2$endTime >= min), ]
	data.route.2 <- data.route.2[which(data.route.2$endTime < max), ]

	length(data.route.2[,1])/(max-min)*3600
}

x <- seq(0, max(routes.no.trim$endTime), by = 10)
#if (THROUGHPUT.WINDOW.WIDTH/2 <= max(routes$endTime) - THROUGHPUT.WINDOW.WIDTH/2)
#	x <- seq(THROUGHPUT.WINDOW.WIDTH/2, max(routes$endTime) - THROUGHPUT.WINDOW.WIDTH/2, by = 10)
middle <- mean(seq(0, max(routes.no.trim$endTime), by = 10))
x <- seq(middle - THROUGHPUT.WINDOW.WIDTH/2, middle + THROUGHPUT.WINDOW.WIDTH/2, by = 10)
minTph <- min(as.numeric(lapply(x, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x, tph.calc, data = routes.no.trim)))
tph.input <- myRound(median(as.numeric(lapply(x, tph.calc, data = routes.no.trim.input))))
tph.output <- myRound(median(as.numeric(lapply(x, tph.calc, data = routes.no.trim.output))))
tph <- tph.input + tph.output

mvcs.output <- length(unique(routes.comb.output$mvc.1))
mvcs.input <- length(unique(routes.comb.input$mvc.1))
mvcs <- mvcs.output + mvcs.input

tph.theor.input <- MAX.TPH.PER.MVC*mvcs.input
tph.theor.output <- MAX.TPH.PER.MVC*mvcs.output
tph.theor <- tph.theor.input + tph.theor.output


duration.input <- max(routes.no.trim.input$endTime) - min(routes.no.trim.input$slotTime, na.rm=T)
duration.output <- max(routes.no.trim.output$endTime) - min(routes.no.trim.output$slotTime, na.rm=T)
##### OLD version
#tph.input <- myRound(length(unique(routes.input$task))/duration.input*3600)
#tph.output <- myRound(length(unique(routes.output$task))/duration.output*3600)
#
#tph.input <- myRound(length(unique(routes.input$task))/max(routes.input$endTime)*3600)
#tph.output <- myRound(length(unique(routes.output$task))/max(routes.output$endTime)*3600)
#tph <- tph.input + tph.output

tpb.input <- myRound(tph.input/bots.num)
tpb.output <- myRound(tph.output/bots.num)
tpb <- tpb.input + tpb.output

tpb.theor.input <- myRound(tph.theor.input/bots.num)
tpb.theor.output <- myRound(tph.theor.output/bots.num)
tpb.theor <- tpb.theor.input + tpb.theor.output

insert.basic.info <- function() {
	par(mfrow = c(2, 1))

	string <- paste("Number of tasks: ", length(unique(routes.no.trim$task)), "\n", sep = "")
	string <- paste(string, "Number of levels: ", levels.num, "\n", sep = "")
	string <- paste(string, "Number of INPUT tasks: ", length(unique(routes.no.trim[which(routes.no.trim$taskType == "INPUT"), ]$task)), "\n", sep = "")
	string <- paste(string, "Number of OUTPUT tasks: ", length(unique(routes.no.trim[which(routes.no.trim$taskType == "OUTPUT"), ]$task)), "\n", sep = "")
	string <- paste(string, "Number of bots: ", bots.num, "\n", sep = "")
	string <- paste(string, "Number of bots per level: ", bots.per.level, "\n", sep = "")
	string <- paste(string, "Pickface ratio: ", testSummary$pickfaceRatio, "\n", sep = "")
	string <- paste(string, "Computation time: ", sec.to.tull.time(testSummary$computationTime), "\n", sep = "")
	string <- paste(string, "Computation time (plan): ", sec.to.tull.time(testSummary$computationPlanTime), "\n", sep = "")
	string <- paste(string, "Computation time (realize): ", sec.to.tull.time(testSummary$computationRealizeTime), "\n", sep = "")
	string <- paste(string, "Move BLOCKER: ", sum(routes.no.trim$routeType == "BLOCKER"), "\n", sep = "")
	string <- paste(string, "Move BLOCKER INPUT: ",
		length(intersect(which(routes.no.trim$routeType == "BLOCKER"), which(routes.no.trim$taskType == "INPUT"))), "\n", sep = "")
	string <- paste(string, "Move BLOCKER OUTPUT: ",
		length(intersect(which(routes.no.trim$routeType == "BLOCKER"), which(routes.no.trim$taskType == "OUTPUT"))), "\n", sep = "")
	textplot(string, cex = 1.1, valign = "top", halign = "left")
	title(paste("Test summary", report.date.header))

	string <- paste("Simulation start time: ", myRound(min(routes.no.trim$startTime)), " [s]\n", sep = "")
	string <- paste(string, "Simulation end time: ", myRound(max(routes.no.trim$endTime)), " [s] = ", sec.to.tull.time(max(routes.no.trim$endTime)), "\n", sep = "")
	string <- paste(string, "Throughput: ", round(tph/3600, 3), " [tasks/s]\n", sep = "")
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
plot(x, lapply(x, tph.calc, data = routes.no.trim), ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[5], xaxt = "n")
lab <- (0:(max(x)/60))*60
axis(1, at = lab, labels = lab/60)
lines(x, lapply(x, tph.calc, data = routes.no.trim.input), lwd = 2, col = col.input)
lines(x, lapply(x, tph.calc, data = routes.no.trim.output), lwd = 2, col = col.output)
abline(h = tph.theor, lwd = 2, col = colors[5])
abline(h = tph.theor.input, lwd = 2, col = col.input)
abline(h = tph.theor.output, lwd = 2, col = col.output)
abline(h = tph, lwd = 1, col = "black")
abline(h=seq(0, tph.theor + 200, by = 200), lwd = 0.5, col = "gray60", lty=8)
abline(v=seq(0, max(routes.no.trim$endTime), by = 60), lwd = 0.5, col = "gray60", lty=8)

legend("topright", cex = 0.8, legend = c("TOTAL", "OUTPUT", "INPUT", "Ave Tph"), lwd=2,
	col=c(colors[5], col.output, col.input, "black"), xjust = 1, yjust = 1, bg = "white")
#legend("topright", cex = 0.8, legend = c("TOTAL", "OUTPUT", "INPUT"), lwd=2,
#	col=c(colors[5], col.output, col.input), xjust = 1, yjust = 1, bg = "white")


