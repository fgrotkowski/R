####################### MVC stats #######################
# output
mvc.data.output <- routes.comb.output[, c("task", "level.2", "endTime.2", "mvc.2", "stayAtFinal.2", "slotTime.2", "slotNr.2", "platformArrival.2")]
colnames(mvc.data.output) <- c("task", "level", "routeEndTime", "mvc", "stayAtFinal", "slotTime", "slotNr", "platformArrival")
mvc.data.output <- mvc.data.output[order(mvc.data.output$mvc), ]
# input
mvc.data.input <- routes.comb.input[, c("task", "level.1", "endTime.1", "mvc.1", "stayAtFinal.1", "slotTime.1", "slotNr.1", "platformArrival.1")]
colnames(mvc.data.input) <- c("task", "level", "routeEndTime", "mvc", "stayAtFinal", "slotTime", "slotNr", "platformArrival")
mvc.data.input <- mvc.data.input[order(mvc.data.input$mvc), ]
mvc.data <- rbind(mvc.data.output, mvc.data.input)
mvc.data <- cbind(mvc.data, type=c(rep("OUTPUT", length(mvc.data.output[,1])), rep("INPUT", length(mvc.data.input[,1]) ) ))

mvcs.output <- unique(mvc.data.output$mvc)
mvcs.input <- unique(mvc.data.input$mvc)
mvcs <- unique(mvc.data$mvc)
mvcs <- mvcs[order(mvcs)]

ids.output <- 1:length(mvcs.output)
ids.input <- length(mvcs.output) + 1:length(mvcs.input)


get.mvc.idx <- function(x) { which(mvcs == x) }
ids.output <- as.numeric(lapply(mvcs.output, get.mvc.idx))
ids.input <- as.numeric(lapply(mvcs.input, get.mvc.idx))

insert.mvc.info <- function() {
	par(mfrow = c(2, 1))

	string <- paste("Number of MVCs: ", length(mvcs), "\n", sep = "")
	string <- paste(string, "Number of OUTPUT MVCs: ", length(mvcs.output), "\n", sep = "")
	string <- paste(string, "Number of INPUT MVCs: ", length(mvcs.input), "\n", sep = "")

	shelves.taken <- 0
	shelves.taken.output <- 0
	shelves.taken.input <- 0
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		attach(one.mvc.data)
		maxx <- max(slotNr, na.rm=T)
		minn <- min(slotNr, na.rm=T)
		tasks <- length(unique(slotNr))
		shelves.taken <- shelves.taken + myRound(100*tasks/(maxx-minn+1))
		if (type[1] == "OUTPUT")
			shelves.taken.output <- shelves.taken.output + myRound(100*tasks/(maxx-minn+1))
		if (type[1] == "INPUT")
			shelves.taken.input <- shelves.taken.input + myRound(100*tasks/(maxx-minn+1))
		detach(one.mvc.data)
	}
	string <- paste(string, "Total shelves taken percentage: ", round(shelves.taken/length(mvcs), 0), "%\n", sep = "")
	string <- paste(string, "Total shelves taken percentage OUTPUT: ",
		round(shelves.taken.output/length(mvcs.output), 0), "%\n", sep = "")
	string <- paste(string, "Total shelves taken percentage INPUT: ",
		round(shelves.taken.input/length(mvcs.input), 0), "%\n", sep = "")
	textplot(string, cex = 1.1, valign = "top", halign = "left")
	title("Total MVC utilization")

	#par(mfrow = c(2, 3))
	tabl <- as.data.frame(matrix(ncol=6))
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		attach(one.mvc.data)
		maxx <- max(slotNr, na.rm=T)
		minn <- min(slotNr, na.rm=T)
		tasks <- length(unique(slotNr))
	#	string <- paste("MVC(", i, ") at position = (", mvc, ", 0)", "\n", sep = "")
	#	string <- paste(string, "Number of tasks: ", tasks, "\n", sep = "")
	#	string <- paste(string, "Min shelf: ", minn, "\n", sep = "")
	#	string <- paste(string, "Max shelf: ", maxx, "\n", sep = "")
	#	string <- paste(string, "Shelves taken percentage: ", myRound(100*tasks/(maxx-minn)), "%\n", sep = "")

		tabl <- rbind(tabl, c(i, as.character(type[1]), tasks, minn, maxx, round(100*tasks/(maxx-minn+1), 0)))

		detach(one.mvc.data)
	#	textplot(string, cex = 1.0, valign = "top", halign = "left")
	}
	colnames(tabl) <- c("MVC", "Type", "Number of tasks", "First shelf taken", "Last shelf taken", "Shelves taken percentage")
	tabl <- tabl[-1,]
	textplot(tabl, show.rownames = FALSE, cex = 0.9, valign = "top", halign = "left")
	title("Detailed MVC utilization")

	par(mfrow = c(1, 1))
	x <- tabl$MVC
	y <- as.numeric(tabl[, "Shelves taken percentage"])
	tab <- as.table(y)
	rownames(tab) <- tabl$MVC
	func.col <- function(lst) {
		ret <- c()
		for (x in lst) {
			if (x == "INPUT") ret <- c(ret, col.input)
			if (x == "OUTPUT") ret <- c(ret, col.output)
		}
		ret
	}
	cols <- func.col(tabl$Type)
	barplot(tab, main = "MVC utilization plot", col = cols, ylim=c(0, 100), xlab = "MVC", ylab = "Shelves taken percentage [%]")
	legend('topright', c("OUTPUT", "INPUT"), fill = c(col.output,col.input), bg = "#FFFFFF", horiz = TRUE,  cex = 0.75)

	################################# Number of tasks per MVC side #########################################
	x <- tabl$MVC
	y <- as.numeric(tabl[, "Number of tasks"])
	tab <- as.table(y)
	rownames(tab) <- tabl$MVC
	barplot(tab, main = "Number of tasks per MVC side", col = cols, ylim=c(0, max(tab)*1.2), xlab = "MVC", ylab = "Number of tasks")
	legend('topright', c("OUTPUT", "INPUT"), fill = c(col.output,col.input), bg = "#FFFFFF", horiz = TRUE,  cex = 0.75)


	par(mfrow = c(1, 1))
}

insert.mvc.info()

############################### Number of tasks per MVC side in time ###################################
mvc.in.time <- function(colors) {
	plot(mvc.data$slotTime/60, lapply(mvc.data$mvc, function (x) mvc.pos.to.id(x)), col = "white", xlab = "Time [min]", ylab = "MVC",  yaxt="n",
		main = paste(paste("Number of tasks per MVC side in time", sep = ""), sep = "\n"))
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		attach(one.mvc.data)
		points(
			slotTime/60, lapply(one.mvc.data$mvc, function (x) mvc.pos.to.id(x)),
			xlab = "Time [min]",
			ylab = "MVC",
			col = colors[i],
			pch = 19,
			cex = 0.8
		)
		detach(one.mvc.data)
	}
	axis(2, at = 1:length(mvcs), labels = 1:length(mvcs))
}

mvc.in.time(as.character(lapply(mvcs,function (x) ifelse(is.element(x, mvcs.input), col.input, col.output))))


plot.slots <- function(mvcs, inOrOut, ids=1:length(mvcs), color) {
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		one.mvc.data <- one.mvc.data[with(one.mvc.data, order(slotNr)), ]
		attach(one.mvc.data)
		plot(
			#route, slotTime/60,
			1:length(task), slotTime/60,
			main = paste("MVC(", ids[i], ")", sep = ""),
			xlab = "Task",
			ylab = "Shelf time [min]",
			col = color,
			pch = 19,
			cex = mvc.cex,
			type = "p"
		)
		par(new = TRUE)  # 2nd graph won't clean the 1st
		plot(
			1:length(task), slotNr,
			col = color,
			pch = 19,
			cex = mvc.cex,
			type = "p",
			axes = FALSE, xlab = "", ylab = ""
		)  # plotting 2ng graph
		axis(4, ylab = "Shelf id")
		mtext("Shelf id", side=4, line=3, cex=0.7)
		detach(one.mvc.data)
	}
	title(paste("Reserved shelves distribution (", inOrOut, " MVCs)", sep = ""), outer=TRUE)
}

if (PLOT.OUTPUT) {
	par(mfrow = c(2, 3), oma=c(0,0,2,0), mar=c(4,7,4,7))
	plot.slots(mvcs.output, "OUTPUT", ids.output, col.output)
}
if (PLOT.INPUT) {
	par(mfrow = c(2, 2), oma=c(0,0,2,0))
	plot.slots(mvcs.input, "INPUT", ids.input, col.input)
}
par(mfrow = c(1, 1))

###### Missed slots ######
missed.slots <- function(mvcs, inOrOut, ids=1:length(mvcs), color) {
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		one.mvc.data <- one.mvc.data[with(one.mvc.data, order(slotNr)), ]
		attach(one.mvc.data)
		missed <- c(as.numeric(slotNr)[1], as.numeric(slotNr))
		missed <- missed[-length(missed)]
		missed <- slotNr - missed
		missed[-1] <- missed[-1] - 1
		plot(
			#route, slotTime,
			1:length(task), missed,
			main = paste("MVC(", ids[i], ")", sep = ""),
			xlab = "Task",
			ylab = "Shelves missed",
			col = color,
			pch = 19,
			cex = mvc.cex,
			type = "p"
		)
		detach(one.mvc.data)
	}
	title(paste("Number of missed shelves for each task (", inOrOut, " MVCs)", sep = ""), outer=TRUE)
}

if (PLOT.OUTPUT) {
	par(mfrow = c(2, 3), oma=c(0,0,2,0), mar=c(4,7,4,7))
	missed.slots(mvcs.output, "OUTPUT", ids.output, col.output)
}
if (PLOT.INPUT) {
	par(mfrow = c(2, 2), oma=c(0,0,2,0))
	missed.slots(mvcs.input, "INPUT", ids.input, col.input)
}
par(mfrow = c(1, 1))

###### TIME SPENT AT MVC ######
plot.at.mvc.time <- function(mvc.data, inOrOut, color, xlim) {
	attach(mvc.data)
	plot(
		routeEndTime/60, stayAtFinal,
		main = paste(inOrOut),
		xlab = "Arrival time [min]",
		ylab = "Waiting time [s]",
		xlim = xlim,
		col = color,
		pch = 19,
		cex = mvc.cex,
		type = "p"
	)
	detach(mvc.data)
}

xlim = c(0, max(mvc.data$routeEndTime/60))
ylim = c(min(mvc.data$stayAtFinal), max(mvc.data$stayAtFinal))
if (PLOT.OUTPUT && PLOT.INPUT) par(mfrow = c(2, 1), oma=c(0,0,2,0))
if (PLOT.OUTPUT) {
	plot.at.mvc.time(mvc.data.output, "OUTPUT", col.output, xlim)
}
if (PLOT.INPUT) {
	plot.at.mvc.time(mvc.data.input, "INPUT", col.input, xlim)
}
title(paste("Waiting on platform vs arrival at MVC time", sep = ""), outer=TRUE)
par(mfrow = c(1, 1))


###### MVC dynamics visualization
mvc.visualization <- function(mvcs, ids=1:length(mvcs), color) {
	for (i in 1:length(mvcs)) {
		mvc <- mvcs[i]
		one.mvc.data <- mvc.data[which(mvc.data$mvc == mvc), ]
		attach(one.mvc.data)
		missed <- c(as.numeric(slotNr)[1], as.numeric(slotNr))
		missed <- missed[-length(missed)]
		missed <- slotNr - missed
		missed[-1] <- missed[-1] - 1
		plot(
			platformArrival/60, level,
			#slotTime, level,
			#panel.first = grid((max(platformArrival) - min(platformArrival))/50, ny = levels.num),
			main = paste(
				paste("MVC visualization. How shelves are reserved in time", sep = ""),
				paste("MVC(", ids[i], ")", sep = ""),
				sep = "\n"),
			xlab = "Time [min]",
			ylab = "Level",
			col = color,
			pch = 19,
			cex = mvc.visual.cex,
			type = "p"
		)
		abline(h=0:(levels.num-1), lwd = 0.5, col = "gray60", lty=8)
		abline(v=seq(0, max(platformArrival/60), by = 1), lwd = 0.5, col = "gray60", lty=8)
		detach(one.mvc.data)
	}
}


mvc.visualization(mvcs.output, ids.output, col.output)
mvc.visualization(mvcs.input, ids.input, col.input)

