############### Task distribution between levels in time ##################
minStartTime <- min(routes.comb$startTime.1/60)
maxStartTime <- max(routes.comb$startTime.1/60)
xlim <- c(minStartTime, maxStartTime)
ylim <- c(min(routes.comb$level), max(routes.comb$level))
par(mfrow = c(2, 1))
pch <- 20
runif.mult <- 0.5
plot(
	routes.comb.input$startTime.1/60, routes.comb.input$level + runif.mult*runif(length(routes.comb.input$level), min = -1, max = 1),
	xlim = xlim, ylim = ylim,
	main = "Distribution of tasks between levels in time (INPUT)",
	xlab = "Start time [min]",
	ylab = "Level",
	col = col.input,
	pch = pch,
	type = "p"
)
plot(
	routes.comb.output$startTime.1/60, routes.comb.output$level + runif.mult*runif(length(routes.comb.output$level), min = -1, max = 1),
	xlim = xlim, ylim = ylim,
	main = "Distribution of tasks between levels in time (OUTPUT)",
	xlab = "Start time [min]",
	ylab = "Level",
	col = col.output,
	pch = pch,
	type = "p"
)
par(mfrow = c(1, 1))

#### Together
# plot(
# 	routes.comb.input$startTime.1, routes.comb.input$level,
# 	xlim = xlim, ylim = ylim,
# 	main = "Distribution of tasks between levels in time (INPUT)",
# 	xlab = "Start time [s]",
# 	ylab = "Level",
# 	col = col.input,
# 	pch = 19,
# 	type = "p"
# )
# plot(
# 	routes.comb.output$startTime.1, routes.comb.input$level,
# 	xlim = xlim, ylim = ylim,
# 	main = "Distribution of tasks between levels in time (OUTPUT)",
# 	xlab = "Start time [s]",
# 	ylab = "Level",
# 	col = col.output,
# 	pch = 19,
# 	type = "p"
# )
# legend("topright", cex = 0.8, legend = c("INPUT", "OUTPUT"), pch = 19,
# 	col=c(col.input, col.output), xjust = 1, yjust = 1, title = "Task type")



#################### Task distribution between levels #####################
if (PLOT.INPUT && PLOT.OUTPUT) {
	mfrow <- c(3, 1)
	if (length(unique(routes.comb$level)) < 6) {
		mfrow <- c(1, 3)
	}
	par(mfrow = mfrow)
	tab <- table(routes.comb$level)
	barplot(tab, main = "Tasks per level", col = colors[2], xlab = "Level", ylab = "Number of tasks")
}
if (PLOT.INPUT) {
	tab <- table(routes.comb.input$level)
	barplot(tab, main = "Tasks per level INPUT", col = col.input, xlab = "Level", ylab = "Number of tasks")
}
if (PLOT.OUTPUT) {
	tab <- table(routes.comb.output$level)
	barplot(tab, main = "Tasks per level OUTPUT", col = col.output, xlab = "Level", ylab = "Number of tasks")
}
par(mfrow = c(1, 1))

#################### Task distribution between bots #####################
if (PLOT.INPUT && PLOT.OUTPUT) {
	par(mfrow = c(3, 1))
	tab <- table(routes.comb$bot)
	barplot(tab, main = "Tasks per bot", col = colors[2], xlab = "Bot", ylab = "Number of tasks")
}
if (PLOT.INPUT) {
	tab <- table(routes.comb.input$bot)
	barplot(tab, main = "Tasks per bot INPUT", col = col.input, xlab = "Bot", ylab = "Number of tasks")
}
if (PLOT.OUTPUT) {
	tab <- table(routes.comb.output$bot)
	barplot(tab, main = "Tasks per level OUTPUT", col = col.output, xlab = "Bot", ylab = "Number of tasks")
}
par(mfrow = c(1, 1))
