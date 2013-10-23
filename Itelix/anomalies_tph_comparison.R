## NO ANOMALIES

x1 <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x1, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x1, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x1, tph.calc, data = routes.no.trim)))
y1 <- lapply(x1, tph.calc, data = routes.no.trim)
plot(x1, y1, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot for MVC failures",
	lwd = 2, col = colors[1], xaxt = "n")


## ONE MVC CRASH
x2 <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x2, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x2, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x2, tph.calc, data = routes.no.trim)))
y2 <- lapply(x2, tph.calc, data = routes.no.trim)
lines(x2, y2, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[2], xaxt = "n")

## TWO MVC CRASH
x3 <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x3, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x3, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x3, tph.calc, data = routes.no.trim)))
y3 <- lapply(x3, tph.calc, data = routes.no.trim)
lines(x3, y3, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[3], xaxt = "n")

## BOT CRASH

## ONE BOT CRASH. BOT 25, at DECK 60:4
x4 <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x4, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x4, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x4, tph.calc, data = routes.no.trim)))
y4 <- lapply(x4, tph.calc, data = routes.no.trim)
lines(x4, y4, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[4], xaxt = "n")


## ONE BOT CRASH, BOT 3, at MVC 3 (INPUT) 20:0
x5 <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x5, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x5, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x5, tph.calc, data = routes.no.trim)))
y5 <- lapply(x5, tph.calc, data = routes.no.trim)
lines(x5, y5, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[5], xaxt = "n")


## ONE BOT CRASH. BOT 1, in STORAGE BLOCKING OTHER BOT 60:30
x6 <- seq(0, max(routes.no.trim$endTime), by = 10)
minTph <- min(as.numeric(lapply(x6, tph.calc, data = routes.no.trim.input)), as.numeric(lapply(x6, tph.calc, data = routes.no.trim.output)), na.rm = T)
maxTph <- max(as.numeric(lapply(x6, tph.calc, data = routes.no.trim)))
y6 <- lapply(x6, tph.calc, data = routes.no.trim)
lines(x6, y6, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[6], xaxt = "n")

############ LEGEND ###########
png(filename = "Anomalies_tph_comparison_4.png",
    width = 900, height = 600, units = "px", pointsize = 12)

colors <- c("black", "red", "blue", "green", "orange", "violet")

plot(x1, y1, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot for MVC failures",
	lwd = 2, col = colors[1], xaxt = "n")

lines(x2, y2, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[2], xaxt = "n")

lines(x3, y3, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[3], xaxt = "n")

lines(x4, y4, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[4], xaxt = "n")

lines(x5, y5, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[5], xaxt = "n")

lines(x6, y6, ylim = c(minTph, tph.theor), las = 1,
	type='l', ylab = "Tph", xlab = "Window center time [min]",
	main = "Throughput plot",
	lwd = 2, col = colors[6], xaxt = "n")

#abline(h = tph.theor, lwd = 2, col = colors[5])
lab <- (0:(max(x1)/60))*60
axis(1, at = lab, labels = lab/60)
abline(h=seq(0, tph.theor + 200, by = 200), lwd = 0.5, col = "gray60", lty=8)
abline(v=seq(0, max(routes.no.trim$endTime), by = 60), lwd = 0.5, col = "gray60", lty=8)
legend("topright", cex = 0.8, legend = c("No anomalies", "One side crashed", "Two (both) sides crashed", "Bot 12 crashed on deck", "Bot 3 crashed at MVC", "Bot 1 crashed in aisle"), lwd=2,
	col=colors, xjust = 1, yjust = 1, bg = "white")

dev.off()


