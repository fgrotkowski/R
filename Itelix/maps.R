dir.wms.view <- paste(dir.prefix, "WMS/wms-view/results/", sep = "")

heatmap.csv <- "heatmap.csv"
dirmap_horiz.csv <- "dirmap_horizontal.csv"
dirmap_vert.csv <- "dirmap_vertical.csv"

##### BOT SECTORS
bots <- sort(unique(routes$bot))
bots.per.page <- floor(MAPS.BOTS.PER.PAGE/bots.per.level)*bots.per.level
if (GIL.CONFIG)
	bots.per.page <- length(bots)
pages <- ceiling(length(bots)/bots.per.page)

bot.sectors.full <- t(table(routes.comb[, c("bot", "mvc")]))
levels <- unique(routes.comb[, c("bot", "level")])
levels <- levels[order(levels$bot), ]

par(cex.main=0.9, oma=c(1,0,1,0))
for (i in 1:pages) {
	min.idx <- (i-1)*bots.per.page + 1
	max.idx <- i*bots.per.page
	bots.subset <- bots[which(bots >=min.idx)]
	bots.subset <- bots.subset[which(bots.subset <= max.idx)]

	bot.sectors <- bot.sectors.full[, which(is.element(as.numeric(colnames(bot.sectors.full)), bots.subset))]
	rownames(bot.sectors) <- lapply(as.numeric(rownames(bot.sectors)), mvc.pos.to.id)
	levels.subset <- levels[bots.subset, ]
	x <- as.numeric(rownames(bot.sectors))
	y <- as.numeric(colnames(bot.sectors))
	z <- as.matrix(bot.sectors)
	#input.mvc <- which(as.numeric(rownames(z)) > 6)
	tmp.fun <- function(x) { which(x == mvcs) }
	input.mvc <- as.numeric(lapply(mvcs.input, tmp.fun))
	z[input.mvc, ] <- -z[input.mvc, ]
	#image.plot(x = x, y = y, z = z, cex=0.5,
	#	col=gray((0:32)/32), main = "Distribution of tasks from each MVC between bots",
	#	legend.lab = "Frequency (occurrences)", legend.mar = 3.8, legend.shrink = 0.7, legend.width = 0.8,
	#	ylab=1:10
	#)
	col.neg <- -min(z)*6
	col.pos <- max(z)*6
	cexRow = 0.2 + 1/log10(bots.per.page)
	if (GIL.CONFIG)
		cexRow <- 0.25
	col <- c(colorRampPalette(brewer.pal(9,"Reds"))(col.neg), "#FFFFFF", colorRampPalette(brewer.pal(9,"Blues"))(col.pos))
	heatmap(t(z), scale="none", Colv=NA, keep.dendro = FALSE,
		xlab = "MVC", ylab = "Bot",
		main = paste("Distribution of tasks from each MVC between bots (levels ", min(levels.subset$level, na.rm=T),
			" - ", max(levels.subset$level, na.rm=T), ")", sep=""),
		col = col, cexRow = cexRow
	)

#	heatmap(z, scale="none", Rowv=NA, keep.dendro = FALSE,
#		xlab = "MVC", ylab = "Bot",
#		main = paste("Distribution of tasks from each MVC between bots (levels ", min(levels.subset$level, na.rm=T),
#			" - ", max(levels.subset$level, na.rm=T), ")", sep=""),
#		col = col
#	)
}
par(resetPar())

################################### MORE CLUSTERIZATION #######################################
############################ MVC sides joined
bot.sectors.full <- as.data.frame(cbind(routes.comb[, c("bot")], as.numeric(lapply(routes.comb[, c("mvc")], mvc.pos.to.mayor))))
colnames(bot.sectors.full) <- c("bot", "mvc")
bot.sectors.full <- t(table(bot.sectors.full))

if (GRZEGORZ.CLUSTERIZATION) {
	par(cex.main=0.9, oma=c(1,0,1,0))
	for (i in 1:pages) {
		min.idx <- (i-1)*bots.per.page + 1
		max.idx <- i*bots.per.page
		bots.subset <- bots[which(bots >=min.idx)]
		bots.subset <- bots.subset[which(bots.subset <= max.idx)]

		bot.sectors <- bot.sectors.full[, which(is.element(as.numeric(colnames(bot.sectors.full)), bots.subset))]
		#rownames(bot.sectors) <- lapply(as.numeric(rownames(bot.sectors)), mvc.pos.to.mayor)
		levels.subset <- levels[bots.subset, ]
		x <- as.numeric(rownames(bot.sectors))
		y <- as.numeric(colnames(bot.sectors))
		z <- as.matrix(bot.sectors)
		input.mvc <- which(as.numeric(rownames(z)) > 3)
		z[input.mvc, ] <- -z[input.mvc, ]
		col.neg <- -min(z)*6
		col.pos <- max(z)*6
		col <- c(colorRampPalette(brewer.pal(9,"Reds"))(col.neg), "#FFFFFF", colorRampPalette(brewer.pal(9,"Blues"))(col.pos))
		heatmap(t(z), scale="none", Colv=NA, keep.dendro = FALSE,
			xlab = "Double MVC id", ylab = "Bot",
			main = paste("Distribution of tasks from each 2-side MVC between bots (levels ", min(levels.subset$level, na.rm=T),
				" - ", max(levels.subset$level, na.rm=T), ")", sep=""),
			col = col
		)
	}
	par(resetPar())

}

######### IN/OUT only ###########
bot.sectors.full <- as.data.frame(cbind(routes.comb[, c("bot")], as.numeric(lapply(routes.comb[, c("mvc")], mvc.pos.to.type))))
colnames(bot.sectors.full) <- c("bot", "mvc")
bot.sectors.full <- t(table(bot.sectors.full))

if (GRZEGORZ.CLUSTERIZATION) {
	par(cex.main=0.9, oma=c(1,0,1,0))
	for (i in 1:pages) {
		min.idx <- (i-1)*bots.per.page + 1
		max.idx <- i*bots.per.page
		bots.subset <- bots[which(bots >=min.idx)]
		bots.subset <- bots.subset[which(bots.subset <= max.idx)]

		bot.sectors <- bot.sectors.full[, which(is.element(as.numeric(colnames(bot.sectors.full)), bots.subset))]
		#rownames(bot.sectors) <- lapply(as.numeric(rownames(bot.sectors)), mvc.pos.to.mayor)
		levels.subset <- levels[bots.subset, ]
		x <- as.numeric(rownames(bot.sectors))
		y <- as.numeric(colnames(bot.sectors))
		z <- as.matrix(bot.sectors)
		input.mvc <- which(as.numeric(rownames(z)) > 0)
		z[input.mvc, ] <- -z[input.mvc, ]
		col.neg <- -min(z)*6
		col.pos <- max(z)*6
		col <- c(colorRampPalette(brewer.pal(9,"Reds"))(col.neg), "#FFFFFF", colorRampPalette(brewer.pal(9,"Blues"))(col.pos))
		heatmap(t(z), scale="none", Colv=NA, keep.dendro = FALSE,
			xlab = "Double MVC id", ylab = "Bot",
			main = paste("Tasks type for each bot (levels ", min(levels.subset$level, na.rm=T),
				" - ", max(levels.subset$level, na.rm=T), ")", sep=""),
			col = col
		)
	}
	par(resetPar())

}


##### HEATMAP
file.copy(paste(dir.wms.view, heatmap.csv, sep = ""), paste(dir, heatmap.csv, sep = ""), overwrite = TRUE)
hmap <- read.csv(heatmap.csv, header = FALSE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")
image.plot(as.matrix(hmap), col=gray((0:32)/32), main = "Frequency of visiting nodes (popular nodes)", legend.lab = "Frequency (occurrences)", legend.mar = 3.8, legend.shrink = 0.7, legend.width = 0.8)


##### DIRMAP
file.copy(paste(dir.wms.view, dirmap_horiz.csv, sep = ""), paste(dir, dirmap_horiz.csv, sep = ""), overwrite = TRUE)
dmap.h <- read.csv(dirmap_horiz.csv, header = FALSE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")
file.copy(paste(dir.wms.view, dirmap_vert.csv, sep = ""), paste(dir, dirmap_vert.csv, sep = ""), overwrite = TRUE)
dmap.v <- read.csv(dirmap_vert.csv, header = FALSE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")

dmap.h <- cbind(0, 0, dmap.h, 0, 0)
dmap.h <- rbind(min(dmap.h), 0, dmap.h, 0, max(dmap.h))
dmap.v <- rbind(0, 0, dmap.v, 0, 0)
dmap.v <- cbind(min(dmap.v), 0, dmap.v, 0, max(dmap.v))
dmap.h <- dmap.h - abs(sign(dmap.h))*min(min(dmap.h) - 1, 0) # >0
dmap.v <- dmap.v - abs(sign(dmap.v))*max(max(dmap.v) + 1, 0) # <0
dmap <- dmap.h + dmap.v


breaks <- seq(min(dmap), max(dmap), by = 1)
col <- colorRampPalette(c("green", "blue", "white", "white", "red", "yellow"), space="Lab")(length(breaks) - 1)
col <- colorRampPalette(brewer.pal(10,"RdBu"))(length(breaks) - 1)

#col[length(breaks)/2] <- "#FFFFFF"
zeros <- c(which(breaks == 0), which(breaks == 0) - 1)
zeros <- c(which(breaks == 0) - 1)


reds <- colorRampPalette(brewer.pal(9,"Reds")[3:9])(zeros - 1)
blues <- colorRampPalette(brewer.pal(9,"Blues")[3:9])(length(breaks) - 1 - zeros)
reds <- colorRampPalette(brewer.pal(9,"PRGn")[1:9])(zeros - 1)
blues <- colorRampPalette(brewer.pal(9,"RdBu")[1:9])(length(breaks) - 1 - zeros)
col <- c(rev(reds), "#FFFFFF", blues)
#col[zeros] <- "#FFFFFF"


heatmap.2(t(mirror.matrix(data.matrix(dmap, rownames.force = NA))), 
    scale="none",
    Rowv=NA,
    Colv=NA,
    breaks = breaks,
    labRow = "", labCol = "",
    col = col,
    cexRow=0.5, cexCol=1.0,
    ylab = "EAST",
    xlab = "SOUTH",
    main = "Directions map",
    key=TRUE, keysize=1.5, trace="none")
mtext("WEST", side=2, line=-1, cex.lab=1, las=3)
mtext("NORTH", side=3, line=-3, cex.lab=1, las=1)
#mtext("EAST", side=4, line=2, cex.lab=1, las=3)
#mtext("SOUTH", side=1, line=2, cex.lab=1, las=1)
