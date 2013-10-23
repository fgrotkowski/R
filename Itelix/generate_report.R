### source script like this:
# source("C:/Users/Filip/WMS/Statistics/generate_report.R")
### Batch run
# C:\Program Files\R\R-2.15.1patched\bin>R.exe  CMD BATCH C:\Users\Filip\WMS\Statistics\generate_report.R
dir.prefix <- "C:/Users/Filip/WMS/"

dir <- paste(dir.prefix, "Statistics/", sep = "")
dir.wms.view <- paste(dir.prefix, "WMS/wms-view/results/", sep = "")
setwd(dir)
source("environment_setup.R")


file.copy(paste(dir.wms.view, routes.csv, sep = ""), paste(dir, routes.csv, sep = ""), overwrite = TRUE)
routes <- read.csv(routes.csv, header = TRUE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")
routes[, c("startTime", "endTime", "time", "stayAtFinal", "slotTime", "platformArrival")] <-
	routes[, c("startTime", "endTime", "time", "stayAtFinal", "slotTime", "platformArrival")]/1000
times <- routes[, c("time")]

file.copy(paste(dir.wms.view, summary.csv, sep = ""), paste(dir, summary.csv, sep = ""), overwrite = TRUE)
testSummary <- read.csv(summary.csv, header = TRUE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")
testSummary[, c("computationTime", "computationPlanTime", "computationRealizeTime")] <- testSummary[, c("computationTime", "computationPlanTime", "computationRealizeTime")]/1000

tasks <- testSummary$tasks
bots.num <- testSummary$bots
levels.num <- length(unique(routes$level))
bots.per.level <- bots.num/levels.num

report.date.filename <- testSummary$fileNameDate
report.date.header <- testSummary$headerDate

# PDF
if (PDF) {
  output.file <- paste(paste(report.date.filename, "Report", sep = "-"),
	paste("", tasks, "tasks", bots.per.level, "bots", levels.num, "levels.pdf", sep="_"), sep = "")
  pdf(file = output.file,
      width = 11, height = 7,
      bg = bg.color,
      title = "Report"
  )
}

##### TODO: need check !!!!!!!!!!!!!!!!!!!!!!!
####################################################################################################################
####################################################################################################################
####################################################################################################################
#ordered.routes <- routes[order(routes$task), ]
#textplot(ordered.routes[c(1:30), ], show.rownames = FALSE, cex = 0.75, valign = "top", halign = "left")
#title("Bot movement summary (first 30 rows)")


########################## DATA PREPARATION ########################

#if (REMOVE_TRIMMED_ROUTES) routes <- routes[which(routes$isTrimmed == "false"), ]



########################################################################################################
####################################### ROUTES WITHOUT TRIMMED ITEMS ###################################
############################## Used by most of the code (without Gantt) ################################
routes <- routes[which(routes$isTrimmed == "false"), ]
routes.input <- routes[which(routes$taskType == "INPUT"), ]
routes.output <- routes[which(routes$taskType == "OUTPUT"), ]
PLOT.INPUT <- TRUE
PLOT.OUTPUT <- TRUE
if (length(routes.input[,1]) == 0) PLOT.INPUT <- FALSE
if (length(routes.output[,1]) == 0) PLOT.OUTPUT <- FALSE

# ROUTES DATA. NEEDED BY MOST OF SOURCES
no.blockers <- routes[which(routes$routeType != "BLOCKER"), ]
blockers <- routes[which(routes$routeType == "BLOCKER"), ]
routes.1 <- routes[which(routes$routeType == "ROUTE_1"), ]
routes.2 <- routes[which(routes$routeType == "ROUTE_2"), ]
routes.1.input <- routes.input[which(routes.input$routeType == "ROUTE_1"), ]
routes.2.input <- routes.input[which(routes.input$routeType == "ROUTE_2"), ]
routes.1.output <- routes.output[which(routes.output$routeType == "ROUTE_1"), ]
routes.2.output <- routes.output[which(routes.output$routeType == "ROUTE_2"), ]
routes.comb <- merge(routes.1, routes.2, by = c("task"), suffixes = c(".1", ".2"))
routes.comb.input <- merge(routes.1.input, routes.2.input, by = c("task"), suffixes = c(".1", ".2"))
routes.comb.output <- merge(routes.1.output, routes.2.output, by = c("task"), suffixes = c(".1", ".2"))

#routes.comb.with.trimmed <- routes.comb
#routes.comb.with.trimmed.input <- routes.comb.input
#routes.comb.with.trimmed.output <- routes.comb.output



########################################################################################################
######################################## ROUTES WITH TRIMMED ITEMS #####################################
############################################ Used by Gantt plot ########################################
routes <- read.csv(routes.csv, header = TRUE, sep = ";", quote="\"", dec=".", fill = TRUE, comment.char="")
routes[, c("startTime", "endTime", "time", "stayAtFinal", "slotTime", "platformArrival")] <-
	routes[, c("startTime", "endTime", "time", "stayAtFinal", "slotTime", "platformArrival")]/1000
routes.input <- routes[which(routes$taskType == "INPUT"), ]
routes.output <- routes[which(routes$taskType == "OUTPUT"), ]
PLOT.INPUT <- TRUE
PLOT.OUTPUT <- TRUE
if (length(routes.input[,1]) == 0) PLOT.INPUT <- FALSE
if (length(routes.output[,1]) == 0) PLOT.OUTPUT <- FALSE

# ROUTES DATA. NEEDED BY MOST OF SOURCES
no.blockers <- routes[which(routes$routeType != "BLOCKER"), ]
blockers <- routes[which(routes$routeType == "BLOCKER"), ]
routes.1 <- routes[which(routes$routeType == "ROUTE_1"), ]
routes.2 <- routes[which(routes$routeType == "ROUTE_2"), ]
routes.1.input <- routes.input[which(routes.input$routeType == "ROUTE_1"), ]
routes.2.input <- routes.input[which(routes.input$routeType == "ROUTE_2"), ]
routes.1.output <- routes.output[which(routes.output$routeType == "ROUTE_1"), ]
routes.2.output <- routes.output[which(routes.output$routeType == "ROUTE_2"), ]
routes.comb.with.trimmed <- merge(routes.1, routes.2, by = c("task", "isTrimmed"), suffixes = c(".1", ".2"))
routes.comb.with.trimmed.input <- merge(routes.1.input, routes.2.input, by = c("task", "isTrimmed"), suffixes = c(".1", ".2"))
routes.comb.with.trimmed.output <- merge(routes.1.output, routes.2.output, by = c("task", "isTrimmed"), suffixes = c(".1", ".2"))



########### B O D Y  O F  T H E  S C R I P T ############

#options(device = function(...) {
#    .Call("R_GD_nullDevice", PACKAGE = "grDevices")
#})

################### TEST SUMMARY ########################
source("test_summary.R")

##################### MVC STATS #########################
source("mvc_stats.R")

################### BOT MOVEMENT ########################
source("bot_movement.R")

################ TASK DISTRIBUTION ######################
source("task_distribution.R")

################# BOT UTILIZATION #######################
source("bot_utilization.R")

####################### MAPS ############################
source("maps.R")

#### PDF CLOSE ####
if (PDF) dev.off()


#### CLEAN UP #####
for (i in 1:10) {
	file.remove(paste("Rplots", i, ".pdf", sep = ""))
}
file.remove("Rplots.pdf")


data = routes.no.trim.output
dd1 <- data[which(data$taskType == "OUTPUT_CONTINUE"), ]
dd1[which(dd1$mvc == 36), ]

