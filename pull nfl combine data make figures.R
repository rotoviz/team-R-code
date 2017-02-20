rm(list = ls())
library(directlabels)
library(dplyr)
library(XML)

yrList <- 2000:2016
posList <- c('QB', 'WR', 'TE', 'RB', 'FB', 'OT', 'OG', 'C', 'DE', 'DT', 'ILB', 'OLB', 'SS', 'FS', 'CB', 'LS', 'K', 'P')

outList <- c()
for (x in yrList) {
  yrOut <- c()
  for (y in posList) {
    URL <- paste0('http://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1&year_min=', x, '&year_max=', x, '&height_min=65&height_max=82&weight_min=149&weight_max=375&pos=', y, '&show=all&order_by=year_id')
    print(paste0('grabbing data for ', y, 's from ', x))
    # THIS IS IMPORTANT; Sys.sleep() ensures you don't violate pfr's terms of service; set > 3
    Sys.sleep(6)
    yrPos.out <- try(as.data.frame(readHTMLTable(URL)), silent = TRUE)
    if(class(yrPos.out) != 'try-error') {
      yrOut[[y]] <- yrPos.out
    }
  }
  
  yrOut.out <- do.call(rbind, yrOut)
  outList[[x]] <- yrOut.out
}

combine.data <- do.call(rbind, outList)
save(combine.data, file = 'combineData.RData')

clean <- data.frame(Year = as.numeric(as.character(combine.data$results.Year)),
                    Player = as.character(combine.data$results.Player),
                    Pos = as.character(combine.data$results.Pos),
                    AV = as.numeric(as.character(combine.data$results.AV)),
                    School = as.character(combine.data$results.School),
                    Height = as.character(combine.data$results.Height),
                    Weight = as.numeric(as.character(combine.data$results.Wt)),
                    Forty = as.numeric(as.character(combine.data$results.40YD)),
                    Vertical = as.numeric(as.character(combine.data$results.Vertical)),
                    BenchReps = as.numeric(as.character(combine.data$results.BenchReps)),
                    Broad = as.numeric(as.character(combine.data$results.Broad.Jump)),
                    Cone = as.numeric(as.character(combine.data$results.3Cone)),
                    Shuttle = as.numeric(as.character(combine.data$results.Shuttle)),
                    DPOS = as.character(combine.data$results.Drafted..tm.rnd.yr.),
                    stringsAsFactors = FALSE)
rm(posList, URL, x, y, yrList, yrOut)

# fix height
height.split <- strsplit(clean$Height, '-', fixed = TRUE)
feet <- as.numeric(lapply(height.split, '[', 1))
inches <- as.numeric(lapply(height.split, '[', 2))
height.fixed <- inches + (feet * 12)
clean$Height.fixed <- height.fixed
clean$Height <- NULL
rm(height.split, feet, inches, height.fixed)

# fix draft detail
detail <- strsplit(clean$DPOS, '/', fixed = TRUE)
team <- lapply(detail, '[', 1)
team.fixed <- unlist(team)
team.fixed <- trimws(team.fixed)
clean$Team.fixed <- team.fixed

round <- lapply(detail, '[', 2)
round.fixed <- unlist(round)
round.fixed <- trimws(round.fixed)
round.fixed <- gsub(pattern = 'st|nd|th|rd', replacement = '', round.fixed)
round.fixed <- as.numeric(round.fixed)
clean$Round.fixed <- round.fixed

pick <- lapply(detail, '[', 3)
pick.fixed <- unlist(pick)
pick.fixed <- trimws(pick.fixed)
pick.fixed <- gsub('st|nd|th|rd| pick', '', pick.fixed)
pick.fixed <- as.numeric(pick.fixed)
clean$Pick.fixed <- pick.fixed

rm(list = ls(pattern = 'team|pick|round|team'))

clean$playedInNFL <- ifelse(is.na(clean$AV), 0, 1)
clean$draftedInd <- ifelse(is.na(clean$Pick.fixed), 0, 1)
cleaner <- subset(clean, Pos != 'Pos')
cleaner$Pos1 <- as.factor(cleaner$Pos)

# summarize data

pos1Groups <- group_by(cleaner, Pos1, Year) %>%
  summarise(countHeight = sum(ifelse(!is.na(Height.fixed), 1, 0)),
            meanHeight = mean(Height.fixed, na.rm = TRUE),
            medianHeight = median(Height.fixed, na.rm = TRUE),
            countWeight = sum(ifelse(!is.na(Weight), 1, 0)),
            meanWeight = mean(Weight, na.rm = TRUE),
            medianWeight = median(Weight, na.rm = TRUE),
            countForty = sum(ifelse(!is.na(Forty), 1, 0)),
            meanForty = mean(Forty, na.rm = TRUE),
            medianForty = median(Forty, na.rm = TRUE),
            countShuttle = sum(ifelse(!is.na(Shuttle), 1, 0)),
            meanShuttle = mean(Shuttle, na.rm = TRUE),
            medianShuttle = median(Shuttle, na.rm = TRUE),
            countCone = sum(ifelse(!is.na(Cone), 1, 0)),
            meanCone = mean(Cone, na.rm = TRUE),
            medianCone = median(Cone, na.rm = TRUE),
            countVertical = sum(ifelse(!is.na(Vertical), 1, 0)),
            meanVertical = mean(Vertical, na.rm = TRUE),
            medianVertical = median(Vertical, na.rm = TRUE),
            countBroad = sum(ifelse(!is.na(Broad), 1, 0)),
            meanBroad = mean(Broad, na.rm = TRUE),
            medianBroad = median(Broad, na.rm = TRUE),
            countBenchReps = sum(ifelse(!is.na(BenchReps), 1, 0)),
            meanBenchReps = mean(BenchReps, na.rm = TRUE),
            medianBenchReps = median(BenchReps, na.rm = TRUE),
            percDrafted = mean(draftedInd, na.rm = TRUE)) %>%
  filter(Pos1 %in% c('RB', 'WR', 'TE', 'QB'))

pos1Groups$Pos1_ff <- factor(as.character(pos1Groups$Pos1), 
                             levels = c('WR', 'RB', 'TE', 'QB'))

# function to make plot
make.qplot <- function(summaryData, metric, grouping, mainTitle, ylimSpan = 0.02, dl = TRUE) {
  
  yMin <- min(summaryData[[metric]]) * (1 - ylimSpan)
  yMax <- max(summaryData[[metric]]) * (1 + ylimSpan)
  
  # use colors that are color blind friendly!
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  plotData <- data.frame(Position = summaryData[[grouping]],
                         Year = summaryData$Year,
                         Metric = summaryData[[metric]])
  
  p <- qplot(data = plotData,
             x = Year,
             y = Metric,
             color = Position,
             main = mainTitle,
             xlab = NULL,
             ylab = NULL) +
    ylim(yMin, yMax) +
    scale_x_continuous(breaks = seq(2000, 2016, 4), expand = c(0.1, 0)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = cbPalette, name = element_blank()) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(vjust = -0.5))
  if(dl) {
    direct.label(p, list("last.qp", cex = 1, dl.trans(x = x + 0.3)))
  }else{
    p
  }
  
}

save.figure <- function(Plot, fn, wdPath = getwd()) {
  setwd(wdPath)
  ggsave(filename = fn, plot = Plot, height = 3, width = 9)
}

# make plots
counts <- make.qplot(summaryData = pos1Groups, metric = 'countHeight', grouping = 'Pos1_ff',
                     mainTitle = 'Counts of Participants')
heights <- make.qplot(summaryData = pos1Groups, metric = 'meanHeight', grouping = 'Pos1_ff', 
                      mainTitle = 'Mean Heights (inches)')
weights <- make.qplot(summaryData = pos1Groups, metric = 'meanWeight', grouping = 'Pos1_ff', 
                      mainTitle = 'Mean Weights (pounds)')
forty <- make.qplot(summaryData = pos1Groups, metric = 'meanForty', grouping = 'Pos1_ff', 
                    mainTitle = 'Mean Forty Yard Dash Times (seconds)')
shuttle <- make.qplot(summaryData = pos1Groups, metric = 'meanShuttle', grouping = 'Pos1_ff', 
                      mainTitle = 'Mean Shuttle Times (seconds)')
cone <- make.qplot(summaryData = pos1Groups, metric = 'meanCone', grouping = 'Pos1_ff', 
                   mainTitle = 'Mean 3 Cone Drill Times (seconds)')
broad <- make.qplot(summaryData = pos1Groups, metric = 'meanBroad', grouping = 'Pos1_ff', 
                    mainTitle = 'Mean Broad Jumps (inches)')
vertical <- make.qplot(summaryData = pos1Groups, metric = 'meanVertical', grouping = 'Pos1_ff', 
                       mainTitle = 'Mean Vertical Jumps (inches)')
bench <- make.qplot(summaryData = pos1Groups, metric = 'meanBenchReps', grouping = 'Pos1_ff', 
                    mainTitle = 'Mean Bench Reps', dl = FALSE)

bench2 <- make.qplot(summaryData = pos1Groups, metric = 'countBenchReps', grouping = 'Pos1_ff', 
                     mainTitle = 'Count Participants Bench Drill', dl = TRUE)

pos1Groups$propBenched <- pos1Groups$countBenchReps / pos1Groups$countHeight
bench3 <- make.qplot(summaryData = pos1Groups, metric = 'propBenched', grouping = 'Pos1_ff', 
                     mainTitle = 'Percent of Position Group Participating in Bench Press Drill', dl = TRUE)
bench3 <- bench3 + scale_y_continuous(labels = percent)
bench3 <- bench3 + scale_color_manual(values = c("#D3D3D3", "#D3D3D3", "#D3D3D3", "#009E73"))

# save plots
save.figure(counts, fn = 'countsPlot.jpeg')
save.figure(heights, fn = 'heightsPlot.jpeg')
save.figure(weights, fn = 'weightsPlot.jpeg')
save.figure(forty, fn = 'fortyPlot.jpeg')
save.figure(shuttle, fn = 'shuttlePlot.jpeg')
save.figure(cone, fn = 'conePlot.jpeg')
save.figure(broad, fn = 'broadPlot.jpeg')
save.figure(vertical, fn = 'verticalPlot.jpeg')
save.figure(bench, fn = 'benchPlot.jpeg')
save.figure(bench3, fn = 'bench3Plot.jpeg')
