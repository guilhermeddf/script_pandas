#install.packages("dplyr")
#install.packages("beanplot")
#install.packages("dtwclust")
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("ggpmisc")
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
#install.packages("fviz_nbclust")
# install.packages("FactoMineR")

library(ggplot2)
library(beanplot)
library(dplyr)
library(effsize)
library(dtwclust)
library(reshape)
library(factoextra)
library(NbClust)
library(ggpmisc)
library(gginnards)

#DRAW BEANPLOT OF CURRENT
# datStaticCICurrent = read.csv("csv_data/projects-size-current-CI.csv", header = TRUE)
# datStaticCICurrent <- datStaticCICurrent %>% mutate(TYPE="CI")
# datStaticNOCICurrent = read.csv("csv_data/projects-size-current-NOCI.csv", header = TRUE)
# datStaticNOCICurrent <- datStaticNOCICurrent %>% mutate(TYPE="NOCI")
# datStaticCurrent = rbind(datStaticCICurrent, datStaticNOCICurrent)
# beanplot(datStaticCurrent$test_ratio ~ interaction(datStaticCurrent$TYPE),side="both", col=list("gray", c("#0097A7", "white")))

#RQ1

#DRAW BEANPLOT OF MEAN RELEASES
datStaticCI = read.csv("csv_data/projects-size-CI.csv", header = TRUE)
datStaticCI <- datStaticCI %>% mutate(TYPE="CI")

datStaticNOCI = read.csv("csv_data/projects-size-NOCI.csv", header = TRUE)
datStaticNOCI <- datStaticNOCI %>% mutate(TYPE="NOCI")

datStatic = rbind(datStaticCI, datStaticNOCI)

datStaticMedian <- datStatic %>%
  group_by(project) %>%
  mutate(median = median(test_ratio)) %>%
  filter(version==1)
beanplot(datStaticMedian$test_ratio ~ interaction(datStaticMedian$TYPE),
         side="both", col=list("gray", c("#0097A7", "white")), main = "Projects' Median Test Ratio")
wilcox.test(datStaticCI$test_ratio, datStaticNOCI$test_ratio)

      
#CHECK FOR TRENDS
#EARLY_CY BUGS 
#NO_CI EARLY_CI DEBT_RATIO

dataframe <- read.csv("/home/case-b315/pythonss/duplicated_lines_for_r.csv", header = TRUE)

dataframe <- dataframe %>% select(project_name, type, version, value)
dataframe <- dataframe %>% filter(type == "NO_CI")
dataframe <- dataframe %>% filter(type == "EARLY_CI")
dataframe <- dataframe %>% filter(type == "LATE_CI")

dataframe <- reshape(dataframe, idvar=c("project_name","type"), timevar="version", direction="wide")

#GET THE BEST NUMBER OF CLUSTERS
set.seed(123)
fviz_nbclust(dataframe[,3:50], kmeans, nstart = 25, k.max = 20,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


#PLOT CLUSTERS
tclust <- tsclust(dataframe[,3:50], type = "fuzzy", k=14, distance="dtw")
plot(tclust)
p <- plot(tclust, plot=FALSE)

l1 <- extract_layers(p, idx = 1L) #get the centroid layer
delete_layers(p, idx = 1L) #delete the centroid layer
append_layers(p, l1)

clusters <- list()
clusters[[2]] <- plot(tclust, type = "centroid", labs.arg = list(title = "Clusters' centroids"))

#CLUSTER SUMMARIES
datRatios$cluster <- NA
for (row in 1:nrow(datRatios)) {
  datRatios[row,3:26]
  pred <- predict(tclust, datRatios[row,3:26])
  datRatios[row,]$cluster <-max.col(pred)
}
clusterSummaries <- datRatios %>% 
  group_by(TYPE, cluster) %>% 
  summarise(n = n())

clusterSummaries$cluster <- factor(clusterSummaries$cluster)
ggplot(data = clusterSummaries, aes(x=cluster, y=TYPE, fill=n)) + 
  geom_tile(color = "white") +
  geom_text(aes(cluster, TYPE, label=n), color = "black", size = 4) +
  scale_fill_gradient(high="red", low="white")





#Print project data on cluster
ci_projects <- unique(datStaticCI$project)

for(proj in ci_projects){
  project_ratio <- datStaticCI %>% filter(project == proj) 
  project_custer <- (datRatios %>% filter(project == proj))[1,27]
  
  p <- plot(tclust, plot=FALSE, clus=project_custer)
  plot(p)
  l <- extract_layers(p, idx = 2L) #get the lines layer
  l[[1]]$data <- l[[1]]$data %>% filter(color == 0) #select only one line
  ratio_dta <- project_ratio %>% select(version, test_ratio) #get data from the project
  l[[1]]$data$value <- ratio_dta$test_ratio[match(l[[1]]$data$t, ratio_dta$version)] #replace the line data with the data from the project
  delete_layers(p, idx = 2L) #delete the layer of lines
  append_layers(p, l) #put the filtered layer
  p <- p + labs(x = "Version", y = "Test Ratio")    
  png(paste0("questionnaire_plots/test_ratio/", proj, "_test_ratio_cluster",".png")) 
  plot(p)
  dev.off()
}


#RQ2
#CI
datStaticCI = read.csv("csv_data/projects-size-CI.csv", header = TRUE)
# datStaticCI$test_ratio <- round(datStaticCI$test_ratio,2)
datStaticCI <- datStaticCI %>% mutate(CI = if_else(version <= 12, "NO", "YES"))
wilcox.test((datStaticCI %>% filter(CI == "NO"))$test_ratio, (datStaticCI %>% filter(CI == "YES"))$test_ratio, alternative = "less")
cliff.delta((datStaticCI %>% filter(CI == "NO"))$test_ratio, (datStaticCI %>% filter(CI == "YES"))$test_ratio)

beanplot((datStaticCI %>% filter(CI == "NO"))$test_ratio, (datStaticCI %>% filter(CI == "YES"))$test_ratio, side="both", col=list("gray", c("#0097A7", "white")),
         xaxt="n", what=c(1,1,1,0))
legend("topright", fill = c("gray", "#0097A7"), legend = c("BEFORE-CI", "AFTER-CI") )
mtext(side = 1, text = "Test Ratio", line = 1)

# datStaticCI_BEFOREMedian <- datStaticCI %>%
#   filter(CI == "NO") %>%
#   group_by(project) %>%
#   mutate(median = median(test_ratio)) %>%
#   filter(version==1)
# datStaticCI_AFTERMedian <- datStaticCI %>%
#   filter(CI == "YES") %>%
#   group_by(project) %>%
#   mutate(median = median(test_ratio)) %>%
#   filter(version==13)
# 
# beanplot(datStaticCI_BEFOREMedian$test_ratio, datStaticCI_AFTERMedian$test_ratio, side="both", col=list("gray", c("#0097A7", "white")),
#          main = "Median Test Ratio (Before and After adopting CI)")
#          legend("topright", fill = c("gray", "#0097A7"), legend = c("NOCI", "CI") )
# 


#NOCI
datStaticNOCI = read.csv("csv_data/projects-size-NOCI.csv", header = TRUE)
# datStaticNOCI$test_ratio <- round(datStaticNOCI$test_ratio,2)
datStaticNOCI <- datStaticNOCI %>% mutate(CI = if_else(version <= 12, "NO", "YES"))
wilcox.test((datStaticNOCI %>% filter(CI == "NO"))$test_ratio, (datStaticNOCI %>% filter(CI == "YES"))$test_ratio, alternative = "less")
cliff.delta((datStaticNOCI %>% filter(CI == "NO"))$test_ratio, (datStaticNOCI %>% filter(CI == "YES"))$test_ratio)

beanplot((datStaticNOCI %>% filter(CI == "NO"))$test_ratio, (datStaticNOCI %>% filter(CI == "YES"))$test_ratio, side="both", col=list("gray", c("#0097A7", "white")),
         main = "", xaxt="n", what=c(1,1,1,0))
legend("topright", fill = c("gray", "#0097A7"), legend = c("early-NOCI", "late-NOCI") )
mtext(side = 1, text = "Test Ratio", line = 1)

# #COMPARE NOCI BEFORE / CI BEFORE
# wilcox.test((datStaticNOCI %>% filter(CI == "NO"))$test_ratio, (datStaticCI %>% filter(CI == "NO"))$test_ratio, alternative = "greater")
# cliff.delta((datStaticNOCI %>% filter(CI == "NO"))$test_ratio, (datStaticCI %>% filter(CI == "NO"))$test_ratio)
# beanplot((datStaticNOCI %>% filter(CI == "NO"))$test_ratio, (datStaticCI %>% filter(CI == "NO"))$test_ratio, side="both", col=list("gray", c("#0097A7", "white")),
#          main = "", xaxt="n", what=c(1,1,1,0))
# legend("topright", fill = c("gray", "#0097A7"), legend = c("Before", "After") )
# 
# #COMPARE NOCI BEFORE / CI BEFORE
# wilcox.test((datStaticNOCI %>% filter(CI == "YES"))$test_ratio, (datStaticCI %>% filter(CI == "YES"))$test_ratio, alternative = "greater")
# cliff.delta((datStaticNOCI %>% filter(CI == "YES"))$test_ratio, (datStaticCI %>% filter(CI == "YES"))$test_ratio)
# beanplot((datStaticNOCI %>% filter(CI == "YES"))$test_ratio, (datStaticCI %>% filter(CI == "YES"))$test_ratio, side="both", col=list("gray", c("#0097A7", "white")),
#          main = "", xaxt="n", what=c(1,1,1,0))
# legend("topright", fill = c("gray", "#0097A7"), legend = c("Before", "After") )


# datStaticNOCI_BEFOREMedian <- datStaticNOCI %>%
#   filter(CI == "NO") %>%
#   group_by(project) %>%
#   mutate(median = median(test_ratio)) %>%
#   filter(version==1)
# datStaticNOCI_AFTERMedian <- datStaticNOCI %>%
#   filter(CI == "YES") %>%
#   group_by(project) %>%
#   mutate(median = median(test_ratio)) %>%
#   filter(version==13)
# 
# beanplot(datStaticNOCI_BEFOREMedian$test_ratio, datStaticNOCI_AFTERMedian$test_ratio, side="both", col=list("gray", c("#0097A7", "white")),
#          main = "Median Test Ratio for Projects not related with CI\n(Before/After central date)")
# legend("topright", fill = c("gray", "#0097A7"), legend = c("BEFORE", "AFTER") )

#COMPARING NOCI WITH CI
# wilcox.test(datStaticNOCI_AFTERMedian$test_ratio, datStaticCI_AFTERMedian$test_ratio, alternative = "less")

#COMPARING THE GAINING CODE PROPORTION
# noCIDiff = datStaticNOCI_AFTERMedian$test_ratio - datStaticNOCI_BEFOREMedian$test_ratio
# ciDiff = datStaticCI_AFTERMedian$test_ratio - datStaticCI_BEFOREMedian$test_ratio
# wilcox.test(noCIDiff, ciDiff, alternative = "less")
# boxplot(noCIDiff, ciDiff, names=c("NOCI","CI"),  main = "Test code ratio difference")
# 
# median(noCIDiff)
# median(ciDiff)
# 

#Grouth Test Ratio
growthTestRatioCI <- datStaticCI %>% arrange(version) %>%  arrange(project) %>%
  group_by(CI, project) %>%
  mutate(Diff = test_ratio - lag(test_ratio)) %>% filter(!is.na(Diff))


growthTestRatioNOCI <- datStaticNOCI %>% arrange(version) %>%  arrange(project) %>%
  group_by(CI, project) %>%
  mutate(Diff = test_ratio - lag(test_ratio)) %>% filter(!is.na(Diff))

#Growth CI 
wilcox.test((growthTestRatioCI %>% filter(CI == "NO"))$Diff, (growthTestRatioCI %>% filter(CI == "YES"))$Diff)
cliff.delta((growthTestRatioCI %>% filter(CI == "NO"))$Diff, (growthTestRatioCI %>% filter(CI == "YES"))$Diff)
beanplot((growthTestRatioCI %>% filter(CI == "NO"))$Diff,  (growthTestRatioCI %>% filter(CI == "YES"))$Diff, side="both", col=list("gray", c("#0097A7", "white")),
         main = "", xaxt="n", what=c(1,1,1,0),
         ylim = c(-0.05, 0.05))
legend("topright", fill = c("gray", "#0097A7"), legend = c("BEFORE-CI", "AFTER-CI") )
mtext(side = 1, text = "Test Ratio Growth", line = 1)

#Growth NOCI
wilcox.test((growthTestRatioNOCI %>% filter(CI == "NO"))$Diff, (growthTestRatioNOCI %>% filter(CI == "YES"))$Diff)
cliff.delta((growthTestRatioNOCI %>% filter(CI == "NO"))$Diff, (growthTestRatioNOCI %>% filter(CI == "YES"))$Diff)
beanplot((growthTestRatioNOCI %>% filter(CI == "NO"))$Diff,  (growthTestRatioNOCI %>% filter(CI == "YES"))$Diff, side="both", col=list("gray", c("#0097A7", "white")),
         main = "", xaxt="n", what=c(1,1,1,0),
         ylim = c(-0.05, 0.05))
legend("topright", fill = c("gray", "#0097A7"), legend = c("early-NOCI", "late-NOCI") )
mtext(side = 1, text = "Test Ratio Growth", line = 1)

#Growth BEFORE/BEFORE
wilcox.test((growthTestRatioNOCI %>% filter(CI == "NO"))$Diff, (growthTestRatioCI %>% filter(CI == "NO"))$Diff)
cliff.delta((growthTestRatioNOCI %>% filter(CI == "NO"))$Diff, (growthTestRatioCI %>% filter(CI == "NO"))$Diff)
beanplot((growthTestRatioNOCI %>% filter(CI == "NO"))$Diff,  (growthTestRatioCI %>% filter(CI == "NO"))$Diff,
         side="both", col=list("gray", c("#0097A7", "white")),
         ylim = c(-0.05, 0.05),
         xaxt="n", what=c(1,1,1,0))
legend("topright", fill = c("gray", "#0097A7"), legend = c("early-NOCI", "before-CI") )
mtext(side = 1, text = "Test Ratio Growth", line = 1)

#Growth AFTER/AFTER
wilcox.test((growthTestRatioNOCI %>% filter(CI == "YES"))$Diff, (growthTestRatioCI %>% filter(CI == "YES"))$Diff)
cliff.delta((growthTestRatioNOCI %>% filter(CI == "YES"))$Diff, (growthTestRatioCI %>% filter(CI == "YES"))$Diff)
beanplot((growthTestRatioNOCI %>% filter(CI == "YES"))$Diff,  (growthTestRatioCI %>% filter(CI == "YES"))$Diff,
         side="both", col=list("gray", c("#0097A7", "white")),
         ylim = c(-0.05, 0.05),
         main = "", xaxt="n", what=c(1,1,1,0))
legend("topright", fill = c("gray", "#0097A7"), legend = c("late-NOCI", "after-CI") )
mtext(side = 1, text = "Test Ratio Growth", line = 1)


#RQ3
coverageCI = read.csv("csv_data/coverage_CI.csv", header = TRUE)
coverageCI <- coverageCI %>% mutate(CI = if_else(version <= 12, "NO", "YES"))
growthRatePercentCI <- coverageCI %>% arrange(version) %>%  arrange(project) %>%
  group_by(CI, project) %>%
  mutate(Diff = percent - lag(percent)) %>% filter(!is.na(Diff))

coverageNOCI = read.csv("csv_data/coverage_NOCI.csv", header = TRUE)
coverageNOCI <- coverageNOCI %>% mutate(CI = if_else(version <= 12, "NO", "YES"))
growthRatePercentNOCI <- coverageNOCI %>% arrange(version) %>%  arrange(project) %>%
  group_by(CI, project) %>%
  mutate(Diff = percent - lag(percent)) %>% filter(!is.na(Diff))




#CI
dat <- coverageCI
growthRatePercentDat <- growthRatePercentCI
#NOCI
dat <- coverageNOCI
growthRatePercentDat <- growthRatePercentNOCI

beanplot(
  (dat %>% filter(CI == "NO"))$percent, (dat %>% filter(CI == "YES"))$percent,
  side="both", col=list("gray", c("#0097A7", "white")),
  main = "% Coverage of NOCI Projects", xaxt="n", what=c(1,1,1,0))
legend("topright", fill = c("gray", "#0097A7"), legend = c("BEFORE", "AFTER") )

wilcox.test((dat %>% filter(CI == "NO"))$percent, (dat %>% filter(CI == "YES"))$percent)
cliff.delta((dat %>% filter(CI == "NO"))$percent, (dat %>% filter(CI == "YES"))$percent)
wilcox.test((growthRatePercentDat %>% filter(CI == "NO"))$Diff, (growthRatePercentDat %>% filter(CI == "YES"))$Diff, alternative = "less")
var((growthRatePercentDat %>% filter(CI == "NO"))$Diff)
var((growthRatePercentDat %>% filter(CI == "YES"))$Diff)

projects <- unique(dat$project)
pctPlots <- list()
grouthRatePlots <- list();

ggplot(dat, aes(x=version, y=percent)) + labs(title="\t\t\t\t\t% Covered of CI projects\n") + ylab("% covered") +
  xlab("Version") + geom_line(aes(x=version, y=percent, colour=project)) +
  geom_vline(xintercept = 12.5, linetype="dotted", color = "black", size=1) +
  theme(legend.position = "none")



for(p in projects){
  print("----------------------")
  print(p)
  print("----------------------")
  
  projectData <- dat[dat$project == p,]
  projectData_AFTER = projectData %>% filter(CI == "YES")
  projectData_BEFORE = projectData %>% filter(CI == "NO")
  
  projectDataGrowth <- growthRatePercentDat[growthRatePercentDat$project == p,]
  projectDataGrowth_AFTER = projectDataGrowth %>% filter(CI == "YES")
  projectDataGrowth_BEFORE = projectDataGrowth %>% filter(CI == "NO")
  
  #percentPlot
  pctPlots[[p]] <- 
    ggplot(data = projectData, aes(x=version, y=percent, group=CI)) +
    ylab("% covered") +
    xlab("Version") +
    ggtitle(p) +
    geom_line(color="red")+
    geom_vline(xintercept = 12.5, linetype="dotted", color = "gray", size=1) +
    geom_smooth(method = 'lm', size=0.5)
  png(paste0("questionnaire_plots/test_coverage/", p, "_test_coverage_cluster",".png")) 
  plot(pctPlots[[p]])
  dev.off()
  
  #growthRatePercentPlot
  grouthRatePlots[[p]] <- ggplot(data = projectDataGrowth, aes(x=version, y=Diff, group=CI)) +
    ylab("Grouth Rate") +
    xlab("Version Transition") +
    ggtitle(p) +
    geom_line(color="red")+
    geom_vline(xintercept = 13, linetype="dotted", color = "gray", size=1) +
    geom_smooth(method = 'lm', size=0.5)
  
  
  print("MEAN NOCI")
  print(mean(projectData_BEFORE$percent))
  print("MEAN CI")
  print(mean(projectData_AFTER$percent))
  
  print("WILCOX PERCENT")
  print(wilcox.test(projectData_BEFORE$percent, projectData_AFTER$percent, alternative = "less"))
  print("CLIFFS DELTA")
  print(cliff.delta(projectData_BEFORE$percent, projectData_AFTER$percent))
  
}
gridExtra::grid.arrange(grobs = pctPlots, ncol=2, top="% Covered of NOCI Projects")
gridExtra::grid.arrange(grobs = grouthRatePlots, ncol=2, top="% Covered")
singlePlot


coverageCI_BEFORE = coverageCI %>% filter(CI == "NO")
coverageNOCI_BEFORE = coverageNOCI %>% filter(CI == "NO")

coverageCI_AFTER = coverageCI %>% filter(CI == "YES")
coverageNOCI_AFTER = coverageNOCI %>% filter(CI == "YES")


#CI/NOCI AFTER
beanplot(
  coverageNOCI_AFTER$percent, coverageCI_AFTER$percent,
  side="both", col=list("gray", c("#0097A7", "white")), main = "" , xaxt="n", what=c(1,1,1,0))
legend("topright", fill = c("gray", "#0097A7"), legend = c("late-NOCI", "after-CI") )
mtext(side = 1, text = "% Coverage", line = 1)


wilcox.test(coverageNOCI_AFTER$percent, coverageCI_AFTER$percent, alternative = "less")
cliff.delta(coverageNOCI_AFTER$percent, coverageCI_AFTER$percent)

#CI/NOCI GROWTH AFTER
beanplot(
  (growthRatePercentNOCI%>%filter(CI == "YES"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff,
  side="both", col=list("gray", c("#0097A7", "white")), main = "Growth % Coverage between versions \n of NOCI/CI Projects after analysis point", xaxt="n", what=c(1,1,1,0),
  ylim = c(-5, 5))
legend("topright", fill = c("gray", "#0097A7"), legend = c("NOCI", "CI") )

wilcox.test(coverageNOCI_AFTER$percent, coverageCI_AFTER$percent, alternative = "less")
cliff.delta(coverageNOCI_AFTER$percent, coverageCI_AFTER$percent)


#CI GROWTH BEFORE/AFTER
beanplot(
  (growthRatePercentCI%>%filter(CI == "NO"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff,
  side="both", col=list("gray", c("#0097A7", "white")), main = "", xaxt="n", what=c(1,1,1,0),
  ylim = c(-5, 5))
legend("topright", fill = c("gray", "#0097A7"), legend = c("BEFORE-CI", "AFTER-CI") )
mtext(side = 1, text = "% Coverage Growth", line = 1)


wilcox.test((growthRatePercentCI%>%filter(CI == "NO"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff, alternative="less")
cliff.delta((growthRatePercentCI%>%filter(CI == "NO"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff)

#NOCI GROWTH BEFORE/AFTER
beanplot(
  (growthRatePercentNOCI%>%filter(CI == "NO"))$Diff, (growthRatePercentNOCI%>%filter(CI == "YES"))$Diff,
  side="both", col=list("gray", c("#0097A7", "white")), main = "", xaxt="n", what=c(1,1,1,0),
  ylim = c(-5, 5))
legend("topright", fill = c("gray", "#0097A7"), legend = c("early-NOCI", "late-NOCI") )
mtext(side = 1, text = "% Coverage Growth", line = 1)

wilcox.test((growthRatePercentNOCI%>%filter(CI == "NO"))$Diff, (growthRatePercentNOCI%>%filter(CI == "YES"))$Diff, alternative="greater")
cliff.delta((growthRatePercentNOCI%>%filter(CI == "NO"))$Diff, (growthRatePercentNOCI%>%filter(CI == "YES"))$Diff)

#NOCI/CI GROWTH AFTER
beanplot(
  (growthRatePercentNOCI%>%filter(CI == "YES"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff,
  side="both", col=list("gray", c("#0097A7", "white")), main = "", xaxt="n", what=c(1,1,1,0),
  ylim = c(-5, 5))
legend("topright", fill = c("gray", "#0097A7"), legend = c("late-NOCI", "after-CI"))
mtext(side = 1, text = "% Coverage Growth", line = 1)

wilcox.test((growthRatePercentNOCI%>%filter(CI == "YES"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff, alternative="less")
cliff.delta((growthRatePercentNOCI%>%filter(CI == "YES"))$Diff, (growthRatePercentCI%>%filter(CI == "YES"))$Diff)
  