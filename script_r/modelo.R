library(arm)
library(lme4)
library(car)
library(MuMIn)
library(texreg)

library(dplyr)
library(ggplot2) 
library(arm)
library(rms)
library(stringr)

# Fields exported from mongo
# mongoexport --host=localhost:27017 --db=ghtorrent --collection=versions_transitions_statistics --type csv --out versions_transitions_statistics1.csv  --fields transitionVersion,project,medianInsertions,medianDeletions,medianFiles,medianChanges,medianNumberOfCodeFiles,medianNumberOfCodeFilesChanges,medianMedianDepthCodeFiles,totalCommits,totalChanges,totalNumberOfCodeFilesChanges,totalNumberOfTestFilesChanges,changesPerCommit,codeChangesPerCommit,testChangesPerCommit,countCommitsWithTests,testCommitRatio,totalCommitters,countLowContributors,totalSubmittedPR,totalClosedPR,totalMergedPR,medianTimesToClosePR,medianAddLinesPR,medianDeletedLinesPR,medianChangedFilesPR,medianAddedCommitsPR,medianAddedCommentsPR,medianReviewCommentsPR,medianMergedInsertionsPR,medianMergedDeletionsPR,medianMergedChangesPR,medianMergedTestChangesPR,medianMergedCodeChangesPR


datStaticCI = read.csv("csv_data/projects-size-CI.csv", header = TRUE)
datStaticCI <- datStaticCI %>% mutate(CI = if_else(version <= 12, "NO", "YES"))
versionsTransitionsStatistics = read.csv("csv_data/versions_transitions_statistics.csv", header = TRUE)


versionsTransitionsStatistics$project <- str_split_fixed(versionsTransitionsStatistics$project, "/", 2)[,2]
ds = merge(x = datStaticCI, y = versionsTransitionsStatistics, by.x = c("project", "version"), by.y = c("project", "transitionVersion"), all.x = TRUE)
ds[is.na(ds)] <- 0

# dsa <- ds %>% filter(version < 11)
dsa <- ds %>% filter(version <= 12)
# dsb <- ds %>% filter(version > 13)
dsb <- ds %>% filter(version >= 13)


#ALL VARS#
# medianInsertions+medianDeletions+medianFiles+medianChanges+medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+totalChanges+totalNumberOfCodeFilesChanges+totalNumberOfTestFilesChanges+changesPerCommit+codeChangesPerCommit+testChangesPerCommit+countCommitsWithTests+testCommitRatio+totalCommitters+totalSubmittedPR+totalClosedPR+totalMergedPR+medianTimesToClosePR+medianAddLinesPR+medianDeletedLinesPR+medianChangedFilesPR+medianAddedCommitsPR+medianAddedCommentsPR+medianReviewCommentsPR+medianMergedInsertionsPR+medianMergedDeletionsPR+medianMergedChangesPR+medianMergedTestChangesPR+medianMergedCodeChangesPR


#DSA
plot(varclus(data=dsa,
             ~
               medianDeletions+medianFiles+medianChanges+medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+changesPerCommit+codeChangesPerCommit+testChangesPerCommit+countCommitsWithTests+testCommitRatio+totalCommitters+totalMergedPR+medianTimesToClosePR+medianAddLinesPR+medianAddedCommentsPR+medianReviewCommentsPR+medianMergedChangesPR+medianMergedTestChangesPR
))
abline(h=0.3,lty=2)

redun(data=dsa, nk=0, 
      ~medianDeletions+medianFiles+medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+changesPerCommit+codeChangesPerCommit+testChangesPerCommit+countCommitsWithTests+testCommitRatio+totalCommitters+totalMergedPR+medianTimesToClosePR+medianAddedCommentsPR+medianReviewCommentsPR+medianMergedChangesPR+medianMergedTestChangesPR
)


#DSB
plot(varclus(data=dsb,
             ~
               medianDeletions+medianFiles+medianChanges+medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+totalNumberOfCodeFilesChanges+changesPerCommit+codeChangesPerCommit+testChangesPerCommit+countCommitsWithTests+testCommitRatio+totalCommitters+totalSubmittedPR+totalMergedPR+medianTimesToClosePR+medianAddLinesPR+medianAddedCommentsPR+medianReviewCommentsPR+medianMergedChangesPR+medianMergedTestChangesPR+medianMergedCodeChangesPR
))
abline(h=0.3,lty=2)

redun(data=dsa, nk=0, 
      ~medianDeletions+medianFiles+medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+totalNumberOfCodeFilesChanges+codeChangesPerCommit+testChangesPerCommit+countCommitsWithTests+testCommitRatio+totalCommitters+totalSubmittedPR+totalMergedPR+medianTimesToClosePR+medianAddedCommentsPR+medianReviewCommentsPR+medianMergedChangesPR+medianMergedTestChangesPR
)


#FIND MODELS
model1 <- lmer(test_ratio ~
                 version:project + 
                 medianFiles
               +medianMedianDepthCodeFiles
               +totalCommits
               +testChangesPerCommit
               +countCommitsWithTests
               +testCommitRatio
               + (1 | project), dsa,  REML=F)

plot(model1)
r.squaredGLMM(model1)  
Anova(model1, type=c("III")) 
summary(model1)
print(model1, correlation=TRUE) #pegar os coeficientes 


#FIND MODELS
model2 <- lmer(test_ratio ~
                 version:project + 
                 medianFiles+medianMedianDepthCodeFiles+totalCommits+testChangesPerCommit+countCommitsWithTests+testCommitRatio+totalSubmittedPR+totalMergedPR+medianAddedCommentsPR
               + (1 | project), dsa,  REML=F)

plot(model2)
r.squaredGLMM(model2)  
Anova(model2, type=c("III")) 
summary(model2)
print(model1, correlation=TRUE) #pegar os coeficientes 

























#ALL VARS#
# medianInsertions + medianDeletions + medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + totalChanges + totalNumberOfCodeFilesChanges + totalNumberOfTestFilesChanges + changesPerCommit + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + testCommitRatio + totalCommitters + countLowContributors #


##TESTS##
# Prepare data:
#checar variaveis correlacionadas... remove e repete até não ter mais nenhuma

#DSA
plot(varclus(data=dsa,
             ~
               medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + totalCommitters
))
abline(h=0.3,lty=2)
#sai totalChanges, fica totalNumberOfCodeFilesChanges (por ser mais específico como métrica de código e pode afetar mais na relação de test_ratio)
# sai changesPerCommit e fica codeChangesPerCommit(por ser mais específico como métrica de código e pode afetar mais na relação de test_ratio)
# sai medianInsertions+medianDeletions e fica medianChanges (representa as duas variaveis)
# sai totalNumberOfTestFilesChanges e fica testChangesPerCommit (mais granular)
# sai testCommitRatio e fica countCommitsWithTests (mais granular)
# sai countLowContributors e fica totalCommiters (countLowContributors usava calculos em commits futuros)
# sai totalNumberOfCodeFilesChanges e fica codeCHangesPerCommit (mais granular)

#remove as redundantes
redun(data=dsa, nk=0, 
      ~medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + totalCommitters)

# sai medianChanges redundant
#RESULT DSA VARIABLES= medianFiles + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + totalCommitters



#DSB
plot(varclus(data=dsb,
             ~
               medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + changesPerCommit + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + testCommitRatio + totalCommitters
))
abline(h=0.3,lty=2)
# sai totalNumberOfTestFilesChanges fica testChangesPerCommit (mais granular)
# sai medianInsertions+medianDeletions e fica medianChanges (representa as duas variaveis)
# sai countLowContributors e fica totalCommiters (countLowContributors usava calculos em commits futuros)
# sai totalChanges e fica totalNumberOfCodeFilesChanges (mas especifica de codigo)
# sai totalNumberOfCodeFilesChanges e fica codeChangesPerCommit (mais granular)

#remove as redundantes
redun(data=dsb, nk=0, 
      ~medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + changesPerCommit + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + testCommitRatio + totalCommitters)

# no reduntant redundant
#RESULT DSB VARIABLES= medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + changesPerCommit + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + testCommitRatio + totalCommitters











#FIND MODELS
#DSA VARIABLES= medianFiles + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + totalCommitters  


model1 <- lmer(test_ratio ~
                 version:project
               + medianFiles
               + totalCommits
               + testChangesPerCommit
               + countCommitsWithTests
               + (1 | project), dsa,  REML=F)



plot(model1)
r.squaredGLMM(model1)  
Anova(model1, type=c("III")) 
summary(model1)
print(model1, correlation=TRUE) #pegar os coeficientes 


write.csv(summary(model1)$coefficients, "model_1_coefficients.csv" )

Anova(model1, type=c("III"))
capture.output(Anova(model1, type=c("III")), file="model_1_anova.txt")


#Variaceis + Raciocínio = FALAR QUE FORAM REMOVIDAS AS VARIAVEIS CORRELACIONADAS (Seguindo o princípio da parcimônia... etc... seguir modelo do paper enviado por daniel)
#SOMAR OS CHIR E REPORTAR A PROPORÇÂO
#Falar da influencia do projeto
#COmentar cada variável (Citar trabalhos relacionados...)

#DSB VARIABLES= medianFiles + medianChanges + medianNumberOfCodeFiles + medianNumberOfCodeFilesChanges + medianMedianDepthCodeFiles + totalCommits + changesPerCommit + codeChangesPerCommit + testChangesPerCommit + countCommitsWithTests + testCommitRatio + totalCommitters
model2 <- lmer(test_ratio ~ 
                 version:project 
               + totalCommits
               + testChangesPerCommit
               + countCommitsWithTests
               + totalCommitters
               + (1 | project),
               dsb,  REML=F)

plot(model2)
r.squaredGLMM(model2)  
Anova(model2, type=c("III")) 
summary(model2)

write.csv(summary(model2)$coefficients, "model_2_coefficients.csv" )
capture.output(Anova(model2, type=c("III")), file="model_2_anova.txt")




# 
# #BEST ANOVA 
# # model1 <- lmer(test_ratio ~ medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+countCommitsWithTests + (1|project), dsa, REML=FALSE)
# #BEST AIC
# # model1 <- lmer(test_ratio ~ totalChanges + totalCommits + totalNumberOfCodeFilesChanges + medianNumberOfCodeFiles + changesPerCommit + codeChangesPerCommit + totalCommitters + countLowContributors + (1|project), dsa, REML=FALSE)
# 
# 
# model1 <- lmer(test_ratio ~ medianNumberOfCodeFiles+medianNumberOfCodeFilesChanges+medianMedianDepthCodeFiles+totalCommits+countCommitsWithTests + (1|project), dsa)
# 
# plot(model1)
# 
# Anova(model1, type=c("III")) 
# summary(model1)
# 
# 
# r.squaredGLMM(model1)  
# 
# 
# #---> MODEL AFTER
#   # totalNumberOfCodeFilesChanges
#   # totalNumberOfTestFilesChanges
#   # changesPerCommit
#   # testChangesPerCommit
#   # testCommitRatio
# #BEST ANOVA 
# model2 <- lmer(test_ratio ~ totalNumberOfTestFilesChanges + changesPerCommit + testCommitRatio + (1|project), dsb, REML=FALSE)
# 
# plot(model2)
# 
# Anova(model2, type=c("III")) 
# summary(model2)
# 
# r.squaredGLMM(model2)