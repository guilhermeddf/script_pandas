#install.packages("dplyr")
#install.packages("beanplot")
#install.packages("dtwclust")
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("ggpmisc")
#install.packages("Hmisc")
#install.packages("arm")
#install.packages("coefplot")
# install.packages("stargazer")
# install.packages("vioplot")

# pkgs <- c("factoextra",  "NbClust")
# install.packages(pkgs)

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
library(Hmisc)
library(MuMIn)
library(rms)
library(coefplot)
library(stargazer)
library(vioplot)

# 'Normalizar' numero de builds / sonar period days
#  Fazer um modelo lm

dat = read.csv("/home/case-b315/Desktop/script_pandas/gustavo/dataset_modelo_praticas_ci.csv", header = TRUE)
# dat <- dat %>% filter(sonar_duration > 90) #at least one month of sonar
dat <- dat %>% filter(ccq_builds_in_sonar_period >= 20) #at least one month of sonar
dat <- mutate(dat, ccq_ratio_activity=ci_builds_in_sonar_period/ccq_builds_in_sonar_period)
dat <- dat %>% filter(ccq_ratio_activity > 1) #at 0.11 ratio

dat <- mutate(dat, ci_builds_activity=ci_builds_total_activity_days/sonar_duration)
dat <- mutate(dat, commits_activity=commits_total_activity_days/sonar_duration)

# dat1 <- dat %>% filter(ci_builds_total_activity_days <= 90)
# hist(dat1$ci_builds_total_activity_days)

# dat <- dat %>% filter(ci_builds_per_sonar_days > 0.1) 
# dat <- dat %>% filter(ci_builds_total_activity_days > 10) 
summary(dat)
nrow(dat)

#RQ - Niveis adocao de CI, correlacionar com TDs??
dat = mutate(dat, cov_quantile = ntile(dat$median_coverage,4))
dat = mutate(dat, citime_quantile = ntile(dat$median_ci_time_running,4))
dat = mutate(dat, time_to_fix_quantile = ntile(dat$median_ci_time_to_fix,4))
dat = mutate(dat, ci_activity_quantile = ntile(dat$ci_builds_activity,4))
# write.csv(dat,"technical_debts/RQ3/RQ3_RQ4_DataSet.csv", row.names = FALSE)

summary((dat %>% filter(dat$ci_activity_quantile == 4))$ci_builds_activity)


######## INDEPENDENT VARIABLE #########
var.independent = "citime_quantile"; var.firstQt="Faster Builds"; var.thirdQt="Slower Builds";
var.independent = "time_to_fix_quantile"; var.firstQt="Faster Fixes"; var.thirdQt="Slower Fixes";
var.independent = "cov_quantile"; var.firstQt="Lower Coverage"; var.thirdQt="Higher Coverage";
var.independent = "ci_activity_quantile"; var.firstQt="Lower CI Activity"; var.thirdQt="Higher CI Activity";

######## RESPONSE VARIABLE #########
var.response = "median_bugs_density"; var.response_title = "Bugs Density";
var.response = "median_code_smells_density"; var.response_title = "Code Smells Density";
var.response = "median_duplicated_lines_density"; var.response_title = "Duplicated Lines Density";
var.response = "median_sqale_ratio"; var.response_title = "Technical Debts Ratio";


vioplot((dat %>% filter(dat[[var.independent]] == 1))[[var.response]],
        (dat %>% filter(dat[[var.independent]] == 4))[[var.response]], 
        col = c("gray", "#0097A7"), names = c(var.firstQt, var.thirdQt), 
        # plotCentre="line",
        ylab = var.response_title)

wilcox.test(
  (dat %>% filter(dat[[var.independent]] == 1))[[var.response]],
  (dat %>% filter(dat[[var.independent]] == 4))[[var.response]]
)
cliff.delta(
  (dat %>% filter(dat[[var.independent]] == 1))[[var.response]],
  (dat %>% filter(dat[[var.independent]] == 4))[[var.response]]
)






# MODEL

plot(varclus(data=dat,
             ~ median_coverage +
               median_ci_time_running +
               median_ci_time_to_fix +
               ci_builds_activity +
               ccq_ratio_activity +
               median_ncloc
             
))
abline(h=0.3,lty=2)

redun(data=dat, nk=0, 
      ~median_coverage +
        median_ci_time_running +
        median_ci_time_to_fix +
        ci_builds_activity +
        ccq_ratio_activity +
        median_ncloc
)

d <- datadist(dat) 
options(datadist='d') 

model <- ols(median_sqale_index ~ 
               (median_coverage +
                  median_ci_time_running +
                  median_ci_time_to_fix +
                  ci_builds_activity) *
               (ccq_ratio_activity +
                  median_ncloc)
             , data = dat, x=TRUE, y=TRUE)

# stargazer(model, title="Regression Results", align=TRUE)


plot(anova(model), what='proportion chisq')

anova(model, test = "Chisq")
rms::validate(model, B=1000)


# #FIND MODELS2
# library(lme4)
# model2 <- lmer(median_sqale_index ~ 
#                   median_coverage +
#                   median_ci_time_running +
#                   median_ci_time_to_fix +
#                   ci_builds_activity +
#                   ccq_ratio_activity
#                + (1 | median_ncloc), dat,  REML=F)
# plot(model2)
# r.squaredGLMM(model2)  
# Anova(model2, type=c("III")) 
# summary(model2)   

