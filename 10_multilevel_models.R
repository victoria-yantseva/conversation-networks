setwd ("k")

 

library(tidyverse)
library (tseries)
library(car)
library(lme4)

fb = read.csv ("flashback_replication_data.csv")

###PREPARE THE DATA
#the code below assumes a chosen threshold of 27 minutes (can be replaced to 13 or 54 min. by choosing convid13 or convid54 columns)

fb <- fb[fb$convid != "NaN",]
fb$convid <- as.character(fb$convid)
fb <- subset (fb, select = -c(convid13, convid54)) 



fb<-fb %>% 
  dplyr::group_by (convid) %>%
  filter(length(convid)>2)%>%
  drop_na() 


fb$am_dummy <- ifelse (fb$num_am>0, 1, 0)
fb$mm_dummy <- ifelse (fb$num_mm>0, 1, 0)
fb$all_dummy <- ifelse (fb$num_all>0, 1, 0)

fb$delta_scaled <- as.vector(scale (fb$time_delta, center = T, scale = T))

fb$delta_logged <-  log(fb$delta_scaled - (min(fb$delta_scaled) - 1))


#create time series with time lagged data
convids <- unique(fb$convid)

length(convids)

lag_dfs <-vector("list")
for(i in 1:length(convids))
{
  print(i)
  conv_agg<- fb[fb$convid == convids[i],]
  
  delta_ts <- ts(conv_agg$delta_logged)
  vader_ts <- ts(conv_agg$vader_score)
  stance_ts <- ts(conv_agg$stance_label)
  
  lag1delta <- stats::lag(delta_ts, -1)
  lag1vader <- stats::lag(vader_ts, -1)
  lag1stance <- stats::lag(stance_ts, -1)

  lag1am <- stats::lag(conv_agg$am_dummy, -1)
  lag1mm <- stats::lag(conv_agg$mm_dummy, -1)
  lag1all <- stats::lag(conv_agg$all_dummy, -1)
  
  lagdata <- ts.intersect(delta_ts, vader_ts, stance_ts, 
                          lag1delta,
                          lag1vader,
                          lag1stance, 
                          lag1am, 
                          lag1mm, 
                          lag1all, dframe=T)

  lag_dfs[[i]] <- lagdata
  lag_dfs[[i]]$convid <- convids[i]

}

lag_dfs <- Filter(function(x) nrow(x) > 0, lag_dfs)
lag_dfs_merged <- bind_rows(lag_dfs)

#MULTILEVEL MODELS

#TIME DELTA - NULL MODEL

m0.glm <- glm(delta_ts ~ 1, family = gaussian, data = lag_dfs_merged)
AIC(logLik(m0.glm))

model0 <- lmer(delta_ts ~ 1 + (1 | convid), data=lag_dfs_merged, REML = F)
AIC(logLik(model0))
summary(model0)



# TIME DELTA - LAG 1  
model1 <- lmer(delta_ts ~ lag1delta + (1  | convid), data=lag_dfs_merged, REML = F)


summary(model1)
car::Anova (model1)


#TIME DELTA AND MEDIA LINKS
model1_2 <- lmer(delta_ts ~ lag1delta  +lag1all +
                 (1  | convid), data=lag_dfs_merged, REML = F)

summary(model1_2)
car::Anova (model1_2)


MuMIn::r.squaredGLMM (model0)
MuMIn::r.squaredGLMM (model1)
MuMIn::r.squaredGLMM (model1_2)



#VADER
m0.glm <- glm (vader_ts ~ 1, data=lag_dfs_merged)
AIC(logLik(m0.glm))

m0 <- lmer(vader_ts ~ 1 + (1 | convid), data=lag_dfs_merged, REML = F)
summary(m0)
AIC(logLik(m0))


m1 <- lmer(vader_ts ~ lag1vader + (1  | convid), data=lag_dfs_merged, REML = F)
summary(m1)
car::Anova (m1)

m1_2 <- lmer(vader_ts ~ lag1vader + lag1all + (1 | convid), data=lag_dfs_merged, REML = F)
summary(m1_2)
car::Anova (m1_2)



MuMIn::r.squaredGLMM (m0)
MuMIn::r.squaredGLMM (m1)
MuMIn::r.squaredGLMM (m1_2)
















