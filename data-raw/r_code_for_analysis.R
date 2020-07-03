##################################
# Packages
library(dplyr)
library(tidyr)
library(survival)
library(survminer)
library(lubridate)

# load dataframe
df <- as.data.frame(read.csv("/Users/Dunistrator/desktop/Dropbox/dj_desktop/Research/14.2020_Kern_DMO_meh/IG upload/200319_DMO_report1_anonymised.csv", header = TRUE, stringsAsFactors = FALSE))

# assign id to individual eye
eye_id <- df %>%
  distinct(anon_id, eye) %>%
  mutate(
    eye_id = row <- seq(1,length(anon_id),1)) 

df <- df %>%
  mutate(
    va_70 = case_when(
      va >= 70 ~"1",
      T~"0"),
    va_35 = case_when(
      va <= 35~"1",
      T~"0"),
    baseline_va_group = case_when(
      baseline_va >= 70 ~ ">=70",
      baseline_va >=60 & baseline_va <=69 ~ "60-69",
      baseline_va >=50 & baseline_va <=59 ~ "50-59",
      baseline_va >=36 & baseline_va <=49 ~ "36-49",
      baseline_va <= 35 ~ "<=35"),) %>%
  left_join(eye_id, by = c("anon_id", "eye"))


##################################
# time-independent covariates
df_independent <- df %>% 
  distinct(eye_id, gender, ethnicity, baseline_age, baseline_va) 

# time-dependent covariates i.e. injection number 
df_inj <- df %>% 
  filter(inj_given == "y") %>%
  distinct(eye_id, follow_up_days, inj_num) %>%
  arrange(eye_id) 


##################################
# first to VA >70
##################################
# event
va_70_1 <- df %>%
  filter(va_70 == "1") %>%
  group_by(eye_id) %>%
  arrange(follow_up_days) %>%
  slice(which.min(follow_up_days)) %>% 
  ungroup() %>%
  mutate(status = 1)

va_70_0 <- df %>%
  anti_join(va_70_1, by = c("anon_id", "eye")) %>%
  group_by(eye_id) %>%
  arrange(follow_up_days) %>%
  slice(which.max(follow_up_days)) %>% 
  ungroup() %>%
  mutate(status = 0) 

va_70 <- rbind(va_70_0, va_70_1) %>%
  mutate(time = follow_up_days + 1) %>%
  select(eye_id, time, status)

# adding in event
va_70_event <- tmerge(df_independent, va_70, id=eye_id,
                      death=event(time, status)) %>% arrange(eye_id)

# adding in covariate
va_70_df <- tmerge(va_70_event, df_inj, id = eye_id, inj_num = tdc(follow_up_days, inj_num))

# Cox proportional hazard modelling
va_70_fit <- coxph(Surv(tstart, tstop, death) ~ gender + ethnicity + baseline_age + baseline_va  + inj_num,
                   data= va_70_df)
summary(va_70_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status) 

surv_va_70 <- survfit(Surv((time/30), status)~1, data = surv_va_70_covariate)

fig_surv_ttova_70 <- ggsurvplot(surv_va_70,
                                legend.title = "",
                                legend.labs = "", # Change legend labels
                                font.y = 14, # FONT size of risk table contents
                                font.x = 14,
                                font.tickslab = 14,
                                pval = F, conf.int = TRUE,
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                linetype = "strata", # Change line type by groups,
                                break.time.by = 5,
                                xlab = "Time (months)",
                                ylab = "Probability of VA \U2264 69",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18"))


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASEVA
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status, baseline_va_group) 

surv_va_70 <- survfit(Surv((time/30), status)~baseline_va_group, data = surv_va_70_covariate)

fig_surv_ttova_70_baseva <- ggsurvplot(surv_va_70,
                                       legend.title = "VA at baseline",
                                       legend.labs = c("\U2264 35","\U2265 70","36-49","50-59", "60-69"), # Change legend labels
                                       font.y = 14, # FONT size of risk table contents
                                       font.x = 14,
                                       font.tickslab = 14,
                                       pval = F, conf.int = F,
                                       fontsize = 5,
                                       risk.table.col = "strata", # Change risk table color by groups
                                       linetype = "strata", # Change line type by groups,
                                       break.time.by = 5,
                                       xlab = "Time (months)",
                                       ylab = "Probability of VA \U2264 69",
                                       surv.median.line = "hv", # Specify median survival
                                       ggtheme = theme_bw(), # Change ggplot2 theme
                                       palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE AGE
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status, baseline_age) 

surv_va_70 <- survfit(Surv((time/30), status)~baseline_age, data = surv_va_70_covariate)

fig_surv_ttova_70_baseage <- ggsurvplot(surv_va_70,
                                        legend.title = "Age at baseline",
                                        font.y = 14, # FONT size of risk table contents
                                        font.x = 14,
                                        font.tickslab = 14,
                                        pval = F, conf.int = F,
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        linetype = "strata", # Change line type by groups,
                                        break.time.by = 5,
                                        xlab = "Time (months)",
                                        ylab = "Probability of VA \U2264 69",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw())

##################################
# first to VA >35
##################################
# event
va_35_1 <- df %>%
  filter(va_35 == "1") %>%
  group_by(eye_id) %>%
  arrange(follow_up_days) %>%
  slice(which.min(follow_up_days)) %>% 
  ungroup() %>%
  mutate(status = 1)

va_35_0 <- df %>%
  anti_join(va_35_1, by = c("anon_id", "eye")) %>%
  group_by(eye_id) %>%
  arrange(follow_up_days) %>%
  slice(which.max(follow_up_days)) %>% 
  ungroup() %>%
  mutate(status = 0) 

va_35 <- rbind(va_35_0, va_35_1) %>%
  mutate(time = follow_up_days + 1) %>%
  select(eye_id, time, status)

# adding in event
va_35_event <- tmerge(df_independent, va_35, id=eye_id,
                      death=event(time, status)) %>% arrange(eye_id)

# adding in covariate
va_35_df <- tmerge(va_35_event, df_inj, id = eye_id, inj_num = tdc(follow_up_days, inj_num))

# Cox proportional hazard modelling
va_35_fit <- coxph(Surv(tstart, tstop, death) ~ gender + ethnicity + baseline_age + baseline_va  + inj_num,
                   data= va_35_df)
summary(va_35_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status) 

surv_va_35 <- survfit(Surv((time/30), status)~1, data = surv_va_35_covariate)

fig_surv_ttova_35 <- ggsurvplot(surv_va_35,
                                legend.title = "",
                                legend.labs = "", # Change legend labels
                                font.y = 14, # FONT size of risk table contents
                                font.x = 14,
                                font.tickslab = 14,
                                pval = F, conf.int = TRUE,
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                linetype = "strata", # Change line type by groups,
                                break.time.by = 5,
                                xlab = "Time (months)",
                                ylab = "Probability of VA \U2264 69",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18"))


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASEVA
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status, baseline_va_group) 

surv_va_35 <- survfit(Surv((time/30), status)~baseline_va_group, data = surv_va_35_covariate)

fig_surv_ttova_35_baseva <- ggsurvplot(surv_va_35,
                                       legend.title = "VA at baseline",
                                       legend.labs = c("\U2264 35","\U2265 35","36-49","50-59", "60-69"), # Change legend labels
                                       font.y = 14, # FONT size of risk table contents
                                       font.x = 14,
                                       font.tickslab = 14,
                                       pval = F, conf.int = F,
                                       fontsize = 5,
                                       risk.table.col = "strata", # Change risk table color by groups
                                       linetype = "strata", # Change line type by groups,
                                       break.time.by = 5,
                                       xlab = "Time (months)",
                                       ylab = "Probability of VA \U2264 69",
                                       surv.median.line = "hv", # Specify median survival
                                       ggtheme = theme_bw(), # Change ggplot2 theme
                                       palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE AGE
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status, baseline_age) 

surv_va_35 <- survfit(Surv((time/30), status)~baseline_age, data = surv_va_35_covariate)

fig_surv_ttova_35_baseage <- ggsurvplot(surv_va_35,
                                        legend.title = "Age at baseline",
                                        font.y = 14, # FONT size of risk table contents
                                        font.x = 14,
                                        font.tickslab = 14,
                                        pval = F, conf.int = F,
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        linetype = "strata", # Change line type by groups,
                                        break.time.by = 5,
                                        xlab = "Time (months)",
                                        ylab = "Probability of VA \U2264 69",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw())


##################################
# falling below 70 after being over 70
##################################
# NEW DF
df_va70 <- df %>%
  filter(follow_up_days >= "1") %>%
  filter(va >= 70) %>%
  group_by(eye_id) %>%
  slice(which.min(follow_up_days)) %>%
  ungroup() %>%
  mutate(dat_va_70 = follow_up_days) 

df_independent2 <- df_independent %>% 
  semi_join(df_va70, by = c("eye_id"))

df_inj2 <- df_inj %>% 
  semi_join(df_va70, by = c("eye_id")) %>% 
  arrange(eye_id)

df_va70_1 <- df %>%
  semi_join(df_va70, by = c("eye_id")) %>%
  left_join((df_va70 %>% select(eye_id, dat_va_70)), by = c("eye_id")) %>%
  mutate(
    tto70 = follow_up_days - dat_va_70) %>%
  filter(tto70 >=0) %>%
  filter(va <= 69) %>%
  group_by(eye_id) %>%
  slice(which.min(tto70)) %>%
  ungroup() %>% 
  mutate(
    status = 1
  )

df_va70_0 <- df %>%
  semi_join(df_va70, by = c("eye_id")) %>%
  left_join((df_va70 %>% select(eye_id, dat_va_70)), by = c("eye_id")) %>%
  mutate(
    tto70 = follow_up_days - dat_va_70) %>%
  anti_join(df_va70_1, by = c("eye_id")) %>%
  group_by(eye_id) %>%
  slice(which.max(follow_up_days)) %>%
  ungroup() %>% 
  mutate(
    status = 0
  ) 

va_70 <- rbind(df_va70_1, df_va70_0) %>%
  mutate(time = tto70 +1) %>%
  select(eye_id, time, status)

# adding in event
va_70_event <- tmerge(df_independent2, va_70, id=eye_id,
                      death=event(time, status)) %>% arrange(eye_id)

# adding in covariate
va_70_df <- tmerge(va_70_event, df_inj2, id=eye_id, inj_num = tdc(follow_up_days, inj_num))

# Cox proportional hazard modelling
va_70_fit <- coxph(Surv(tstart, tstop, death) ~ gender + ethnicity + baseline_age + baseline_va  + inj_num,
                   data= va_70_df)
summary(va_70_fit)


# Plotting survival curves i.e. actual events by a covariate
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status) 

surv_va_70 <- survfit(Surv((time/30), status)~1, data = surv_va_70_covariate)

fig_surv_ttova_70 <- ggsurvplot(surv_va_70,
                                legend.title = "",
                                legend.labs = "", # Change legend labels
                                font.y = 14, # FONT size of risk table contents
                                font.x = 14,
                                font.tickslab = 14,
                                pval = F, conf.int = TRUE,
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                linetype = "strata", # Change line type by groups,
                                break.time.by = 5,
                                xlab = "Time (months)",
                                ylab = "Probability of VA \U2265 70",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18"))

# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASEVA
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status, baseline_va_group) 

surv_va_70 <- survfit(Surv((time/30), status)~baseline_va_group, data = surv_va_70_covariate)

fig_surv_ttova_70_70_baseva <- ggsurvplot(surv_va_70,
                                          legend.title = "VA at baseline",
                                          legend.labs = c("\U2264 35","\U2265 70","36-49","50-59", "60-69"), # Change legend labels
                                          font.y = 14, # FONT size of risk table contents
                                          font.x = 14,
                                          font.tickslab = 14,
                                          pval = F, conf.int = F,
                                          fontsize = 5,
                                          risk.table.col = "strata", # Change risk table color by groups
                                          linetype = "strata", # Change line type by groups,
                                          break.time.by = 5,
                                          xlab = "Time (months)",
                                          ylab = "Probability of VA \U2265 70",
                                          surv.median.line = "hv", # Specify median survival
                                          ggtheme = theme_bw(), # Change ggplot2 theme
                                          palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))

# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE AGE
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (follow_up_days)) %>%
  select(eye_id, time, status, baseline_age) 

surv_va_70 <- survfit(Surv((time/30), status)~baseline_age, data = surv_va_70_covariate)

fig_surv_ttova_70_baseage <- ggsurvplot(surv_va_70,
                                        legend.title = "Age at baseline",
                                        font.y = 14, # FONT size of risk table contents
                                        font.x = 14,
                                        font.tickslab = 14,
                                        pval = F, conf.int = F,
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        linetype = "strata", # Change line type by groups,
                                        break.time.by = 5,
                                        xlab = "Time (months)",
                                        ylab = "Probability of VA \U2265 70",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw())

