##################################
# load packages
library(tidyverse)
library(survival)
library(survminer)

##################################
# load anonymised dataset
df <- read.csv("Moorfields_AMD_Database_10_years.csv") %>%
  mutate(baseva_inj1 = case_when(
    va_inj1 >= 70 ~ ">=70",
    va_inj1 >=50 & va_inj1 <=69 ~ "50-69",
    va_inj1 >=36 & va_inj1 <=49 ~ "36-49",
    va_inj1 <= 35 ~ "<=35"))

##################################
# time-independent covariates
df_indie <- df %>% 
  distinct(anon_id,.keep_all = T) %>%
  select(anon_id, gender, ethnicity,  va_inj1, crt_inj1)

# df for time-dependent covariates i.e. INJECTION NUMBER 
df_inj <- df %>% 
  filter(inj_given == "y") %>%
  distinct(anon_id, ttoinj_d, inj_num) %>%
  arrange(anon_id)

#####################################
# first to VA <<35
#####################################
# event
va_35_1 <- df %>%
  filter(va <= 35) %>%
  group_by(anon_id,eye) %>%
  arrange(ttoinj_d) %>%
  slice(which.min(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(
    status = 1)

va_35_0 <- df %>%
  anti_join(va_35_1, by = c("anon_id", "eye")) %>%
  group_by(anon_id,eye) %>%
  arrange(ttoinj_d) %>%
  slice(which.max(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(status = 0) 

va_35 <- rbind(va_35_0, va_35_1) %>%
  mutate(time = ttoinj_d +1) %>%
  select(anon_id, time, status)

# adding in event
va_35_event <- tmerge(df_indie, va_35, id=anon_id,
                      death=event(time, status)) %>% arrange(anon_id)

# adding in covariate
va_35_df <- tmerge(va_35_event, df_inj, id=anon_id, inj_num = tdc(ttoinj_d, inj_num))

# Cox proportional hazard modelling
va_35_fit <- coxph(Surv(tstart, tstop, death) ~ gender  +  va_inj1 + crt_inj1 + inj_num,
                   data= va_35_df)
summary(va_35_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (ttoinj_d)) %>%
  select(anon_id, time, status) 

surv_va_35 <- survfit(Surv((time/30), status)~1, data = surv_va_35_covariate)

fig_surv_ttova_35 <- ggsurvplot(surv_va_35,
                                legend.title = "",
                                legend.labs = "", # Change legend labels
                                font.y = 16, # FONT size of risk table contents
                                font.x = 16,
                                font.tickslab = 16,
                                pval = F, conf.int = TRUE,
                                risk.table = TRUE, # Add risk table
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                linetype = "strata", # Change line type by groups,
                                break.time.by = 5,
                                xlab = "Time (months)",
                                ylab = "Probability of VA \U2265 36",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18"))

# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE VA
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (ttoinj_d)) %>%
  select(anon_id, time, status, baseva_inj1) 

surv_va_35 <- survfit(Surv((time/30), status)~baseva_inj1, data = surv_va_35_covariate)

fig_surv_ttova_35_baseva <- ggsurvplot(surv_va_35,
                                       legend.title = "VA at baseline",
                                       legend.labs = c("\U2264 35","\U2265 70","36-49","50-69"), # Change legend labels
                                       font.y = 16, # FONT size of risk table contents
                                       font.x = 16,
                                       font.tickslab = 16,
                                       pval = F, conf.int = F,
                                       risk.table = TRUE, # Add risk table
                                       fontsize = 5,
                                       risk.table.col = "strata", # Change risk table color by groups
                                       linetype = "strata", # Change line type by groups,
                                       break.time.by = 5,
                                       xlab = "Time (months)",
                                       ylab = "Probability of VA \U2265 36",
                                       surv.median.line = "hv", # Specify median survival
                                       ggtheme = theme_bw(), # Change ggplot2 theme
                                       palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))



#####################################
# first to VA >70
#####################################
# event
va_70_1 <- df %>%
  filter(va >= 70) %>%
  group_by(anon_id,eye) %>%
  arrange(ttoinj_d) %>%
  slice(which.min(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(
    status = 1)

va_70_0 <- df %>%
  anti_join(va_70_1, by = c("anon_id", "eye")) %>%
  group_by(anon_id,eye) %>%
  arrange(ttoinj_d) %>%
  slice(which.max(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(status = 0) 

va_70 <- rbind(va_70_0, va_70_1) %>%
  mutate(time = ttoinj_d +1) %>%
  select(anon_id, time, status)

# adding in event
va_70_event <- tmerge(df_indie, va_70, id=anon_id,
                      death=event(time, status)) %>% arrange(anon_id)

# adding in covariate
va_70_df <- tmerge(va_70_event, df_inj, id=anon_id, inj_num = tdc(ttoinj_d, inj_num))

# Cox proportional hazard modelling
va_70_fit <- coxph(Surv(tstart, tstop, death) ~ gender  +  va_inj1 + crt_inj1 + inj_num,
                   data= va_70_df)
summary(va_70_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (ttoinj_d)) %>%
  select(anon_id, time, status) 

surv_va_70 <- survfit(Surv((time/30), status)~1, data = surv_va_70_covariate)

fig_surv_ttova_70 <- ggsurvplot(surv_va_70,
                                font.y = 16, # FONT size of risk table contents
                                font.x = 16,
                                font.tickslab = 16,
                                pval = F, conf.int = F,
                                risk.table = TRUE, # Add risk table
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                linetype = "strata", # Change line type by groups,
                                break.time.by = 5,
                                xlab = "Time (months)",
                                ylab = "Probability of VA \U2264 69",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))

# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE VA
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (ttoinj_d)) %>%
  select(anon_id, time, status, baseva2_inj1) 

surv_va_70 <- survfit(Surv((time/30), status)~baseva_inj1, data = surv_va_70_covariate)

fig_surv_ttova_70_baseva <- ggsurvplot(surv_va_70,
                                       legend.title = "VA at baseline",
                                       legend.labs = c("\U2264 35","\U2265 70","36-49","50-60"), # Change legend labels
                                       font.y = 16, # FONT size of risk table contents
                                       font.x = 16,
                                       font.tickslab = 16,
                                       pval = F, conf.int = F,
                                       risk.table = TRUE, # Add risk table
                                       fontsize = 5,
                                       risk.table.col = "strata", # Change risk table color by groups
                                       linetype = "strata", # Change line type by groups,
                                       break.time.by = 5,
                                       xlab = "Time (months)",
                                       ylab = "Probability of VA \U2264 69",
                                       surv.median.line = "hv", # Specify median survival
                                       ggtheme = theme_bw(), # Change ggplot2 theme
                                       palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))




#####################################
# first to ga
#####################################
# event
ga_1 <- df %>%
  filter(ga == "1") %>%
  group_by(anon_id,eye) %>%
  mutate(ttoinj_d = ttoga_mo * 30) %>%
  arrange(ttoinj_d) %>%
  slice(which.min(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(
    status = 1)

ga_0 <- df %>%
  anti_join(ga_1, by = c("anon_id", "eye")) %>%
  group_by(anon_id,eye) %>%
  arrange(ttoinj_d) %>%
  slice(which.max(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(status = 0) 

ga <- rbind(ga_0, ga_1) %>%
  mutate(time = ttoinj_d +1) %>%
  select(anon_id, time, status)

# adding in event
ga_event <- tmerge(df_indie, ga, id=anon_id,
                      death=event(time, status)) %>% arrange(anon_id)

# adding in covariate
ga_df <- tmerge(ga_event, df_inj, id=anon_id, inj_num = tdc(ttoinj_d, inj_num))

# Cox proportional hazard modelling
ga_fit <- coxph(Surv(tstart, tstop, death) ~ gender  +  va_inj1 + crt_inj1 + inj_num,
                   data= ga_df)
summary(ga_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_ga_covariate <- rbind(ga_0, ga_1) %>%
  mutate(time = (ttoinj_d)) %>%
  select(anon_id, time, status) 

surv_ga <- survfit(Surv((time/30), status)~1, data = surv_ga_covariate)

fig_surv_ttoga <- ggsurvplot(surv_ga,
                                font.y = 16, # FONT size of risk table contents
                                font.x = 16,
                                font.tickslab = 16,
                                pval = F, conf.int = F,
                                risk.table = TRUE, # Add risk table
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                linetype = "strata", # Change line type by groups,
                                break.time.by = 5,
                                xlab = "Time (months)",
                                ylab = "Probability of GA",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"))

