##################################
# install packages
install.packages("tidyverse")
install.packages("survival")
install.packages("survminer")
install.packages("lubridate")
install.packages("tableone")

# load packages
library(tidyverse)
library(survival)
library(survminer)
library(lubridate)
library(tableone)

##################################
# load anonymised dataset
df <- read.csv("MEH_AMD_survivaloutcomes_database.csv") 

##################################
# time-independent covariates
df_indie <- df %>% 
  distinct(anon_id,.keep_all = T) %>%
  mutate(mean_inj_interval = as.integer((mean_inj_interval))) %>%
  select(anon_id, gender, ethnicity, va_inj1, age_group, date_inj1, loaded, mean_inj_interval, regimen, va_inj1_group)

# df for time-dependent covariates i.e. injection number 
df_inj <- df %>% 
  filter(injgiven == "1") %>%
  distinct(anon_id, time, injnum) %>%
  arrange(anon_id)

# df for visual acuity outcome
df_va <- df %>%
  distinct(anon_id, time, va)


#####################################
# first to VA reaching 70 ETDRS letters or above
#####################################
# remove those that start at 70
baseva_70 <- df_indie %>% filter(va_inj1 >= 70)

df_indie_70 <- df_indie  %>% 
  anti_join(baseva_70, by = c("anon_id")) %>%
  mutate(va_inj1 = (va_inj1/5) ) # PER 5 ETDRS letters

df_inj_70 <- df_inj %>% 
  anti_join(baseva_70, by = c("anon_id")) 

# event
va_70_1 <- df %>%
  anti_join(baseva_70, by = c("anon_id")) %>%
  filter(va >= 70) %>%
  group_by(anon_id) %>%
  arrange(time) %>%
  slice(which.min(time)) %>% 
  ungroup() %>%
  mutate(status = 1)

va_70_0 <- df %>%
  anti_join(baseva_70, by = c("anon_id")) %>%
  anti_join(va_70_1, by = c("anon_id")) %>%
  group_by(anon_id) %>%
  arrange(time) %>%
  slice(which.max(time)) %>% 
  ungroup() %>%
  mutate(status = 0) 

va_70 <- rbind(va_70_0,va_70_1) %>%
  mutate(time = (time+1)) %>%
  select(anon_id, time, status) # time-to-event

# adding in event
va_70_event <- tmerge(df_indie_70, va_70, id=anon_id,
                      death=event(time, status)) %>% arrange(anon_id)

# adding in covariate
va_70_df <- tmerge(va_70_event, df_inj_70, id=anon_id, injnum = tdc(time, injnum))

# Cox proportional hazard modelling
va_70_fit <- coxph(Surv(tstart, tstop, death) ~ gender + ethnicity + va_inj1 + age_group + date_inj1 + loaded + mean_inj_interval + regimen ,
                   data=va_70_df)

summary(va_70_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status) 

surv_va_70 <- survfit(Surv((time/365), status)~1, data = surv_va_70_covariate) # how to change 


fig_surv_ttova_70 <- ggsurvplot(surv_va_70,
                                title = "(a) Probability of VA equal to or greater than ETDRS letter score 70 (20/40) - All",
                                font.title = c(16, "bold", "black"),
                                font.y = 16, # FONT size of risk table contents
                                font.x = 16,
                                font.tickslab = 16,
                                legend.title="", # Legend title, can be used to remove Strata
                                pval = F, conf.int = F,
                                risk.table = TRUE, # Add risk table
                                
                                risk.table.y.text = FALSE,
                                cumevents.y.text = FALSE,
                                
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                break.time.by = 0.5,
                                xlab = "Time (years)",
                                ylab = "Probability of VA \U2265 70 (20/40)",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),
                                fun = "event",# INVERT table 
                                censor = F, # Remove ticks
                                xlim = c(0, 8.0),
                                ylim = c(0, 1.0))  


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY Regimen
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, regimen) 

surv_va_70 <- survfit(Surv((time/365), status)~regimen, data = surv_va_70_covariate)

surv_va_70_logrank_regimen <- survdiff(Surv((time/365), status)~regimen, data = surv_va_70_covariate)

fig_surv_ttova_70_regimen <- ggsurvplot(surv_va_70,
                                        title = "(d) Sub-stratified by anti-VEGF agent",
                                        font.title = c(16, "bold", "black"),
                                        legend.title = "Regimen",
                                        legend.labs = c("AFB", "RBZ" ), # Change legend labels
                                        font.y = 16, # FONT size of risk table contents
                                        font.x = 16,
                                        font.tickslab = 16,
                                        pval = F, conf.int = F,
                                        risk.table = TRUE, # Add risk table
                                        
                                        risk.table.y.text = FALSE,
                                        cumevents.y.text = FALSE,
                                        
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        break.time.by = 0.5,
                                        xlab = "Time (years)",
                                        ylab = "Probability of VA \U2265 70 (20/40)",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw(), # Change ggplot2 theme
                                        palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                 
                                        fun = "event",# INVERT table                                
                                        censor = F, # Remove ticks                                 
                                        xlim = c(0, 8.0),
                                        ylim = c(0, 1.0)) 

# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE VA
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, va_inj1_group) 

surv_va_70 <- survfit(Surv((time/365), status)~va_inj1_group, data = surv_va_70_covariate)
surv_va_70_logrank_baselineva <- survdiff(Surv((time/365), status)~va_inj1_group, data = surv_va_70_covariate)

fig_surv_ttova_70_baseva <- ggsurvplot(surv_va_70,
                                       title = "(b) Sub-stratified by baseline VA",
                                       font.title = c(16, "bold", "black"),
                                       legend.title = "VA at baseline",
                                       legend.labs = c("\U2264 35 (20/200)","36-49 (20/200-20/100)","50-69 (20/100-20/40)"), # Change legend labels
                                       font.y = 16, # FONT size of risk table contents
                                       font.x = 16,
                                       font.tickslab = 16,
                                       pval = F, conf.int = F,
                                       risk.table = TRUE, # Add risk table
                                       fontsize = 5,
                                       risk.table.col = "strata", # Change risk table color by groups
                                       
                                       risk.table.y.text = FALSE,
                                       cumevents.y.text = FALSE,
                                       
                                       break.time.by = 0.5,
                                       xlab = "Time (years)",
                                       ylab = "Probability of VA \U2265 70 (20/40)",
                                       surv.median.line = "hv", # Specify median survival
                                       ggtheme = theme_bw(), # Change ggplot2 theme
                                       palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                 
                                       fun = "event",# INVERT table                                  
                                       censor = F, # Remove ticks                                 
                                       xlim = c(0, 8.0),
                                       ylim = c(0, 1.0)) 


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE AGE
surv_va_70_covariate <- rbind(va_70_0, va_70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, age_group) 

# Setting levels
surv_va_70_covariate$age_group <- factor(surv_va_70_covariate$age_group,#####
                                            levels= c("50-59","60-69","70-79",">80"))

surv_va_70 <- survfit(Surv((time/365), status)~age_group, data = surv_va_70_covariate)
surv_va_70_logrank_baselineage <- survdiff(Surv((time/365), status)~age_group, data = surv_va_70_covariate)

fig_surv_ttova_70_baseage <- ggsurvplot(surv_va_70,
                                        title = "(c) Sub-stratified by baseline age",
                                        font.title = c(16, "bold", "black"),
                                        legend.title = "Age at baseline",
                                        legend.labs = c("50-59","60-69","70-79","\U2265 80"), # Change legend labels
                                        font.y = 16, # FONT size of risk table contents
                                        font.x = 16,
                                        font.tickslab = 16,
                                        pval = F, conf.int = F,
                                        risk.table = TRUE, # Add risk table
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        
                                        risk.table.y.text = FALSE,
                                        cumevents.y.text = FALSE,
                                        
                                        break.time.by = 0.5,
                                        xlab = "Time (years)",
                                        ylab = "Probability of VA \U2265 70 (20/40)",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw(), # Change ggplot2 theme
                                        palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                 
                                        fun = "event",# INVERT table                                  
                                        censor = F, # Remove ticks                                 
                                        xlim = c(0, 8.0),
                                        ylim = c(0, 1.0)) 


#####################################
# first to VA deteriorating to 35 EDTRS letters or below
#####################################
# remove those that are already at 35 or below
baseva_35 <- df_indie %>% filter(va_inj1 <= 35)

df_indie_35 <- df_indie  %>% 
  anti_join(baseva_35, by = c("anon_id")) %>%
  mutate(va_inj1 = (va_inj1/5) ) # PER 5 letters

df_inj_35 <- df_inj %>% 
  anti_join(baseva_35, by = c("anon_id")) 

# event
va_35_1 <- df %>%
  anti_join(baseva_35, by = c("anon_id")) %>%
  filter(va <= 35) %>%
  group_by(anon_id) %>%
  arrange(time) %>%
  slice(which.min(time)) %>% 
  ungroup() %>%
  mutate(status = 1)

va_35_0 <- df %>%
  anti_join(baseva_35, by = c("anon_id")) %>%
  anti_join(va_35_1, by = c("anon_id")) %>%
  group_by(anon_id) %>%
  arrange(time) %>%
  slice(which.max(time)) %>% 
  ungroup() %>%
  mutate(status = 0) 

va_35 <- rbind(va_35_0,va_35_1) %>%
  mutate(time = (time+1)) %>%
  select(anon_id, time, status) # time-to-event

# adding in event
va_35_event <- tmerge(df_indie_35, va_35, id=anon_id,
                      death=event(time, status)) %>% arrange(anon_id)

# adding in covariate
va_35_df <- tmerge(va_35_event, df_inj_35, id=anon_id, injnum = tdc(time, injnum))

# Cox proportional hazard modelling
va_35_fit <- coxph(Surv(tstart, tstop, death) ~ gender + ethnicity + va_inj1 + age_group + date_inj1 + loaded + mean_inj_interval + regimen ,
                   data=va_35_df)

summary(va_35_fit)

# Plotting survival curves i.e. actual events by a covariate
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status) 

surv_va_35 <- survfit(Surv((time/365), status)~1, data = surv_va_35_covariate)

fig_surv_ttova_35 <- ggsurvplot(surv_va_35,
                                title = "(a) Probability of VA equal to or less than ETDRS letter score 35 (20/200) - All",
                                font.title = c(16, "bold", "black"),
                                font.y = 16, # FONT size of risk table contents
                                font.x = 16,
                                font.tickslab = 16,
                                pval = F, conf.int = F,
                                risk.table = TRUE, # Add risk table
                                fontsize = 5,
                                risk.table.col = "strata", # Change risk table color by groups
                                
                                risk.table.y.text = FALSE,
                                cumevents.y.text = FALSE,
                                
                                break.time.by = 0.5,
                                xlab = "Time (years)",
                                ylab = "Probability of VA \U2264 35 (20/200)",
                                surv.median.line = "hv", # Specify median survival
                                ggtheme = theme_bw(), # Change ggplot2 theme
                                palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                 
                                fun = "event",# INVERT table                                  
                                censor = F, # Remove ticks                                 
                                xlim = c(0, 8.0),
                                ylim = c(0, 1.0)) 



# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY Regimen
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, regimen) 

surv_va_35 <- survfit(Surv((time/365), status)~regimen, data = surv_va_35_covariate)
surv_va_35_logrank_regimen <- survdiff(Surv((time/365), status)~regimen, data = surv_va_35_covariate)

fig_surv_ttova_35_regimen <- ggsurvplot(surv_va_35,
                                        title = "(d) Sub-stratified by anti-VEGF agent",
                                        font.title = c(16, "bold", "black"),
                                        legend.title = "Regimen",
                                        legend.labs = c("AFB", "RBZ" ), # Change legend labels
                                        font.y = 16, # FONT size of risk table contents
                                        font.x = 16,
                                        font.tickslab = 16,
                                        pval = F, conf.int = F,
                                        risk.table = TRUE, # Add risk table
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        
                                        risk.table.y.text = FALSE,
                                        cumevents.y.text = FALSE,
                                        
                                        break.time.by = 0.5,
                                        xlab = "Time (years)",
                                        ylab = "Probability of VA \U2264 35 (20/200)",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw(), # Change ggplot2 theme
                                        palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                
                                        fun = "event",# INVERT table                                  
                                        censor = F, # Remove ticks                                 
                                        xlim = c(0, 8.0),
                                        ylim = c(0, 1.0)) 


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE VA
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, va_inj1_group) 

surv_va_35_covariate$va_inj1_group <- factor(surv_va_35_covariate$va_inj1_group, 
                                            levels = c("36-49","50-69",">=70")) ######

surv_va_35 <- survfit(Surv((time/365), status)~va_inj1_group, data = surv_va_35_covariate)
surv_va_35_logrank_baselineva<- survdiff(Surv((time/365), status)~va_inj1_group, data = surv_va_35_covariate)

fig_surv_ttova_35_baseva <- ggsurvplot(surv_va_35,
                                       title = "(b) Sub-stratified by baseline VA",
                                       font.title = c(16, "bold", "black"),
                                       legend.title = "VA at baseline",
                                       legend.labs = c("36-49 (20/200-20/100)","50-69 (20/100-20/40)","≥ 70 (20/40)"), # Change legend labels
                                       font.y = 16, # FONT size of risk table contents
                                       font.x = 16,
                                       font.tickslab = 16,
                                       pval = F, conf.int = F,
                                       risk.table = TRUE, # Add risk table
                                       fontsize = 5,
                                       risk.table.col = "strata", # Change risk table color by groups
                                       risk.table.y.text = FALSE,
                                       cumevents.y.text = FALSE,
                                       
                                       
                                       break.time.by = 0.5,
                                       xlab = "Time (years)",
                                       ylab = "Probability of VA \U2264 35 (20/200)",
                                       surv.median.line = "hv", # Specify median survival
                                       ggtheme = theme_bw(), # Change ggplot2 theme
                                       palette = c("#990000","#588300","#FAAB18","#333333"),                                 
                                       fun = "event",# INVERT table                                  
                                       censor = F, # Remove ticks                                 
                                       xlim = c(0, 8.0),
                                       ylim = c(0, 1.0)) 



# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE AGE
surv_va_35_covariate <- rbind(va_35_0, va_35_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, age_group) 

surv_va_35_covariate$age_group <- factor(surv_va_35_covariate$age_group,
                                            levels= c("50-59","60-69","70-79",">80"))

surv_va_35 <- survfit(Surv((time/365), status)~age_group, data = surv_va_35_covariate)
surv_va_35_logrank_baselineage <- survdiff(Surv((time/365), status)~age_group, data = surv_va_35_covariate)

fig_surv_ttova_35_baseage <- ggsurvplot(surv_va_35,
                                        title = "(c) Sub-stratified by baseline age",
                                        font.title = c(16, "bold", "black"),
                                        legend.title = "Age at baseline",
                                        legend.labs = c("50-59","60-69","70-79","\U2265 80"), # Change legend labels
                                        font.y = 16, # FONT size of risk table contents
                                        font.x = 16,
                                        font.tickslab = 16,
                                        pval = F, conf.int = F,
                                        risk.table = TRUE, # Add risk table
                                        fontsize = 5,
                                        risk.table.col = "strata", # Change risk table color by groups
                                        
                                        # cumevents.title = "Number of events",
                                        # cumevents = TRUE, #Events table
                                        # cumevents.col = "strata",
                                        risk.table.y.text = FALSE,
                                        cumevents.y.text = FALSE,
                                        
                                        
                                        break.time.by = 0.5,
                                        xlab = "Time (years)",
                                        ylab = "Probability of VA \U2264 35 (20/200)",
                                        surv.median.line = "hv", # Specify median survival
                                        ggtheme = theme_bw(), # Change ggplot2 theme
                                        palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                
                                        fun = "event",# INVERT table                                  
                                        censor = F, # Remove ticks                                 
                                        xlim = c(0, 8.0),
                                        ylim = c(0, 1.0)) 


##################################
# Duration of sustaining VA greater than or equal to 70
##################################
# Selecting those who reach 70 
df_va70 <- df %>%
  filter(va >= 70) %>%
  group_by(anon_id) %>%
  slice(which.min(time)) %>%
  ungroup() %>%
  mutate(dat_va_70 = time) 

df_indie <- df_indie %>% 
  semi_join(df_va70, by = c("anon_id")) %>% 
  distinct(anon_id,.keep_all = T) %>%
  mutate(va_inj1 = (va_inj1/5) ) # PER 5 letters

df_inj <- df_inj %>% 
  semi_join(df_va70, by = c("anon_id")) 

df_va70_1 <- df %>%
  semi_join(df_va70, by = c("anon_id")) %>%
  left_join((df_va70 %>% select(anon_id, dat_va_70)), by = c("anon_id")) %>%
  mutate(tto70 = time - dat_va_70) %>%
  filter(tto70 >=0) %>%
  filter(va <= 69) %>%
  group_by(anon_id) %>%
  slice(which.min(tto70)) %>%
  ungroup() %>% 
  mutate(    status = 1  )

df_va70_0 <- df %>%
  semi_join(df_va70, by = c("anon_id")) %>%
  left_join((df_va70 %>% select(anon_id, dat_va_70)), by = c("anon_id")) %>%
  mutate(tto70 = time - dat_va_70) %>%
  anti_join(df_va70_1, by = c("anon_id")) %>%
  group_by(anon_id) %>%
  slice(which.max(time)) %>%
  ungroup() %>% 
  mutate(    status = 0  ) 

va_70 <- rbind(df_va70_1, df_va70_0) %>%
  mutate(time = tto70 +1) %>%
  select(anon_id, time, status)

# adding in event
va_70_event <- tmerge(df_indie, va_70, id=anon_id,
                      death=event(time, status)) %>% arrange(anon_id)

# adding in covariate
va_70_df <- tmerge(va_70_event, df_inj, id=anon_id, injnum = tdc(time, injnum))

# Cox proportional hazard modelling
va_70_fit <- coxph(Surv(tstart, tstop, death) ~ gender + ethnicity + va_inj1 + age_group + date_inj1 + loaded + mean_inj_interval + regimen ,
                   data=va_70_df)

# Plotting survival curves i.e. actual events by a covariate
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status) 

surv_va_70 <- survfit(Surv((time/365), status)~1, data = surv_va_70_covariate)

fig_surv_ttova_70to69 <- ggsurvplot(surv_va_70,
                                    title = "(a) Probability of failure to sustain ETDRS letter score equal to or greater than 70 (20/40) - All",
                                    font.title = c(16, "bold", "black"),
                                    font.y = 16, # FONT size of risk table contents
                                    font.x = 16,
                                    font.tickslab = 16,
                                    pval = F, conf.int = F,
                                    risk.table = TRUE, # Add risk table
                                    fontsize = 5,
                                    risk.table.col = "strata", # Change risk table color by groups
                                    
                                    risk.table.y.text = FALSE,
                                    cumevents.y.text = FALSE,
                                    
                                    break.time.by = 0.5,
                                    xlab = "Time (years)",
                                    ylab = "Probability of VA \U2264 69 (20/40)",
                                    surv.median.line = "hv", # Specify median survival
                                    ggtheme = theme_bw(), # Change ggplot2 theme
                                    palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                 
                                    fun = "event",# INVERT table                                  
                                    censor = F, # Remove ticks                                 
                                    xlim = c(0, 8.0)) 

# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY Regimen
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, regimen) 

surv_va_70 <- survfit(Surv((time/365), status)~regimen, data = surv_va_70_covariate)
surv_va_70_logrank_regimen <- survdiff(Surv((time/365), status)~regimen, data = surv_va_70_covariate)

fig_surv_ttova_70to69_regimen <- ggsurvplot(surv_va_70,
                                            title = "(d) Sub-stratified by anti-VEGF agent",
                                            font.title = c(16, "bold", "black"),
                                            
                                            legend.title = "Regimen",
                                            legend.labs = c("AFB", "RBZ" ), # Change legend labels
                                            font.y = 16, # FONT size of risk table contents
                                            font.x = 16,
                                            font.tickslab = 16,
                                            pval = F, conf.int = F,
                                            risk.table = TRUE, # Add risk table
                                            fontsize = 5,
                                            risk.table.col = "strata", # Change risk table color by groups
                                            
                                            risk.table.y.text = FALSE,
                                            cumevents.y.text = FALSE,
                                            
                                            break.time.by = 0.5,
                                            xlab = "Time (years)",
                                            ylab = "Probability of VA \U2264 69 (20/40)",
                                            surv.median.line = "hv", # Specify median survival
                                            ggtheme = theme_bw(), # Change ggplot2 theme
                                            palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                 
                                            fun = "event",# INVERT table                                  
                                            censor = F, # Remove ticks                                 
                                            xlim = c(0, 8.0)) 


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE VA
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, va_inj1_group) 

surv_va_70_covariate$va_inj1_group <- factor(surv_va_70_covariate$va_inj1_group,
                                            levels = c("<=35","36-49","50-69", ">=70"))
surv_va_70 <- survfit(Surv((time/365), status)~va_inj1_group, data = surv_va_70_covariate)
surv_va_70_logrank_baselineva <- survdiff(Surv((time/365), status)~va_inj1_group, data = surv_va_70_covariate)

fig_surv_ttova_70to69_baseva <- ggsurvplot(surv_va_70,
                                           title = "(b) Sub-stratified by baseline VA",
                                           font.title = c(16, "bold", "black"),
                                           
                                           legend.title = "VA at baseline",
                                           legend.labs = c("\U2264 35 (20/200)","36-49 (20/200-20/100)","50-69 (20/100-20/40)","\U2265 70 (20/40)"), # Change legend labels
                                           font.y = 16, # FONT size of risk table contents
                                           font.x = 16,
                                           font.tickslab = 16,
                                           pval = F, conf.int = F,
                                           risk.table = TRUE, # Add risk table
                                           fontsize = 5,
                                           risk.table.col = "strata", # Change risk table color by groups
                                           
                                           risk.table.y.text = FALSE,
                                           cumevents.y.text = FALSE,
                                           
                                           break.time.by = 0.5,
                                           xlab = "Time (years)",
                                           ylab = "Probability of VA \U2264 69 (20/40)",
                                           surv.median.line = "hv", # Specify median survival
                                           ggtheme = theme_bw(), # Change ggplot2 theme
                                           palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                                
                                           fun = "event",# INVERT table                                  
                                           censor = F, # Remove ticks                                
                                           xlim = c(0, 8.0)) 


# Plotting survival curves i.e. actual events by a covariate SUBSTRATIFIED BY BASE AGE
surv_va_70_covariate <- rbind(df_va70_0, df_va70_1) %>%
  mutate(time = (time)) %>%
  select(anon_id, time, status, age_group) 

surv_va_70_covariate$age_group <- factor(surv_va_70_covariate$age_group,
                                            levels = c("50-59","60-69","70-79",">80"))


surv_va_70 <- survfit(Surv((time/365), status)~age_group, data = surv_va_70_covariate)
surv_va_70_logrank_baselineage <- survdiff(Surv((time/365), status)~age_group, data = surv_va_70_covariate)

fig_surv_ttova_70to69_baseage <- ggsurvplot(surv_va_70,
                                            title = "(c) Sub-stratified by baseline age",
                                            font.title = c(16, "bold", "black"),
                                            
                                            legend.title = "Age at baseline",
                                            legend.labs = c("50-59","60-69","70-79",">80"), # Change legend labels
                                            font.y = 16, # FONT size of risk table contents
                                            font.x = 16,
                                            font.tickslab = 16,
                                            pval = F, conf.int = F,
                                            risk.table = TRUE, # Add risk table
                                            fontsize = 5,
                                            risk.table.col = "strata", # Change risk table color by groups
                                            
                                            risk.table.y.text = FALSE,
                                            cumevents.y.text = FALSE,
                                            
                                            break.time.by = 0.5,
                                            xlab = "Time (years)",
                                            ylab = "Probability of VA \U2264 69 (20/40)",
                                            surv.median.line = "hv", # Specify median survival
                                            ggtheme = theme_bw(), # Change ggplot2 theme
                                            palette = c("#1380A1","#990000","#588300","#FAAB18","#333333"),                  
                                            fun = "event",# INVERT table                               
                                            censor = F, # Remove ticks                               
                                            xlim = c(0, 8.0)) 


####################################################################################
# Tables
####################################################################################
# Distinct
distinct <- df %>%
  group_by(anon_id) %>%
  slice(which.max(injnum))  %>%
  ungroup()


####################################################################################
# Demographics
listVars <- c("gender", "age_group", "ethnicity", "va_inj1",
              "date_inj1", "injnum", "mean_inj_interval", "loaded", "time")           
catVars <- c("gender", "age_group", "ethnicity", 
             "date_inj1", "loaded")

table1_overall <- 
  CreateTableOne(
    vars = listVars, 
    data =  distinct,
    factorVars = catVars)

print(table1_overall, showAllLevels = TRUE, noSpaces = TRUE)


table1_regimen <- 
  CreateTableOne(
    vars = listVars, 
    data =  distinct,
    factorVars = catVars,
    strata = c("regimen","date_inj1"))

print(table1_regimen, showAllLevels = TRUE, noSpaces = TRUE)

