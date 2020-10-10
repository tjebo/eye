##################################
# load packages
library(tidyverse)


# load anonymised dataset
df <- read.csv("Moorfields_AMD_Database_10_years.csv")
##################################
# Collecting annual foveal thickness
df_crt <- df %>%
  select(anon_id, eye, inj_num, inj_given, ttoinj_d, crt, crt_inj1, crt_lastvisit)  %>%
  filter(!is.na(crt))

crt_base <- df_crt %>%
  mutate(timepoint = "0") %>%
  filter(inj_given == "y" & inj_num == "1") %>%
  group_by(anon_id) %>%
  arrange(ttoinj_d) %>%
  slice(which.min(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_6mo <- df_crt  %>%
  mutate(timepoint = "6") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(180-ttoinj_d))))) %>%
  filter(abs(180-ttoinj_d) <= 50) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_12mo <- df_crt  %>%
  mutate(timepoint = "12") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(365-ttoinj_d))))) %>%
  filter(abs(365-ttoinj_d) <= 90) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_24mo <- df_crt %>%
  mutate(timepoint = "24") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(730-ttoinj_d))))) %>%
  filter(abs(730-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_36mo <- df_crt %>%
  mutate(timepoint = "36") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(1095-ttoinj_d))))) %>%
  filter(abs(1095-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_48mo <- df_crt %>%
  mutate(timepoint = "48") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(1460-ttoinj_d))))) %>%
  filter(abs(1460-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_60mo <- df_crt %>%
  mutate(timepoint = "60") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(1825-ttoinj_d))))) %>%
  filter(abs(1825-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_72mo <- df_crt %>%
  mutate(timepoint = "72") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(2190-ttoinj_d))))) %>%
  filter(abs(2190-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_84mo <- df_crt %>%
  mutate(timepoint = "84") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(2555-ttoinj_d))))) %>%
  filter(abs(2555-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_96mo <- df_crt %>%
  mutate(timepoint = "96") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(2920-ttoinj_d))))) %>%
  filter(abs(2920-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_108mo <- df_crt %>%
  mutate(timepoint = "108") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(3285-ttoinj_d))))) %>%
  filter(abs(3285-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(crt_change = crt - crt_inj1)

crt_120mo <- df_crt  %>%
  mutate(timepoint = "120") %>% 
  semi_join(df %>% filter(ltfu == "0")) %>% 
  mutate(crt = crt_lastvisit) %>%
  distinct(anon_id,eye, crt, .keep_all = T) %>%
  mutate(crt_change = crt - crt_inj1)

df_crt_comb <- rbind(crt_base, crt_6mo, crt_12mo, crt_24mo, crt_36mo, crt_48mo, crt_60mo,
                     crt_72mo, crt_84mo,crt_96mo,crt_108mo,crt_120mo) %>%
  select(anon_id, eye, timepoint, crt, crt_change)


##################################
# Collecting annual VA
df_va <- df %>%
  select(anon_id, eye, inj_num, inj_given, ttoinj_d, va, va_inj1, va_lastvisit)  %>%
  filter(!is.na(va))

va_base <- df_va %>%
  mutate(timepoint = "0") %>%
  filter(inj_given == "y" & inj_num == "1") %>%
  group_by(anon_id) %>%
  arrange(ttoinj_d) %>%
  slice(which.min(ttoinj_d)) %>% 
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_6mo <- df_va  %>%
  mutate(timepoint = "6") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(180-ttoinj_d))))) %>%
  filter(abs(180-ttoinj_d) <= 50) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_12mo <- df_va  %>%
  mutate(timepoint = "12") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(365-ttoinj_d))))) %>%
  filter(abs(365-ttoinj_d) <= 90) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_24mo <- df_va %>%
  mutate(timepoint = "24") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(730-ttoinj_d))))) %>%
  filter(abs(730-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_36mo <- df_va %>%
  mutate(timepoint = "36") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(1095-ttoinj_d))))) %>%
  filter(abs(1095-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_48mo <- df_va %>%
  mutate(timepoint = "48") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(1460-ttoinj_d))))) %>%
  filter(abs(1460-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_60mo <- df_va %>%
  mutate(timepoint = "60") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(1825-ttoinj_d))))) %>%
  filter(abs(1825-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_72mo <- df_va %>%
  mutate(timepoint = "72") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(2190-ttoinj_d))))) %>%
  filter(abs(2190-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_84mo <- df_va %>%
  mutate(timepoint = "84") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(2555-ttoinj_d))))) %>%
  filter(abs(2555-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_96mo <- df_va %>%
  mutate(timepoint = "96") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(2920-ttoinj_d))))) %>%
  filter(abs(2920-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_108mo <- df_va %>%
  mutate(timepoint = "108") %>%
  group_by(anon_id, eye) %>%
  slice(which.min(((abs(3285-ttoinj_d))))) %>%
  filter(abs(3285-ttoinj_d) <= 120) %>%
  ungroup() %>%
  mutate(va_change = va - va_inj1)

va_120mo <- df_va  %>%
  mutate(timepoint = "120") %>% 
  semi_join(df %>% filter(ltfu == "0")) %>% 
  mutate(va = va_lastvisit) %>%
  distinct(anon_id,eye, va, .keep_all = T) %>%
  mutate(va_change = va - va_inj1)

df_va_comb <- rbind(va_base, va_6mo, va_12mo, va_24mo, va_36mo, va_48mo, va_60mo,
                    va_72mo, va_84mo,va_96mo,va_108mo,va_120mo) %>%
  select(anon_id, eye, timepoint, va, va_change)

###########
df_comb <- df_va_comb %>%
  left_join(df_crt_comb, by = c("anon_id", "eye", "timepoint") )


########################
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = signif(mean   (xx[[col]], na.rm=na.rm),3),
                     sd   = signif(sd     (xx[[col]], na.rm=na.rm),3)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- signif(datac$sd / sqrt(datac$N),3)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- signif(datac$se * ciMult,3)
  return(datac)
}

########################
sum_crt_total <- summarySE(df_comb, measurevar="crt", groupvars= "timepoint", na.rm = T) 
sum_crtchange_total <- summarySE(df_comb, measurevar="crt_change", groupvars= "timepoint", na.rm = T) 
sum_va_total <- summarySE(df_comb, measurevar="va", groupvars= "timepoint", na.rm = T) 
sum_vachange_total <- summarySE(df_comb, measurevar="va_change", groupvars= "timepoint", na.rm = T) 


write.csv(sum_crt_total, file = "crttotal.csv")
write.csv(sum_crtchange_total, file = "crtchangetotal.csv")

