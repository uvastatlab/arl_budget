library(Hmisc)
library(readxl)
library(tidyverse)
library(scales)

# read in prior ARL data, keep only US
arl <- readRDS("arl.Rds")
arl_us <- subset(arl, subset == "us_only")
saveRDS(arl_us, file = "arl_us.Rds")
rm(arl)
arl_us <- readRDS("arl_us.Rds")

# read in grant data
grant_data <- read_excel("2017ARLIndexTop30LibraryDatawithGrantData.xlsx", sheet = 1)
grant_data$..12 <- NULL

# drop duplicate columns
grant_data <- grant_data[,-c(3:7)]

# add labels
for(i in seq_along(names(grant_data))){
  label(grant_data[[i]]) <- names(grant_data)[i]
}
rm(i)

# abbreviate var names
names(grant_data) <- tolower(abbreviate(names(grant_data)))

# merge in grant data
arl_us <- left_join(arl_us, grant_data, by = c("inam" = "insn"))
rm(grant_data)

# read in electronic resources and investment data
elec_inv_data <- read_excel("2017ARLIndexAllUSLibraryDataPlusIpedsData.xlsx", sheet = 1)
elec_inv_data <- subset(elec_inv_data, Type != "C")

# keep desired columns
elec_inv_data <- elec_inv_data[,c("Institution Name", "Total Electronic Circulations", 
                                  "Investment Return per  FTE enrollment", 
                                  "Investment Return Total", 
                                  "Investment Return plus Govt Grants/Contracts")]

# add labels
for(i in seq_along(names(elec_inv_data))){
  label(elec_inv_data[[i]]) <- names(elec_inv_data)[i]
}
rm(i)

# abbreviate var names
names(elec_inv_data) <- tolower(abbreviate(names(elec_inv_data)))

# merge in electronic circ and investment data
arl_us <- left_join(arl_us, elec_inv_data, by = c("inam" = "insn"))
rm(elec_inv_data)


# read in herd data
herd_data <- read_excel("herd2017_dst_20.xlsx", sheet = 1, skip = 3)

# keep only name, rank, 2017 (and remove "All Institutions")
herd_data <- herd_data[-1,c(1,2,21)]
herd_data <- herd_data %>% 
  rename(resexp = `2017`) %>% 
  mutate(resexp = as.numeric(resexp),
         Rank = as.numeric(Rank)) 

herd_data <- herd_data %>% 
  mutate(name = str_remove(Institution, "U. "),
         name = str_remove(name, "U."))


saveRDS(arl_us, file = "arl_us.Rds")


arl_us <- readRDS("arl_us.Rds")

# generate student counts, grants/investments in millions
arl_us <- arl_us %>% 
  mutate(ugrad = totstu - gradstu,
         docstu = gradstu - msda,
         invgrant = irpgg/100000)

# predictor candidates
vars <- c("indn","totexp","totstu","gradstu","msda","phdawd","phdfld","fac","type","law","med",
          "ggacpe", "ttec", "inrt", "irpfe", "irpgg","ugrad","docstu","invgrant")

summary(arl_us[,vars])
arl_us$type <- factor(arl_us$type)
arl_us$law <- factor(arl_us$law)
arl_us$med <- factor(arl_us$med)
summary(arl_us[,vars])

arl_us$irpfe <- ifelse(arl_us$irpfe==0,NA,arl_us$irpfe)

# 2 vars have 1 missing, 1 var has 3 missing;
# perhaps we could find these out?
nas <- sapply(arl_us[,vars],function(x)sum(is.na(x)))
nas[nas>0]
i <- apply(arl_us[,vars],1,function(x)any(is.na(x)))
as.data.frame(arl_us[i,c("inam",vars)])

# median imputation
arl_us$phdawd[is.na(arl_us$phdawd)] <- median(arl_us$phdawd, na.rm = T)
arl_us$phdfld[is.na(arl_us$phdfld)] <- median(arl_us$phdfld, na.rm = T)
arl_us$reftrans[is.na(arl_us$reftrans)] <- median(arl_us$reftrans, na.rm = T)
arl_us$irpfe[is.na(arl_us$irpfe)] <- median(arl_us$irpfe, na.rm = T)

summary(arl_us[,vars])


# create two data frames, one for each DV
arl_us <- as.data.frame(arl_us)
rownames(arl_us) <- arl_us$inam
arl_totexp <- arl_us[,c(vars)]
arl_explm <- arl_us[,c(vars)]

# pull out UVa
uva <- which(rownames(arl_us)=="VIRGINIA")

uva_totexp <- arl_totexp[uva,]
uva_explm <- arl_explm[uva,]

arl_totexp <- arl_totexp[-uva,]
arl_explm <- arl_explm[-uva,]

rm(i, nas)


save.image(file = "arl.Rdata")

#####
load("arl.Rdata")

arl_totexp1 <- arl_totexp %>% filter(indn < 46)
arl_totexp2 <- arl_totexp %>% filter(indn > 5 & indn < 50)

project1 <- read_csv("project1.csv")

mod_totexp <- lm(totexp ~ fac + ugrad + gradstu + phdfld + invgrant, 
                 data = arl_totexp)
summary(mod_totexp)
pred_proj <- predict(mod_totexp, newdata = project1, interval = "confidence")
pred_proj


pred0 <- bind_cols(year = rep(2017:2022, 2), 
                   projected = c(pred_proj[1:6], project1$total),
                   lwr = c(pred_proj[7:12], rep(NA, 6)), 
                   upr = c(pred_proj[13:18], rep(NA, 6)),
                   source = rep(c("model", "cap"), each = 6))

ggplot(pred0, aes(y = projected, x = year, color = source)) + 
  geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2)


mod_totexp1 <- lm(totexp ~ fac + ugrad + gradstu + phdfld + irpgg, 
                  data = arl_totexp1)
summary(mod_totexp1)
pred_proj1 <- predict(mod_totexp1, newdata = project1, interval = "confidence")
pred_proj1

pred1 <- bind_cols(year = rep(2017:2022, 2), 
                   projected = c(pred_proj1[1:6], project1$total1),
                   lwr = c(pred_proj1[7:12], rep(NA, 6)), 
                   upr = c(pred_proj1[13:18], rep(NA, 6)),
                   source = rep(c("model", "cap"), each = 6))

ggplot(pred1, aes(y = projected, x = year, color = source)) + 
  geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2)


ks <- c("CALIFORNIA, BERKELEY", "CORNELL", "DUKE", "EMORY", 
        "IOWA", "JOHNS HOPKINS", "MICHIGAN", "NORTH CAROLINA", 
        "PENNSYLVANIA")
arl_ks <- arl_us %>% filter(inam %in% ks)
mod_totexp_ks <- lm(totexp ~ fac + ugrad + gradstu + phdfld, 
                  data = arl_ks)
summary(mod_totexp_ks)
pred_projks <- predict(mod_totexp_ks, newdata = project1, interval = "confidence")
pred_projks
