# data prep
library(tidyverse)
library(readxl)
sheets <- excel_sheets("ARL stats by peer groups 2017.xlsx")
arl.lst <- map(sheets, ~ read_xlsx(path = "ARL stats by peer groups 2017.xlsx", sheet = .x ))
names(arl.lst) <- str_replace_all(str_to_lower(str_squish(sheets)), " ", "_")
arl <- bind_rows(arl.lst, .id = "subset")
# drop 2016 Index; only in ARL_peers and not used
arl$`2016 Index` <- NULL
saveRDS(arl, file = "arl.Rds")
