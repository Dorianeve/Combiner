rm(list = ls())
library(openxlsx)
library(janitor)
library(magrittr)
library(openxlsx2)
library(tidyverse)
library(lubridate)

df <- read.csv("data/arr/aoe/aoe_cleaned.csv", encoding = "UTF-8")
include <- read.csv("data/arr/aoe/utility_table/aoe_primary_tag.csv", encoding = "UTF-8")



include %<>%
  select(UID, Include) %>%
  rename(Include_manual = Include)

df %<>%
  left_join(include, by = "UID")

df %<>%
  mutate(Include = ifelse(!is.na(Include_manual), Include_manual, Include))

write.csv(aoe, "data/arr/aoe/aoe_cleaned.csv", row.names = FALSE)
