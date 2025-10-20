rm(list = ls())
library(openxlsx)
library(janitor)
library(magrittr)
library(openxlsx2)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(dplyr)
# libraries required for the ARR pipeline

if(!require(openxlsx)){
  install.packages('openxlsx')
  library(openxlsx)
}

if(!require(Hmisc)){
  install.packages('Hmisc')
  library(Hmisc)
}

if(!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(stringdist)){
  install.packages('stringdist')
  library(stringdist)
}

if(!require(foreach)){
  install.packages('foreach')
  library(foreach)
}

if(!require(janitor)) {
  install.packages("janitor")
  library(janitor)
}

if(!require(ggplot2))  {
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(scales))  {
  install.packages("scales")
  library(scales)
}

if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

if(!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if(!require(purrr)) {
  install.packages("purrr")
  library(purrr)
}

if(!require(yaml)) {
  install.packages("yaml")
  library(yaml)
}

if(!require(ggplot2)) {
  install.packages(("ggplot2"))
}

# function for filtering the AvailabilityOfEvidence dataset

collapse_rows <- function(df) {
  df <- df %>%
    mutate(EvidenceavailableNumeric = ifelse(is.na(EvidenceavailableNumeric), 0, EvidenceavailableNumeric))
  if (all(df$EvidenceavailableNumeric == 0, na.rm = TRUE) > 0) {
    return(df[1, ])
  } else if (sum(df$EvidenceavailableNumeric == 1, na.rm=TRUE) == 1) {
    return (df[df$EvidenceavailableNumeric == 1, ] [1, ])
  } else if (sum(df$EvidenceavailableNumeric == 1, na.rm=TRUE) > 1) {
    prim_rows <- df[df$EvidenceavailableNumeric==1 & df$include==1, ]
    #print(prim_rows)
    if (nrow(prim_rows) > 0) {
      return(prim_rows[1, ])}
    else {
      return(df[df$EvidenceavailableNumeric == 1, ] [1, ])
    }
  }
}

# function for availability of evidence

convert_to_decimal <- function(x) {
  if (grepl("%", x)) {
    # Remove the '%' sign and convert to numeric
    x_numeric <- as.numeric(gsub("%", "", x))
    # Convert to decimal
    return(x_numeric / 100)
  } else {
    # Convert already decimal strings to numeric
    return(as.numeric(x))
  }
}


df <- read.csv("data/arr/aoe/aoe_cleaned.csv", encoding = "UTF-8")


# start printing in log
date_string <- format(today(), "%Y-%m-%d")
#sink(paste0("log/AoE - quantitative analysis log ", strftime(Sys.Date(),"%y%m%d"), ".txt"))

df <- read.csv("output data/cleaning/Availability of Evidence - quantitative_for analysis.csv")
print(paste0(nrow(df), "rows loaded."))
wb <- createWorkbook()
df <- df %>% 
  mutate (include = case_when (
    Include..Supervisors. == "Primary analysis" ~ 1,
    Include..Supervisors. == "Secondary analysis" ~ 2,
    is.na(Include..Supervisors.) ~ 0 
  ))


# Indicator 3 ---- 
print("Indicator #3 analysis")

# ** a. total programs
ind_3 <- df %>%
  filter(Indicator == 3)


ind_3 <- ind_3 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_3), " rows for analysis."))

unique(ind_3$LeadGRN)

programs<- ind_3 %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs (unique LeadGRN)",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs

# ** b. total eligible programs
#CTD: We need to drop the regional grants from this indicator too - added below lines 60-73 then added to the table so people can see what was included/excluded from sample

ind_3 <- ind_3[!grepl("Regional", ind_3$Country), ]

programs_country <- ind_3 %>% group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of country programs (unique LeadGRN)",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_country

data <- rbind(programs, programs_country) 

# ** c. total programs with evidence available
programs_evidenceavailable <- ind_3 %>%
  filter(Evidenceavailable == "Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with evidence available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable

data <- rbind(data, programs_evidenceavailable)

# ** d. % programs evidence available

programs_Perevidenceavailable <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence available` = `# of programs with evidence available` / `# of country programs (unique LeadGRN)`) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence available`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence available`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence avaliable") %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_Perevidenceavailable

data <- rbind(data, programs_Perevidenceavailable)  

# ** e. # programs evidence of change
#CTD: Added new set of code 103-127 to calculate the number of programs with evidence of change

programs_evchange <- ind_3 %>%
  filter(EvidenceofchangeNumeric != 0) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with evidence of change",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evchange

data <- rbind(data, programs_evchange) 

programs_evchange_solid <- ind_3 %>%
  filter(EvidenceofchangeNumeric == 3 | EvidenceofchangeNumeric == 2) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with partial/solid evidence of change",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evchange_solid

data <- rbind(data, programs_evchange_solid) 

# ** f. % programs evidence of change

programs_Perevidencechange <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence of change` = `# of programs with evidence of change` / `# of programs with evidence available`) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence of change`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence of change`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence of change") %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_Perevidencechange

data <- rbind(data, programs_Perevidencechange)  

# programs_Perevidencechange_solid <- data %>%
#   pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
#   pivot_wider(names_from = id, values_from = value) %>%
#   mutate(`% of programs with partial/solid evidence of change` = `# of programs with partial/solid evidence of change` / `# of programs with evidence of change`) %>% #CTD changed per country programs 
#   select(name,  `% of programs with partial/solid evidence of change`) %>%
#   pivot_wider(names_from = name, values_from =  `% of programs with partial/solid evidence of change`) %>%
#   mutate(FER = if("FER" %in% colnames(.)) FER else 0,
#          MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
#          id = "% of programs with partial/solid evidence of change") %>%
#   select(id, TotalECWPrograms, FER, MYRP)

# muted above because of an issue
programs_Perevidencechange_solid <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP), names_to = "ProgramType", values_to = "ProgramValue") %>%
  pivot_wider(names_from = id, values_from = ProgramValue, values_fn = list) %>%
  unnest(cols = c(`# of programs with partial/solid evidence of change`, `# of programs with evidence of change`)) %>%
  mutate(across(starts_with("#"), ~replace_na(., 0))) %>%
  mutate(`% of programs with partial/solid evidence of change` = `# of programs with partial/solid evidence of change` / `# of programs with evidence of change`) %>%
  select(ProgramType, `% of programs with partial/solid evidence of change`) %>%
  pivot_wider(names_from = ProgramType, values_from = `% of programs with partial/solid evidence of change`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with partial/solid evidence of change") %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_Perevidencechange_solid


data <- rbind(data, programs_Perevidencechange_solid)  

# ** g. # programs partial or solid increase

programs_code_childSIPI <- ind_3 %>%
  filter(codeNumeric_child == 1 | codeNumeric_child == 2) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with partial or solid evidence of increase",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_code_childSIPI

data <- rbind(data, programs_code_childSIPI)
data

# ** h. # programs solid increase
programs_code_childSI <- ind_3 %>%
  filter(codeNumeric_child == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with solid evidence of increase",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_code_childSI

data <- rbind(data, programs_code_childSI)
data

# ** i. % programs partial solid increase
programs_PerPartialSolid <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with partial or solid evidence of increase` = `# of programs with partial or solid evidence of increase` / `# of programs with partial/solid evidence of change`) %>% #CTD: updated to # of programs with evidence of change
  select(name,  `% of programs with partial or solid evidence of increase`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with partial or solid evidence of increase`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with partial or solid evidence of increase") %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_PerPartialSolid

data <- rbind(data, programs_PerPartialSolid)  
data

# ** j. % programs solid increase

programs_PerSolid <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with solid evidence of increase` = `# of programs with solid evidence of increase` / `# of programs with partial or solid evidence of increase`) %>% #CTD: update to # of programs with evidence of change
  select(name,  `% of programs with solid evidence of increase`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with solid evidence of increase`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with solid evidence of increase") %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_PerSolid

data <- rbind(data, programs_PerSolid)  
data

# ** k. # programs gender information
programs_gequity1 <- ind_3 %>%
  filter(Gequity1 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys baseline",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity1

programs_gequity2 <- ind_3 %>%
  filter(Gequity2 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys endline",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity2

programs_gequity3 <- ind_3 %>%
  filter(Gequity3 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys baseline/endline = 1",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity3

data <- rbind(data, programs_gequity1, programs_gequity2, programs_gequity3)
data


# CTD: add columns so audience can follow calculation

# ** formatting
index <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
calculation <- c("", "", "", "c/b", "", "", "e/c", "f/c", "", "", "i/e", "j/i", "", "", "")

data <- data %>%
  mutate (index = index) %>%
  mutate (calculation = calculation) %>% 
  select (index, id, calculation, TotalECWPrograms, FER, MYRP) 
data

# ** output

addWorksheet(wb, "Indicator3")
writeData(wb, sheet = "Indicator3", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(counts, data, checkgrn, cr_nfe, crosstab, data, df_cr, 
   programs, programs_country, programs_evidenceavailable, programs_NFE,
   programs_evidenceavailable_num, programs_evchange, programs_evchange_solid, programs_evidenceavailable_rate, 
   programs_Perevidenceavailable, programs_Perevidenceavailable_rate, programs_Perevidencechange, programs_Perevidencechange_solid,
   programs_code_childSI, programs_code_childSIPI, programs_PerPartialSolid, programs_PerSolid,
   programs_evidenceavailable_ind, programs_indmean, programs_indmeantot, programs_indsum, programs_indsumtest,
   programs_Perevidenceavailable_ind, programs_Perevidencechange, programs_Perevidenceavailablerate, 
   programs_gequity1, programs_gequity2, programs_gequity3)

## 3 Gender equitable----

ge <- ind_3 %>%
  mutate(
    Overallbaseline = sapply(Overallbaseline, convert_to_decimal),
    Girlsbaseline = sapply(Girlsbaseline, convert_to_decimal),
    Boysbaseline = sapply(Boysbaseline, convert_to_decimal),
    Girlsendline = sapply(Girlsendline, convert_to_decimal),
    Boysendline = sapply(Boysendline, convert_to_decimal),
    fdiff = Girlsendline - Girlsbaseline,
    mdiff = Boysendline - Boysbaseline,
    bdiff = Girlsbaseline - Boysbaseline,
    ediff = Girlsendline - Boysendline,
    ebdiff = ediff - bdiff,
    ggap = case_when(
      ediff < bdiff & ebdiff < 0 ~ "redm",
      ediff < bdiff & ebdiff > 0 ~ "redf",
      ediff > bdiff & ebdiff > 0 ~ "incf",
      ediff > bdiff & ebdiff < 0 ~ "incm",
      bdiff == 0 & ediff == 0 & ebdiff == 0 ~ "none",
      TRUE ~ NA_character_
    ),
    gequity = case_when(
      ggap != "incm" ~ "Yes",
      ggap == "incm" & ediff < (Overallbaseline * 0.05) ~ "Yes",
      ggap == "incm" & ediff > (Overallbaseline * 0.05) ~ "No",
      TRUE ~ NA_character_
    )) %>%
  filter(Gequity3 == 1) 


total <- ge %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs that have partial or solid evidence of increase and have baseline and endline data for both boys and girls (Gequity3==1)",
         index = "a") 
total

if (nrow(total) == 0) {
  total <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs that have partial or solid evidence of increase and have baseline and endline data for both boys and girls (Gequity3==1)", 
                  index = "a")
} else {
  total <- total 
}

total

bg_improve <- ge %>%
  filter(fdiff > 0 & mdiff > 0) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where both boys and girls improve (fdiff > 0 & mdiff > 0)",
         index = "b") 

if (nrow(bg_improve) == 0) {
  bg_improve <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where both boys and girls improve (fdiff > 0 & mdiff > 0)", 
                       index = "b")
} else {
  bg_improve <- bg_improve 
}

bg_improve

gap_reduced <- ge %>%
  filter(ediff < bdiff) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where gender gap is reduced (ediff < bdiff)",
         index = "c") 

if (nrow(gap_reduced) == 0) {
  gap_reduced <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender gap is reduced (ediff < bdiff)", 
                        index = "c")
} else {
  gap_reduced <- gap_reduced 
}

gap_reduced

gap_increased <- ge %>%
  filter(ediff > bdiff) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where gender gap increased (ediff > bdiff)",
         index = "d") 

if (nrow(gap_increased) == 0) {
  gap_increased <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender gap increased (ediff > bdiff)", 
                          index = "d")
} else {
  gap_increased <- gap_increased 
}

gap_increased

equitable_increased <- ge %>%
  filter(fdiff > 0 & mdiff > 0 & gequity == "Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(
    FER = if("FER" %in% colnames(.)) FER else 0,
    MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
    Total = FER + MYRP,
    id = "# of programs where gender equitable increases (if fdiff > 0 & mdiff > 0 and gequity = yes)",
    index = "e")
equitable_increased

if (nrow(equitable_increased) == 0) {
  equitable_increased <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender equitable increases (if fdiff > 0 & mdiff > 0 and gequity = yes)", index = "e")
} else {
  equitable_increased <- equitable_increased 
}

ge_increase_relative <- rbind(total, equitable_increased) %>%
  pivot_wider(names_from = index, values_from = c(FER, MYRP, Total, id)) %>%
  mutate(Total = Total_e / Total_a,
         FER = FER_e / FER_a,
         MYRP = MYRP_e / MYRP_a,
         id = "% of programs with gender equitable increases (E/A)",
         index = "f") %>%
  select(id, index, Total, FER, MYRP)

ge_increase_relative

data <- rbind(total, bg_improve, gap_reduced, gap_increased, equitable_increased, ge_increase_relative)

data <- data %>%
  select(index, id, Total, MYRP, FER)
data


addWorksheet(wb, "Indicator3GE")
writeData(wb, sheet = "Indicator3GE", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_3, data, total, bg_improve, gap_reduced, gap_increased, equitable_increased, ge_increase_relative)

print("===================================================================")
# Indicator 4 ----

print("Indicator #4 analysis")

# ** a. total programs
ind_4 <- df %>%
  filter(Indicator == 4)

ind_4 <- ind_4 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_4), " rows for analysis."))

unique(ind_4$LeadGRN)

programs<- ind_4 %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs (unique LeadGRN)",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs

# ** b. total eligible programs

ind_4 <- ind_4[!grepl("Regional", ind_4$Country), ]

programs_country <- ind_4 %>% group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of country programs (unique LeadGRN)",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_country

data <- rbind(programs, programs_country) 

# ** c. total programs with evidence available

programs_evidenceavailable <- ind_4 %>%
  filter(Evidenceavailable == "Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with evidence available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable

data <- rbind(data, programs_evidenceavailable)

# ** d. % programs evidence available

programs_Perevidenceavailable <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence available` = `# of programs with evidence available` / `# of country programs (unique LeadGRN)`) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence available`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence available`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence avaliable") %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_Perevidenceavailable

data <- rbind(data, programs_Perevidenceavailable)  

# ** e. # programs with numeric evidence available

programs_evidenceavailable_num <- ind_4 %>%
  filter(Evidenceavailable == "Yes" & Evidenceofchange != "Weak") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with numeric evidence available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_num

data <- rbind(data, programs_evidenceavailable_num) 

# ** f. # programs with numeric evidence as rate available

# convert_to_decimal <- function(x) {
#   if (grepl("%", x)) {
#     # Remove the '%' sign and convert to numeric
#     x_numeric <- as.numeric(gsub("%", "", x))
#     # Convert to decimal
#     return(x_numeric / 100)
#   } else {
#     # Convert already decimal strings to numeric
#     return(as.numeric(x))
#   }
# }

ind_4 <- ind_4 %>% 
  mutate(Value = case_when(
    Evidenceofchange == "Baseline" ~ Overallbaseline,
    Evidenceofchange == "Partial" ~ Overallendline,
    Evidenceofchange == "Solid" ~ Overallendline,
  )) %>%
  mutate(Value = sapply(Value, convert_to_decimal)) %>%
  mutate(ValueP = case_when(
    Value <= 1 ~ "Yes",
    Value > 1 ~ "No"
  ))

programs_evidenceavailable_rate <- ind_4 %>%
  filter(ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with numeric evidence as rate available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_rate

data <- rbind(data, programs_evidenceavailable_rate)

# ** g. % programs with numeric evidence as rate

programs_Perevidenceavailablerate <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence available as rate` = `# of programs with numeric evidence as rate available` / `# of programs with evidence available`) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence available as rate`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence available as rate`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence avaliable as rate") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data <- rbind(data, programs_Perevidenceavailablerate)  

# ** h. # of programs with rate > .75

ind_4 <- ind_4 %>% 
  mutate(Indcalc = case_when(
    Value < .75 ~ "No",
    TRUE ~ "Yes"
  )
  ) 

programs_evidenceavailable_ind <- ind_4 %>%
  filter(Indcalc=="Yes" & ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with rate > 75% available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_ind

data <- rbind(data, programs_evidenceavailable_ind)  

# ** i. % of programs with rate > .75

programs_Perevidenceavailable_ind <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with rate > 75%` = `# of programs with rate > 75% available` / `# of programs with numeric evidence as rate available`) %>% #CTD changed per country programs 
  select(name,  `% of programs with rate > 75%`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with rate > 75%`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with rate > 75%") %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_Perevidenceavailable_ind

data <- rbind(data, programs_Perevidenceavailable_ind)  

# ** j. average rate


programs_indmean <- ind_4 %>%
  filter(ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(
    mean = mean(Value, na.rm=TRUE),
    sd = sd(Value, na.rm=TRUE),
    median = median(Value, na.rm=TRUE)
  )%>%
  ungroup() 

programs_indmeantot <- ind_4 %>%
  filter(ValueP=="Yes") %>%
  summarise(
    Typeofinvestment = "TotalECWPrograms",
    mean = mean(Value, na.rm=TRUE),
    sd = sd(Value, na.rm=TRUE),
    median = median(Value, na.rm=TRUE)
  )

programs_indsum <- bind_rows(programs_indmeantot, programs_indmean)

programs_indsumtest <- programs_indsum %>%
  pivot_longer(cols = -Typeofinvestment, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = Typeofinvestment, values_from = value)


data <- rbind(data, programs_indsumtest)  

# ** k. # of programs gender information

programs_gequity1 <- ind_4 %>%
  filter(ValueP=="Yes" & Gequity1 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys baseline",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity1

programs_gequity2 <- ind_4 %>%
  filter(ValueP=="Yes" & Gequity2 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys endline",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity2

programs_gequity3 <- ind_4 %>%
  filter(ValueP=="Yes" & Gequity3 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys baseline/endline = 1",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity3

data <- rbind(data, programs_gequity1, programs_gequity2, programs_gequity3)

# ** formatting

index <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
calculation <- c("", "", "", "c/b", "", "", "f/c", "", "h/g", "", "", "", "", "", "")

data <- data %>%
  mutate (index = index) %>%
  mutate (calculation = calculation) %>% 
  select (index, id, calculation, TotalECWPrograms, FER, MYRP) 

# ** output

addWorksheet(wb, "Indicator4")
writeData(wb, sheet = "Indicator4", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(counts, data, checkgrn, cr_nfe, crosstab, data, df_cr, 
   programs, programs_country, programs_evidenceavailable, programs_NFE,
   programs_evidenceavailable_num, programs_evchange, 
   programs_Perevidenceavailable, programs_Perevidenceavailable_rate, programs_Perevidencechange,  
   programs_indmean, programs_indmeantot, programs_indsum, programs_indsumtest,
   programs_Perevidenceavailable_ind, programs_Perevidencechange, programs_Perevidenceavailablerate, 
   programs_gequity1, programs_gequity2, programs_gequity3)

## 4 Gender equitable ----

ind_4 <- ind_4 %>% 
  filter(ValueP=="Yes") %>% 
  mutate(ValueF = case_when(
    Evidenceofchange == "Baseline" ~ Girlsbaseline,
    Evidenceofchange == "Partial" ~ Girlsendline,
    Evidenceofchange == "Solid" ~ Girlsendline,
  )) %>%
  mutate(ValueF = sapply(ValueF, convert_to_decimal)) %>% 
  mutate(ValueM = case_when(
    Evidenceofchange == "Baseline" ~ Boysbaseline,
    Evidenceofchange == "Partial" ~ Boysendline,
    Evidenceofchange == "Solid" ~ Boysendline,
  )) %>%
  mutate(ValueM = sapply(ValueM, convert_to_decimal)) 

ind_4 <- ind_4 %>% 
  mutate(fmdiff = ifelse(is.na(ValueF - ValueM), NA_character_, ValueF - ValueM)) %>% 
  mutate(gequity = case_when(
    is.na(ValueF) | is.na(ValueM) ~ NA_character_,
    Value > .75 & ValueF > .75 & ValueM > .75 ~ "Yes",
    Value > .75 & fmdiff <= .1 & fmdiff >= -.1 ~ "Yes", 
    TRUE ~ "No"
  ))

programs_evidenceavailable_ge <- ind_4 %>%
  filter(!is.na(ValueF) & !is.na(ValueM) & ValueP =="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs numeric evidence as rate and gender disaggregated data",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_ge

data_ge <- rbind(programs_evidenceavailable_rate, programs_evidenceavailable_ge)

programs_Perevidenceavailable_ge <- data_ge %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence available as rate and gender disaggregated data` = `# of programs numeric evidence as rate and gender disaggregated data`/`# of programs with numeric evidence as rate available` ) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence available as rate and gender disaggregated data`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence available as rate and gender disaggregated data`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence available as rate and gender disaggregated data") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data_ge <- rbind(data_ge, programs_Perevidenceavailable_ge)  

programs_evidenceavailableind_ge <- ind_4 %>%
  filter(!is.na(ValueF) & !is.na(ValueM) & Value > .75) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with rate > 75% and gender disaggregated data",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailableind_ge

data_ge <- rbind(data_ge, programs_evidenceavailable_ind, programs_evidenceavailableind_ge)

programs_Perevidenceavailableind_ge <- data_ge %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with rate > 75% and gender disaggregated data` = `# of programs with rate > 75% and gender disaggregated data`/`# of programs with rate > 75% available` ) %>% #CTD changed per country programs 
  select(name,  `% of programs with rate > 75% and gender disaggregated data`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with rate > 75% and gender disaggregated data`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with rate > 75% and gender disaggregated data") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data_ge <- rbind(data_ge, programs_Perevidenceavailableind_ge)  

programs_ge <- ind_4 %>%
  filter(gequity=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with gender equitable rate",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_ge

data_ge <- rbind(data_ge, programs_ge)  

programs_Per_ge <- data_ge %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with gender equitable rate` = `# of programs with gender equitable rate`/`# of programs with rate > 75% and gender disaggregated data` ) %>% #CTD changed per country programs 
  select(name,  `% of programs with gender equitable rate`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with gender equitable rate`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with gender equitable rate") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data_ge <- rbind(data_ge, programs_Per_ge)  

programs_genmean <- ind_4 %>%
  filter(ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(
    gmean = mean(ValueF, na.rm=TRUE),
    bmean = mean(ValueM, na.rm=TRUE),
    gsd = sd(ValueF, na.rm=TRUE),
    bsd = sd(ValueM, na.rm=TRUE),
    gmedian = median(ValueF, na.rm=TRUE),
    bmedian = median(ValueM, na.rm=TRUE)
  )%>%
  ungroup() 

programs_genmeantot <- ind_4 %>%
  filter(ValueP=="Yes") %>%
  summarise(
    Typeofinvestment = "TotalECWPrograms",
    gmean = mean(ValueF, na.rm=TRUE),
    bmean = mean(ValueM, na.rm=TRUE),
    gsd = sd(ValueF, na.rm=TRUE),
    bsd = sd(ValueM, na.rm=TRUE),
    gmedian = median(ValueF, na.rm=TRUE),
    bmedian = median(ValueM, na.rm=TRUE)
  )

programs_gensum <- bind_rows(programs_genmeantot, programs_genmean)

programs_gensumtest <- programs_gensum %>%
  pivot_longer(cols = -Typeofinvestment, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = Typeofinvestment, values_from = value)


data_ge <- rbind(data_ge, programs_gensumtest)

addWorksheet(wb, "Indicator4GE")
writeData(wb, sheet = "Indicator4GE", data_ge, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_4, data_ge, total, programs_gensumtest)

# Indicator 5 ----

print("Indicator #5 analysis")

# ** a. total programs
ind_5 <- df %>%
  filter(Indicator == 5)

ind_5 <- ind_5 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_5), " rows for analysis."))

unique(ind_5$LeadGRN)

programs<- ind_5 %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs (unique LeadGRN)",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs

# ** b. total eligible programs

ind_5 <- ind_5[!grepl("Regional", ind_5$Country), ]

programs_country <- ind_5 %>% group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,         
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of country programs (unique LeadGRN)",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_country

data <- rbind(programs, programs_country) 

df_cr <- read.csv("output data/cleaning/Cleaned - CR Combiner - For Analysis.csv")

cr_nfe <- df_cr %>% 
  filter(Exercise == "ARR23 Annual" & Typeofreporting == "Joint" & Status == "Reached" & Typeofeducation == "Non-formal education") %>% 
  group_by(LeadGRN) %>% 
  summarise (sum = sum(Number))

ind_5 <- ind_5 %>% 
  left_join(cr_nfe, by = "LeadGRN")

programs_NFE <- ind_5 %>% 
  filter(sum != "NA") %>% 
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with NFE",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_NFE

data <- rbind(data, programs_NFE) 

# ** c. total programs with evidence available

#check that programs that don't have nfe do not have evidence available for this indicator
ind_5 <- ind_5 %>% 
  mutate(nfe = ifelse(!is.na(sum) & sum > 0, 1, 0))

crosstab <- table(ind_5$EvidenceavailableNumeric, ind_5$nfe) %>% 
  as.data.frame()

checkgrn <- ind_5 %>% 
  filter(EvidenceavailableNumeric==1 & nfe==0) %>% 
  select(LeadGRN, EvidenceavailableNumeric, nfe)

print(checkgrn)


programs_evidenceavailable <- ind_5 %>%
  filter(Evidenceavailable == "Yes" & nfe==1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with NFE with evidence available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable

data <- rbind(data, programs_evidenceavailable)

# ** d. % programs evidence available

programs_Perevidenceavailable <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with NFE with evidence available` = `# of programs with NFE with evidence available` / `# of programs with NFE`) %>% #CTD changed per country programs 
  select(name,  `% of programs with NFE with evidence available`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with NFE with evidence available`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with NFE with evidence avaliable") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data <- rbind(data, programs_Perevidenceavailable)  

# ** e. # programs with numeric evidence available

programs_evidenceavailable_num <- ind_5 %>%
  filter(Evidenceavailable == "Yes" & nfe==1 & Evidenceofchange != "Weak") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with numeric evidence available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_num

data <- rbind(data, programs_evidenceavailable_num) 

# ** f. # programs with numeric evidence as rate available

# convert_to_decimal <- function(x) {
#   if (grepl("%", x)) {
#     # Remove the '%' sign and convert to numeric
#     x_numeric <- as.numeric(gsub("%", "", x))
#     # Convert to decimal
#     return(x_numeric / 100)
#   } else {
#     # Convert already decimal strings to numeric
#     return(as.numeric(x))
#   }
# }

ind_5 <- ind_5 %>% 
  mutate(Value = case_when(
    Evidenceofchange == "Baseline" ~ Overallbaseline,
    Evidenceofchange == "Partial" ~ Overallendline,
    Evidenceofchange == "Solid" ~ Overallendline,
  )) %>%
  mutate(Value = sapply(Value, convert_to_decimal)) %>%
  mutate(ValueP = case_when(
    Value <= 1 ~ "Yes",
    Value > 1 ~ "No"
  ))

#ind_5test <- ind_5 %>% 
#  select (LeadGRN, Overallbaseline, Overallendline, Evidenceofchange, Value, ValueP, nfe)

programs_evidenceavailable_rate <- ind_5 %>%
  filter(ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with numeric evidence as rate available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_rate

data <- rbind(data, programs_evidenceavailable_rate)

# ** g. % programs with numeric evidence as rate

programs_Perevidenceavailablerate <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence available as rate` = `# of programs with numeric evidence as rate available` / `# of programs with NFE with evidence available`) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence available as rate`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence available as rate`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence avaliable as rate") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data <- rbind(data, programs_Perevidenceavailablerate)  

# ** h. # of programs with rate > .60

ind_5 <- ind_5 %>% 
  mutate(Indcalc = case_when(
    Value < .60 ~ "No",
    TRUE ~ "Yes"
  )
  ) 

programs_evidenceavailable_ind <- ind_5 %>%
  filter(Indcalc=="Yes" & ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with rate > 60% available",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_ind

data <- rbind(data, programs_evidenceavailable_ind)  

# ** i. % of programs with rate > .60

programs_Perevidenceavailable_ind <- data %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with rate > 60%` = `# of programs with rate > 60% available` / `# of programs with numeric evidence as rate available`) %>% #CTD changed per country programs 
  select(name,  `% of programs with rate > 60%`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with rate > 60%`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with rate > 60%") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data <- rbind(data, programs_Perevidenceavailable_ind)  

# ** j. average rate


programs_indmean <- ind_5 %>%
  filter(ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(
    mean = mean(Value, na.rm=TRUE),
    sd = sd(Value, na.rm=TRUE),
    median = median(Value, na.rm=TRUE)
  )%>%
  ungroup() 

programs_indmeantot <- ind_5 %>%
  filter(ValueP=="Yes") %>%
  summarise(
    Typeofinvestment = "TotalECWPrograms",
    mean = mean(Value, na.rm=TRUE),
    sd = sd(Value, na.rm=TRUE),
    median = median(Value, na.rm=TRUE)
  )

programs_indsum <- bind_rows(programs_indmeantot, programs_indmean)

programs_indsumtest <- programs_indsum %>%
  pivot_longer(cols = -Typeofinvestment, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = Typeofinvestment, values_from = value)


data <- rbind(data, programs_indsumtest)  

# ** k. # of programs gender information

programs_gequity1 <- ind_5 %>%
  filter(ValueP=="Yes" & Gequity1 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys baseline",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity1

programs_gequity2 <- ind_5 %>%
  filter(ValueP=="Yes" & Gequity2 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys endline",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity2

programs_gequity3 <- ind_5 %>%
  filter(ValueP=="Yes" & Gequity3 == 1) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0, # Ensure FER exists and set it to 0 if missing
         id = "# of programs with girls/boys baseline/endline = 1",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)

programs_gequity3

data <- rbind(data, programs_gequity1, programs_gequity2, programs_gequity3)


# ** formatting

index <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
calculation <- c("", "", "", "", "d/c", "", "", "g/d", "", "i/g", "", "", "", "", "", "")

data <- data %>%
  mutate (index = index) %>%
  mutate (calculation = calculation) %>% 
  select (index, id, calculation, TotalECWPrograms, FER, MYRP) 

# ** output

addWorksheet(wb, "Indicator5")
writeData(wb, sheet = "Indicator5", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)


rm(counts, data, checkgrn, cr_nfe, crosstab, data, df_cr, 
   programs, programs_country, programs_evidenceavailable, programs_NFE,
   programs_evidenceavailable_num, programs_evchange, programs_evchange_solid, 
   programs_Perevidenceavailable, programs_Perevidenceavailable_rate, programs_Perevidencechange, programs_Perevidencechange_solid,
   programs_code_childSI, programs_code_childSIPI, programs_PerPartialSolid, programs_PerSolid,
   programs_indmean, programs_indmeantot, programs_indsum, programs_indsumtest,
   programs_Perevidenceavailable_ind, programs_Perevidencechange, programs_Perevidenceavailablerate, 
   programs_gequity1, programs_gequity2, programs_gequity3)

## 5 Gender equitable ----

ind_5 <- ind_5 %>% 
  filter(ValueP=="Yes") %>% 
  mutate(ValueF = case_when(
    Evidenceofchange == "Baseline" ~ Girlsbaseline,
    Evidenceofchange == "Partial" ~ Girlsendline,
    Evidenceofchange == "Solid" ~ Girlsendline,
  )) %>%
  mutate(ValueF = sapply(ValueF, convert_to_decimal)) %>% 
  mutate(ValueM = case_when(
    Evidenceofchange == "Baseline" ~ Boysbaseline,
    Evidenceofchange == "Partial" ~ Boysendline,
    Evidenceofchange == "Solid" ~ Boysendline,
  )) %>%
  mutate(ValueM = sapply(ValueM, convert_to_decimal)) 

ind_5 <- ind_5 %>% 
  mutate(fmdiff = ifelse(is.na(ValueF - ValueM), NA_character_, ValueF - ValueM)) %>% 
  mutate(gequity = case_when(
    is.na(ValueF) | is.na(ValueM) ~ NA_character_,
    Value > .60 & ValueF > .60 & ValueM > .60 ~ "Yes",
    Value > .60 & fmdiff <= .1 & fmdiff >= -.1 ~ "Yes", 
    TRUE ~ "No"
  ))

programs_evidenceavailable_ge <- ind_5 %>%
  filter(!is.na(ValueF) & !is.na(ValueM) & ValueP =="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs numeric evidence as rate and gender disaggregated data",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailable_ge

data_ge <- rbind(programs_evidenceavailable_rate, programs_evidenceavailable_ge)

programs_Perevidenceavailable_ge <- data_ge %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with evidence available as rate and gender disaggregated data` = `# of programs numeric evidence as rate and gender disaggregated data`/`# of programs with numeric evidence as rate available` ) %>% #CTD changed per country programs 
  select(name,  `% of programs with evidence available as rate and gender disaggregated data`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with evidence available as rate and gender disaggregated data`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with evidence available as rate and gender disaggregated data") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data_ge <- rbind(data_ge, programs_Perevidenceavailable_ge)  

programs_evidenceavailableind_ge <- ind_5 %>%
  filter(!is.na(ValueF) & !is.na(ValueM) & Value > .60) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with rate > 60% and gender disaggregated data",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_evidenceavailableind_ge

data_ge <- rbind(data_ge, programs_evidenceavailable_ind, programs_evidenceavailableind_ge)

programs_Perevidenceavailableind_ge <- data_ge %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with rate > 60% and gender disaggregated data` = `# of programs with rate > 60% and gender disaggregated data`/`# of programs with rate > 60% available` ) %>% #CTD changed per country programs 
  select(name,  `% of programs with rate > 60% and gender disaggregated data`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with rate > 60% and gender disaggregated data`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with rate > 60% and gender disaggregated data") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data_ge <- rbind(data_ge, programs_Perevidenceavailableind_ge)  

programs_ge <- ind_5 %>%
  filter(gequity=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "# of programs with gender equitable rate",
         TotalECWPrograms = FER + MYRP) %>%
  select(id, TotalECWPrograms, FER, MYRP)
programs_ge

data_ge <- rbind(data_ge, programs_ge)  

programs_Per_ge <- data_ge %>%
  pivot_longer(cols = c(TotalECWPrograms, FER, MYRP)) %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(`% of programs with gender equitable rate` = `# of programs with gender equitable rate`/`# of programs with rate > 60% and gender disaggregated data` ) %>% #CTD changed per country programs 
  select(name,  `% of programs with gender equitable rate`) %>%
  pivot_wider(names_from = name, values_from =  `% of programs with gender equitable rate`) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         id = "% of programs with gender equitable rate") %>%
  select(id, TotalECWPrograms, FER, MYRP)

data_ge <- rbind(data_ge, programs_Per_ge)  

programs_genmean <- ind_5 %>%
  filter(ValueP=="Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(
    gmean = mean(ValueF, na.rm=TRUE),
    bmean = mean(ValueM, na.rm=TRUE),
    gsd = sd(ValueF, na.rm=TRUE),
    bsd = sd(ValueM, na.rm=TRUE),
    gmedian = median(ValueF, na.rm=TRUE),
    bmedian = median(ValueM, na.rm=TRUE)
  )%>%
  ungroup() 

programs_genmeantot <- ind_5 %>%
  filter(ValueP=="Yes") %>%
  summarise(
    Typeofinvestment = "TotalECWPrograms",
    gmean = mean(ValueF, na.rm=TRUE),
    bmean = mean(ValueM, na.rm=TRUE),
    gsd = sd(ValueF, na.rm=TRUE),
    bsd = sd(ValueM, na.rm=TRUE),
    gmedian = median(ValueF, na.rm=TRUE),
    bmedian = median(ValueM, na.rm=TRUE)
  )

programs_gensum <- bind_rows(programs_genmeantot, programs_genmean)

programs_gensumtest <- programs_gensum %>%
  pivot_longer(cols = -Typeofinvestment, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = Typeofinvestment, values_from = value)


data_ge <- rbind(data_ge, programs_gensumtest)

addWorksheet(wb, "Indicator5GE")
writeData(wb, sheet = "Indicator5GE", data_ge, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

print("===================================================================")

# Indicator 6 ----

# ** a. total programs
print("Indicator #6 analysis")

ind_6 <- df %>%
  filter(Indicator == 6 & Typeofinvestment == "MYRP") #CTD: removed the regional from here so will mirror indicator 3


ind_6 <- ind_6 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_6), " rows for analysis."))
unique(ind_6$LeadGRN)

#CTD added code lines to add to table total number of MYRPs

programs<- ind_6 %>%
  summarise(Total = n()) %>%
  mutate(id = "# of programs (unique LeadGRN)") %>%
  select(id, Total)
programs

# ** b. total eligible programs
#CTD added code lines 283-291to add to table number of MYRPs without regional
ind_6 <- ind_6[!grepl("Regional", ind_6$Country), ]

programs_country<- ind_6 %>%
  summarise(Total = n()) %>%
  mutate(id = "# of country programs (unique LeadGRN)") %>%
  select(id, Total)
programs_country

data <- rbind(programs, programs_country)


# ** c. total programs with evidence available
#CTD added code to add to table number of MYRPs evidence available
program_evavail <- ind_6 %>%
  filter(Evidenceavailable=="Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "# of programs with evidence available") %>%
  select(id, Total)
program_evavail

data <- rbind(data, program_evavail)

# ** d. % programs evidence available
#CTD added code lines 295-302 to add to table % of MYRPs evidence available

programs_Perevidenceavailable <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`% of programs with evidence available` = `# of programs with evidence available` / `# of country programs (unique LeadGRN)`) %>% #CTD changed per country programs 
  select(`% of programs with evidence available`) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Total")

data <- rbind(data, programs_Perevidenceavailable)  

# ** e. # programs evidence of change
total <- ind_6 %>%
  filter(Evidenceofchange!="Baseline") %>% 
  filter(!is.na(Evidenceofchange))  %>% #CTD: I couldn't get this to work in one filter statement, excluding baseline and NA (although there shouldn't be any NAs ...)
  summarise(Total = n()) %>%
  mutate(id = "# of programs with evidence of change") %>%
  select(id, Total)
total

data <- rbind(data, total)  

programs_evchange_solid <- ind_6 %>%
  filter(EvidenceofchangeNumeric == 3 | EvidenceofchangeNumeric == 2) %>%
  summarise(Total = n()) %>%
  mutate(id = "# of programs with partial/ solid evidence of change") %>%
  select(id, Total)

data <- rbind(data, programs_evchange_solid) 

# ** f. % programs evidence of change

programs_Perevidencechange <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`% of programs with evidence of change` = `# of programs with evidence of change` / `# of programs with evidence available`) %>% #CTD changed per country programs 
  select(`% of programs with evidence of change`) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Total")

data <- rbind(data, programs_Perevidencechange)  

programs_Perevidencechange_solid <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`% of programs with partial/ solid evidence of change` = `# of programs with partial/ solid evidence of change` / `# of programs with evidence available`) %>% #CTD changed per country programs 
  select(`% of programs with partial/ solid evidence of change`) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Total")

data <- rbind(data, programs_Perevidencechange_solid)  

# ** g. # programs partial or solid increase

totals <- ind_6 %>%
  filter(NewCode == "PI" | NewCode == "SI") %>%
  group_by(NewCode) %>%
  summarise(Total = n()) %>%
  rename(id = NewCode)
totals

#CTD added data in the code below
data <- rbind(data, totals)

# ** h. % programs partial or solid increase

#CTD added unpivoting and renaming columns so matches Indicator 3
data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`PI+SI` = PI + SI,
         SI = ifelse(!"SI" %in% colnames(.), 0, SI),
         #PerPI = PI / `# of MYRPs with evidence of change`,
         `PerPI+SI` = `PI+SI` / `# of programs with partial/ solid evidence of change`,
         PerSI = SI / `# of programs with evidence of change`)%>%
  select(-PI) %>%
  select('# of programs (unique LeadGRN)', '# of country programs (unique LeadGRN)', '# of programs with evidence available',  '% of programs with evidence available', '# of programs with evidence of change', '# of programs with partial/ solid evidence of change', '% of programs with evidence of change', '% of programs with partial/ solid evidence of change', 'PI+SI', SI, 'PerPI+SI', PerSI) %>%
  rename("# of programs with evidence of partial or solid increase" = 'PI+SI', 
         "# of programs with evidence of solid increase" = SI,
         "% of programs with evidence of partial or solid increase" = 'PerPI+SI', 
         "% of programs with evidence of solid increase" = PerSI ) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Result")

# ** i. # of programs gender information
programs_gequity1 <- ind_6 %>%
  filter(Gequity1 == 1) %>%
  summarise(Result = n()) %>%
  mutate(id = "# of programs with girls/boys baseline") %>%
  select(id, Result)

programs_gequity1

programs_gequity2 <- ind_6 %>%
  filter(Gequity2 == 1) %>%
  summarise(Result = n()) %>%
  mutate(id = "# of programs with girls/boys endline") %>%
  select(id, Result)

programs_gequity2


programs_gequity3 <- ind_6 %>%
  filter(Gequity3 == 1) %>%
  summarise(Result = n()) %>%
  mutate(id = "# of programs with girls/boys baseline/endline") %>%
  select(id, Result)

programs_gequity3

data <- rbind(data, programs_gequity1, programs_gequity2, programs_gequity3)

# ** formatting
index <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
calculation <- c("", "", "", "c/b", "", "", "e/c", "f/c", "", "", "i/f", "j/e", "", "", "")

data <- data %>%
  mutate (index = index) %>%
  mutate (calculation = calculation) %>% 
  select (index, id, calculation, Result) 


# ** output

addWorksheet(wb, "Indicator6")
writeData(wb, sheet = "Indicator6", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(counts, data, checkgrn, cr_nfe, crosstab, data, df_cr, 
   programs, programs_country, programs_evidenceavailable, programs_NFE, total, totals,
   program_evavail, programs_evidenceavailable_num, programs_evchange, programs_evchange_solid, programs_evidenceavailable_rate, 
   programs_Perevidenceavailable, programs_Perevidenceavailable_rate, programs_Perevidencechange, programs_Perevidencechange_solid,
   programs_code_childSI, programs_code_childSIPI, programs_PerPartialSolid, programs_PerSolid,
   programs_evidenceavailable_ind, programs_indmean, programs_indmeantot, programs_indsum, programs_indsumtest,
   programs_Perevidenceavailable_ind, programs_Perevidencechange, programs_Perevidenceavailablerate, 
   programs_gequity1, programs_gequity2, programs_gequity3)

## 6 Gender equitable----

ge <- ind_6 %>%
  mutate(
    Overallbaseline = sapply(Overallbaseline, convert_to_decimal),
    Girlsbaseline = sapply(Girlsbaseline, convert_to_decimal),
    Boysbaseline = sapply(Boysbaseline, convert_to_decimal),
    Girlsendline = sapply(Girlsendline, convert_to_decimal),
    Boysendline = sapply(Boysendline, convert_to_decimal),
    fdiff = Girlsendline - Girlsbaseline,
    mdiff = Boysendline - Boysbaseline,
    bdiff = Girlsbaseline - Boysbaseline,
    ediff = Girlsendline - Boysendline,
    ebdiff = ediff - bdiff,
    ggap = case_when(
      ediff < bdiff & ebdiff < 0 ~ "redm",
      ediff < bdiff & ebdiff > 0 ~ "redf",
      ediff > bdiff & ebdiff > 0 ~ "incf",
      ediff > bdiff & ebdiff < 0 ~ "incm",
      bdiff == 0 & ediff == 0 & ebdiff == 0 ~ "none",
      TRUE ~ NA_character_
    ),
    gequity = case_when(
      ggap != "incm" ~ "Yes",
      ggap == "incm" & ediff < (Overallbaseline * 0.05) ~ "Yes",
      ggap == "incm" & ediff > (Overallbaseline * 0.05) ~ "No",
      TRUE ~ NA_character_
    )) %>%
  filter(Gequity3 == 1 &
           (NewCode == "SI" | NewCode == "PI")) 


total <- ge %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs that have partial or solid evidence of increase and have baseline and endline data for both boys and girls (Gequity3==1)",
         index = "a") 

if (nrow(total) == 0) {
  total <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs that have partial or solid evidence of increase and have baseline and endline data for both boys and girls (Gequity3==1)", 
                  index = "a")
} else {
  total <- total 
}



bg_improve <- ge %>%
  filter(fdiff > 0 & mdiff > 0) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where both boys and girls improve (fdiff > 0 & mdiff > 0)",
         index = "b") 

if (nrow(bg_improve) == 0) {
  bg_improve <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where both boys and girls improve (fdiff > 0 & mdiff > 0)", 
                       index = "b")
} else {
  bg_improve <- bg_improve 
}

gap_reduced <- ge %>%
  filter(ediff < bdiff) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where gender gap is reduced (ediff < bdiff)",
         index = "c") 

if (nrow(gap_reduced) == 0) {
  gap_reduced <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender gap is reduced (ediff < bdiff)", 
                        index = "c")
} else {
  gap_reduced <- gap_reduced 
}

gap_increased <- ge %>%
  filter(ediff > bdiff) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where gender gap increased (ediff > bdiff)",
         index = "d") 

if (nrow(gap_increased) == 0) {
  gap_increased <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender gap increased (ediff > bdiff)", 
                          index = "d")
} else {
  gap_increased <- gap_increased 
}

equitable_increased <- ge %>%
  filter(fdiff > 0 & mdiff > 0 & gequity == "Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(
    FER = if("FER" %in% colnames(.)) FER else 0,          
    MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
    Total = FER + MYRP,
    id = "# of programs where gender equitable increases (if fdiff > 0 & mdiff > 0 and gequity = yes)",
    index = "e")

if (nrow(equitable_increased) == 0) {
  equitable_increased <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender equitable increases (if fdiff > 0 & mdiff > 0 and gequity = yes)", index = "e")
} else {
  equitable_increased <- equitable_increased 
}

ge_increase_relative <- rbind(total, equitable_increased) %>%
  pivot_wider(names_from = index, values_from = c(FER, MYRP, Total, id)) %>%
  mutate(Total = Total_e / Total_a,
         FER = FER_e / FER_a,
         MYRP = MYRP_e / MYRP_a,
         id = "% of programs with gender equitable increases (E/A)",
         index = "f") %>%
  select(id, index, Total, FER, MYRP)

ge_increase_relative

data <- rbind(total, bg_improve, gap_reduced, gap_increased, equitable_increased, ge_increase_relative)

data <- data %>%
  select(index, id, Total, MYRP, FER)
data

addWorksheet(wb, "Indicator6GE")
writeData(wb, sheet = "Indicator6GE", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_6, data, total, bg_improve, gap_reduced, gap_increased, equitable_increased, ge_increase_relative)

print("===================================================================")
print("Indicator #7 analysis")

# Indicator 7 ---- 

# ** a. total programs
ind_7 <- df %>%
  filter(Indicator == 7 & Typeofinvestment == "MYRP") #CTD: removed the regional from here so will mirror indicator 3

ind_7 <- ind_7 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_7), " rows for analysis."))
unique(ind_7$LeadGRN)

#CTD added code lines 371-388 to add to table total number of MYRPs

programs<- ind_7 %>%
  summarise(Total = n()) %>%
  mutate(id = "# of programs (unique LeadGRN)") %>%
  select(id, Total)
programs

# ** b. total eligible programs
#CTD added code lines 381-390 to add to table number of MYRPs without regional
ind_7 <- ind_7[!grepl("Regional", ind_7$Country), ]

programs_country<- ind_7 %>%
  summarise(Total = n()) %>%
  mutate(id = "# of country programs (unique LeadGRN)") %>%
  select(id, Total)
programs_country

data <- rbind(programs, programs_country)

#CTD added code lines 392-401 to add to table number of MYRPs evidence available

# ** c. total programs with evidence available
program_evavail <- ind_7 %>%
  filter(Evidenceavailable=="Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "# of programs with evidence available") %>%
  select(id, Total)
program_evavail

data <- rbind(data, program_evavail)

# ** d. % programs evidence available
#CTD added code to add to table % of MYRPs evidence available

programs_Perevidenceavailable <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`% of programs with evidence available` = `# of programs with evidence available` / `# of country programs (unique LeadGRN)`) %>% #CTD changed per country programs 
  select(`% of programs with evidence available`) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Total")

data <- rbind(data, programs_Perevidenceavailable)  

# ** e. # programs evidence of change

total <- ind_7 %>%
  filter(Evidenceofchange!="Baseline") %>% 
  filter(!is.na(Evidenceofchange))  %>% #CTD: I couldn't get this to work in one filter statement, excluding baseline and NA (although there shouldn't be any NAs ...)
  summarise(Total = n()) %>%
  mutate(id = "# of programs with evidence of change") %>%
  select(id, Total)
total

data <- rbind(data, total)  

programs_evchange_solid <- ind_7 %>%
  filter(EvidenceofchangeNumeric == 3 | EvidenceofchangeNumeric == 2) %>%
  summarise(Total = n()) %>%
  mutate(id = "# of programs with partial/ solid evidence of change") %>%
  select(id, Total)

data <- rbind(data, programs_evchange_solid) 

# ** f. % programs evidence of change

programs_Perevidencechange <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`% of programs with evidence of change` = `# of programs with evidence of change` / `# of programs with evidence available`) %>% #CTD changed per country programs 
  select(`% of programs with evidence of change`) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Total")

data <- rbind(data, programs_Perevidencechange)  

programs_Perevidencechange_solid <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(`% of programs with partial/ solid evidence of change` = `# of programs with partial/ solid evidence of change` / `# of programs with evidence available`) %>% #CTD changed per country programs 
  select(`% of programs with partial/ solid evidence of change`) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Total")

data <- rbind(data, programs_Perevidencechange_solid)  

# ** g. # programs partial or solid increase

totals <- ind_7 %>%
  filter(NewCode == "PI" | NewCode == "SI") %>%
  group_by(NewCode) %>%
  summarise(Total = n()) %>%
  rename(id = NewCode)
totals

data <- rbind(data, totals)
data

# ** h. % programs partial or solid increase

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(SI = if("SI" %in% colnames(.)) SI else 0,
         PI = if("PI" %in% colnames(.)) PI else 0,
         `PI+SI` = PI + SI,
         `PerPI+SI` = `PI+SI` / `# of programs with partial/ solid evidence of change`,
         PerSI = SI / `# of programs with evidence of change`) %>%
  select(-PI) %>%
  select('# of programs (unique LeadGRN)', '# of country programs (unique LeadGRN)', '# of programs with evidence available',  '% of programs with evidence available', '# of programs with evidence of change', '# of programs with partial/ solid evidence of change', '% of programs with evidence of change', "% of programs with partial/ solid evidence of change", 'PI+SI', SI, 'PerPI+SI', PerSI) %>%
  rename("# of programs with evidence of partial or solid increase" = 'PI+SI', "# of programs with evidence of solid increase" = SI,"% of programs with evidence of partial or solid increase" = 'PerPI+SI', "% of programs with evidence of solid increase" = PerSI ) %>%
  pivot_longer(cols=everything(), names_to = "id", values_to = "Result")

# ** i. # of programs gender information
programs_gequity1 <- ind_7 %>%
  filter(Gequity1 == 1) %>%
  summarise(Result = n()) %>%
  mutate(id = "# of programs with girls/boys baseline") %>%
  select(id, Result)

programs_gequity1

programs_gequity2 <- ind_7 %>%
  filter(Gequity2 == 1) %>%
  summarise(Result = n()) %>%
  mutate(id = "# of programs with girls/boys endline") %>%
  select(id, Result)

programs_gequity2


programs_gequity3 <- ind_7 %>%
  filter(Gequity3 == 1) %>%
  summarise(Result = n()) %>%
  mutate(id = "# of programs with girls/boys baseline/endline") %>%
  select(id, Result)

programs_gequity3

data <- rbind(data, programs_gequity1, programs_gequity2, programs_gequity3)

# ** formatting
index <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
calculation <- c("", "", "", "c/b", "", "", "e/c", "f/c", "", "", "i/f", "j/e", "", "", "")

data <- data %>%
  mutate (index = index) %>%
  mutate (calculation = calculation) %>% 
  select (index, id, calculation, Result) 


# ** output
addWorksheet(wb, "Indicator7")
writeData(wb, sheet = "Indicator7", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(counts, data, checkgrn, cr_nfe, crosstab, data, df_cr, 
   programs, programs_country, programs_evidenceavailable, programs_NFE, total, totals,
   program_evavail, programs_evidenceavailable_num, programs_evchange, programs_evchange_solid, programs_evidenceavailable_rate, 
   programs_Perevidenceavailable, programs_Perevidenceavailable_rate, programs_Perevidencechange, programs_Perevidencechange_solid,
   programs_code_childSI, programs_code_childSIPI, programs_PerPartialSolid, programs_PerSolid,
   programs_evidenceavailable_ind, programs_indmean, programs_indmeantot, programs_indsum, programs_indsumtest,
   programs_Perevidenceavailable_ind, programs_Perevidencechange, programs_Perevidenceavailablerate, 
   programs_gequity1, programs_gequity2, programs_gequity3)


## 7 Gender equitable----

ge <- ind_7 %>%
  mutate(
    Overallbaseline = sapply(Overallbaseline, convert_to_decimal),
    Girlsbaseline = sapply(Girlsbaseline, convert_to_decimal),
    Boysbaseline = sapply(Boysbaseline, convert_to_decimal),
    Girlsendline = sapply(Girlsendline, convert_to_decimal),
    Boysendline = sapply(Boysendline, convert_to_decimal),
    fdiff = Girlsendline - Girlsbaseline,
    mdiff = Boysendline - Boysbaseline,
    bdiff = Girlsbaseline - Boysbaseline,
    ediff = Girlsendline - Boysendline,
    ebdiff = ediff - bdiff,
    ggap = case_when(
      ediff < bdiff & ebdiff < 0 ~ "redm",
      ediff < bdiff & ebdiff > 0 ~ "redf",
      ediff > bdiff & ebdiff > 0 ~ "incf",
      ediff > bdiff & ebdiff < 0 ~ "incm",
      bdiff == 0 & ediff == 0 & ebdiff == 0 ~ "none",
      TRUE ~ NA_character_
    ),
    gequity = case_when(
      ggap != "incm" ~ "Yes",
      ggap == "incm" & ediff < (Overallbaseline * 0.05) ~ "Yes",
      ggap == "incm" & ediff > (Overallbaseline * 0.05) ~ "No",
      TRUE ~ NA_character_
    )) %>%
  filter(Gequity3 == 1 &
           (NewCode == "SI" | NewCode == "PI")) 


total <- ge %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs that have partial or solid evidence of increase and have baseline and endline data for both boys and girls (Gequity3==1)",
         index = "a") 

if (nrow(total) == 0) {
  total <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs that have partial or solid evidence of increase and have baseline and endline data for both boys and girls (Gequity3==1)", 
                  index = "a")
} else {
  total <- total 
}



bg_improve <- ge %>%
  filter(fdiff > 0 & mdiff > 0) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where both boys and girls improve (fdiff > 0 & mdiff > 0)",
         index = "b") 

if (nrow(bg_improve) == 0) {
  bg_improve <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where both boys and girls improve (fdiff > 0 & mdiff > 0)", 
                       index = "b")
} else {
  bg_improve <- bg_improve 
}

gap_reduced <- ge %>%
  filter(ediff < bdiff) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where gender gap is reduced (ediff < bdiff)",
         index = "c") 

if (nrow(gap_reduced) == 0) {
  gap_reduced <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender gap is reduced (ediff < bdiff)", 
                        index = "c")
} else {
  gap_reduced <- gap_reduced 
}

gap_increased <- ge %>%
  filter(ediff > bdiff) %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(FER = if("FER" %in% colnames(.)) FER else 0,          
         MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
         Total = FER + MYRP,
         id = "# of programs where gender gap increased (ediff > bdiff)",
         index = "d") 

if (nrow(gap_increased) == 0) {
  gap_increased <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender gap increased (ediff > bdiff)", 
                          index = "d")
} else {
  gap_increased <- gap_increased 
}

equitable_increased <- ge %>%
  filter(fdiff > 0 & mdiff > 0 & gequity == "Yes") %>%
  group_by(Typeofinvestment) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Count) %>%
  mutate(
    FER = if("FER" %in% colnames(.)) FER else 0,          
    MYRP = if("MYRP" %in% colnames(.)) MYRP else 0,
    Total = FER + MYRP,
    id = "# of programs where gender equitable increases (if fdiff > 0 & mdiff > 0 and gequity = yes)",
    index = "e")

if (nrow(equitable_increased) == 0) {
  equitable_increased <- tibble(FER = 0, MYRP = 0, Total = 0, id = "# of programs where gender equitable increases (if fdiff > 0 & mdiff > 0 and gequity = yes)", index = "e")
} else {
  equitable_increased <- equitable_increased 
}

ge_increase_relative <- rbind(total, equitable_increased) %>%
  pivot_wider(names_from = index, values_from = c(FER, MYRP, Total, id)) %>%
  mutate(Total = Total_e / Total_a,
         FER = FER_e / FER_a,
         MYRP = MYRP_e / MYRP_a,
         id = "% of programs with gender equitable increases (E/A)",
         index = "f") %>%
  select(id, index, Total, FER, MYRP)

ge_increase_relative

data <- rbind(total, bg_improve, gap_reduced, gap_increased, equitable_increased, ge_increase_relative)

data <- data %>%
  select(index, id, Total, MYRP, FER)
data

addWorksheet(wb, "Indicator7GE")
writeData(wb, sheet = "Indicator7GE", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_7, data, total, bg_improve, gap_reduced, gap_increased, equitable_increased, ge_increase_relative)

# Indicator 8 ---- 
print("===================================================================")
print("Indicator #8 analysis")

ind_8 <- df %>%
  filter(Indicator == 8 & Typeofinvestment == "MYRP" & !grepl("regional", Country, ignore.case = TRUE))

ind_8 <- ind_8 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()


print(paste0(nrow(ind_8), " rows for analysis."))
unique(ind_8$LeadGRN)

total <- ind_8 %>%
  filter(!is.na(Evidenceofchange) & Evidenceofchange != "Baseline") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN with evidence of change") %>%
  select(id, Total)
total

totals <- ind_8 %>%
  filter(NewCode == "PI" | NewCode == "SI") %>%
  group_by(NewCode) %>%
  summarise(Total = n()) %>%
  rename(id = NewCode)
totals

total_evidenceavailable <- ind_8 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN with evidence available") %>%
  select(id, Total)
total_evidenceavailable

data <- rbind(total, totals, total_evidenceavailable)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(PI = if("PI" %in% colnames(.)) PI else 0,
         SI = if("SI" %in% colnames(.)) SI else 0,
         `PI+SI` = PI + SI,
         PerPI = PI / `Total LeadGRN with evidence of change`,
         PerSI = SI / `Total LeadGRN with evidence of change`,
         `PerPI+SI` = `PI+SI` / `Total LeadGRN with evidence of change`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator8",
    values_to = "Number"
  )
data

addWorksheet(wb, "Indicator8")
writeData(wb, sheet = "Indicator8", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_8, total, totals, data)


# Indicator 11 ----
print("===================================================================")
print("Indicator #11 analysis")

ind_11 <- df %>%
  filter(Indicator == 11 & Typeofinvestment == "MYRP")

ind_11 <- ind_11 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_11), " rows for analysis."))
unique(ind_11$LeadGRN)

total <- ind_11 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (MYRP) with evidence available") %>%
  select(id, Total)
total

totals <- ind_11 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_11 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")

totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (MYRP) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (MYRP) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (MYRP) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator11",
    values_to = "Number"
  )
data

addWorksheet(wb, "Indicator11")
writeData(wb, sheet = "Indicator11", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(data, ind_11, total, totals, totals_numeric)

# Indicator 13 ----
print("===================================================================")
print("Indicator #13 analysis")

# MYRP - FER aggreagated
ind_13 <- df %>%
  filter(Indicator == 13)

ind_13 <- ind_13 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_13), " rows for analysis."))
unique(ind_13$LeadGRN)

total <- ind_13 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (MYRP|FER) with evidence available") %>%
  select(id, Total)
total

totals <- ind_13 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_13 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (MYRP|FER) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (MYRP|FER) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (MYRP|FER) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator13_MYRP|FER",
    values_to = "Number"
  )
data

myrp_fer <- data
rm(data, ind_13, total, totals, totals_numeric)

# MYRP
ind_13 <- df %>%
  filter(Indicator == 13 & Typeofinvestment == "MYRP")

ind_13 <- ind_13 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_13), " rows for analysis."))
unique(ind_13$LeadGRN)

total <- ind_13 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (MYRP) with evidence available") %>%
  select(id, Total)
total

totals <- ind_13 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals

totals_numeric <- ind_13 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (MYRP) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (MYRP) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (MYRP) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator13_MYRP",
    values_to = "Number"
  )
data

myrp <- data
rm(data, ind_13, total, totals, totals_numeric)

# FER
ind_13 <- df %>%
  filter(Indicator == 13 & Typeofinvestment == "FER")

ind_13 <- ind_13 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_13), " rows for analysis."))
unique(ind_13$LeadGRN)

total <- ind_13 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (FER) with evidence available") %>%
  select(id, Total)
total

totals <- ind_13 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_13 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (FER) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (FER) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (FER) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator13_FER",
    values_to = "Number"
  )
data

fer <- data

data <- cbind(myrp_fer, myrp, fer)
data

addWorksheet(wb, "Indicator13")
writeData(wb, sheet = "Indicator13", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(data, ind_13, total, totals, totals_numeric)

# Indicator 14 ----
print("===================================================================")
print("Indicator #14 analysis")

# MYRP - FER aggreagated
ind_14 <- df %>%
  filter(Indicator == 14)

ind_14 <- ind_14 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_14), " rows for analysis."))
unique(ind_14$LeadGRN)

total <- ind_14 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (MYRP|FER) with evidence available") %>%
  select(id, Total)
total

totals <- ind_14 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_14 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (MYRP|FER) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (MYRP|FER) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (MYRP|FER) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator14_MYRP|FER",
    values_to = "Number"
  )
data

myrp_fer <- data
rm(data, ind_14, total, totals, totals_numeric)

# MYRP
ind_14 <- df %>%
  filter(Indicator == 14 & Typeofinvestment == "MYRP")

ind_14 <- ind_14 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_14), " rows for analysis."))
unique(ind_14$LeadGRN)

total <- ind_14 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (MYRP) with evidence available") %>%
  select(id, Total)
total

totals <- ind_14 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_14 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (MYRP) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (MYRP) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (MYRP) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator14_MYRP",
    values_to = "Number"
  )
data

myrp <- data
rm(data, ind_14, total, totals, totals_numeric)

# FER
ind_14 <- df %>%
  filter(Indicator == 14 & Typeofinvestment == "FER")

ind_14 <- ind_14 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_14), " rows for analysis."))
unique(ind_14$LeadGRN)

total <- ind_14 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (FER) with evidence available") %>%
  select(id, Total)
total

totals <- ind_14 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_14 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (FER) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (FER) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (FER) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator14_FER",
    values_to = "Number"
  )
data

fer <- data

data <- cbind(myrp_fer, myrp, fer)
data

addWorksheet(wb, "Indicator14")
writeData(wb, sheet = "Indicator14", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(data, ind_14, total, totals, totals_numeric, myrp, fer, myrp_fer)


# Indicator 15A ----
print("===================================================================")
print("Indicator #15a analysis")

## MYRP + FER ----
df <- df %>%
  mutate(Startdate = as.Date(Startdate),
         Currentenddate = as.Date(Currentenddate))

# end in 2023 - two data points flag
ind_15end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) & Evidenceofchange == "Solid") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid != 0, 1, 0))

ind_15endtotal <- ind_15end %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs ending in 2023 with evidence on time (2 timepoints)")

ind_15ge_end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) & Evidenceofchange == "Solid") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15end <- ind_15end %>%
  left_join(ind_15ge_end, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "MYRP+FER") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)

# active in 2022 and 2023, but not ending in 2023
ind_15notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5)) %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid + Partial + Baseline != 0, 1, 0))

ind_15notendtotal <- ind_15notend %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs not ending in 2023 with evidence on time (1 timepoint)")

programs_total <- rbind(ind_15endtotal, ind_15notendtotal)

ind_15ge_notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5)) %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15notend <- ind_15notend %>%
  left_join(ind_15ge_notend, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "MYRP+FER") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)


denominator <- df %>%
  filter(year(Startdate) <= 2022 & year(Currentenddate) >= 2023) %>%
  summarise(NumberLeadGRNs = n_distinct(LeadGRN))
denominator

nominatorEnd <- ind_15end %>%
  group_by(Check) %>%
  summarise(EvidenceGenderFlag = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(EvidenceGenderFlag)
nominatorEnd

nominatorNotEnd <- ind_15notend %>%
  group_by(Check) %>%
  summarise(NotEndEvidenceGender = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(NotEndEvidenceGender)
nominatorNotEnd


ind_15_values <- cbind(denominator, nominatorEnd, nominatorNotEnd)

ind_15_values <- ind_15_values %>%
  mutate(Check = (EvidenceGenderFlag + NotEndEvidenceGender) / NumberLeadGRNs,
         Typeofinvestment = "MYRP+FER")

myrp_fer_values <- ind_15_values
# myrp_fer <- ind_15

rm(ind_15, ind_15_values, ind_15ge, denominator, nominator)


## MYRP ----
df <- df %>%
  mutate(Startdate = as.Date(Startdate),
         Currentenddate = as.Date(Currentenddate))

# end in 2023
ind_15end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) & Evidenceofchange == "Solid" &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid != 0, 1, 0))

ind_15endmyrp <- ind_15end %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs ending in 2023 with evidence on time (2 timepoints)")

ind_15ge_end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) & Evidenceofchange == "Solid" &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15end <- ind_15end %>%
  left_join(ind_15ge_end, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "MYRP") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)

# active in 2022 and 2023, but not ending in 2023
ind_15notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid + Partial + Baseline != 0, 1, 0))

ind_15notendmyrp <- ind_15notend %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs not ending in 2023 with evidence on time (1 timepoint)")

programs_myrp <- rbind(ind_15endmyrp, ind_15notendmyrp)

ind_15ge_notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15notend <- ind_15notend %>%
  left_join(ind_15ge_notend, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "MYRP") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)


denominator <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) >= 2023) &
           Typeofinvestment == "MYRP") %>%
  summarise(NumberLeadGRNs = n_distinct(LeadGRN))
denominator

nominatorEnd <- ind_15end %>%
  group_by(Check) %>%
  summarise(EvidenceGenderFlag = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(EvidenceGenderFlag)
nominatorEnd

nominatorNotEnd <- ind_15notend %>%
  group_by(Check) %>%
  summarise(NotEndEvidenceGender = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(NotEndEvidenceGender)
nominatorNotEnd


ind_15_values <- cbind(denominator, nominatorEnd, nominatorNotEnd)

ind_15_values <- ind_15_values %>%
  mutate(Check = (EvidenceGenderFlag + NotEndEvidenceGender) / NumberLeadGRNs,
         Typeofinvestment = "MYRP")

myrp_values <- ind_15_values
# myrp <- ind_15

rm(ind_15, ind_15_values, ind_15ge, denominator, nominator)


## FER ----
df <- df %>%
  mutate(Startdate = as.Date(Startdate),
         Currentenddate = as.Date(Currentenddate))

# end in 2023
ind_15end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) & Evidenceofchange == "Solid" &
           Typeofinvestment == "FER") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid != 0, 1, 0))

ind_15endfer <- ind_15end %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs ending in 2023 with evidence on time (2 timepoints)")

ind_15ge_end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) & Evidenceofchange == "Solid" &
           Typeofinvestment == "FER") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15end <- ind_15end %>%
  left_join(ind_15ge_end, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "FER") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)

# active in 2022 and 2023, but not ending in 2023
ind_15notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) &
           Typeofinvestment == "FER") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid + Partial + Baseline != 0, 1, 0))

ind_15notendfer <- ind_15notend %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs not ending in 2023 with evidence on time (1 timepoint)")

programs_fer <- rbind(ind_15endfer, ind_15notendfer)

ind_15ge_notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 3 | Indicator == 4 | Indicator == 5) &
           Typeofinvestment == "FER") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15notend <- ind_15notend %>%
  left_join(ind_15ge_notend, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "FER") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)


denominator <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) >= 2023) &
           Typeofinvestment == "FER") %>%
  summarise(NumberLeadGRNs = n_distinct(LeadGRN))
denominator

nominatorEnd <- ind_15end %>%
  group_by(Check) %>%
  summarise(EvidenceGenderFlag = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(EvidenceGenderFlag)
nominatorEnd

nominatorNotEnd <- ind_15notend %>%
  group_by(Check) %>%
  summarise(NotEndEvidenceGender = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(NotEndEvidenceGender)
nominatorNotEnd


ind_15_values <- cbind(denominator, nominatorEnd, nominatorNotEnd)

ind_15_values <- ind_15_values %>%
  mutate(Check = (EvidenceGenderFlag + NotEndEvidenceGender) / NumberLeadGRNs,
         Typeofinvestment = "FER")

fer_values <- ind_15_values
# fer <- ind_15

## Results aggregation ----

ind_15_values <- rbind(myrp_values, fer_values, myrp_fer_values)

ind_15_values <- ind_15_values %>%
  select(Typeofinvestment, NumberLeadGRNs, EvidenceGenderFlag, NotEndEvidenceGender, Check)
ind_15_values

# preparing output
ind_15_values <- ind_15_values %>%
  pivot_longer(cols = -Typeofinvestment, names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Value) %>%
  mutate(id = case_when
         (Measure == "NumberLeadGRNs"  ~ "# programs active in 2022-2023",
           Measure == "EvidenceGenderFlag" ~ "# programs ending in 2023 with Evidence and Gender datapoints",
           Measure == "NotEndEvidenceGender" ~ "# programs not ending in 2023 with Evidence and Gender datapoints",
           Measure == "Check" ~ "% programs with evidence on time over number of active programs",
           TRUE ~ NA_character_)) %>%
  select(id,  `MYRP+FER`, MYRP, FER)

programs_total <- programs_total %>%
  mutate(Typeofinvestment = "MYRP+FER")
programs_myrp <- programs_myrp %>%
  mutate(Typeofinvestment = "MYRP")
programs_fer <- programs_fer %>%
  mutate(Typeofinvestment = "FER")

programs_total <- rbind(programs_total, programs_myrp, programs_fer)

programs_total <- programs_total %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Total)


ind_15_values <- rbind(ind_15_values, programs_total)

addWorksheet(wb, "Indicator15a")
writeData(wb, sheet = "Indicator15a", ind_15_values, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

# ind_15 <- rbind(myrp, fer, myrp_fer)

# ind_15 <- ind_15 %>%
# select(Typeofinvestment, LeadGRN, Evidence, GenderFlag, Check)


# addWorksheet(wb, "Indicator15aLeadGRN")
# writeData(wb, sheet = "Indicator15aLeadGRN", ind_15, colNames = TRUE, rowNames = FALSE)
# file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
# saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_15, ind_15_values, ind_15ge, denominator, nominator)

# Indicator 15B ---- 
print("===================================================================")
print("Indicator #15b analysis") 

## MYRP ----
df <- df %>%
  mutate(Startdate = as.Date(Startdate),
         Currentenddate = as.Date(Currentenddate))

# end in 2023
ind_15end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 6 | Indicator == 7) & Evidenceofchange == "Solid" &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid != 0, 1, 0))

ind_15endmyrp <- ind_15end %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs ending in 2023 with evidence on time (2 timepoints)")

ind_15ge_end <- df %>%
  mutate(Evidenceofchange = case_when(
    `Evidenceformulatedtoassesschange.Supervisors.` == "Yes" ~ "Solid",
    TRUE ~ Evidenceofchange
  )) %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) == 2023) &
           (Indicator == 6 | Indicator == 7) & Evidenceofchange == "Solid" &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15end <- ind_15end %>%
  left_join(ind_15ge_end, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "MYRP") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)

# active in 2022 and 2023, but not ending in 2023
ind_15notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 6 | Indicator == 7) &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Evidenceofchange) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Evidenceofchange, values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `Partial` = ifelse(!"Partial" %in% colnames(.), 0, `Partial`),
    `Solid` = ifelse(!"Solid" %in% colnames(.), 0, `Solid`),
    `Baseline` = ifelse(!"Baseline" %in% colnames(.), 0, `Baseline`),
    Evidence = ifelse(Solid + Partial + Baseline != 0, 1, 0))

ind_15notendmyrp <- ind_15notend %>%
  filter(Evidence == 1) %>%
  summarise(Count = n()) %>%
  summarise(Total = sum(Count)) %>%
  mutate(id = "# of programs not ending in 2023 with evidence on time (1 timepoint)")


ind_15ge_notend <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) > 2023) &
           (Indicator == 6 | Indicator == 7) &
           Typeofinvestment == "MYRP") %>%
  group_by(LeadGRN, Gequity1, Gequity2) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c(Gequity1, Gequity2), values_from = Count) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    `0_0` = ifelse(!"0_0" %in% colnames(.), 0, `0_0`),
    `0_1` = ifelse(!"0_1" %in% colnames(.), 0, `0_1`),
    `1_0` = ifelse(!"1_0" %in% colnames(.), 0, `1_0`),
    `1_1` = ifelse(!"1_1" %in% colnames(.), 0, `1_1`),
    GenderFlag = ifelse(`0_1` + `1_0` + `1_1` != 0, 1, 0))

ind_15notend <- ind_15notend %>%
  left_join(ind_15ge_notend, by = "LeadGRN") %>%
  mutate(Check = ifelse(Evidence + GenderFlag == 2, 1, 0),
         Typeofinvestment = "MYRP") %>%
  select(Typeofinvestment, LeadGRN, GenderFlag, Check)


denominator <- df %>%
  filter((year(Startdate) <= 2022 & year(Currentenddate) >= 2023) &
           Typeofinvestment == "MYRP") %>%
  summarise(NumberLeadGRNs = n_distinct(LeadGRN))
denominator

nominatorEnd <- ind_15end %>%
  group_by(Check) %>%
  summarise(EvidenceGenderFlag = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(EvidenceGenderFlag)
nominatorEnd

nominatorNotEnd <- ind_15notend %>%
  group_by(Check) %>%
  summarise(NotEndEvidenceGender = sum(Check, na.rm = TRUE)) %>%
  filter(Check == 1) %>%
  select(NotEndEvidenceGender)
nominatorNotEnd


ind_15_values <- cbind(denominator, nominatorEnd, nominatorNotEnd)

ind_15_values <- ind_15_values %>%
  mutate(Check = (EvidenceGenderFlag + NotEndEvidenceGender) / NumberLeadGRNs,
         Typeofinvestment = "MYRP")

# myrp <- ind_15
ind_15_values <- ind_15_values %>%
  pivot_longer(cols = -Typeofinvestment, names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Value) %>%
  mutate(id = case_when
         (Measure == "NumberLeadGRNs"  ~ "# programs active in 2022-2023",
           Measure == "EvidenceGenderFlag" ~ "# programs ending in 2023 with Evidence and Gender datapoints",
           Measure == "NotEndEvidenceGender" ~ "# programs not ending in 2023 with Evidence and Gender datapoints",
           Measure == "Check" ~ "% programs with evidence on time over number of active programs",
           TRUE ~ NA_character_)) %>%
  select(id, MYRP)
ind_15_values

program_total <- rbind(ind_15endmyrp, ind_15notendmyrp) 

program_total <- program_total %>%
  rename(MYRP = Total)

ind_15_values <- rbind(ind_15_values, program_total)


addWorksheet(wb, "Indicator15b")
writeData(wb, sheet = "Indicator15b", ind_15_values, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)


# addWorksheet(wb, "Indicator15bLeadGRN")
# writeData(wb, sheet = "Indicator15bLeadGRN", ind_15, colNames = TRUE, rowNames = FALSE)
# file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
# saveWorkbook(wb, file_path, overwrite = TRUE)

rm(ind_15, ind_15_values, ind_15ge, denominator, nominator)


# Indicator 17 ---- 
print("===================================================================")
print("Indicator #17 analysis")

ind_17 <- df %>%
  filter(Indicator == 17 & Typeofinvestment == "MYRP")

ind_17 <- ind_17 %>%
  group_by(LeadGRN) %>%
  group_modify(~ collapse_rows(.x)) %>%
  ungroup()

print(paste0(nrow(ind_17), " rows for analysis."))
unique(ind_17$LeadGRN)

total <- ind_17 %>%
  filter(Evidenceavailable == "Yes") %>%
  summarise(Total = n()) %>%
  mutate(id = "Total LeadGRN (MYRP) with evidence available") %>%
  select(id, Total)
total

totals <- ind_17 %>%
  filter(Typeofchange == "A moderate extent" | Typeofchange == "A significant extent") %>%
  group_by(Typeofchange) %>%
  summarise(Total = n()) %>%
  rename(id = Typeofchange)
totals


totals_numeric <- ind_17 %>%
  filter(TypeofchangeNumeric_sys == 0 |
           TypeofchangeNumeric_sys == 1 |
           TypeofchangeNumeric_sys == 2 | 
           TypeofchangeNumeric_sys == 3) %>%
  group_by(TypeofchangeNumeric_sys) %>%
  summarise(Total = n()) %>%
  rename(id = TypeofchangeNumeric_sys) %>%
  mutate(id = case_when(
    id == 0 ~ "NoneCount",
    id == 1 ~ "SmallCount",
    id == 2 ~ "ModerateCount",
    id == 3 ~ "SignificantCount"
  )) %>%
  pivot_wider(names_from = id, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(
    NoneCount = ifelse(!"NoneCount" %in% colnames(.), 0, NoneCount),
    SmallCount = ifelse(!"SmallCount" %in% colnames(.), 0, SmallCount),
    ModerateCount = ifelse(!"ModerateCount" %in% colnames(.), 0, ModerateCount),
    SignificantCount = ifelse(!"SignificantCount" %in% colnames(.), 0, SignificantCount)
  ) %>%
  pivot_longer(cols = c(NoneCount, SmallCount, ModerateCount, SignificantCount), 
               names_to = "id", 
               values_to = "Total")
totals_numeric


data <- rbind(total, totals, totals_numeric)
data

data <- data %>%
  pivot_wider(names_from = id, values_from = Total) %>%
  mutate(Moderate = if("A moderate extent" %in% colnames(.)) `A moderate extent` else 0,
         Significant  = if("A significant extent" %in% colnames(.)) `A significant extent` else 0,
         `Moderate+Significant` = Moderate + Significant,
         PerModerate  = Moderate / `Total LeadGRN (MYRP) with evidence available`,
         PerSignificant = Significant / `Total LeadGRN (MYRP) with evidence available`,
         `PerModerate+Significant` =   `Moderate+Significant` / `Total LeadGRN (MYRP) with evidence available`,
         `Threshold1(SM_SG)` = SignificantCount / (SmallCount + ModerateCount + SignificantCount),
         `Threshold2(NO_SG)` = SignificantCount / (NoneCount + SmallCount + ModerateCount + SignificantCount)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator17",
    values_to = "Number"
  )
data

addWorksheet(wb, "Indicator17")
writeData(wb, sheet = "Indicator17", data, colNames = TRUE, rowNames = FALSE)
file_path <- paste0("output data/analysis/", date_string, " AoE Analysis - Quantitative.xlsx")
saveWorkbook(wb, file_path, overwrite = TRUE)

rm(data, ind_17, total, totals, totals_numeric)

rm(df)
print("End of script.")
