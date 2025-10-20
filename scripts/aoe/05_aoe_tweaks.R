  rm(list = ls())
  library(openxlsx)
  library(janitor)
  library(magrittr)
  library(openxlsx2)
  library(tidyverse)
  library(lubridate)
  
  df <- read.csv("data/arr/aoe/aoe_kobo_clean.csv", encoding = "UTF-8")
  grants_db <- read.csv("data/arr/aoe/aoe_grants_db.csv", encoding = "UTF-8")
  # old_df <- read.csv("data/arr/aoe/aoe_2023.csv", encoding = "UTF-8")
  
  # CURRENT AOE ----
  # Merging GMS ----
  grants_db %<>%
    filter(Reportingrole != "Non-lead") %>%
    rename(LeadGRN = GMGRN) %>%
    select(ProgrammeID,
           LeadGRN, 
           Granteeorganization,
           Startdate,
           Enddate,
           Currentenddate,
           Crisis_Protracted,
           Crisis_New_sudden_onset,
           Crisis_Escalation,
           Crisis_New_displacement,
           Crisis_Anticipatory_action,
           Emergency_Conflict,
           Emergency_Natural_disaster,
           Emergency_Climate,
           Emergency_Public_health,
           Emergency_Economic,
           Emergency_Other,
           Activein2023,
           Activein2024)
    
  
  df %<>%
    left_join(grants_db, by = c("pid" = "ProgrammeID"))
  
  # Variables creation ----
  ## Objective ----
  df %<>%
    mutate(Objective = case_when(
        indicator %in% c(3, 4, 5) ~ "1|Participation, retention, completion",
        indicator %in% c(6, 7, 8) ~ "2|Learning outcomes",
        indicator %in% c(11, 13, 14, 17) ~ "4|National systems at the nexus",
        TRUE ~ NA_character_
      )
    )
  
  ## ReportYear ----
  df %<>%
    mutate(ReportYear = 2024)
  
  ## Evidenceofchange ----
  df %<>%
    mutate(
      Evidenceofchange = case_when(
        indicator %in% 3:8 & tolower(evchange) == "yes" & tolower(evstrength) == "weak" ~ "Weak",
        indicator %in% 3:8 & tolower(evchange) == "yes" & tolower(evstrength) == "partial" ~ "Partial",
        indicator %in% 3:8 & tolower(evchange) == "yes" & tolower(evstrength) == "strong" ~ "Strong",
        indicator %in% 3:8 & tolower(evchange) == "no" ~ "Baseline",
        TRUE ~ NA_character_
      )
    )
  
  
  ## NewCode ----
  df %<>%
    mutate(
      NewCode = case_when(
        tolower(trimws(evcalc)) == "baseline" ~ "B",
        tolower(trimws(evcalc)) == "solid improvement" ~ "SI",
        tolower(trimws(evcalc)) == "partial improvement" ~ "P",
        tolower(trimws(evcalc)) == "solid stable" ~ "SS",
        tolower(trimws(evcalc)) == "partial stable" ~ "PS",
        tolower(trimws(evcalc)) == "solid decline" ~ "SD",
        tolower(trimws(evcalc)) == "partial decline" ~ "PD",
        tolower(trimws(evcalc)) == "weak improvement" ~ "WI",
        tolower(trimws(evcalc)) == "weak stable" ~ "WS",
        tolower(trimws(evcalc)) == "weak decline" ~ "WD",
        tolower(trimws(evcalc)) == "none" ~ "N",
        tolower(trimws(evcalc)) == "a small extent" ~ "SE",
        tolower(trimws(evcalc)) == "a moderate extent" ~ "ME",
        tolower(trimws(evcalc)) == "a significant extent" ~ "SGE",
        TRUE ~ NA_character_
      )
    )
    
  ## EvidenceavailableNumeric ----
  df %<>%
    mutate(
      EvidenceavailableNumeric = case_when(
        tolower(trimws(evavail)) == "yes" ~ 1,
        tolower(trimws(evavail)) == "no" ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  ## EvidenceofchangeNumeric ----
  df %<>%
    mutate(
      EvidenceofchangeNumeric = case_when(
        tolower(trimws(evchange)) == "yes" & tolower(trimws(evstrength)) == "weak" ~ 1,
        tolower(trimws(evchange)) == "yes" & tolower(trimws(evstrength)) == "partial" ~ 2,
        tolower(trimws(evchange)) == "yes" & tolower(trimws(evstrength)) == "solid" ~ 3,
        tolower(trimws(evchange)) == "no" ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  ## TypeofchangeNumeric_child ----
  df %<>%
    mutate(
      TypeofchangeNumeric_child = case_when(
        indicator %in% 3:8 & tolower(trimws(evchange)) == "yes" & grepl("improvement", tolower(evcalc)) ~ 1,
        indicator %in% 3:8 & tolower(trimws(evchange)) == "yes" & grepl("stable", tolower(evcalc)) ~ 0,
        indicator %in% 3:8 & tolower(trimws(evchange)) == "yes" & grepl("decline", tolower(evcalc)) ~ -1,
        TRUE ~ NA_real_
      )
    )
  
  ## TypeofchangeNumeric_sys ----
  df %<>%
    mutate(
      TypeofchangeNumeric_sys = case_when(
        indicator %in% c(11, 13, 14, 17) & tolower(trimws(evavail)) == "yes" & grepl("none", tolower(evcalc)) ~ 0,
        indicator %in% c(11, 13, 14, 17) & tolower(trimws(evavail)) == "yes" & grepl("small extent", tolower(evcalc)) ~ 1,
        indicator %in% c(11, 13, 14, 17) & tolower(trimws(evavail)) == "yes" & grepl("moderate extent", tolower(evcalc)) ~ 2,
        indicator %in% c(11, 13, 14, 17) & tolower(trimws(evavail)) == "yes" & grepl("significant extent", tolower(evcalc)) ~ 3,
        TRUE ~ NA_real_
      )
    )
  
  ## CodeNumeric_child ----
  df %<>%
    mutate(
      codeNumeric_child = case_when(
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "solid improvement" ~ 1,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "partial improvement" ~ 2,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "solid stable" ~ 3,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "partial stable" ~ 4,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "solid decline" ~ 5,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "partial decline" ~ 6,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "baseline" ~ 7,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "weak improvement" ~ 8,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "weak stable" ~ 9,
        indicator %in% 3:8 & tolower(trimws(evcalc)) == "weak decline" ~ 11,
        TRUE ~ NA_real_
      )
    )
  
  # Drops ----
  df %<>%
    select(-c(active23_man,
              sub_objective_prefix,
              calc2,
              calc3,
              calc4,
              group))
  
  # ID ----
  df %<>%
    mutate(
      matchlab_repl = if_else(
        is.na(matchlab),
        as.character(2999 + cumsum(is.na(matchlab))),
        as.character(matchlab)
      ),
      UID = paste0(LeadGRN, matchlab_repl, ReportYear)
    ) %>%
    select(-matchlab_repl)
  
  
  # PREVIOUS AOE ----
  old_df <- read.xlsx("data/arr/aoe/aoe_2023.xlsx", sheet = "AoE Template 2024", 
                  detectDates = TRUE, sep.names = " ")
  colnames(old_df) <- gsub("\\s+", "", colnames(old_df))
  names(old_df)
  
  # Fix the Grantee to avoid NAs
  old_df %<>%
    mutate(GMGRN = ifelse(is.na(GMGRN), LeadgranteeGRN, GMGRN)) %>%
    select(-c(LeadgranteeGRN))
  
  # Variable creation ----
  ## indicator ----
  subtext_lookup <- tribble(~sub_objective, ~indicator_code,
                            "0.1|Girls inclusion", NA,
                            "0.2|Violence", NA,
                            "0.3|MHPSS", NA, 
                            "0.4|WASH", NA,
                            "0.5|Disaster risk reduction", NA,
                            "1.1|EA-Enroll", 3,   
                            "1.2|EA-Attendance", 3, 
                            "1.3|RPC-Retention", 4, 
                            "1.4|RPC-Progression", 4,
                            "1.5|RPC-Completion", 4,
                            "1.6|TR-NFE", 5,
                            "2.1|Academic", 6,
                            "2.2|SEL", 7, 
                            "2.3|Holistic", NA,
                            "2.4|Gender outcomes", 8, 
                            "2.5|Not specified", NA,
                            "2.6|Other", NA,
                            "3.1|Teacher performance and well-being", NA,
                            "3.2|School and classroom plans, management, and quality", NA,
                            "3.3|National and sub-national policies, frameworks, and systems", NA,
                            "3.4|Other", NA,
                            "4.1|Localization", 13,
                            "4.2|Coordination", 11,
                            "4.3|Data and evidence systems", 17,
                            "4.4|Accountability", 14,
                            "5.1|Adoption", NA,
                            "5.2|Coordination", NA,
                            "5.3|Knowledge products", NA)
  old_df <- old_df %>%
    left_join(subtext_lookup, by = c("Sub-objective" = "sub_objective")) %>%
    rename(Indicator = indicator_code)
  
  ## sub_indicator ----
  # lookup table
  subobjective_lookup <- tribble(
    ~prefix,         ~sub_text,                                     
    "1.1|EA-Enroll", "1.1 | Enrollment",                            
    "1.2|EA-Attendance", "1.2 | Attendance",                        
    "1.3|RPC-Retention", "1.3 | Retention",                       
    "1.4|RPC-Progression", "1.4 | Progression",                    
    "1.5|RPC-Completion", "1.5 | Completion",                      
    "1.6|TR-NFE",    "1.6 | Transition",                             
    "2.1|Academic",       "2.1 | Academic",                            
    "2.2|SEL",       "2.2 | Social and emotional",
    "2.3|Holistic",        "2.3 | Household outcomes",                    
    "2.4|Gender outcomes",        "2.4 | Gender outcomes",                      
    "2.5|Not specified",        "2.5 | Other children’s outcomes",            
    "2.6|Other",       "2.6 | Other outcomes",                         
    "3.1|Teacher performance and well-being",        "3.1 | Teacher outcomes",                       
    "3.2|School and classroom plans, management, and quality",        "3.2 | School and classroom outcomes",          
    "3.3|National and sub-national policies, frameworks, and systems",        "3.3 | National policy and systems",          
    "3.4|Other",       "3.4 | Other opportunities and services",       
    "4.1|Localization",       "4.1 | Localization",   
    "4.2|Coordination",       "4.2 | Coordination - Programmatic", 
    "4.3|Data and evidence systems",       "4.3 | Data - Programmatic - outcomes", 
    "4.4|Accountability",       "4.4 | Accountability", 
    "5.1|Adoption",      "5.1 | Adaptability",    
    "5.2|Coordination",       "5.2 | Coordination",      
    "5.3|Knowledge products",       "5.3 | Knowledge",  
  )
  
  old_df <- old_df %>%
    left_join(subobjective_lookup, by = c("Sub-objective" = "prefix")) %>%
    rename(sub_objective = sub_text) %>%
    mutate(sub_objective = ifelse(is.na(sub_objective), `Sub-objective`, sub_objective)) %>%
    select(-c(`Sub-objective`,`Sub-objectiveold`, `Sub-objectiveshort`)) %>%
    rename(`Sub-objective` = sub_objective)
  
  
  
  ## Typeof change ----
  old_df %<>%
    mutate(
      Typeofchange = case_when(
        Indicator %in% 3:8 &
          Evidenceofchange %in% c("Weak", "Partial", "Strong") &
          Typeofchange == "Increase" ~ "Improvement",
        
        Indicator %in% 3:8 &
          Evidenceofchange %in% c("Weak", "Partial", "Strong") &
          Typeofchange == "Decrease" ~ "Decline",
        
        TRUE ~ Typeofchange  # leave unchanged otherwise
      )
    )
  
  ## disag -----
  old_df %<>%
    mutate(
      # Create base disag by pasting SP1, SP2, SP3 (ignore if NA)
      disag = paste(SP1, SP2, SP3, sep = "|"),
      
      # Append 'sex' if any gender-disaggregated column is not NA
      disag = if_else(
        !is.na(Girlsbaseline) | !is.na(Girlsendline) |
          !is.na(Boysbaseline) | !is.na(Boysendline),
        paste0(disag, "|sex"),
        disag
      )
    )
  
  ## repeat index -----
  old_df %<>%
    group_by(GMGRN, `Sub-objective`) %>%
    mutate(row_num = row_number()) %>%
    ungroup()
  
  
  ## evcalc ----
  old_df %<>%
    mutate(
      evcalc = case_when(
        NewCode == "B"   ~ "Baseline",
        NewCode == "SI"  ~ "Solid improvement",
        NewCode == "PI"  ~ "Partial improvement",
        NewCode == "SS"  ~ "Solid stable",
        NewCode == "PS"  ~ "Partial stable",
        NewCode == "SD"  ~ "Solid decline",
        NewCode == "PD"  ~ "Partial decline",
        NewCode == "WI"  ~ "Weak improvement",
        NewCode == "WS"  ~ "Weak stable",
        NewCode == "WD"  ~ "Weak decline",
        NewCode == "N"   ~ "None",
        NewCode == "SE"  ~ "A small extent",
        NewCode == "ME"  ~ "A moderate extent",
        NewCode == "SGE" ~ "A significant extent",
        TRUE ~ NA_character_
      )
  )
  
  ## Setting ----
  old_df %<>%
    mutate(
      setting = case_when(
        Indicator %in% 3:8 & grepl("Formal education", Population, ignore.case = TRUE) ~ 
          "Formal school settings aimed at improving learning outcomes",
        Indicator %in% 3:8 & grepl("Non-formal", Population, ignore.case = TRUE) ~ 
          "Non-formal school settings aimed at improving learning outcomes",
        Indicator %in% 3:8 & grepl("Children - Other", Population, ignore.case = TRUE) ~ 
          "Other",
        Indicator %in% 3:8 & grepl("Children - Unclear", Population, ignore.case = TRUE) ~ 
          "Unclear/not specified",
        TRUE ~ NA_character_
      )
    )
  
  
  ## pop ----
  
  old_df %<>%
    mutate(
      pop = case_when(
        Indicator %in% 3:8 & grepl("Out of school", Population, ignore.case = TRUE) ~ 
          "Out-of-school children",
        TRUE ~ NA_character_
      )
    )
  
  ## EvidenceavailableNumeric ----
  old_df %<>%
    mutate(EvidenceavailableNumeric = ifelse(Evidenceavailable == "Yes", 1, 
                                             ifelse(Evidenceavailable == "No", 0, NA)))
  
  
  ## EvidenceofchangeNumeric ----
  old_df %<>%
    mutate(Evidenceofchange = ifelse(Evidenceavailable == "No", NA, Evidenceofchange),
           EvidenceofchangeNumeric = ifelse(Evidenceofchange == "Baseline", 0, 
                                            ifelse(Evidenceofchange == "Weak", 1,
                                                   ifelse(Evidenceofchange == "Partial", 2,
                                                          ifelse(Evidenceofchange == "Solid", 3, NA)))))
  
  
  
  ## TypeofchangeNumeric + child/sys-----
  old_df %<>%
    mutate(Typeofchange = ifelse(Evidenceavailable == "No", NA, Typeofchange),
           Typeofchange = ifelse(Evidenceofchange == "Baseline" &
                                   (Objective == "1|Participation, retention, completion" |
                                      Objective == "2|Learning outcomes" |
                                      Objective == "3|Opportunities and services"), NA, Typeofchange),
           TypeofchangeNumeric_child = ifelse(((Objective == "1|Participation, retention, completion" |
                                                  Objective == "2|Learning outcomes" |
                                                  Objective == "3|Opportunities and services") &
                                                 Evidenceofchange == "Weak"), -1,
                                              ifelse(((Objective == "1|Participation, retention, completion" |
                                                         Objective == "2|Learning outcomes" |
                                                         Objective == "3|Opportunities and services") & Evidenceofchange == "Partial"), 0,
                                                     ifelse(((Objective == "1|Participation, retention, completion" |
                                                                Objective == "2|Learning outcomes" |
                                                                Objective == "3|Opportunities and services") & Evidenceofchange == "Solid"), 1, NA))),
           TypeofchangeNumeric_sys = ifelse(((Objective == "4|National systems at the nexus" |
                                                Objective == "5|Global systems at the nexus") &
                                               Evidenceavailable == "Yes" & Typeofchange == "None"), 0,
                                            ifelse(((Objective == "4|National systems at the nexus" |
                                                       Objective == "5|Global systems at the nexus") &
                                                      Evidenceavailable == "Yes" & Typeofchange == "A small extent"), 1,
                                                   ifelse(((Objective == "4|National systems at the nexus" |
                                                              Objective == "5|Global systems at the nexus") &
                                                             Evidenceavailable == "Yes" & Typeofchange == "A moderate extent"), 2,
                                                          ifelse(((Objective == "4|National systems at the nexus" |
                                                                     Objective == "5|Global systems at the nexus") &
                                                                    Evidenceavailable == "Yes" & Typeofchange == "A significant extent"), 3, NA)))))
  
  
  ## codeNumeric_child ----
  old_df %<>%
    mutate(codeNumeric_child = case_when(
    NewCode == "SI" ~ 1,
    NewCode == "PI" ~ 2,
    NewCode == "SS" ~ 3,
    NewCode == "PS" ~ 4,
    NewCode == "SD" ~ 5,
    NewCode == "PD" ~ 6,
    NewCode == "B" ~ 7,
    NewCode == "Y" ~ 8,
    NewCode == "WI" ~ 8,
    NewCode == "WS" ~ 9,
    NewCode == "WD" ~ 11,
    TRUE ~ NA_real_  # Optional: Assign NA for any unmatched codes
  ))
  
  # Drops ----
  old_df %<>%
    select(-c(Activeprogramsin2023,
              GroupingLeadgranteeGRN,
              SP1,
              SP1baseline,
              SP1endline,
              SP2baseline,
              SP2endline,
              SP2,
              SP3,
              SP3baseline,
              SP3endline,
              starts_with("Crisis-"),
              starts_with("Emergency-")))
  
  ## GMS Merge ----
  old_df %<>%
    rename(LeadGRN = GMGRN) %>%
    select(-c(Granteeorganization)) %>%
    left_join(grants_db, by = "LeadGRN") 
  
  # ID ----
  old_df %<>%
    mutate(UID = paste0(UID, ReportYear))
  
  
  # //////// --------------
  
  # Hard renaming of old_df ----
  old_df %<>%
    rename(text = Notes1)
  
  # Consolidation headers new df to old standards for analysis code ----
  names <- read.csv("data/arr/aoe/col_names.csv", encoding = "UTF-8")
  
  names %<>%
    mutate(Match = ifelse(Direct.match.varname == "", NA, Direct.match.varname)) %>%
    filter(!is.na(Match)) %>%
    select(Varname, Match)
  
  # Create named vector for renaming
  rename_vector <- setNames(names$Match, names$Varname)
  
  # Rename columns where names(df) match Varname
  df %<>%
    rename_with(~ rename_vector[.x], .cols = names(rename_vector))          
  
  # Coerce all columns to character
  df %<>% mutate(across(everything(), as.character))
  old_df %<>% mutate(across(everything(), as.character))
  
  # Bind the rows
  aoe <- dplyr::bind_rows(df, old_df)
  
  # Assignment of counting variable (evnumb) -----
  ## evnumb_indicator -----
  aoe %<>%
    group_by(LeadGRN, Indicator, ReportYear) %>%
    mutate(
      evnumb_indicator = sum(tolower(trimws(Evidenceavailable)) == "yes", na.rm = TRUE)
    ) %>%
    ungroup()
  
  ## evnumb_subobjective ----
  aoe %<>%
    group_by(LeadGRN, Indicator, `Sub-objective`, ReportYear) %>%
    mutate(
      evnumb_subobjective = sum(tolower(trimws(Evidenceavailable)) == "yes", na.rm = TRUE)
    ) %>%
    ungroup()
  
  ## evnumb_indicator_cum cumulative-----
  aoe %<>%
    group_by(LeadGRN, Indicator) %>%
    mutate(
      evnumb_indicator_cum = sum(tolower(trimws(Evidenceavailable)) == "yes", na.rm = TRUE)
    ) %>%
    ungroup()
  
  ## evnumb_subobjective_cum cumulative ----
  aoe %<>%
    group_by(LeadGRN, Indicator, `Sub-objective`) %>%
    mutate(
      evnumb_subobjective_cum = sum(tolower(trimws(Evidenceavailable)) == "yes", na.rm = TRUE)
    ) %>%
    ungroup()

  ## INCLUDE ----
  aoe %<>%
    mutate(Include = ifelse(
      evnumb_indicator_cum == 1, "Primary", NA))

  ## Export ----
  export_include <- aoe %>%
    filter(evnumb_indicator_cum > 1 & Indicator == 3:8)
  
  export_include %<>%
    select(UID, validate, ProgrammeID, LeadGRN, repeatindex, MnEfocalpoint,
           Country, Typeofinvestment, Indicator,
           `Sub-objective`, Evidenceavailable, evcalc,
           text,
           Overallbaseline, Overallendline,
           Girlsbaseline, Girlsendline,
           Boysbaseline, Boysendline,
           match1, matchlab, setting, pop,
           Levelofrepresentation, method_rating_gen, method_rating_change,
           `Include?(Supervisors)`, Include) %>%
    arrange(ProgrammeID, Indicator, `Sub-objective`)
  
  write.csv(export_include, "data/arr/aoe/utility_table/aoe_primary_tag.csv", row.names = FALSE)
  
  # Gequity (custom flag) -----
  aoe %<>%
    rowwise() %>%
    mutate(
      # Flags for baseline data for both girls and boys
      Gequity1 = ifelse(!is.na(Girlsbaseline) & !is.na(Boysbaseline), 1, 0),
      # Flags for endline data for both girls and boys
      Gequity2 = ifelse(!is.na(Girlsendline) & !is.na(Boysendline), 1, 0),
      # Flags for both baseline and endline data for both girls and boys
      Gequity3 = ifelse(!is.na(Girlsbaseline) & !is.na(Boysbaseline) & !is.na(Girlsendline) & !is.na(Boysendline), 1, 0),
      # Flags for baseline data for girls
      Gequity4 = ifelse(!is.na(Girlsbaseline), 1, 0),
      # Flags for endline data for girls
      Gequity5 = ifelse(!is.na(Girlsendline), 1, 0),
      # Flags for both baseline and endline data for girls
      Gequity6 = ifelse(!is.na(Girlsbaseline) & !is.na(Girlsendline), 1, 0),
      CustomFlag = ifelse((Objective %in% c("1|Participation, retention, completion", "2|Learning outcomes") | 
                             `Sub-objective` == "3.1|Teacher performance and well-being") &
                            Evidenceavailable == "Yes", 1, 0)
    ) %>%
    ungroup()
  
  aoe %<>%
    mutate(Gequity1 = ifelse(CustomFlag == 0, NA, Gequity1),
           Gequity2 = ifelse(CustomFlag == 0, NA, Gequity2),
           Gequity3 = ifelse(CustomFlag == 0, NA, Gequity3),
           Gequity4 = ifelse(CustomFlag == 0, NA, Gequity4),
           Gequity5 = ifelse(CustomFlag == 0, NA, Gequity5),
           Gequity6 = ifelse(CustomFlag == 0, NA, Gequity6)) %>%
    select(-CustomFlag)
  
## Duration
  aoe %<>%
    mutate(
      Startdate = ymd(Startdate),  # Convert Start_Date to Date type
      Currentenddate = ymd(Currentenddate),      # Convert End_Date to Date type
      Duration = as.numeric(difftime(ymd("2024-12-31"), Startdate, units = "days")),  # Calculate duration in days
      Less_than_6_months = ifelse(Duration < 180, 1, 0),  # Flag for less than 6 months
      Between_6_months_and_1_year = ifelse(Duration >= 180 & Duration < 365, 1, 0),  # Flag for 6 months to 1 year
      More_than_3_years = ifelse(Duration > 1095, 1, 0),  # Flag for more than 3 years
      End_Date_Flag2024 = ifelse(Currentenddate >= ymd("2024-01-01") & Currentenddate <= ymd("2024-12-31"), 1, 0),  # Flag for end dates in 2024
      End_Date_Flag2023 = ifelse(Currentenddate >= ymd("2023-01-01") & Currentenddate <= ymd("2023-12-31"), 1, 0)  # Flag for end dates in 2024
    )
  
write.csv(aoe, "data/arr/aoe/aoe_cleaned.csv", row.names = FALSE)
  
  
