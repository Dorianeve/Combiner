df %<>%
  filter(!grepl("other", Attribute) &
           !grepl("disabilities", Attribute))

library(writexl)

write_xlsx(df, "carly.xlsx")

df %<>%
  filter(grepl("ARR24", Exercise))

df %<>%
  filter(AnalysisCode != "Enter code" & AnalysisCode != "enter code")
