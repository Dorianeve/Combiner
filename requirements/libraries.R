# libraries required for the ARR pipeline

if(!require(httr)){
  install.packages('httr')
  library(httr)
}

if(!require(openxlsx)){
  install.packages('openxlsx')
  library(openxlsx)
}

if(!require(openxlsx2)){
  install.packages('openxlsx2')
  library(openxlsx)
}

if(!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(ggplot2))  {
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

if(!require(magrittr)) {
  install.packages(("magrittr"))
  library(magrittr)
}

if(!require(lubridate)) {
  install.packages(("lubridate"))
  library(lubridate)
}

if(!require(janitor)) {
  install.packages(("janitor"))
  library(janitor)
}