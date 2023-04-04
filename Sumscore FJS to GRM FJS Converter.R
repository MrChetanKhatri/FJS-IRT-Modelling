library(tidyverse) 
library(readxl)
library(mirt)

##### Read data and create dataframe ####

#create a dataframe with 12 columns, representing 12 items of FJS. label this as fjsdata
basic <- read_excel("basic data.xlsx") %>% as.data.frame()
fjsdata <- subset(basic, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12"))

#code to calculate sumscore of FJS
basic <- basic %>% 
  rowwise() %>% 
  mutate(
    complete = sum(!is.na(c(FJSN1,FJSN2,FJSN3,FJSN4,FJSN5,FJSN6,FJSN7,FJSN8,FJSN9,FJSN10,FJSN11,FJSN12))), #makes a column to show how many completed responses there arte
    FJS_score_raw = sum(c(FJSN1,FJSN2,FJSN3,FJSN4,FJSN5,FJSN6,FJSN7,FJSN8,FJSN9,FJSN10,FJSN11,FJSN12), na.rm=TRUE), #adds up all available item responses
    FJS_score = 100-(FJS_score_raw/complete)*25) #formula to calculate FJS score

# The scores are invalid if there are more than 4 missing responses, this line removes all with more then 4 missins items
basic[basic$complete<8,"FJS_score"]<-NA

#save file titled "FJSgrm.RDS to working directory
grm <- readRDS("FJSgrm.RDS")

f.scores <- fscores(grm, response.pattern = fjsdata)[,1] %>% as.data.frame() #produces all EAP scores
basic$FJS.EAP.score <- f.scores #add column to main dataframe

#change the score to go from 0-100 to be comparable with original FJS
minimum_theta <- fscores(grm, response.pattern = rep(0,12))[,1]
maximum_theta <- fscores(grm, response.pattern = rep(4,12))[,1]

FJS.grm <- 100 - (round((100*(f.scores - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
basic$FJS.GRM.score <- FJS.grm #add column to main dataframe
