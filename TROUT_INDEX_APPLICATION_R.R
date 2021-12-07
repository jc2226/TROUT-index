#### This script will calculate TROUT (trauma frailty outcomes) Index scores##
###input: data frame containing ICD10 codes as column value
### outout: TROUT INDEX score. Optional TROUT INDEX frailty risk strata 

####packages ####
library(dyplr)
library(tidyr)
library(data.table)

#function identify only first 3 characters of ICD10 codes 
first3<-function(x) {substr(x,start=1,stop=3)}

#keep only first  3 characters of ICD10 codes  within dataframe
#columns containing ICD10 codes  titled "I10_DX..." i.e. I10_DX1, I10_DX2,...
df <- df %>%
  mutate_at(vars(contains("I10_DX")),first3) %>%
  mutate_at(vars(contains("I10_DX")),as.factor)

#create new column for every unique ICD10 code 
df_unique_icd10<-df %>% select(UNIQUE_IDENTIFIER, starts_with("I10_DX")) #UNIQUE_IDENTIFIER= column to identify each subject 
  df_unique_icd10<-gather(df_unique_icd10,icdcolumn,icdcode,I10_DX1:I10_DX40,factor_key=T) #in this example, there were 40 columns with ICD10 codes
  df_unique_icd10<-df_unique_icd10[!df_unique_icd10$icdcode=="",]
  df_unique_icd10 <- df_unique_icd10 %>% mutate_if(is.character,as.factor)
  
  df_unique_icd10<- select (df_unique_icd10,-icdcolumn)
  df_unique_icd10<- distinct (df_unique_icd10,across())
  df_unique_icd10<-dcast(df_unique_icd10, KEY_NIS ~ icdcode, length)

#Some TROUT INDEX input parameters (heart failure/cardiomyopathy, ischemic heart disease, hypertensive heart/kidney disease) comprise >1 ICD10 code
  df_unique_icd10$hfcm <-ifelse ((df_unique_icd10$I50==1|df_unique_icd10$I42==1),1,0)
  df_unique_icd10$ihd <-ifelse ((df_unique_icd10$Z95==1|df_unique_icd10$I25==1),1,0)
  df_unique_icd10$htn_hkt <-ifelse ((df_unique_icd10$I13==1|df_unique_icd10$I12==1|df_unique_icd10$I11==1),1,0)

#table linking TROUT INDEX score to each input baseline condition 
tfi<-read.csv("~file_directory/tfi_scores.csv") #append file directory where csv file was downloaded to
  
#CALCULATE TROUT SCORE
  completevalidate<-df_unique_icd10
  for (i in seq_len(nrow(tfi)))
  {
    df_unique_icd10[,tfi$term[i]]<-tfi$point[i]*df_unique_icd10[[tfi$term[i]]]
  }
  
  completevalidate<- completevalidate %>% 
    mutate(tfi_score=rowSums(select(.,any_of(tfi$term))))

#CATEGORIZE TROUT INDEX FRAILTY RISK STRATA 
  CUTOFF_1=19.937451
  CUTOFF_2=37.109230
  
  completevalidate$tfi_category<-ifelse(completevalidate$tfi_score<CUTOFF_1,0,
                                        ifelse(completevalidate$tfi_score<CUTOFF_2,1,2))
  completevalidate$tfi_category<-as.factor(completevalidate$tfi_category)
  
  #evalute proportion within each of 3 frailty risk strata
  rowPerc(xtabs(~tfi_category,completevalidate))