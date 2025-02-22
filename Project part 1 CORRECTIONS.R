setwd("C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project")
pacman::p_load(tidyverse, magrittr, DataExplorer, lubridate, fastDummies)
df <- readRDS("trainingData.RDS")

#### 145 variables is a lot of variables.   ----------------------------------
df %<>% select(., -Housing_NO, -Housing_YES, -USStateRegion_Other, -HsType_Other, 
               -starts_with("SpecCon")) 
#### You have at least one variable with missing values. You have one --------
df %<>% select(.,-ConfDate)
#### There are many, many dummy variables that have mostly one value, --------
# 1. DecisionType --> DecisionType_H and DecisionType_Other
df %<>% select(., -DecisionType_S, -DecisionType_Other)

# 2. AlumniConnection
df %>% select(starts_with("AlumniConnection")) %>% head(1)
df %<>% mutate(AlumniConnection_Other = AlumniConnection_A+AlumniConnection_B+AlumniConnection_C+
                 AlumniConnection_D)
df %<>% select(., -AlumniConnection_A, -AlumniConnection_B, -AlumniConnection_C, -AlumniConnection_D) 

# 3. FullRace
df %>% select(starts_with("FullRace")) %>% head(1)
df %<>% mutate(FullRace_Other = (FullRace_AI+FullRace_AIBL+FullRace_AIBLWH+FullRace_AIHSWH+FullRace_ASBL+
                                   FullRace_ASHSWH+FullRace_ASPI+FullRace_ASPIWH+FullRace_ASWH+FullRace_BLHS+FullRace_BLWH+FullRace_HS+
                                   FullRace_MAWH+FullRace_PIWH+FullRace_UK+FullRace_Other))
df %<>% select(., -FullRace_AI, -FullRace_AIBL, -FullRace_AIBLWH, -FullRace_AIHSWH, -FullRace_ASBL, -FullRace_ASHSWH, -FullRace_ASPI, 
               -FullRace_ASPIWH, -FullRace_ASWH, -FullRace_BLHS, -FullRace_BLWH, -FullRace_HS, -FullRace_MAWH, -FullRace_PIWH, -FullRace_UK)

#4. HomeState
df %>% select(starts_with("HomeState")) %>% head(1)
# which(colnames(df)=="HomeState_CA") 
# which(colnames(df)=="HomeState_WI")
# sort(colSums(df[,18:37]), decreasing = T)
df %<>% mutate(HomeState_Other = (HomeState_PA+HomeState_NY+HomeState_CT+HomeState_NJ+HomeState_MO+HomeState_CA+
                                    HomeState_KY+HomeState_MD+HomeState_MA+HomeState_MN+HomeState_VA+HomeState_CO+HomeState_WI+
                                    HomeState_TN+HomeState_GA+HomeState_TX+HomeState_Other)) 
df %<>% select(., -HomeState_PA, -HomeState_NY, -HomeState_CT, -HomeState_NJ, -HomeState_MO, -HomeState_CA, -
                 HomeState_KY, -HomeState_MD, -HomeState_MA, -HomeState_MN, -HomeState_VA, -HomeState_CO, -HomeState_WI, -
                 HomeState_TN, -HomeState_GA, -HomeState_TX, -HomeState_Other) 

# 5. Major
# df %>% select(starts_with("Major")) %>% head(1)
# which(colnames(df)=="Major_56") 
# which(colnames(df)=="Major_FANC")
# sort(colSums(df[,44:73]), decreasing = T)
# table(df$Major_Other)
df %<>% mutate(Major_Other = (Major_AP90+Major_EAKN+Major_APBE+Major_AS09+`Major_Business AS15`+Major_ASU1+
                                Major_AS56+Major_AP56+Major_EA56+Major_AS59+Major_FANC+Major_APCL+Major_AS05+Major_Other)) 
df %<>% select(., -Major_AP90, -Major_EAKN, -Major_APBE, -Major_AS09, -`Major_Business AS15`, -Major_ASU1, -Major_AS56, 
               -Major_AP56, -Major_EA56, -Major_AS59, -Major_FANC, -Major_APCL, -Major_AS05, -Major_Other) 

# 6. Division
# df %>% select(starts_with("Division")) %>% head(1)
# which(colnames(df)=="Division_AP") 
# which(colnames(df)=="Division_FA")
# sort(colSums(df[,35:43]), decreasing = T)

# 7. USSStateRegion
# df %>% select(starts_with("USStateRegion")) %>% head(1)
# which(colnames(df)=="USStateRegion_Eastern Midwest") 
# which(colnames(df)=="USStateRegion_Western Midwest")
# sort(colSums(df[,71:77]), decreasing = T)

#### Re-code your target, Yes=students who left, No=students who are  --------
table(df$target)
df$target <- recode_factor(df$target, "No" = "Yes", "Yes" = "No") 

saveRDS(df,"C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project\\trainingDataUPDATE.RDS")

plot_bar(df)
plot_missing(df)















