setwd("C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project")
pacman::p_load(tidyverse, magrittr, DataExplorer, lubridate, fastDummies)
source("data summary.r")
sapply(df, function(x) sum(is.na(x)))
saveRDS(df,"C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project\\trainingData.RDS")

df <- read.csv("domestic_data.csv")
#### Change name of "retain" column to "target" and remove NAs -------------
df$target <- recode_factor(df$retained, "0" = "No", "1" = "Yes") 
df %<>% select(.,-retained) %>% drop_na(target)
#### Remove columns with 100% missing --------------------------------------
df <- df[colSums(!is.na(df)) > 0]
#### Remove entries before 2010 --------------------------------------------
df %<>% filter(DateFrom != 2005 & DateFrom != 2006 & DateFrom != 2007 & DateFrom != 2008 &
                 DateFrom != 2009)
#### Gsub() for Y and Blank data -------------------------------------
df$FirstGen <- as.integer(gsub("Y", "1", df$FirstGen))
df$FirstGen <- as.integer(ifelse(is.na(df$FirstGen), "0", df$FirstGen))
df$ConfirmCode <- as.integer(gsub("Y", "1", df$ConfirmCode))
df$ConfirmCode <- as.integer(ifelse(is.na(df$ConfirmCode), "0", df$ConfirmCode))
#### Remove international student testing columns -------------------------
df %<>% select(., -starts_with("TIB"), -starts_with("IEL"), -starts_with("TCP"),
               -starts_with("CEEB"), -starts_with("TOEFL")) 
#### Keep only the domestic students --------------------------------------
df$InternationalFlag <- gsub("domestic", "Domestic", df$InternationalFlag)
df %<>% filter(InternationalFlag == "Domestic")
#### Remove redundant/unneeded variables ---------------------------------------------
df %<>% select(.,-starts_with("ACTMax"), -SAT.R.ERW, -SAT.R.Math, -Hispanic, -HSClust,
               -EduNbrhd, -OriginalGPA, -Ethn_HS_YN, -FullEthn, -FullEthn.Race, -Math, 
               -Lang, -CountyCode, -Harrison, -ApplicationDate_Formatted)
#### removing character columns -------------------------------
df %<>% select(., -Visa.Type, -VisaType, -Citizen, -Phone, -NationDesc,
               -InitialContact, -InternationalFlag, -MAI, -ACEFlag, -FSBDirect, -ConCode, 
               -StudentType, -MCFlag, -WhichTestBest, -FirstSchool, -SecondSchool, -ThirdSchool,
               -FourthSchool, -FifthSchool, -SixthSchool, -SeventhSchool, -EighthSchool, 
               -TenthSchool, -Com.App.Acad.Int, -MeritGPAThresholdFlag, -Intl.Scholarship, 
               -College.Since.9th.Grade, -Primary_Parent_Occupation, -Sec_Parent_Occupation, 
               -Permanent.Home, -DisciplinaryQuestion1, -Citizenship2, -StateResidency, -Question,
               -Status) 
#### AlumniConnection -----------------------------------------
df$AlumniConnection <- gsub("O - Other", "O", df$AlumniConnection)
df$AlumniConnection <- gsub("S - Sibling", "S", df$AlumniConnection)
df$AlumniConnection <- gsub("M - Mother", "M", df$AlumniConnection)
df$AlumniConnection <- gsub("F - Father", "F", df$AlumniConnection)
df$AlumniConnection <- gsub("A - Mother, Father, Sibling", "A", df$AlumniConnection)
df$AlumniConnection <- gsub("B - Mother, Father", "B", df$AlumniConnection)
df$AlumniConnection <- gsub("C - Mother, Sibling", "C", df$AlumniConnection)
df$AlumniConnection <- gsub("D - Father, Sibling", "D", df$AlumniConnection)
#### Gender to a binary column ----------------------------------------------
df$Gender <- ifelse(df$Gender == "M", 1, 0)
#### Home state columns - keep top 23 ---------------------------------------
df %<>% drop_na(HomeState)
df <- dummy_columns(df, select_columns = "HomeState", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
# which(colnames(df)=="HomeState_WY") #154
# which(colnames(df)=="HomeState_AE") #95
# sort(colSums(df[,95:154]), decreasing = T)
df %<>% mutate(HomeState_Other = (HomeState_FL+HomeState_NC+HomeState_KS+HomeState_DC+HomeState_AZ+HomeState_NH+HomeState_SC+HomeState_IA+HomeState_WA+
                                    HomeState_WV+HomeState_NE+HomeState_RI+HomeState_ME+HomeState_OR+HomeState_VT+
                                    HomeState_LA+HomeState_AR+HomeState_AL+HomeState_NV+HomeState_DE+HomeState_OK+
                                    HomeState_UT+HomeState_AE+HomeState_ID+HomeState_MT+HomeState_ND+HomeState_NM+
                                    HomeState_AP+HomeState_ON+HomeState_AK+HomeState_HI+HomeState_VI+HomeState_GE+
                                    HomeState_Guangdong+HomeState_Guangdongsheng+`HomeState_Hong Kong Island`+HomeState_NS+
                                    HomeState_PR+HomeState_SD+HomeState_WY)) 
df %<>% select(., -HomeState_FL, -HomeState_NC, -HomeState_KS, -HomeState_DC, -HomeState_AZ, -HomeState_NH, -HomeState_SC, 
               -HomeState_IA, -HomeState_WA, -HomeState_WV, -HomeState_NE, -HomeState_RI, 
               -HomeState_ME, -HomeState_OR, -HomeState_VT, -HomeState_LA, -HomeState_AR, 
               -HomeState_AL, -HomeState_NV, -HomeState_DE, -HomeState_OK, -HomeState_UT, 
               -HomeState_AE, -HomeState_ID, -HomeState_MT, -HomeState_ND, -HomeState_NM, 
               -HomeState_AP, -HomeState_ON, -HomeState_AK, -HomeState_HI, -HomeState_VI, 
               -HomeState_GE, -HomeState_Guangdong, -HomeState_Guangdongsheng, -`HomeState_Hong Kong Island`, 
               -HomeState_NS, -HomeState_PR, -HomeState_SD, -HomeState_WY)
#### Removing ACT and SAT data -------------------------------------------
df %<>% select(., -SATVerbal, -SATMath, -SATWRSC, -SATComp, -Satv25, -Satv75,
               -Satm25, -Satm75, -Act25, -Act75, -TermCode, -RoundedGPA, -hs.code,
               -Super.ACT, -Retained_recode, -ACTWritingMax, -ClassRank) 
df %<>% select(., -OhioCountyRegion) 
#### Multi Racial Columns ------------------------------------------------
df %<>% select(.,-OneRace, -Race, -BL_Race, -HS_Race, -AS_Race, -AI_Race, -PI_Race, -WH_Race,
         -AlmostFullRace) 
df$FullRace <- gsub("American Indian or Alaska Native", "AI", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Asian, Black or African American", "AIASBL", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Asian, White", "AIASWH", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Black or African American", "AIBL", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Black or African American, White", "AIBLWH", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Hispanic/Latino", "AIHS", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Hispanic/Latino, White", "AIHSWH", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, White", "AIPIWH", df$FullRace)
df$FullRace <- gsub("American Indian or Alaska Native, White", "AIWH", df$FullRace)
df$FullRace <- gsub("Asian", "AS", df$FullRace)
df$FullRace <- gsub("Asian, Black or African American", "ASBL", df$FullRace)
df$FullRace <- gsub("Asian, Black or African American, White", "ASBLWH", df$FullRace)
df$FullRace <- gsub("Asian, Hispanic/Latino", "ASHS", df$FullRace)
df$FullRace <- gsub("Asian, Hispanic/Latino, Native Hawaiian or Other Pacific Islander, White", "ASHSPIWH", df$FullRace)
df$FullRace <- gsub("Asian, Hispanic/Latino, White", "ASHSWH", df$FullRace)
df$FullRace <- gsub("Asian, Native Hawaiian or Other Pacific Islander", "ASPI", df$FullRace)
df$FullRace <- gsub("Asian, Native Hawaiian or Other Pacific Islander, White", "ASPIWH", df$FullRace)
df$FullRace <- gsub("Asian, White", "ASWH", df$FullRace)
df$FullRace <- gsub("Black or African American", "BL", df$FullRace)
df$FullRace <- gsub("Black or African American, Hispanic/Latino", "BLHS", df$FullRace)
df$FullRace <- gsub("Black or African American, Hispanic/Latino, White", "BLHSWH", df$FullRace)
df$FullRace <- gsub("Black or African American, Native Hawaiian or Other Pacific Islander, White", "BLPRWH", df$FullRace)
df$FullRace <- gsub("Black or African American, White", "BLWH", df$FullRace)
df$FullRace <- gsub("Hispanic/Latino", "HS", df$FullRace)
df$FullRace <- gsub("Hispanic/Latino, White", "HSWH", df$FullRace)
df$FullRace <- gsub("Native Hawaiian or Other Pacific Islander", "PI", df$FullRace)
df$FullRace <- gsub("Native Hawaiian or Other Pacific Islander, White", "PIWH", df$FullRace)
df$FullRace <- gsub("White", "WH", df$FullRace)
df$FullRace <- gsub("AI, AS, BL", "AIASBL", df$FullRace)
df$FullRace <- gsub("AI, AS, WH", "AIASWH", df$FullRace)
df$FullRace <- gsub("AI, BL", "AIBL", df$FullRace)
df$FullRace <- gsub("AI, BL, WH", "AIBLWH", df$FullRace)
df$FullRace <- gsub("AI, HS", "AIHS", df$FullRace)
df$FullRace <- gsub("AI, HS, WH", "AIHSWH", df$FullRace)
df$FullRace <- gsub("AI, PI, WH", "AIPIWH", df$FullRace)
df$FullRace <- gsub("AI, WH", "AIWH", df$FullRace)
df$FullRace <- gsub("AS, BL", "ASBL", df$FullRace)
df$FullRace <- gsub("AS, BL, WH", "ASBLWH", df$FullRace)
df$FullRace <- gsub("AS, HS", "ASHS", df$FullRace)
df$FullRace <- gsub("AS, HS, PI, WH", "ASHSPIWH", df$FullRace)
df$FullRace <- gsub("AS, HS, WH", "ASHSWH", df$FullRace)
df$FullRace <- gsub("AS, PI", "ASPI", df$FullRace)
df$FullRace <- gsub("AS, PI, WH", "ASPIWH", df$FullRace)
df$FullRace <- gsub("AS, WH", "ASWH", df$FullRace)
df$FullRace <- gsub("BL, HS", "BLHS", df$FullRace)
df$FullRace <- gsub("BL, HS, WH", "BLHSWH", df$FullRace)
df$FullRace <- gsub("BL, PI, WH", "BLPIWH", df$FullRace)
df$FullRace <- gsub("BL, WH", "BLWH", df$FullRace)
df$FullRace <- gsub("HS, PI, WH", "HSPIWH", df$FullRace)
df$FullRace <- gsub("HS, WH", "HSWH", df$FullRace)
df$FullRace <- gsub("PI, WH", "PIWH", df$FullRace)
df$FullRace <- gsub("AI, ASWH", "AIASWH", df$FullRace)
df %<>% drop_na(FullRace)
df <- dummy_columns(df, select_columns = "FullRace", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
# which(colnames(df)=="FullRace_WH") #134
# which(colnames(df)=="FullRace_AI") #88
# sort(colSums(df[,88:134]), decreasing = T)
df %<>% mutate(FullRace_Other = (FullRace_BLHSWH+FullRace_AIHS+FullRace_ASHS+FullRace_PI+FullRace_AIASWH+FullRace_PRWH+FullRace_ASBLWH+FullRace_AIMAWH+FullRace_AIAS+FullRace_AIASBL+
                                   FullRace_AIBLHSWH+FullRace_ASHSPIWH+FullRace_MA+FullRace_PR+FullRace_AIPIWH+FullRace_BLMA+
                                   FullRace_HSPIWH+FullRace_AIASBLWH+FullRace_AIASMA+FullRace_AIBLHS+FullRace_AIBLPR+
                                   FullRace_ASBLHS+FullRace_ASBLPIWH+FullRace_BLPIWH+FullRace_BLPR+FullRace_BLPRWH+FullRace_MAPI))
df %<>% select(.,-FullRace_BLHSWH, -FullRace_AIHS, -FullRace_ASHS, -FullRace_PI, -FullRace_AIASWH, -FullRace_PRWH, 
               -FullRace_ASBLWH, -FullRace_AIMAWH, -FullRace_AIAS,-FullRace_AIASBL, -FullRace_AIBLHSWH, -FullRace_ASHSPIWH, 
               -FullRace_MA, -FullRace_PR, -FullRace_AIPIWH, -FullRace_BLMA, -FullRace_HSPIWH,
               -FullRace_AIASBLWH, -FullRace_AIASMA, -FullRace_AIBLHS, -FullRace_AIBLPR, 
               -FullRace_ASBLHS, -FullRace_ASBLPIWH, -FullRace_BLPIWH, -FullRace_BLPR, 
               -FullRace_BLPRWH, -FullRace_MAPI)
#### Imputation for Quantitative Variables -------------------------------
df$M_ClassSize<-ifelse(is.na(df$ClassSize), 1, 0)
df$ClassSize[is.na(df$ClassSize)]<-median(df$ClassSize, na.rm=TRUE)
df$M_ACTComposite<-ifelse(is.na(df$ACTComposite), 1, 0)
df$ACTComposite[is.na(df$ACTComposite)]<-median(df$ACTComposite, na.rm=TRUE)
df$M_ACTEng<-ifelse(is.na(df$ACTEng), 1, 0)
df$ACTEng[is.na(df$ACTEng)]<-median(df$ACTEng, na.rm=TRUE)
df$M_ACTMath<-ifelse(is.na(df$ACTMath), 1, 0)
df$ACTMath[is.na(df$ACTMath)]<-median(df$ACTMath, na.rm=TRUE)
df$M_ACTRdng<-ifelse(is.na(df$ACTRdng), 1, 0)
df$ACTRdng[is.na(df$ACTRdng)]<-median(df$ACTRdng, na.rm=TRUE)
df$M_ACTSci<-ifelse(is.na(df$ACTSci), 1, 0)
df$ACTSci[is.na(df$ACTSci)]<-median(df$ACTSci, na.rm=TRUE)
df$GPA[is.na(df$GPA)]<-median(df$GPA, na.rm=TRUE)
df$AcadRS [is.na(df$AcadRS)]<-median(df$AcadRS, na.rm=TRUE)
df$ON [is.na(df$ON)]<-median(df$ON, na.rm=TRUE)
df$M_ACTWRSC<-ifelse(is.na(df$ACTWRSC), 1, 0)
df$ACTWRSC[is.na(df$ACTWRSC)]<-median(df$ACTWRSC, na.rm=TRUE)
df<- filter(df, (ACTWRSC!="0"))
df<- filter(df, (AcadRS>"0"))
df<- filter(df, (ClassSize>="0"))
#### Removing quantitative variable columns ------------------------------------
df %<>% select(., -RankPercent, -GPAScale, -HS.Code, -GPAOrig, -ZipCode, -Zip, -Zipcode,
               -Zip5, -ï..tag)
#### Division, major, concentration --------------------------------------
df$Division <- gsub("FSB", "BU", df$Division)
df <- dummy_columns(df, select_columns = "Division", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
df$Major <- gsub("Undeclared - Applied Science","AP56",df$Major)
df$Major <- gsub("EGR: Manufacturing Engineering","AP84",df$Major)
df$Major <- gsub("Computer Science","AP90",df$Major)
df$Major <- gsub("Bioengineering","APBE",df$Major)
df$Major <- gsub("Computer Engineering","APCE",df$Major)
df$Major <- gsub("Chemical Engineering","APCL",df$Major)
df$Major <- gsub("Electrical Engineering","APEE",df$Major)
df$Major <- gsub("General Engineering","APGE",df$Major)
df$Major <- gsub("Mechanical Engineering","APME",df$Major)
df$Major <- gsub("Engineering Management","APQV",df$Major)
df$Major <- gsub("Software Engineering","APSE",df$Major)
df$Major <- gsub("Microbiology","AS05",df$Major)
df$Major <- gsub("Anthropology","AS06",df$Major)
df$Major <- gsub("Botany","AS07",df$Major)
df$Major <- gsub("Chemistry","AS09",df$Major)
df$Major <- gsub("Classical Humanities","AS11",df$Major)
df$Major <- gsub("American Studies","AS12",df$Major)
df$Major <- gsub("Economics","AS15",df$Major)
df$Major <- gsub("English: Literature","AS18",df$Major)
df$Major <- gsub("French","AS21",df$Major)
df$Major <- gsub("Geology","AS24",df$Major)
df$Major <- gsub("German","AS25",df$Major)
df$Major <- gsub("Political Science","AS26",df$Major)
df$Major <- gsub("History","AS29",df$Major)
df$Major <- gsub("Mathematics & Statistics","AS37",df$Major)
df$Major <- gsub("Philosophy","AS42",df$Major)
df$Major <- gsub("Physics","AS45",df$Major)
df$Major <- gsub("Psychology","AS47",df$Major)
df$Major <- gsub("Sociology","AS51",df$Major)
df$Major <- gsub("Spanish","AS52",df$Major)
df$Major <- gsub("Zoology","AS55",df$Major)
df$Major <- gsub("International Studies","AS59",df$Major)
df$Major <- gsub("Public Administration","AS65",df$Major)
df$Major <- gsub("Engineering Physics","AS68",df$Major)
df$Major <- gsub("Urban and Regional Planning","AS94",df$Major)
df$Major <- gsub("East Asian Languages & Cultures","ASAL",df$Major)
df$Major <- gsub("Biochemistry","ASBC",df$Major)
df$Major <- gsub("Biology","ASBI",df$Major)
df$Major <- gsub("Biological Physics","ASBP",df$Major)
df$Major <- gsub("Classical Languages","ASCL",df$Major)
df$Major <- gsub("Diplomacy and Global Politics","ASDP",df$Major)
df$Major <- gsub("Earth Science","ASEA",df$Major)
df$Major <- gsub("Environmental Earth Science","ASEV",df$Major)
df$Major <- gsub("Gerontology","ASGE",df$Major)
df$Major <- gsub("Linguistics","ASIB",df$Major)
df$Major <- gsub("English: Creative Writing","ASIC",df$Major)
df$Major <- gsub("Mathematics","ASII",df$Major)
df$Major <- gsub("Individualized Studies","ASIN",df$Major)
df$Major <- gsub("Statistics","ASIP",df$Major)
df$Major <- gsub("Italian Studies","ASIS",df$Major)
df$Major <- gsub("Speech Path & Audiology","ASJB",df$Major)
df$Major <- gsub("Journalism","ASIC",df$Major)
df$Major <- gsub("Media and Culture","ASMC",df$Major)
df$Major <- gsub("Medical Laboratory Science","ASML",df$Major)
df$Major <- gsub("Professional Writing","ASPW",df$Major)
df$Major <- gsub("Quantitative Economics","ASQE",df$Major)
df$Major <- gsub("Russian,East Eur & Eurasian St","ASRE",df$Major)
df$Major <- gsub("Social Justice Studies","ASSJ",df$Major)
df$Major <- gsub("Strategic Communication","ASST",df$Major)
df$Major <- gsub("University Studies","ASSU",df$Major)
df$Major <- gsub("Women, Gender&Sexuality Studies","ASWG",df$Major)
df$Major <- gsub("Accountancy","BU01",df$Major)
df$Major <- gsub("Business Economics","BU14",df$Major)
df$Major <- gsub("Finance","BU19",df$Major)
df$Major <- gsub("Marketing","BU36",df$Major)
df$Major <- gsub("Undeclared - Business","BU56",df$Major)
df$Major <- gsub("Interdisciplinary Bus Managemt","BUIB",df$Major)
df$Major <- gsub("Information Systems","BUIS",df$Major)
df$Major <- gsub("Management and Leadership","BUMN",df$Major)
df$Major <- gsub("Supply Chain & Operations Mgmt","BUSO",df$Major)
df$Major <- gsub("Undeclared - Education","EA56",df$Major)
df$Major <- gsub("Physical Science/Chemistry Edu","EACH",df$Major)
df$Major <- gsub("Family Studies","EASF",df$Major)
df$Major <- gsub("Early Childhood Education","EAGA",df$Major)
df$Major <- gsub("Earth Science/Life Sci Edu","EAGC",df$Major)
df$Major <- gsub("Earth Science/Chemistry Edu","EAGD",df$Major)
df$Major <- gsub("Integrated Eng Lang Arts Edu","EAGF",df$Major)
df$Major <- gsub("Integrated Mathematics Edu","EAGG",df$Major)
df$Major <- gsub("Integrated Social Studies Edu","EAGJ",df$Major)
df$Major <- gsub("Chinese Education","EAGP",df$Major)
df$Major <- gsub("Spanish Education","EAGQ",df$Major)
df$Major <- gsub("Life Science Education","EAGR",df$Major)
df$Major <- gsub("Physical Science Education","EAGU",df$Major)
df$Major <- gsub("Middle Childhood Education","EAGV",df$Major)
df$Major <- gsub("Special Education","EAGW",df$Major)
df$Major <- gsub("Special Education","EA67",df$Major)
df$Major <- gsub("Kinesiology","EAKN",df$Major)
df$Major <- gsub("Life Sciences/Chemistry Educ","EALC",df$Major)
df$Major <- gsub("Athletic Training","EALH",df$Major)
df$Major <- gsub("Nutrition","EANU",df$Major)
df$Major <- gsub("Sport Leadership & Management","EASM",df$Major)
df$Major <- gsub("Social Work","EASW",df$Major)
df$Major <- gsub("Art","FA03",df$Major)
df$Major <- gsub("Art Education","FA04",df$Major)
df$Major <- gsub("Graphic Design","FA05",df$Major)
df$Major <- gsub("Music Performance","FA38",df$Major)
df$Major <- gsub("Music Education","FA39",df$Major)
df$Major <- gsub("Major Undeclared","FA56",df$Major)
df$Major <- gsub("Theatre","FA86",df$Major)
df$Major <- gsub("Art & Architecture History","FAAA",df$Major)
df$Major <- gsub("Interior Design","FAID",df$Major)
df$Major <- gsub("Interactive Media Studies","FAIM",df$Major)
df$Major <- gsub("Music","FAJA",df$Major)
df$Major <- gsub("Architecture","FANC",df$Major)
df$Major <- gsub("AS52 Education","EAGQ",df$Major)
df$Major <- gsub("ASEA/AS09 Edu","EAGD",df$Major)
df$Major <- gsub("ASEA/Life Sci Edu","EAGC",df$Major)
df$Major <- gsub("Biological AS45","ASBP",df$Major)
df$Major <- gsub("Business Economics","BU14",df$Major)
df$Major <- gsub("Engineering AS45","AS68",df$Major)
df$Major <- gsub("Environmental ASEA","ASEV",df$Major)
df$Major <- gsub("FA03 & FANC AS29","ASLR",df$Major)
df$Major <- gsub("FA03 Education", "FA04",df$Major)
df$Major <- gsub("Human Capital BUMN", "BUKO",df$Major)
df$Major <- gsub("Integrated ASII Edu", "EAGG",df$Major)
df$Major <- gsub("Life Sciences/AS09 Educ", "EALC",df$Major)
df$Major <- gsub("Physical Sciences/AS09", "EACH",df$Major)
df$Major <- gsub("Quantitative AS15", "ASQE",df$Major)
df$Major <- gsub("Undeclared: Business", "BU56",df$Major)
df <- dummy_columns(df, select_columns = "Major", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
# which(colnames(df)=="Major_Public Health") 
# which(colnames(df)=="Major_56")
# sort(colSums(df[,117:261]), decreasing = T)
df %<>% mutate(Major_Other = Major_ASJB+Major_ASIC+Major_AS29+Major_ASCP+Major_BUMN+
               Major_EAGV+Major_ASPM+Major_FA39+Major_EALH+Major_APCE+
               Major_EASM+Major_AS15+Major_EAGF+Major_EANU+Major_EAGG+
               Major_APQV+Major_AS18+Major_EAGW+Major_BUIB+Major_FA86+
               Major_FA56+Major_ASPJ+Major_ASII+Major_FA05+Major_AS45+
               Major_FAIM+Major_FAID+Major_ASJN+Major_APEE+Major_AS06+
               Major_EAGJ+Major_FA38+Major_FA03+Major_ASST+Major_AS37+
               Major_EAHP+Major_ASEV+Major_EASW+Major_ASML+Major_ASDP+
               Major_ASPW+Major_APSE+Major_BUSO+Major_EAHS+Major_EAMA+
               Major_AS51+Major_AS52+Major_BUKO+Major_FA04+Major_FAJA+
               Major_EAEX+Major_ASIB+Major_AS42+Major_AS68+Major_BUMO+
               Major_AS07+Major_ASBP+Major_AS21+Major_ASIP+Major_EASR+
               Major_AS24+Major_ASMC+Major_BUMK+Major_EAGQ+Major_EAGU+
               `Major_Public Health`+Major_ASSJ+Major_BUIS+Major_ASAL+Major_AS65+
               `Major_Communication Design`+Major_AS11+Major_AS25+`Major_BUIS and Analytics`+Major_FAAA+
               Major_ASEA+Major_ASES+Major_EAGC+Major_ASCL+Major_ASGE+
               Major_EAFS+Major_EAGD+Major_EAGR+`Major_FA38: Instrumental`+Major_AS94+
               Major_EACH+Major_ASLR+Major_ASQE+Major_AP84+Major_AS92+
               Major_ASRE+Major_ASU3+Major_EAGM+Major_AS12+Major_ASIN+
               Major_EAGE+Major_EALC+Major_AS23+Major_AS66+Major_ASWG+
               Major_EAGN+Major_EAGO+Major_EAGP+`Major_FA38: Vocal`+Major_FALR+
               Major_ASIS+Major_ASLS+Major_EAGT+Major_EASF+Major_AP72+
               Major_ASIG+Major_AS75+Major_EAGB+`Major_FA38: Composition`+`Major_Physical Science/AS09 Edu`)
df %<>% select(.,-Major_ASJB,-Major_ASIC,-Major_AS29,-Major_ASCP,-Major_BUMN,-
                 Major_EAGV,-Major_ASPM,-Major_FA39,-Major_EALH,-Major_APCE,-
                 Major_EASM,-Major_AS15,-Major_EAGF,-Major_EANU,-Major_EAGG,-
                 Major_APQV,-Major_AS18,-Major_EAGW,-Major_BUIB,-Major_FA86,-
                 Major_FA56,-Major_ASPJ,-Major_ASII,-Major_FA05,-Major_AS45,-
                 Major_FAIM,-Major_FAID,-Major_ASJN,-Major_APEE,-Major_AS06,-
                 Major_EAGJ,-Major_FA38,-Major_FA03,-Major_ASST,-Major_AS37,-
                 Major_EAHP,-Major_ASEV,-Major_EASW,-Major_ASML,-Major_ASDP,-
                 Major_ASPW,-Major_APSE,-Major_BUSO,-Major_EAHS,-Major_EAMA,-
                 Major_AS51,-Major_AS52,-Major_BUKO,-Major_FA04,-Major_FAJA,-
                 Major_EAEX,-Major_ASIB,-Major_AS42,-Major_AS68,-Major_BUMO,-
                 Major_AS07,-Major_ASBP,-Major_AS21,-Major_ASIP,-Major_EASR,-
                 Major_AS24,-Major_ASMC,-Major_BUMK,-Major_EAGQ,-Major_EAGU,-
                 `Major_Public Health`,-Major_ASSJ,-Major_BUIS,-Major_ASAL,-Major_AS65,-
                 `Major_Communication Design`,-Major_AS11,-Major_AS25,-`Major_BUIS and Analytics`,-Major_FAAA,-
                 Major_ASEA,-Major_ASES,-Major_EAGC,-Major_ASCL,-Major_ASGE,-
                 Major_EAFS,-Major_EAGD,-Major_EAGR,-`Major_FA38: Instrumental`,-Major_AS94,-
                 Major_EACH,-Major_ASLR,-Major_ASQE,-Major_AP84,-Major_AS92,-
                 Major_ASRE,-Major_ASU3,-Major_EAGM,-Major_AS12,-Major_ASIN,-
                 Major_EAGE,-Major_EALC,-Major_AS23,-Major_AS66,-Major_ASWG,-
                 Major_EAGN,-Major_EAGO,-Major_EAGP,-`Major_FA38: Vocal`,-Major_FALR,-
                 Major_ASIS,-Major_ASLS,-Major_EAGT,-Major_EASF,-Major_AP72,-
                 Major_ASIG,-Major_AS75,-Major_EAGB,-`Major_FA38: Composition`,-`Major_Physical Science/AS09 Edu`) 
#table(df$Concentration)
df$Concentration <- ifelse(is.na(df$Concentration), "Missing", df$Concentration)
df <- dummy_columns(df, select_columns = "Concentration",
                    remove_first_dummy = T,
                    remove_selected_columns = T)
df %<>% mutate(Concentration_Other = (Concentration_76+Concentration_84+Concentration_89+Concentration_EL+Concentration_HB+
                                      Concentration_IN+Concentration_M2+Concentration_SD+Concentration_SE+Concentration_SF+Concentration_SG+Concentration_SH+
                                      Concentration_SI+Concentration_VO)) 
df %<>% select(.,-Concentration_76,-Concentration_84,-Concentration_89,-Concentration_EL,-Concentration_HB,-Concentration_IN,-Concentration_M2,
               -Concentration_SD,-Concentration_SE,-Concentration_SF,-Concentration_SG,-Concentration_SH,-Concentration_SI,-Concentration_VO) 
df %<>% select(.,-Concentration_Missing) 
#### Removing and creating dummies for chr columns -----------------------
df %<>% select(.,-ACTChoice, -HighSchoolCode, -CountyDesc, -County,
               -Geomkt, -IntlTestScoreThresholdFlag, -Parent1Degree,
               -Parent2Degree, -Parent.1.Education.Level, -Parent.2.Educational.Level, 
               -MeritGPA, -Citizenship, -AIDY_Code, -SuppMajor3, -SuppMajor3,
               -DecisionDateFormatted, -ConfirmDate_Formatted, -DecisionDate_Formatted, -MiamiRanks,
               -SuppMajor1, -SuppMajor2, -DataFrom, -EER, -ACTEquivalent, -DecisionDate, -Dec, -Decision,
               -HighSchoolState, -ApplicationDate)
#table(df$AlumniConnection)
df$AlumniConnection <- ifelse(is.na(df$AlumniConnection), "None", df$AlumniConnection) #fix NA values
df <- dummy_columns(df, select_columns = "AlumniConnection", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
#table(df$ApplicationType)
df <- dummy_columns(df, select_columns = "ApplicationType", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
#table(df$Housing)
df$Housing <- gsub("Off Campus", "NO", df$Housing)
df$Housing <- ifelse(is.na(df$Housing), "YES", df$Housing)
df$Housing <- gsub("On Campus", "YES", df$Housing)
df <- dummy_columns(df, select_columns = "Housing", 
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)
#table(df$USStateRegion)
df %<>% drop_na(USStateRegion) 
df <- dummy_columns(df, select_columns = "USStateRegion",
                    remove_first_dummy = T,
                    remove_selected_columns = T)
#table(df$Bridges)
df$Bridges <- as.integer(gsub("Y", "1", df$Bridges))
df$Bridges <- as.integer(ifelse(is.na(df$Bridges), "0", df$Bridges))
#table(df$HsType)
df$HsType <- gsub("1", "Public", df$HsType)
df$HsType <- gsub("2", "Private", df$HsType)
df$HsType <- gsub("3", "Catholic", df$HsType)
df$HsType <- gsub("4", "Other", df$HsType)
df$HsType <- gsub("Charter", "Other", df$HsType)
df$HsType <- gsub("Private Secular", "Private", df$HsType)
df$HsType <- gsub("Religious", "Private", df$HsType)
df$HsType <- gsub("Home School", "Other", df$HsType)
df$HsType <- ifelse(is.na(df$HsType), "Missing", df$HsType)
df <- dummy_columns(df, select_columns = "HsType",
                    remove_first_dummy = T,
                    remove_selected_columns = T)
df %<>% select(.,-HsType_Missing, -HsType_Unknown) 
#table(df$DecisionType)
df$DecisionType <- ifelse(is.na(df$DecisionType), "Missing", df$DecisionType)
df <- dummy_columns(df, select_columns = "DecisionType",
                    remove_first_dummy = T,
                    remove_selected_columns = T)
# which(colnames(df)=="Major_Public Health") 
# which(colnames(df)=="DecisionType_DN")
# sort(colSums(df[,139:150]), decreasing = T)
df %<>% select(.,-DecisionType_Missing)
df %<>% mutate(DecisionType_Other = (DecisionType_N+DecisionType_HN+DecisionType_SQ+DecisionType_HQ+
                                       DecisionType_DN+DecisionType_SY+DecisionType_DQ+DecisionType_HY)) 
df %<>% select(., -DecisionType_N,-DecisionType_HN,-DecisionType_SQ,-DecisionType_HQ,
                 -DecisionType_DN,-DecisionType_SY,-DecisionType_DQ,-DecisionType_HY) 
df$SpecCon <- paste(df$Special.Consideration,df$SpecialConsideration)
#table(df$SpecCon)
df$SpecCon <- gsub("AT NA", "AT ", df$SpecCon)
df$SpecCon <- gsub("NA AT", "AT ", df$SpecCon)
df$SpecCon <- gsub("CA NA", "CA ", df$SpecCon)
df$SpecCon <- gsub("NA CA", "CA ", df$SpecCon)
df$SpecCon <- gsub("DI NA", "DI ", df$SpecCon)
df$SpecCon <- gsub("NA DI", "DI ", df$SpecCon)
df$SpecCon <- gsub("FC NA", "FC ", df$SpecCon)
df$SpecCon <- gsub("NA FC", "FC ", df$SpecCon)
df$SpecCon <- gsub("FI NA", "FI ", df$SpecCon)
df$SpecCon <- gsub("NA FI", "FI ", df$SpecCon)
df$SpecCon <- gsub("FM NA", "FM ", df$SpecCon)
df$SpecCon <- gsub("NA FM", "FM ", df$SpecCon)
df$SpecCon <- gsub("FR NA", "FR ", df$SpecCon)
df$SpecCon <- gsub("NA FR", "FR ", df$SpecCon)
df$SpecCon <- gsub("FT NA", "FT ", df$SpecCon)
df$SpecCon <- gsub("NA FT", "FT ", df$SpecCon)
df$SpecCon <- gsub("PR NA", "PR ", df$SpecCon)
df$SpecCon <- gsub("NA PR", "PR ", df$SpecCon)
df$SpecCon <- gsub("SR NA", "SR ", df$SpecCon)
df$SpecCon <- gsub("NA SR", "SR ", df$SpecCon)
df <- dummy_columns(df, select_columns = "SpecCon",
                    remove_first_dummy = T,
                    remove_selected_columns = T)
df %<>% select(., -Special.Consideration, -SpecialConsideration, -`SpecCon_NA NA`) 
#sort(colSums(df[,140:150]), decreasing = T)
df[0,]

names(df)[144] <- "SpecCon_ES"
names(df)[143] <- "SpecCon_DI"
names(df)[146] <- "SpecCon_FI"
names(df)[149] <- "SpecCon_FT"
names(df)[150] <- "SpecCon_PR"
names(df)[145] <- "SpecCon_FC"
names(df)[147] <- "SpecCon_FM"
names(df)[148] <- "SpecCon_FR"
names(df)[142] <- "SpecCon_CA"
names(df)[151] <- "SpecCon_SR"
names(df)[141] <- "SpecCon_AT"
df %<>% mutate(SpecCon_Other = (SpecCon_ES+SpecCon_DI+SpecCon_FI+SpecCon_FT+SpecCon_PR
                                +SpecCon_FC))
df %<>% select(.,-SpecCon_ES,-SpecCon_DI,-SpecCon_FI,-SpecCon_FT,-SpecCon_PR,
               -SpecCon_FC) 
#### ConfirmDate and ConfirmedDate ---------------------------------------
df$ConfirmDate<-ymd(df$ConfirmDate)
df$ConfirmedDate<-dmy(df$ConfirmedDate)
df %<>% mutate(ConfDate = coalesce(ConfirmDate,ConfirmedDate)) 
df %<>% select(., -ConfirmDate, -ConfirmedDate) 
str(df$ConfDate)













