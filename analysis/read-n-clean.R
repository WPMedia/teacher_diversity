require(tidyverse)
require(vroom)
require(stringr)
require(here)

#######################
### Read it all in  ###
#######################

files = list.files(here(pattern="*.csv"))

list2env(
  lapply(setNames(files, make.names(gsub("*.csv$", "", files))), 
         read.csv, stringsAsFactors=FALSE), envir = .GlobalEnv)

### natl student demographic data from NCES ###
students1617 <- students1617 %>%
  rename(leaid = LEAID) %>%
  mutate(leaid = ifelse(ST %in% c("AL", "AK", "AZ", "AR", "CA", "CO", "CT"), paste0("0", leaid), as.character(leaid)),
         lea_name = tolower(LEA_NAME))

urban_rural <- students1617 %>% select(leaid, d_Locale_Txt)

students1718 <- students1718 %>%
  rename(leaid = LEAID) %>%
  mutate(leaid = ifelse(ST %in% c("AL", "AK", "AZ", "AR", "CA", "CO", "CT"), paste0("0", leaid), as.character(leaid)),
         lea_name = tolower(LEA_NAME))

students1718 <- left_join(students1718, urban_rural, by="leaid")

#######################
### The great clean ###
#######################

### for each state: 
# calculate proportion of teachers by race/ethnicity
# where states have unknown, remove if > 10%
# calculate percentages without unknown in total
# join with student data for appropriate year
####

### AK ###
AK <- AK_teachers_1718 %>%  
  mutate(AIAN = American.Indian + Alaska.Native,
         District.Name = ifelse(District.Name == "", NA, tolower(District.Name))) %>%
  fill(District.Name) %>%
  group_by(District.Name) %>%
  summarise(whiteT = sum(White..Caucasian.),
            blackT = sum(African.American),
            hispanicT = sum(Hispanic),
            asianT = sum(Asian),
            aianT = sum(AIAN),
            multiT = sum(Two.or.More.Races),
            nhpiT = sum(Pacific.Islander),
            totalT = sum(Totals.by.School)) %>%
  rename(lea_name = District.Name) %>%
  mutate(yr = "2017-2018", 
         lea_name = ifelse(lea_name == "mount edgecumbe", "mount edgecumbe high school agency",
                           ifelse(lea_name == "delta/greely school district", "delta-greely school district", lea_name))) %>%
  mutate_at(vars(whiteT:nhpiT), ~(round((./(totalT))*100,2))) 

AK <- right_join(AK, students1718 %>% filter(ST == "AK"), by="lea_name")

### AL ###
AL <- AL_teachers_1718 %>% 
  rename(blackT = Black.or.African.American.Teachers, 
         whiteT = White.Teachers, 
         hispanicT = Latino.Teachers, 
         asianT = Asian.Teachers, 
         otherT = Other.Teachers, 
         lea_name = District) %>%
  mutate(totalT = blackT + whiteT + hispanicT + asianT + otherT, 
         yr = "2017-2018",
         lea_name = tolower(trimws(lea_name))) %>%
  filter(otherT / totalT < .2) %>%
  mutate_at(vars(blackT:otherT), ~(round((./(totalT)*100),2)))

AL <- right_join(AL, students1718 %>% filter(ST == "AL"), by="lea_name")

### AR ###
AR <- AR_teachers_1718 %>%
  rename(blackT = Black.African.American, 
         whiteT = White, 
         hispanicT = Hispanic, 
         asianT = Asian, 
         aianT = Native.American.Alaskan.Native,
         nhpiT = Native.Hawaiian.Pacific.Islander,
         multiT = X2.or.More.Races,
         totalT = Total) %>%
  mutate(yr = "2017-2018",
         ST_LEAID = ifelse(nchar(District.LEA) == 6, paste0("AR-0", District.LEA), paste0("AR-", District.LEA))) %>%
  mutate_at(vars(multiT:whiteT), ~(round((./(totalT)*100),2))) %>%
  select(-District.LEA, -District.Description)

AR <- right_join(AR, students1718 %>% filter(ST == "AR"), by="ST_LEAID")

### AZ ###
# reports race and ethnicity separately, which is terrible

# district-collected data with race and ethnicity combined
AZ <- AZ_teacher_count %>%
  filter(!(is.na(totalT))) %>%
  mutate(leaid = str_pad(leaid, 7, pad = "0")) %>%
  select(-ST, -lea_name, -ST_LEAID, -otherT) %>%
  rename(yr = year)

# state-provided SEDR data
AZ_race_ethnicity <- AZ_teachers_race1617 %>%
  filter(ReportingGroup == "Teacher") %>%
  merge(AZ_teachers_ethnicity1617 %>% filter(ReportingGroup == "Teacher"),
        by = c("LEA", "School", "Certified", "ReportingGroup", "PositionCode", "Description"), all=TRUE) %>%
  select(-Certified, -ReportingGroup, -Description) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalR = WhiteMale + WhiteFemale + BlackMale + BlackFemale + AsianMale + AsianFemale + PacificIslanderMale +
           PacificIslanderFemale + AmericanIndianMale + AmericanIndianFemale + MultiMale + MultiFemale,
         totalE = HispanicMale + HispanicFemale + NonHispanicMale + NonHispanicFemale)

AZ_districts <- AZ_race_ethnicity %>%
  group_by(LEA) %>%
  summarise_at(vars(WhiteMale:totalE), sum) %>%
  filter(totalR != 0 & totalE != 0) %>%
  # just keeping districts where white is the only race or a small proportion of hispanics
  filter((WhiteMale + WhiteFemale == totalR) | ((HispanicMale + HispanicFemale)/totalE < 0.05)) %>%
  mutate(hispanicT = HispanicMale + HispanicFemale,
         whiteT = (WhiteMale + WhiteFemale) - hispanicT,
         blackT = BlackMale + BlackFemale,
         asianT = AsianMale + AsianFemale,
         nhpiT =  PacificIslanderMale + PacificIslanderFemale,
         aianT = AmericanIndianMale + AmericanIndianFemale,
         multiT = MultiMale + MultiFemale,
         unknownT = ifelse(totalE > totalR, totalE - totalR, 0),
         totalT = ifelse(totalE > totalR, totalE, totalR),
         lea_name = tolower(LEA),
         yr = "2016-17") %>%
  filter(unknownT / totalT < 0.1) %>%
  mutate_at(vars(hispanicT:multiT), ~(round((./(totalT - unknownT)*100),2))) %>%
  select(lea_name, hispanicT:multiT, totalT, yr)

# district-collected data not all same year so matching to what's appropriate
AZ16 <- AZ %>% 
  filter(yr == "2016-17") %>%
  left_join(students1617 %>% filter(ST == "AZ"), by="leaid") %>%
  select(-d_Locale)

AZ17 <- AZ %>% 
  filter(yr != "2016-17") %>%
  right_join(students1718 %>% filter(ST == "AZ"), by="leaid") %>%
  filter(!(leaid %in% AZ16$leaid))

AZ2 <- AZ_districts %>% 
  left_join(students1617 %>% filter(ST == "AZ"), by="lea_name")  %>%
  select(-d_Locale)

AZ <- rbind(AZ16, AZ17)

AZ <- AZ %>%
  filter(!(leaid %in% AZ2$leaid))

AZ <- rbind(AZ, AZ2)

AZ <- AZ %>% filter(!(is.na(totalS)))

### CA ###
CA <- CA_teachers_1718 %>%
  mutate(nhpiT = Pacific.Islander.not.Hispanic + Filipino.not.Hispanic,
         yr = "2017-2018",
         ST_LEAID = ifelse(nchar(District.Code) == 6, paste0("CA-0", District.Code), paste0("CA-", District.Code))) %>%
  rename(blackT = African.American.not.Hispanic, 
         whiteT = White.not.Hispanic, 
         hispanicT = Hispanic.of.Any.Race, 
         asianT = Asian.not.Hispanic, 
         aianT = American.Indian.Alaska.Native.not.Hispanic,
         multiT = Two.or.More.Races.not.Hispanic,
         unknownT = None.Selected,
         totalT = Total) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(hispanicT:unknownT, nhpiT), ~(round((./(totalT - unknownT)*100),2))) %>%
  select(-contains("."), -District, -District.Code, -unknownT)

CA <- right_join(CA, students1718 %>% filter(ST == "CA"), by="ST_LEAID")

### CO ###
CO <- CO_teachers_1718 %>%
  filter(Organization.Code != "Grand Total") %>%
  rename(blackT = Black.or.African.American.Total, 
         whiteT = White.Total, 
         hispanicT = Hispanic.or.Latino.Total, 
         asianT = Asian.Total, 
         aianT = American.Indian.or.Alaska.Native.Total,
         multiT = Two.or.More.Races.Total,
         nhpiT = Native.Hawaiian.or.Other.Pacific.Islander.Total,
         totalT = Total) %>%
  mutate_at(vars(blackT, whiteT, hispanicT, asianT, aianT, multiT, nhpiT, totalT), funs(as.numeric(gsub(",", "", .)))) %>%
  mutate(yr = "2017-2018",
         ST_LEAID = paste0("CO-", Organization.Code)) %>%
  select(-contains("Male"), -contains("Female"), -contains(".")) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./(totalT))*100, 2)))

CO <- right_join(CO, students1718 %>% filter(ST == "CO"), by="ST_LEAID")

### CT ###
CT <- CT_teachers_1718 %>%
  select(-X..of.Total) %>%
  mutate(Race = case_when(
    Race == "American Indian Or Alaska Native" ~ "aianT",
    Race == "Asian" ~ "asianT",
    Race == "Black Or African American" ~ "blackT",
    Race == "Hispanic Or Latino" ~ "hispanicT",
    Race == "Native Hawaiian Or Other Pacific Islander" ~ "nhpiT",
    Race == "Not Reported" ~ "unknownT",
    Race == "Two Or More Races" ~ "multiT",
    Race == "White" ~ "whiteT")) %>%
  spread(Race, Count) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + nhpiT + unknownT + multiT + whiteT,
         lea_name = tolower(District),
         lea_name = ifelse(grepl("norwich free academy|the gilbert school", lea_name), 
                           gsub(" district|", "", lea_name), lea_name), 
         lea_name = gsub("the woodstock academy district", "woodstock academy", lea_name),
         yr = "2017-2018") %>%
  filter(unknownT/totalT < .1) %>%
  mutate_at(vars(aianT:nhpiT, whiteT), ~(round((./(totalT - unknownT))*100, 2))) %>%
  select(-District, -unknownT)

CT <- right_join(CT, students1718 %>% filter(ST == "CT") %>%
                   mutate(lea_name = ifelse(lea_name == "north branford school distric", "north branford school district",
                                            ifelse(lea_name == "north stonington school distr", "north stonington school district",
                                                   lea_name))), 
                 by="lea_name")

### DC ###
DC <- DC_teachers_1718 %>% 
  select(-lea_name) %>% 
  mutate(leaid = as.character(leaid)) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./(100 - unknownT))*100, 2))) %>%
  select(-unknownT)

DC <- left_join(DC, students1718, by="leaid")

### DE ###
DE <- DE_teachers_1718 %>%
  rename(blackT = African.American,
         aianT = American.Indian,
         asianT = Asian,
         nhpiT = Hawaiian,
         hispanicT = Hispanic.Latino,
         whiteT = White,
         multiT = Multi.Racial,
         lea_name = District.Name, 
         totalT = Number.of.Teachers) %>%
  mutate_at(vars(blackT:multiT), ~(as.numeric(gsub("%", "", .)))) %>%
  mutate(yr = "2017-2018",
         lea_name = tolower(lea_name)) %>%
  filter(District.Type == "Regular") %>%
  select(-School.Year,-District.Code, -District.Type, -Unknown)

DE <- right_join(DE, students1718 %>% filter(ST == "DE"), by="lea_name")

### FL ###
FL <- FL_teachers_1718 %>%
  rename(lea_name = X.1) %>%
  filter(lea_name != "FLORIDA") %>%
  mutate_at(vars(Male.White:X.2), funs(as.numeric(gsub(",", "", .)))) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(aianT = Male.American.Indian.or.Alaska.Native + Female.American.Indian.or.Alaska.Native,
         asianT = Male.Asian + Female.Asian,
         blackT = Male.Black.or.African.American + Female.Black.or.African.American,
         hispanicT = Male.Hispanic.Latino + Female.Hispanic.Latino,
         nhpiT = Male.Native.Hawaiian.or.Other.Pacific.Islander + Female.Native.Hawaiian.or.Other.Pacific.Islander,
         multiT = Male.Two.or.More.Races + Female.Two.or.More.Races,
         whiteT = Male.White + Female.White,
         totalT = X.2,
         lea_name = tolower(lea_name),
         yr = "2017-2018") %>%
  select(-contains("."), -X) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./(totalT))*100,2)))

FL <- right_join(FL, students1718 %>% filter(ST == "FL") %>%
                   mutate(lea_name = ifelse(lea_name == "dade", "miami-dade", lea_name)),
                 by="lea_name")

### GA ### 
GA <- GA_teachers_1718 %>%
  filter(EMPLOYEE_TYPE == "PK-12 Teachers" & DATA_CATEGORY == "Race/Ethnicity" & INSTN_NUMBER == "ALL" & SCHOOL_DSTRCT_CD != "ALL") %>% 
  mutate(DATA_SUB_CATEGORY = tolower(DATA_SUB_CATEGORY),
         DATA_SUB_CATEGORY = ifelse(DATA_SUB_CATEGORY == "multiracial", "multi",
                                    ifelse(DATA_SUB_CATEGORY == "native american", "aian", DATA_SUB_CATEGORY))) %>%
  spread(DATA_SUB_CATEGORY, MEASURE) %>%
  mutate(totalT = aian + asian + black + hispanic + multi + white,
         yr = "2017-2018",
         ST_LEAID = paste0("GA-", SCHOOL_DSTRCT_CD)) %>%
  filter(!(grepl("State Schools-|Charter Schools-", SCHOOL_DSTRCT_NM))) %>%
  # whitfield teachers reported here as 96% multi. this is almost certainly an error. removing.
  filter(SCHOOL_DSTRCT_NM != "Whitfield County") %>%
  select(-SCHOOL_DSTRCT_NM, -LONG_SCHOOL_YEAR, -INSTN_NUMBER, -DATA_CATEGORY, -EMPLOYEE_TYPE, -INSTN_NAME, -SCHOOL_DSTRCT_CD) 

colnames(GA)[c(1:6)] <- paste0(colnames(GA)[c(1:6)], "T")

GA <- GA %>%
  mutate_at(vars(aianT:whiteT), funs(round((./(totalT))*100)),2) 

GA <- right_join(GA, students1718 %>% filter(ST == "GA"), by="ST_LEAID")

### HI ###
HI <- HI_teachers_1718 %>%
  mutate(leaid = as.character(leaid),
         otherT = otherT + asianT,
         asianT = NA,
         multiT = NA) %>%
  select(-lea_name)

HI <- left_join(HI, students1718, by="leaid")

### IA ###
IA <- IA_teacher_1718 %>%
  filter(!(is.na(District))) %>%
  rename(lea_name = District.or.AEA.Name, 
         aianT = Native.American.Percent,
         asianT = Asian.Percent,
         blackT = Black.Percent,
         hispanicT = Hispanic.Percent,
         multiT = Multi.Race.Percent,
         nhpiT = Pacific.Islander.Percent,
         whiteT = White.Percent, 
         totalT = Total) %>%
  select(-contains(".Count"), -District) %>%
  mutate(yr = "2017-2018",
         lea_name = tolower(lea_name),
         lea_name = ifelse(grepl("eddyville", lea_name), "eddyville-blakesburg- fremont csd", 
                           ifelse(grepl("estherville lincoln", lea_name), "estherville lincoln central",
                                  ifelse(grepl("exira", lea_name), "exira-elk horn- kimballton", 
                                         ifelse(lea_name == "decorah community", "decorah", lea_name)))),
         lea_name = gsub("west fork csd", "west fork", lea_name),
         totalT = as.numeric(gsub(",", "", totalT)))

IA <- right_join(IA, students1718 %>% filter(ST == "IA") %>%
                   mutate(lea_name = gsub(" comm school district| community school district| com sch dist| comm sch dist| school district", "", lea_name)),
                 by="lea_name")

### ID ###
ID <- ID_teachers_1718 %>%
  filter(schoolnumber == "District Totals:") %>%
  rename(aianT = AM, 
         asianT = AS, 
         blackT = BL,
         hispanicT = H,
         nhpiT = HO,
         multiT = M,
         whiteT = WH) %>%
  mutate_all(~(ifelse(is.na(.), 0, .))) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + multiT + nhpiT + whiteT,
         yr = "2017-2018", 
         ST_LEAID = paste0("ID-", leanumber)) %>%
  mutate_at(vars(whiteT:multiT), funs(round((./(totalT))*100)),2) %>%
  select(-contains("SCHOOL"), -DataYear, -leanumber, -leaname) 

ID <- right_join(ID, students1718 %>% filter(ST == "ID"), by="ST_LEAID")

### IL ###
IL <- IL_teachers_1617 %>%
  filter(grepl("Teacher", Position)) %>%
  filter(!(grepl("Dean", Position))) %>%
  mutate(ST_LEAID = paste0("IL-", str_pad(Region, 2, pad = "0"), "-", str_pad(County.Code, 3, pad = "0"), "-", District, "-", str_pad(Type, 2, pad = "0"))) %>%
  group_by(ST_LEAID) %>%
  count(Race.Ethnicity) %>%
  mutate(Race.Ethnicity = case_when(
    Race.Ethnicity == "American Indian or Alaska Native" ~ "aianT",
    Race.Ethnicity == "Asian" ~ "asianT",
    Race.Ethnicity == "Black or African American" ~ "blackT",
    Race.Ethnicity == "Hispanic or Latino" ~ "hispanicT",
    Race.Ethnicity == "Native Hawaiian or Other Pacific Islander" ~ "nhpiT",
    Race.Ethnicity == "Unknown" ~ "unknownT",
    Race.Ethnicity == "Two or More Races" ~ "multiT",
    Race.Ethnicity == "White" ~ "whiteT")) %>%
  spread(Race.Ethnicity, n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + nhpiT + multiT + whiteT + unknownT,
         yr = "2016-2017") %>%
  filter(unknownT / totalT < 0.1) %>%
  mutate_at(vars(aianT:nhpiT, whiteT), ~(round((./(totalT - unknownT))*100, 2))) %>%
  select(-unknownT)

IL <- right_join(IL, students1617 %>% filter(ST == "IL"), by="ST_LEAID")

### IN ###
IN <- IN_teachers_1718 %>%
  filter(!(is.na(Corp))) %>%
  mutate(Ethnicity = case_when(
    Ethnicity == "American Indian/Alaskan Native" ~ "aianT",
    Ethnicity == "Asian" ~ "asianT",
    Ethnicity == "Black" ~ "blackT",
    Ethnicity == "Hispanic Ethnicity and of any race" ~ "hispanicT",
    Ethnicity == "Native Hawaiian or Other Pacific Islander" ~ "nhpiT",
    Ethnicity == "Multiracial (two or more races)" ~ "multiT",
    Ethnicity == "White" ~ "whiteT")) %>%
  spread(Ethnicity, Teacher.Count)  %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + nhpiT + multiT + whiteT,
         yr = "2017-2018",
         ST_LEAID = str_pad(Corp, 4, pad = "0"),
         ST_LEAID = paste0("IN-", ST_LEAID)) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./(totalT))*100, 2))) %>%
  select(-Corp, -Corp.Name)

IN <- right_join(IN, students1718 %>% filter(ST == "IN"), by="ST_LEAID")

### KS ###
colnames(KS_teachers1617) <- tolower(colnames(KS_teachers1617))

KS <- KS_teachers1617 %>%
  mutate(ST_LEAID = paste0("KS-", district.number)) %>%
  rename(nhpi = nhop,
         multi = multi.races.ethinicities) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(ST_LEAID) %>%
  summarise_at(vars(total:multi), funs(sum)) %>%
  ungroup() %>%
  mutate(yr = "2016-2017") 

colnames(KS)[c(2:9)] <- paste0(colnames(KS)[c(2:9)], "T")
KS <- KS %>%
  mutate_at(vars(hispanicT:multiT), funs(round((./(totalT))*100)),2)  

KS <- right_join(KS, students1617 %>% filter(ST == "KS"), by="ST_LEAID")

### KY ### 
KY <- KY_teachers_1617 %>%
  filter(SCH_NAME == "---District Total---") %>%
  rename(aianT = AIAN_FTE_TOTAL, 
         asianT = ASIAN_FTE_TOTAL,
         blackT = BLACK_FTE_TOTAL,
         hispanicT = HISPANIC_FTE_TOTAL,
         multiT = TWO_OR_MORE_FTE_TOTAL,
         nhpiT = HAWAIIAN_FTE_TOTAL,
         whiteT = WHITE_FTE_TOTAL,
         totalT = FTE_TCH_TOTAL) %>%
  mutate_at(vars(totalT:multiT), funs(as.numeric(gsub(",", "", .)))) %>%
  mutate(yr = "2016-2017",
         ST_LEAID = paste0("KY-", str_pad(CNTYNO, 3, pad = "0"), str_pad(DIST_NUMBER, 3, pad = "0"), "000")) %>%
  select(ST_LEAID, aianT, asianT, blackT, hispanicT, multiT, nhpiT, whiteT, totalT) %>%
  mutate_at(vars(aianT:whiteT), funs(round((./(totalT))*100)),2) 

KY <- right_join(KY, students1617 %>% filter(ST == "KY"), by="ST_LEAID")

### LA ###
LA <- LA_teachers_1718 %>%
  mutate(Race.Ethnicity = case_when(
    Race.Ethnicity == "African American" ~ "blackT",
    Race.Ethnicity == "Asian" ~ "asianT",
    Race.Ethnicity == "Hispanic" ~ "hispanicT",
    Race.Ethnicity == "Multiple Races/Ethnicities" ~ "multiT",
    Race.Ethnicity == "Native American" ~ "aianT",
    Race.Ethnicity == "Pacific Islander" ~ "nhpiT",
    Race.Ethnicity == "White" ~ "whiteT"),
    lea_name = tolower(School.System.Name),
    yr = "2017-2018",
    School.System.Code = ifelse(School.System.Code == "R36", "036", School.System.Code)) %>%
  spread(Race.Ethnicity, Percent.of.Teachers) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  select(-Year,  -School.System.Name)

# population is in a separate file, so pulling in teacher population
LA <- left_join(LA, LA_teacher_count %>% select(LEA.Code, teacher_count) %>% rename(School.System.Code = LEA.Code), by="School.System.Code") %>%
  mutate(totalT = as.numeric(gsub(",", "", teacher_count))) %>%
  select(-teacher_count, -School.System.Code)

LA <- right_join(LA, students1718 %>% filter(ST == "LA"), by="lea_name")

### MA ### 
MA <- MA_teachers_1718 %>%
  filter(ORG_CODE8 != "0") %>%
  rename(lea_name = DISTRICT_NAME,
         aianT = Native.American,
         asianT = Asian,
         blackT = African.American,
         hispanicT = Hispanic,
         multiT = Multi.Race..Non.Hispanic,
         nhpiT = Native.Hawaiian..Pacific.Islander,
         whiteT = White,
         totalT = Total.Headcount) %>%
  mutate_at(vars(blackT:totalT), ~(as.numeric(gsub(",", "", .)))) %>%
  mutate_at(vars(blackT:multiT), ~(round((./(totalT))*100, 2))) %>%
  mutate(yr = "2017-2018", 
         lea_name = tolower(lea_name)) %>%
  select(-FY_CODE, -ORG_CODE8)

MA <- right_join(MA, students1718 %>% filter(ST == "MA"), by="lea_name")

### MD ###
MD <- MD_teachers_1617 %>%
  filter(!(LEA %in% c("Total", "Local Education Agency", ""))) %>%
  mutate_at(vars(Total:Other.Female), ~as.numeric(gsub(",", "", .))) %>%
  rename(totalT = Total,
         whiteT = White.Total,
         blackT = Black.Total,
         otherT = Other.Total) %>%
  select(LEA, totalT, whiteT, blackT, otherT) %>%
  mutate_at(vars(whiteT:otherT), ~(round((./(totalT))*100, 2))) %>%
  mutate(yr = "2017-2018",
         lea_name = ifelse(LEA == "Baltimore City", paste(tolower(LEA), "public schools"),
                           paste(tolower(LEA), "county public schools"))) %>%
  select(-LEA)

MD <- right_join(MD, students1617 %>% filter(ST == "MD") %>%
                   mutate(lea_name = gsub("washingtion", "washington", lea_name)), by="lea_name")

### MI ###
MI <- MI_teachers_1718 %>%
  filter(StaffGroup == "Teachers" & LocationType == "District" & 
           ReportCategoryOverall == "Race/Ethnicity" & EntityType == "LEA District") %>%
  select(-SchoolYear, -DistrictName, -IsdCode, -IsdName, -BuildingName, -BuildingCode, 
         -LocationType, -EntityType, -StaffGroup, -FTE, -FTEPercent, -ReportCategoryOverall, -HeadCountPercent) %>%
  mutate(ReportCategory = case_when(
    ReportCategory == "American Indian or Alaska Native" ~ "aianT",
    ReportCategory == "Asian" ~ "asianT",
    ReportCategory == "Black, not of Hispanic origin" ~ "blackT",
    ReportCategory == "Hispanic" ~ "hispanicT",
    ReportCategory == "Two or More Races" ~ "multiT",
    ReportCategory == "Native Hawaiian or Other Pacific Islander" ~ "nhpiT",
    ReportCategory == "White, not of Hispanic origin" ~ "whiteT")) %>%
  spread(ReportCategory, HeadCount) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + nhpiT + multiT + whiteT,
         yr = "2017-2018",
         ST_LEAID = ifelse(nchar(DistrictCode) == 4, paste0("MI-0", DistrictCode), paste0("MI-", DistrictCode))) %>%
  mutate_at(vars(aianT:whiteT), funs(round((./(totalT))*100,2))) %>%
  select(-DistrictCode)

MI <- right_join(MI, students1718 %>% filter(ST == "MI"), by="ST_LEAID")

### MN ###
MN <- MN_teachers_1718 %>%
  distinct(District.Name, File.Folder.Number, .keep_all = TRUE) %>%
  mutate_at(vars(Hispanic..Latino:White), ~ ifelse(. == "Y", 1, 0)) %>%
  # sync up two differing counts
  mutate(race2 = ifelse(American.Indian.or.Alaskan.Native == "1", "American Indian or Alaskan Native", ""),
         race2 = ifelse(Black.or.African.American == "1", paste0(race2,"Black, Not of Hispanic Origin"), paste0(race2,"")),
         race2 = ifelse(Hispanic..Latino == "1", paste0(race2,"Hispanic"), paste0(race2,"")),
         race2 = ifelse(White == "1", paste0(race2,"White, Not of Hispanic Origin"), paste0(race2,"")),
         race2 = ifelse(Asian == "1" | Native.Hawaiian.or.Other.Pacific.Islander == "1", paste0(race2,"Asian or Pacific Islander"), paste0(race2,"")),
         
         race.ethnicity = ifelse(State.Ethnicity == "Asian or Pacific Islander" & race2 == "Asian or Pacific Islander" & Native.Hawaiian.or.Other.Pacific.Islander == 1, "nhpiT",
                                 ifelse(State.Ethnicity == "Asian or Pacific Islander" & race2 == "Asian or Pacific Islander" & Asian == 1, "asianT",
                                        ifelse(State.Ethnicity == "", "unknownT",
                                               ifelse(State.Ethnicity == race2, State.Ethnicity,
                                                      ifelse(State.Ethnicity == "Hispanic" | Hispanic..Latino == 1, "Hispanic",
                                                             ifelse(Hispanic..Latino + American.Indian.or.Alaskan.Native + Asian + Black.or.African.American + Native.Hawaiian.or.Other.Pacific.Islander + White > 1, "multiT",
                                                                    "unknownT")))))),
         lea_name = tolower(District.Name)) %>%
  group_by(Type, District, race.ethnicity) %>%
  count() %>%
  ungroup() %>%
  mutate(race.ethnicity = case_when(
    race.ethnicity == "American Indian or Alaskan Native" ~ "aianT",
    race.ethnicity == "Black, Not of Hispanic Origin" ~ "blackT",
    race.ethnicity == "Hispanic" ~ "hispanicT",
    race.ethnicity == "White, Not of Hispanic Origin" ~ "whiteT",
    race.ethnicity %in% c("unknownT", "multiT", "asianT", "nhpiT") ~ race.ethnicity)) %>%
  spread(race.ethnicity, n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  filter(unknownT / (aianT + asianT + blackT + hispanicT + multiT + whiteT + nhpiT + unknownT) < .1) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + multiT + whiteT + nhpiT,
         yr = "2017-2018",
         ST_LEAID = paste0("MN-", str_pad(Type, 2, pad = "0"), str_pad(District, 4, pad = "0"))) %>%
  mutate_at(vars(aianT:whiteT, unknownT), funs(round((./(totalT))*100,2))) %>%
  select(-Type, -District, -unknownT)

MN <- right_join(MN, students1718 %>% filter(ST == "MN"), by="ST_LEAID")

### MO ###
MO <- MO_teachers_1718 %>%
  rename(race = RACE...ETHNICITY) %>%
  filter(DISTRICT.COUNTY.CODE != "(blank)") %>%
  mutate(race = case_when(
    race == "ASIAN/PACIFIC ISLANDER" ~ "asianT",
    race == "INDIAN" ~ "aianT",
    race == "BLACK" ~ "blackT",
    race == "HISPANIC" ~ "hispanicT",
    race == "MULTI-RACE" ~ "multiT",
    race == "WHITE" ~ "whiteT")) %>%
  spread(race, TEACHER.COUNT) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = asianT + aianT + blackT + hispanicT  + multiT + whiteT,
         yr = "2017-2018",
         ST_LEAID = paste0("MO-", str_pad(DISTRICT.COUNTY.CODE, 6, pad = "0"))) %>%
  mutate_at(vars(aianT:whiteT), funs(round((./(totalT))*100)),2) %>%
  select(-DISTRICT.COUNTY.CODE, -DISTRICT.NAME)

MO <- right_join(MO, students1718 %>% filter(ST == "MO"), by="ST_LEAID")

### MS ###
MS <- MS_teachers_1718 %>%
  filter(D.Name != "") %>%
  group_by(Sch.Dist, Race) %>%
  summarise(Count = sum(Cnt)) %>%
  mutate(Race = case_when(
    Race == "AS" ~ "asianT",
    Race == "B" ~ "blackT",
    Race == "H" ~ "hispanicT",
    Race == "PI" ~ "nhpiT",
    Race == "TM" ~ "multiT",
    Race == "W" ~ "whiteT",
    is.na(Race) ~ "unknownT"
  ))  %>%
  spread(Race, Count) %>%
  ungroup() %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = asianT + blackT + hispanicT  + nhpiT + multiT + whiteT + unknownT,
         yr = "2017-2018",
         ST_LEAID = paste0("MS-", str_pad(Sch.Dist, 4, pad="0"))) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(asianT:nhpiT, whiteT), funs(round((./(totalT - unknownT))*100,2))) %>%
  select(-unknownT, -Sch.Dist)

MS <- right_join(MS, students1718 %>% filter(ST == "MS"), by="ST_LEAID")

### MT ###
# Montana reports ethnicity and race separately
MT_race <- MT_teachers_race1617 %>%
  mutate(RaceName = case_when(
    RaceName == "American Indian/Alaskan Native" ~ "aianT",
    RaceName == "Asian" ~ "asianT",
    RaceName == "Black or African American" ~ "blackT",
    RaceName == "Native Hawaiian or Pacific Islander" ~ "nhpiT",
    RaceName == "White, Non-Hispanic" ~ "whiteT",
    RaceName %in% c("NULL", "Unknown") ~ "unknownT")) %>%
  group_by(LEA, School.Code, RaceName) %>%
  summarise(RaceCount = sum(RaceCount)) %>%
  spread(RaceName, RaceCount)  %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + nhpiT + whiteT + unknownT)

MT_ethnic <- MT_teacher_ethnicity1617 %>%
  mutate(Hispanic.Code = case_when(
    Hispanic.Code == "Y" ~ "hispanicT",
    Hispanic.Code %in% c("N", "U") ~ "nonHispanicT")) %>%
  group_by(LEA, School.Code, Hispanic.Code) %>%
  summarise(Ethnicity.Count = sum(Ethnicity.Count)) %>%
  spread(Hispanic.Code, Ethnicity.Count)  %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalTH = hispanicT + nonHispanicT) 

MT <- merge(MT_race, MT_ethnic, by=c("LEA", "School.Code"), all = TRUE)

MT <- MT %>%
  mutate(unknownT = ifelse(totalTH > totalT, unknownT + (totalTH - totalT), unknownT),
         totalT = ifelse(totalTH > totalT, totalTH, totalT)) %>%
  filter(unknownT / totalT < .1) %>%
  mutate(
    # 95% of schools either report no aian/asian/black/nhpi or no hispanic, so reporting separately is not a huge issue
    # Dept of Ed indicated for white-hispanic teachers, they likely selected 'unknown' for race
    # so first we'll remove hispanic count from unknown. then we'll remove from white count.
    # for 5% of schools hispanics could be another race. here we made the assumption that they are white, the predominant hispanic race in MT 
    # hispanics are less than 3% of teachers
    # will tag as estimate on map
    remainder = unknownT - hispanicT,
    whiteT = ifelse(remainder < 0, whiteT + remainder, whiteT),
    unknownT = ifelse(remainder < 0, 0, 
                      ifelse(remainder >=0, unknownT - hispanicT, unknownT))) %>%
  select(LEA, aianT, asianT, blackT, nhpiT, whiteT, hispanicT, unknownT, totalT)

MT <- MT %>%
  group_by(LEA) %>%
  summarise_at(vars(aianT:totalT), ~sum(.)) %>%
  mutate_at(vars(aianT:hispanicT), funs(round((./(totalT - unknownT))*100)),2) %>%
  ungroup() %>%
  mutate(ST_LEAID = paste0("MT-", str_pad(LEA, 4, pad = "0"))) %>%
  select(-LEA, -unknownT) 

MT <- right_join(MT, students1617 %>% filter(ST == "MT"), by="ST_LEAID")

### NC ###
NC <- NC_teachers_1218 %>%
  filter(Position == "Teachers" & Year == "2018") %>%
  rename(whiteT = RaceWhite,
         blackT = RaceBlack,
         otherT = RaceOther,
         lea_name = LEA.Name) %>%
  mutate(lea_name = tolower(lea_name),
         yr = "2017-2018") %>%
  group_by(LEA) %>%
  summarise_at(vars(whiteT:otherT), ~(sum(.))) %>%
  mutate(totalT = whiteT + blackT + otherT) %>%
  mutate_at(vars(whiteT:otherT), ~(round((./(totalT))*100, 2))) %>%
  mutate(ST_LEAID = paste0("NC-", str_pad(LEA, 3, pad = "0"))) %>%
  select(-LEA)

NC <- right_join(NC, students1718 %>% filter(ST == "NC"), by="ST_LEAID")

### ND ###
ND <- ND_teachers_1718 %>%
  rename(totalT = Total, 
         aianT = Am.Ind, 
         asianT = Asian,
         blackT = Black, 
         hispanicT = Hispanic,
         nhpiT = Pacific, 
         whiteT = White) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(yr = "2017-2018", 
         ST_LEAID = paste0("ND-", gsub("-", "", DistrictStateIssuedID))) %>%
  select(-DistrictStateIssuedID, -SchoolYear, -Name) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./totalT)*100,2))) 

ND <- right_join(ND, students1718 %>% filter(ST == "ND"), by="ST_LEAID")

### NE ###
# NE provided school-level with no district information. pulling in district-school crosswalk
NE_crosswalk <- NE_district_school_match %>%
  distinct(SCHOOL_NAME, .keep_all = TRUE) %>%
  select(DISTRICT_NAME, SCHOOL_NAME)

NE <- NE_teachers_1718 %>%
  rename(aianT = AmIndian_AlaskaNative, 
         asianT = Asian,
         blackT = Black_AfricanAmerican,
         hispanicT = Hispanic,
         nhpiT = Hawaiian_PacificIslander,
         multiT = Multiple,
         whiteT = White,
         totalT = Total,
         SCHOOL_NAME = DistrictName) %>%
  merge(NE_crosswalk, by="SCHOOL_NAME") %>%
  group_by(DISTRICT_NAME) %>%
  summarise_at(vars(aianT:totalT), ~(sum(.))) %>% 
  mutate(yr = "2017-2018",
         lea_name = tolower(DISTRICT_NAME), 
         unknownT = totalT - (aianT + asianT + blackT + hispanicT + nhpiT + multiT + whiteT)) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(aianT:multiT), funs(round((./(totalT - unknownT))*100,2))) %>%
  select(-unknownT, -DISTRICT_NAME)

NE <- right_join(NE, students1718 %>% filter(ST == "NE"), by="lea_name")

### NJ ###
NJ <- NJ_teachers_1718 %>%
  filter(Teachers.Admins == "Teachers") %>%
  rename(whiteT = White, 
         hispanicT = Hispanic,
         blackT = Black.or.African.American,
         asianT = Asian, 
         aianT = American.Indian.or.Alaska.Native,
         nhpiT = Native.Hawaiian.or.Pacific.Islander,
         multiT = Two.or.More.Races) %>% 
  filter(whiteT != "N") %>%
  mutate(ST_LEAID = paste0("NJ-", CountyCode, DistrictCode)) %>%
  mutate_at(vars(whiteT:multiT), ~as.numeric(.)) %>%
  select(-CountyName, -DistrictName, -Teachers.Admins, -Female, -Male) %>%
  left_join(NJ_teacher_count %>% select(DistrictCode, CountyCode, TeacherCount_District), by=c("CountyCode", "DistrictCode")) %>%
  mutate(totalT = as.numeric(TeacherCount_District)) %>%
  select(-DistrictCode, -CountyCode, -TeacherCount_District)

NJ <- right_join(NJ, students1718 %>% filter(ST == "NJ"), by="ST_LEAID")

### NM ###
NM <- NM_teachers_1718 %>%
  filter(Location_Organization_Type_Code == "Public" & Assignment_Category == "Teacher") %>%
  mutate_at(vars(starts_with("TTL")), ~as.numeric(gsub(",", "", .))) %>%
  group_by(District_Code) %>%
  filter(row_number()==1) %>%
  summarise_at(vars(starts_with("TTL")), ~sum(.)) %>%
  select(District_Code, (ends_with("_Dist"))) %>%
  select(-TTL_AllStaff_Dist, -TTL_Male_Dist, -TTL_Female_Dist) %>%
  rename(aianT = TTL_RaceNAm_Dist,
         hispanicT = TTL_RaceHisp_Dist,
         whiteT = TTL_RaceWht_Dist,
         blackT = TTL_RaceBlk_Dist,
         asianT = TTL_RaceAsian_Dist,
         nhpiT = TTL_RaceHawaiin_Dist,
         multiT = TTL_RaceMulti_Dist,
         totalT = TTL_Cat_Dist) %>%
  mutate(unknownT = totalT - (aianT + asianT + blackT + hispanicT + nhpiT + whiteT + multiT)) %>%
  filter(unknownT/totalT < 0.1 & unknownT/totalT > -0.1) %>%
  mutate_at(vars(aianT:multiT), funs(round((./(totalT - unknownT))*100,2))) %>%
  mutate(ST_LEAID = paste0("NM-", str_pad(District_Code, 3, pad = "0"))) %>%
  select(-District_Code, -unknownT) 

NM <- right_join(NM, students1718 %>% filter(ST == "NM"), by="ST_LEAID")

### NV ###
NV <- NV_teachers_1718 %>%
  mutate_all(~trimws(.)) %>%
  group_by(School.District.Name) %>%
  count(Reported.Ethnicity.Race) %>%
  mutate(Reported.Ethnicity.Race = case_when(
    Reported.Ethnicity.Race == "AMERICAN OR ALASKAN NATIVE" ~ "aianT",
    Reported.Ethnicity.Race %in% c("ASIAN", "ASIAN PACIFIC ISLANDER (INACT)") ~ "asianT",
    Reported.Ethnicity.Race == "BLACK/AFRICAN AMERICAN" ~ "blackT",
    Reported.Ethnicity.Race == "HISPANIC/LATINO" ~ "hispanicT",
    Reported.Ethnicity.Race == "MULTIRACIAL/MULTIETHNIC" ~ "multiT",
    Reported.Ethnicity.Race == "NATIVE HAWAIIAN/PACF ISL" ~ "nhpiT",
    Reported.Ethnicity.Race == "WHITE" ~ "whiteT",
    Reported.Ethnicity.Race %in% c("UNKNOWN", "OTHER (INACTIVE)") ~ "unknownT"),
    lea_name = tolower(School.District.Name),
    lea_name = gsub(" school district", "", lea_name)) %>%
  group_by(lea_name, Reported.Ethnicity.Race) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  spread(Reported.Ethnicity.Race, n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + nhpiT + whiteT + unknownT,
         yr = "2017-2018") %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(aianT:nhpiT, whiteT), funs(round((./(totalT - unknownT))*100)),2) %>%
  select(-unknownT)

NV <- right_join(NV, students1718 %>% filter(ST == "NV") %>%
                   mutate(lea_name = gsub(" school district| county", "", lea_name)),
                 by="lea_name")

### NY ###
colnames(ny_teachers_1617) <- tolower(colnames(ny_teachers_1617))

# NY provides both district and school-level numbers.
# the district and school sums don't always match up
# speaking with research org, only use school numbers 

NY_school_sum <- ny_teachers_1617 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(ST_LEAID = paste0("NY-", str_pad(district_code, 12, pad = "0"))) %>%
  filter(current_year_status == "OPEN" & total_type == "PUBLIC BUILDING TOTAL") %>% 
  mutate(district_name = ifelse(grepl("NYC GEOG DIST", district_name), "NEW YORK CITY PUBLIC SCHOOLS", district_name),
         ST_LEAID = ifelse(district_name == "NEW YORK CITY PUBLIC SCHOOLS", "NY-310100010000", ST_LEAID)) %>%
  group_by(ST_LEAID) %>%
  summarise_at(vars(amer.indian.or.alaska.native:total), ~sum(.)) %>%
  rename(aianT = amer.indian.or.alaska.native, 
         asianT = asian, 
         blackT = black.or.african.american,
         hispanicT = hispanic.or.latino,
         whiteT = white,
         multiT = multi.racial,
         nhpiT = native.hawaiian.or.pacific.isl,
         totalT = total) %>%
  mutate(unknownT = totalT - (aianT + asianT + blackT + hispanicT + whiteT + multiT + nhpiT)) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(aianT:multiT), funs(round((./(totalT - unknownT))*100,2))) %>%
  select(-unknownT)

NY <- right_join(NY_school_sum, students1617 %>% filter(ST == "NY"), by="ST_LEAID")

### OH ###
OH <- OH_teachers_1718 %>%
  filter(ORG_TYP_SD == "Public District") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(blackT = Black.or.African.American..non.H,
         whiteT = White..non.Hispanic.,
         unknownT = Not.Specified,
         multiT = Multiracial,
         hispanicT = Hispanic.Latino,
         nhpiT = Native.Hawaiian.or.Other.Pacific,
         asianT = Asian,
         aianT = American.Indian.or.Alaskan.Nativ,
         yr = School.Year) %>%
  mutate(totalT = whiteT + blackT + hispanicT + asianT + aianT + multiT + nhpiT + unknownT,
         ST_LEAID = paste0("OH-", str_pad(District.IRN, 6, pad="0"))) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(blackT:whiteT, multiT:aianT), funs(round((./(totalT - unknownT))*100,2))) %>%
  select(-County, -ORG_TYP_SD, -District.IRN, -District.Name, -unknownT)

OH <- right_join(OH, students1718 %>% filter(ST == "OH"), by="ST_LEAID")

### OK ###
OK <- OK_teachers_1718 %>%
  filter(jobdesc == "TEACHER") %>%
  select(TeacherNumber:district_name, race_desc) %>%
  distinct(TeacherNumber, co, dist, .keep_all = TRUE) %>%
  mutate(ST_LEAID = paste0("OK-", str_pad(co, 2, pad="0"), "-", dist)) %>%
  group_by(ST_LEAID) %>%
  count(race_desc) %>%
  mutate(race_desc = case_when(
    race_desc == "American Indian/Alaskan Native" ~ "aianT",
    race_desc == "Asian" ~ "asianT",
    race_desc == "Black/African American" ~ "blackT",
    race_desc == "Hispanic" ~ "hispanicT",
    race_desc == "Native Hawaian/Pacific Islander" ~ "nhpiT",
    race_desc == "Multiple (two or more races)" ~ "multiT",
    race_desc == "Caucasian and Others" ~ "whiteT")) %>%
  spread(race_desc, n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT  + nhpiT + multiT + whiteT,
         yr = "2017-2018") %>%
  mutate_at(vars(aianT:whiteT), funs(round((./(totalT))*100,2)))

OK <- right_join(OK, students1718 %>% filter(ST == "OK"), by="ST_LEAID")

### OR ###
OR <- OR_teachers_1718 %>%
  rename(lea_name = District.Name, 
         asianT = Asian...Not.Hispanic,
         blackT = Black.African.American...Not.Hispanic,
         hispanicT = Hispanic.Latino,
         aianT = American.Indian.Alaskan.Native...Not.Hispanic,
         multiT = Multi.Racial...Not.Hispanic,
         nhpiT = Native.Hawaiian.Pacific.Islander...Not.Hispanic,
         whiteT = White...Not.Hispanic) %>%
  mutate(lea_name = tolower(lea_name)) %>%
  group_by(lea_name) %>%
  summarise_at(vars(asianT:whiteT), funs(sum(.))) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT  + nhpiT + multiT + whiteT,
         yr = "2017-2018") %>%
  mutate_at(vars(asianT:whiteT), funs(round((./(totalT))*100)),2)

OR <- right_join(OR, students1718 %>% filter(ST == "OR"), by="lea_name")

### PA ###
PA <- PA_teachers_1718 %>%
  filter(LEAType == "SD") %>%
  mutate(Race = case_when(
    Race == "American Indian / Alaskan Native" ~ "aianT",
    Race  == "Asian" ~ "asianT",
    Race  == "Black or African American" ~ "blackT",
    Race  == "Hispanic" ~ "hispanicT",
    Race  == "Native Hawaiian or other Pacific Islander(not hispanic)" ~ "nhpiT",
    Race  == "Multi-Racial" ~ "multiT",
    Race  == "White" ~ "whiteT")) %>%
  spread(Race, HeadCount) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT  + nhpiT + multiT + whiteT,
         yr = "2017-2018",
         lea_name = tolower(LEAName)) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./totalT)*100,2))) %>%
  select(-LEAType, -SY, -AUN, -LEAName)

PA <- right_join(PA, students1718 %>% filter(ST == "PA"), by="lea_name")

### RI ###
RI <- RI_teacher_1718 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(hispanicT = Hispanic,
         aianT = NativeAmerican,
         asianT = Asian,
         blackT = Black,
         nhpiT = Pacific.Islander_Hawaiin,
         multiT = TwoOrMore,
         whiteT = White,
         unknownT = RaceNotReported) %>%
  mutate(totalT = aianT + asianT + blackT + nhpiT + multiT + whiteT + unknownT,
         lea_name = tolower(LEAName)) %>%
  filter(unknownT / totalT < .1) %>%  
  mutate(unknownTH = unknownT - hispanicT) %>%
  mutate_at(vars(hispanicT, aianT:whiteT), ~(round((./(totalT - unknownTH))*100,2))) %>%
  select(-LEACode, -LEAName, -unknownT, -unknownTH, -NotHispanic, -EthnicityNotReported)

RI <- right_join(RI, students1718 %>% filter(ST == "RI"), by="lea_name")

### SC ###
SC <- SC_teachers_1718 %>%
  mutate(ST_LEAID = paste0("SC-", DISTRICT.ID),
         DISTRICT.ID = as.numeric(DISTRICT.ID),
         yr = "2017-2018") %>%
  filter(!(DISTRICT.ID %in% c(9999, NA))) %>%
  mutate_at(vars(WHITE.MALES:TOTAL.NUMBER.OF.TEACHERS), ~as.numeric(gsub(",", "",.))) %>%
  mutate(whiteT = WHITE.MALES + WHITE.FEMALES + WHITE.GENDER.NOT.REPORTED,
         blackT = BLACK.MALES + BLACK.FEMALES + BLACK.GENDER.NOT.REPORTED,
         asianT = ASIAN.MALES + ASIAN.FEMALES + ASIAN.GENDER.NOT.REPORTED,
         hispanicT = HISPANIC.MALES + HISPANIC.FEMALES + HISPANIC.GENDER.NOT.REPORTED,
         aianT = INDIAN.MALES + INDIAN.FEMALES + INDIAN.GENDER.NOT.REPORTED,
         unknownT = MALE.RACE.NOT.REPORTED + FEMALE.RACE.NOT.REPORTED + RACE.AND.GENDER.NOT.REPORTED,
         totalT = TOTAL.NUMBER.OF.TEACHERS) %>%
  select(ST_LEAID, yr:totalT) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(whiteT:aianT), ~(round((./(totalT - unknownT)*100),2))) %>%
  select(-unknownT)

SC <- right_join(SC, students1718 %>% filter(ST == "SC") %>%
                   mutate(lea_name = gsub(" 0", " ", lea_name)),
                 by="ST_LEAID")

### SD ###
SD <- SD_teachers_1718 %>%
  filter(!(is.na(District.Number))) %>%
  rename(asianT = Asian,
         blackT = Black, 
         nhpiT = Pac..Islander,
         aianT = Native.American,
         whiteT = White,
         hispanicT = Hispanic,
         multiT = Multiple.Races,
         totalT = Total.Head.Count.Race.Ethnicity) %>%
  mutate(ST_LEAID = paste0("SD-", str_pad(District.Number, 5, pad="0"))) %>%
  mutate_at(vars(asianT:multiT), ~(round((./totalT)*100,2))) %>%
  select(-District.Name, -District.Number)

SD <- right_join(SD, students1718 %>% filter(ST == "SD"), by="ST_LEAID")

### TN ###
TN <- TN_teachers_1617 %>%
  filter(!is.na(District.Number) & Staff.Type == "Teacher") %>%
  rename(aianT = American.Indian.or.Alaska.Native,
         asianT = Asian, 
         blackT = Black.or.African.American,
         hispanicT = Hispanic.or.Latino,
         nhpiT = Native.Hawaiian.or.Other.Pacific.Islander,
         multiT = Two.or.More, 
         whiteT = White,
         unknownT = Unidentified) %>%
  mutate_at(vars(aianT:unknownT), ~as.numeric(gsub("%", "", .))) %>%
  mutate(yr = "2016-2017",
         District.Name = gsub(" Municipal Schools| Public Schools| City Schools| Special School District| Schools","", District.Name),
         lea_name = tolower(District.Name)) %>%
  select(-District.Name, -Staff.Type) %>%
  # TN only provided proportions, so bringing in separate teacher headcount data
  left_join(TN_teacher_count %>% select(DISTRICT, TEACHERS) %>% rename(District.Number = DISTRICT), by="District.Number") %>%
  mutate(totalT = as.numeric(TEACHERS)) %>%
  filter(unknownT < 10) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./(100 - unknownT))*100,2))) %>%
  select(-District.Number, -TEACHERS, -unknownT)

TN <- right_join(TN, students1617 %>% filter(ST == "TN"), by="lea_name")

### TX ###
TX <- TX_teachers_1718 %>%
  select(DISTRICT, ETHNICX, TOTAL_TEACHER_HEAD_COUNT) %>%
  mutate(ETHNICX = case_when(
    ETHNICX == "American Indian or Alaska Nat" ~ "aianT",
    ETHNICX == "Asian" ~ "asianT",
    ETHNICX == "Black or African American" ~ "blackT",
    ETHNICX == "Hispanic/Latino" ~ "hispanicT",
    ETHNICX == "Native Hawaiian/Other Pacific" ~ "nhpiT",
    ETHNICX == "Two or more races" ~ "multiT",
    ETHNICX == "White" ~ "whiteT")) %>%
  spread(ETHNICX, TOTAL_TEACHER_HEAD_COUNT) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT  + nhpiT + multiT + whiteT,
         yr = "2017-2018",
         ST_LEAID = paste0("TX-", str_pad(DISTRICT, 6, pad="0"))) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./totalT)*100,2))) %>%
  select(-DISTRICT)

TX <- right_join(TX, students1718 %>% filter(ST == "TX"), by="ST_LEAID")

### VA ###
VA <- VA_teachers_1718 %>%
  filter(!(is.na(totalT))) %>%
  mutate(leaid = as.character(LEAID)) %>%
  select(-ST, -LEA_NAME, -ST_LEAID, -LEAID)  %>%
  rename(yr = year)

# VA was collected district-by-district so it's not all a single year
VA16 <- VA %>% 
  filter(yr == "2016-17") %>%
  left_join(students1617 %>% filter(ST == "VA"), by="leaid") %>%
  select(-d_Locale)

VA17plus <- VA %>% 
  filter(yr != "2016-17") %>%
  right_join(students1718 %>% filter(ST == "VA"), by="leaid") %>%
  filter(!(leaid %in% VA16$leaid))

VA <- rbind(VA16, VA17plus)

### WI ###
WI <- WI_teachers_1718 %>%
  filter(Work.Agency.Type == "03 - Public school district") %>%
  distinct(Entity.ID, Assignment.Work.Agency, .keep_all = TRUE) %>%
  group_by(Assignment.Work.Agency, RaceEthnicity) %>%
  count() %>%
  ungroup() %>%
  mutate(RaceEthnicity = case_when(
    grepl("I -", RaceEthnicity) ~ "aianT",
    grepl("A -", RaceEthnicity) ~ "asianT",
    grepl("B -", RaceEthnicity) ~ "blackT",
    grepl("H -", RaceEthnicity) ~ "hispanicT",
    grepl("P -", RaceEthnicity) ~ "nhpiT",
    grepl("T -", RaceEthnicity) ~ "multiT",
    grepl("W -", RaceEthnicity) ~ "whiteT")) %>%
  spread(RaceEthnicity, n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT  + nhpiT + multiT + whiteT,
         yr = "2017-2018",
         lea_name = trimws(gsub(".*\\ -", "", tolower(Assignment.Work.Agency)))) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./totalT)*100,2))) %>%
  select(-Assignment.Work.Agency)

WI <- right_join(WI, students1718 %>% filter(ST == "WI"), by="lea_name")

### WA ###
WA <- WA_teachers_1718 %>%
  mutate(OrganizationName = tolower(OrganizationName)) %>%
  filter(grepl("school district|public schools", OrganizationName)) %>%
  select(Number, OrganizationCode,RaceEthnicity) %>%
  mutate(RaceEthnicity = case_when(
    RaceEthnicity == "American Indian/Alaskan Native" ~ "aianT",
    RaceEthnicity  == "Asian" ~ "asianT",
    RaceEthnicity  == "Black/African American" ~ "blackT",
    RaceEthnicity == "Hispanic/Latino of any race(s)" ~ "hispanicT",
    RaceEthnicity  == "Native Hawaiian/Other Pacific Islander" ~ "nhpiT",
    RaceEthnicity  == "Two or More Races" ~ "multiT",
    RaceEthnicity  == "White" ~ "whiteT",
    RaceEthnicity  == "Not Provided" ~ "unknownT")) %>%
  spread(RaceEthnicity, Number) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(totalT = aianT + asianT + blackT + hispanicT + nhpiT + multiT + whiteT + unknownT,
         yr = "2017-2018",
         ST_LEAID = paste0("WA-", str_pad(OrganizationCode, 5, pad="0"))) %>%
  filter(unknownT / totalT < .1) %>%
  mutate_at(vars(aianT:nhpiT, whiteT), ~(round((./(totalT - unknownT))*100,2))) %>%
  select(-unknownT, -OrganizationCode)

WA <- right_join(WA, students1718 %>% filter(ST == "WA"), by="ST_LEAID")

### WV ###
WV <- WV_teachers_1718 %>%
  mutate(hispanicT = Hispanic.Female + Hispanic.Males,
         whiteT = White.Male + White.Female,
         blackT = Black.Male + Black.Female,
         asianT = Asian.Male + Asian.Female,
         nhpiT = Pacific.Male + Pacific.Female,
         aianT = American.Indian.Male + American.Indian.Male,
         multiT = Multi.Race.Male + Multi.Race.Female,
         totalT = Male.Female.Total,
         lea_name = paste(tolower(trimws(COUNTY.NAME)), "county schools"),
         lea_name = gsub("monogalia", "monongalia", lea_name)) %>%
  select(lea_name, hispanicT:totalT) %>%
  mutate_at(vars(hispanicT:multiT), ~(round((./totalT)*100,2)))

WV <- right_join(WV, students1718 %>% filter(ST == "WV") %>% mutate(lea_name = gsub("  ", " ", lea_name)), by="lea_name")

### WY ###
WY <- WY_teachers_1718 %>%
  filter(DISTRICT_ID != "STATE") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_at(vars(aianT:whiteT), ~(round((./totalT)*100,2))) %>%
  mutate(yr = "2017-2018",
         lea_name = tolower(lea_name),
         lea_name = gsub(" #", " county school district #", lea_name)) %>%
  select(-DISTRICT_ID)

WY <- right_join(WY, students1718 %>% filter(ST == "WY") %>%
                   mutate(lea_name = gsub("# ", "#", lea_name)),
                 by="lea_name")

########################
### All together now ###
########################
teacher_student <- bind_rows(AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IN, IL, KS, KY, LA, MA, MD,
                             MI, MN, MO, MS, MT, NC, NE, ND, NJ, NM, NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, 
                             TX, VA, WA, WI, WV, WY)

# clean up data to account for different state reporting categories
teacher_student <- teacher_student %>%
  mutate(multiT = ifelse(ST %in% c("AL", "DC", "HI", "MD", "NC", "ND", "SC"), NA, multiT),
         nhpiT = ifelse(ST %in% c("AL", "DC", "GA", "MD", "NC", "SC", "GA"), NA, nhpiT),
         aianT = ifelse(ST %in% c("AL", "MD", "MS", "NC"), NA, aianT),
         asianT = ifelse(ST %in% c("MD", "NC"), NA, asianT),
         hispanicT = ifelse(ST %in% c("MD", "NC"), NA, hispanicT),
         
         otherS = ifelse(ST == "AL", nhpiS + multiS + aianS,
                         ifelse(ST %in% c("MD", "NC"), nhpiS + multiS + aianS + asianS + hispanicS, 
                                ifelse(ST == "HI", multiS + asianS,  
                                       ifelse(leaid == "5100090", aianS + nhpiS, NA)))),
         
         asianS = ifelse(ST == "MO" | leaid == "5102640" | leaid == "0404720", asianS + nhpiS, asianS),
         
         multiS = ifelse(ST %in% c("AL","HI", "MD", "NC"), NA, multiS),
         nhpiS = ifelse(ST %in% c("AL", "MD", "NC", "MO") | leaid %in% c("5102640","5100090", "0404720"), NA, nhpiS),
         aianS = ifelse(ST %in% c("AL", "MD", "NC") | leaid == "5100090", NA, aianS),
         asianS = ifelse(ST %in% c("MD", "NC", "HI"), NA, asianS),
         hispanicS = ifelse(ST %in% c("MD", "NC"), NA, hispanicS),
         
         # add a POC group
         pocT = round(100 - whiteT, 1),
         pocS = round(100 - whiteS, 1)) %>% 
  # NY group confirmed these districts are not accurate
  filter(!(leaid %in% c("3620730", "3611640")))

# approximate coverage
(teacher_student %>% filter(!is.na(whiteT)) %>% summarise(totalS = sum(totalS)))/sum(students1718$totalS)

# teacher-student ratio and total teacher filter
teacher_student <- teacher_student %>%
  mutate(ts_ratio = totalS / totalT) %>%
  filter(ts_ratio < 25 & ts_ratio > 2) %>%
  filter(totalT > 3)

# clean up LEA_NAME capitalization issues
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), tolower(substring(c, 2)),
        sep="", collapse=" ")
}

MoStr <- function(y) {
  c <- strsplit(y, "R-")[[1]]
  paste(c[1], toupper(c[2]),
        sep="R-", collapse="-")
}

bad_caps <- teacher_student %>%
  filter(LEA_NAME == tolower(LEA_NAME) | LEA_NAME == toupper(LEA_NAME))

bad_caps$LEA_NAME <- sapply(bad_caps$LEA_NAME, CapStr)

bad_caps <- bad_caps %>%
  mutate(LEA_NAME = trimws(gsub("(^|\\p{P})\\s*(.)", "\\1\\U\\2", LEA_NAME, perl=T)), 
         LEA_NAME = gsub("'S", "'s", LEA_NAME),
         LEA_NAME = gsub("\\.", ". ", LEA_NAME),
         
         LEA_NAME = ifelse(ST == "TX", gsub("Isd", "ISD", LEA_NAME), LEA_NAME),
         
         LEA_NAME = ifelse(ST == "IL", gsub(" Cusd ", " CUSD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Ccsd ", " CCSD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Hsd ", " HSD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Sd ", " SD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Esd ", " ESD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Cud ", " CUD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Ud ", " UD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Ed ", " ED ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Psd ", " PSD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" E Cons D ", " ECD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Cesd ", " CESD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Chsd ", " CHSD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Usd ", " USD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Gsd ", " GSD ", LEA_NAME), LEA_NAME),
         LEA_NAME = ifelse(ST == "IL", gsub(" Cisd ", " CISD ", LEA_NAME), LEA_NAME),
         
         LEA_NAME = ifelse(ST == "MO" & grepl("R-", LEA_NAME), sapply(LEA_NAME, MoStr), LEA_NAME),
         LEA_NAME = ifelse(leaid == "2932310", gsub("OF WARREN CO", "Of Warren Co", LEA_NAME), LEA_NAME))

teacher_student <- teacher_student %>% filter(!(leaid %in% bad_caps$leaid))

teacher_student <- rbind(teacher_student, bad_caps)
