setwd("/Users/rabinowitzk/Documents/Projects/2019/teacher-diversity/data-formatted")
require(tidyverse)

ts_sN <- teacher_student %>%
  select(LEA_NAME, leaid, ST, yr, aianN:whiteN) %>%
  gather(race, pop, aianN:whiteN) %>%
  mutate(race = gsub("N", "", race))
  
ts_sP <- teacher_student %>%
  select(LEA_NAME, leaid, ST, yr, totalS, aianS:whiteS) %>%
  gather(race, propS, aianS:whiteS) %>%
  mutate(race = gsub("S", "", race))
  
ts_tP <- teacher_student %>%
  select(LEA_NAME, leaid, ST, yr, totalT, whiteT:nhpiT) %>%
  gather(race, propT, whiteT:nhpiT) %>%
  mutate(race = gsub("T", "", race))

ts_long <- merge(ts_sN, ts_sP, by = c("LEA_NAME", "leaid", "ST", "yr", "race"), all = TRUE)
ts_long <- merge(ts_long, ts_tP, by = c("LEA_NAME", "leaid", "ST", "yr", "race"), all = TRUE)

# weighted mean by race for districts
ts_long %>% 
  group_by(race) %>%
  summarise(meanT = weighted.mean(propT, totalT, na.rm = TRUE),
            meanS = weighted.mean(propS, totalS, na.rm = TRUE)) 

# gaps 
gaps <- ts_long %>%
  mutate(gap = propS - propT,
         gap_ind = ifelse(gap <= 0, 1, 0))

# proportion of students in districts with more/less teachers of their race
gaps %>% 
  filter(!(is.na(gap_ind))) %>%
  group_by(race, gap_ind) %>% 
  summarise(pop = sum(pop, na.rm = TRUE)) %>% 
  mutate(prop = pop/sum(pop)) 

# gap grouping
gaps <- gaps %>%
  mutate(gap_grp = case_when(
    gap < 0 ~ "No Gap",
    gap < 10 ~ "Gap under 10%",
    gap < 25 ~ "Gap between 10-25%",
    gap < 50 ~ "Gap between .25-50%",
    gap < 100 ~ "Gap between ..50-100%"))

# proportion of students in districts with more/less teachers of their race, grouped
gaps %>%
  filter(!(is.na(gap_grp))) %>%
  filter(race %in% c("white", "black", "hispanic", "asian")) %>%
  group_by(race, gap_grp) %>%
  summarise(students = sum(pop)) %>%
  group_by(race) %>%
  mutate(propStudent = (students/sum(students))*100)


# students with low prop of X race teachers
gaps <- gaps %>%
  mutate(low_prop = ifelse(propT < 5, 1, 0))

gaps %>%
  filter(!(is.na(gap_grp))) %>%
  filter(race %in% c("white", "black", "hispanic", "asian")) %>%
  group_by(race, low_prop) %>%
  summarise(students = sum(totalS)) %>%
  group_by(race) %>%
  mutate(propStudent = (students/sum(students))*100)

# black/hispanic low prop and all white
teacher_student %>%
  mutate(noBlHi = ifelse(blackT < 5 | hispanicT < 5, 1, 0)) %>%
  filter(!is.na(hispanicT)) %>% 
  group_by(noBlHi) %>% 
  summarise(pop = sum(totalS)) %>% 
  ungroup() %>% 
  mutate(prop = pop/sum(pop))
  
teacher_student %>%
  mutate(allWhite = ifelse(whiteT == 100, 1, 0)) %>%
  group_by(allWhite) %>% 
  summarise(pop = sum(totalS)) %>% 
  ungroup() %>% 
  mutate(prop = pop/sum(pop))    

# scatters
ggplot(ts_long %>% filter(pop != 0 & race %in% c("white", "black", "hispanic", "asian")), aes(x=propS, y=propT)) + 
  geom_point(shape=21, aes(size = pop), fill = "#77cdd5", color = "white", alpha = 0.7, stroke = 0.5) +
  scale_size_area(limits = c(0, 475000), max_size = 20) +
  scale_x_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) + 
  scale_y_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) + 
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal() +
  theme(legend.position = "none",
        aspect.ratio=1,
        panel.grid.minor = element_blank()) +
  coord_fixed() + 
  facet_wrap(~race)

### title I funds ###
# pull in data on title I funds per school district 
# calculate title I funds per student
finances <- Sdf16_1a.4 %>%
  filter(MEMBERSCH > 0 & C14 >= 0) %>%
  mutate(leaid = str_pad(LEAID, 7, pad = "0"),
         titleIPP = C14/MEMBERSCH) %>%
  select(leaid, MEMBERSCH, C14, titleIPP)

ts_fin <- left_join(teacher_student, finances, by = "leaid")
summary(ts_fin$titleIPP)

# high defined as top 25%
ts_fin <- ts_fin %>%
  mutate(highTitleI = ifelse(titleIPP > 324.6, 1, 0),
         receivesTitleI = ifelse(titleIPP > 0, 1, 0))

# student and teacher demo by high title I
ts_fin %>% filter(!is.na(pocT)) %>% 
  group_by(highTitleI) %>%
  filter(!(is.na(highTitleI))) %>%
  summarise(mean_pocT = weighted.mean(pocT, totalT),
            mean_pocS = weighted.mean(pocS, totalS),
            cont = n())