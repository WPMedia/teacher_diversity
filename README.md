[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

# About this story

**[America’s schools are more diverse than ever. But the teachers are still mostly white.](https://www.washingtonpost.com/graphics/2019/local/education/teacher-diversity/)**

Minority students, particularly Latinos, are on the rise, but few of their classes are taught by teachers of the same race.

# About the folders in this repo

* **[data folder](https://github.com/WPMedia/teacher_diversity/tree/master/data)**
  - [data_notes.csv](https://github.com/WPMedia/teacher_diversity/blob/master/data/data_notes.csv) State-specific notes on teacher race and ethnicity data including nonstandard race reporting and links to original data where it exists. 
  - [raw_teacher_race_ethnicity.csv](https://github.com/WPMedia/teacher_diversity/blob/master/data/raw_teacher_race_ethnicity.csv) The data received from states and districts with minimal cleaning -- and no filtering -- beyond the standardization necesssary to make this national file. Please read data notes if you intend to work with this data. 
   - [teacher_student_race_ethnicity.csv](https://github.com/WPMedia/teacher_diversity/blob/master/data/teacher_student_race_ethnicity.csv) A cleaned up national file used for this analysis that includes the share of teachers (columns appended with "T") and students (columns appended with "S") by race and ethnicity for each school district, along with a student count (columns appended with "N").
   
   LEA stands for Local Education Authority, or school district. National (leaid) and state-level (st_leaid) identification numbers are included.  
  

* **[analysis folder](https://github.com/WPMedia/teacher_diversity/tree/master/analysis)** 
  - [read-n-clean](https://github.com/WPMedia/teacher_diversity/blob/master/analysis/read-n-clean.R) Reads in and cleans data state-by-state and creates a single national file.
  - [teacher-analysis](https://github.com/WPMedia/teacher_diversity/blob/master/analysis/teacher-analysis.R) Data analysis on national file

# Source data

For 44 states and the District of Columbia, teacher data was obtained from each state’s Department of Education. Teacher data for Arizona and Virginia was obtained from school districts. Maine, New Hampshire, Vermont and Utah were unable to provide reliable data, or any data at all. The collected teacher data covers districts that serve 94 percent of American students. 

Student data relies on the [Common Core of Data](https://nces.ed.gov/ccd/ccddata.asp) from the National Center for Education Statistics. 


Additional details for this analysis can be found in the [About this story](https://www.washingtonpost.com/graphics/2019/local/education/teacher-diversity/) section of the story. 

# Guidelines for using data

If you publish an online story, graphic, map or other piece of journalism based on this data set, please credit The Washington Post, link to the [original source](https://www.washingtonpost.com/graphics/2019/local/school-diversity-data/), and [send us an email](mailto:maria.sanchezdiez@washpost.com) when you’ve hit publish. We want to learn what you discover and will attempt to link to your work as part of cataloguing the impact of this project.

# Contact information
Please reach out to Kate Rabinowitz at kate.rabinowitz@washpost.com with any questions. 


