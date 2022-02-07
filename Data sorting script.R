## SET LIBRARY PATH ############################################################
#set library to avoid Onedrive:
.libPaths("C:/R library")
.libPaths()
#now should show as: "C:/R library".


## INSTALL PACKAGES AND ORGANISE LIBRARY #######################################
if(!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)
if(!require("readxl")) {install.packages("readxl")}
library(readxl)


## IMPORT DATA #################################################################
mainframe.df <- read_excel("Sample Data for UCLan BSc (v.1.1).xlsx",
                           sheet = "Collisions & Casualties",
                           col_names = TRUE)

mainframe.df <- mainframe.df %>%
  group_by(`Collision Reference`) %>%
  slice(1) %>%
  arrange(`Collision Date`)

student_data.df <- mainframe.df %>%
  select(`Collision Reference`, `Collision Day`, `Collision Time`, `Road Class`,
         `Road Type Decode`, `Speed Limit`, `Road Surface Conditions Decode`, `Weather Conditions Decode`,
         `Light Conditions Decode`)


## IMPORT CONTRIB FACTORS DATA ###########################################################
Contrib_factors.df <- read_excel("Sample Data for UCLan BSc (v.1.1).xlsx",
                           sheet = "Contributory Factors",
                           col_names = TRUE)

Contrib_factors.df$`Contributory Factor Decode` %>%
  as.factor() %>%
  levels() #provides list of factors

#Impaired by alcohol or drugs:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Impared by alcohol or drugs` = case_when(
      Contrib_factors.df$`Contributory Factor Decode` == "Impaired by alcohol" |
      Contrib_factors.df$`Contributory Factor Decode` == "Impaired by drugs (illicit or medicinal)" 
      ~ 1,
      TRUE ~ 0))

#Loss of control vehicle/self:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Loss of control vehicle/self` = case_when(
    Contrib_factors.df$`Contributory Factor Decode` == "Loss of control" |
    Contrib_factors.df$`Contributory Factor Decode` == "Nervous  uncertain or panic" |
    Contrib_factors.df$`Contributory Factor Decode` == "Sudden braking" |
    Contrib_factors.df$`Contributory Factor Decode` == "Swerved"
    ~ 1,
    TRUE ~ 0))

#Not adhering to road/vehicle requirements:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Not adhering to road/vehicle requirements` = case_when(
    Contrib_factors.df$`Contributory Factor Decode` == "Disobeyed Give Way or Stop sign or markings" |
    Contrib_factors.df$`Contributory Factor Decode` == "Disobeyed automatic traffic signal" |
    Contrib_factors.df$`Contributory Factor Decode` == "Disobeyed double white lines" |
    Contrib_factors.df$`Contributory Factor Decode` == "Disobeyed pedestrian crossing facility" |
    Contrib_factors.df$`Contributory Factor Decode` == "Exceeding speed limit" |
    Contrib_factors.df$`Contributory Factor Decode` == "Failed to signal or misleading signal" |
    Contrib_factors.df$`Contributory Factor Decode` == "Following too close" |
    Contrib_factors.df$`Contributory Factor Decode` == "Illegal turn or direction of travel" |
    Contrib_factors.df$`Contributory Factor Decode` == "Not displaying lights at night or in poor visibility" |
    Contrib_factors.df$`Contributory Factor Decode` == "Pedestrian wearing dark clothing at night" |
    Contrib_factors.df$`Contributory Factor Decode` == "Too close to cyclist  horse rider or pedestrian"
    ~ 1,
    TRUE ~ 0))

#Other:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Other` = case_when(
    Contrib_factors.df$`Contributory Factor Decode` == "Defective brakes" |
    Contrib_factors.df$`Contributory Factor Decode` == "Emergency vehicle on a call" |
    Contrib_factors.df$`Contributory Factor Decode` == "Illness or disability  mental or physical" |
    Contrib_factors.df$`Contributory Factor Decode` == "Tyres illegal  defective or under-inflated" |
    Contrib_factors.df$`Contributory Factor Decode` == "Unfamiliar with model of vehicle" |
    Contrib_factors.df$`Contributory Factor Decode` == "Junction overshoot" |
    Contrib_factors.df$`Contributory Factor Decode` == "Junction restart (moving off at junction)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Learner or inexperienced driver/rider" |
    Contrib_factors.df$`Contributory Factor Decode` == "Other - please specify below"
    ~ 1,
    TRUE ~ 0))

#Reckless/negligent driving:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Reckless/negligent driving` = case_when(
    Contrib_factors.df$`Contributory Factor Decode` == "Aggressive driving" |
    Contrib_factors.df$`Contributory Factor Decode` == "Careless  reckless or in a hurry" |
    Contrib_factors.df$`Contributory Factor Decode` == "Dangerous action in carriageway (eg. playing)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Poor turn or manoeuvre" |
    Contrib_factors.df$`Contributory Factor Decode` == "Stolen vehicle" |
    Contrib_factors.df$`Contributory Factor Decode` == "Travelling too fast for conditions" |
    Contrib_factors.df$`Contributory Factor Decode` == "Vehicle door opened or closed negligently" |
    Contrib_factors.df$`Contributory Factor Decode` == "Vehicle in course of crime" |
    Contrib_factors.df$`Contributory Factor Decode` == "Vehicle travelling along pavement"
    ~ 1,
    TRUE ~ 0))

#Road/evironmental conditions:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Road/evironmental conditions` = case_when(
    Contrib_factors.df$`Contributory Factor Decode` == "Animal or object in carriageway" |
    Contrib_factors.df$`Contributory Factor Decode` == "Buildings  road signs  street furniture" |
    Contrib_factors.df$`Contributory Factor Decode` == "Crossing road masked by stationary or parked vehicle" |
    Contrib_factors.df$`Contributory Factor Decode` == "Dazzling headlights" |
    Contrib_factors.df$`Contributory Factor Decode` == "Dazzling sun" |
    Contrib_factors.df$`Contributory Factor Decode` == "Defective traffic signals" |
    Contrib_factors.df$`Contributory Factor Decode` == "Deposit on road (eg. oil  mud  chippings)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Rain  sleet  snow or fog" |
    Contrib_factors.df$`Contributory Factor Decode` == "Road layout (eg. bend  hill  narrow carriageway)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Road layout (eg. bend  winding road  hill crest)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Slippery road (due to weather)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Stationary or parked vehicle(s)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Temporary road layout (eg. contraflow)" |
    Contrib_factors.df$`Contributory Factor Decode` == "Vegetation" |
    Contrib_factors.df$`Contributory Factor Decode` == "Inadequate or masked signs or road markings" |
    Contrib_factors.df$`Contributory Factor Decode` == "Vehicle blind spot" 
    ~ 1,
    TRUE ~ 0))

#Undue care and attention:
Contrib_factors.df <- Contrib_factors.df %>%
  mutate(`Undue care and attention` = case_when(
    Contrib_factors.df$`Contributory Factor Decode` == "Cyclist entering road from pavement" |
    Contrib_factors.df$`Contributory Factor Decode` == "Distraction in vehicle" |
    Contrib_factors.df$`Contributory Factor Decode` == "Failed to judge other person's path or speed" |
    Contrib_factors.df$`Contributory Factor Decode` == "Failed to judge vehicle's path or speed" |
    Contrib_factors.df$`Contributory Factor Decode` == "Distraction outside vehicle" |
    Contrib_factors.df$`Contributory Factor Decode` == "Failed to look properly" |
    Contrib_factors.df$`Contributory Factor Decode` == "Fatigue"
    ~ 1, 
    TRUE ~ 0))

#use below code to summarise across all main categories - just add the others
a <- Contrib_factors.df %>%
  group_by(`Collision Reference`) %>%
  summarise("Impared by alcohol or drugs" = max(`Impared by alcohol or drugs`), 
            "Loss of control vehicle/self" = max(`Loss of control vehicle/self`),
            "Not adhering to road/vehicle requirements" = max(`Not adhering to road/vehicle requirements`),
            "Other" = max(Other), 
            "Reckless/negligent driving"= max(`Reckless/negligent driving`),
            "Road/evironmental conditions" = max(`Road/evironmental conditions`), 
            "Undue care and attention" = max(`Undue care and attention`))
glimpse(a)
glimpse(student_data.df)

#For some reason Collision Reference has changed capitalisation. Whatever;
#Uppercase on a
a$`Collision Reference` <- toupper(a$`Collision Reference`)

full_student_data.df <- left_join(student_data.df, a, by = "Collision Reference")

glimpse(full_student_data.df)

View(full_student_data.df)

#Leave in missing data and <lecturer> can decide how to present it to the class

#write to excel:
write_excel_csv(x = full_student_data.df, file = "Student Roads Data.csv")
