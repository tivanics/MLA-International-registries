#UK data
#Donor type of allograft
ukfull$DTYPE <- 
  recode(ukfull$DTYPE,
         c(1 ~ 1, #"Deceased heartbeating"
           2 ~ 2, #"Non-heartbeating"
           5 ~ 5, #"Living related" 
           6 ~ 6, #"Living unrelated", 
           7 ~ 7, #"Domino", 
           9 ~ 9, #"Donor type unknown"
           13 ~ 13, #"Living unrelated - altruistic
           TRUE ~ NA_real_
         ))

#Donor cause of death
ukfull$DONCOD <- 
  recode(ukfull$DCOD,
         20 ~ 1, 
         21 ~ 1, 
         22 ~ 1, 
         23 ~ 1,
         24 ~ 1,
         29 ~ 1, 
         30 ~ 1, 
         31 ~ 1,
         39 ~ 1, 
         10 ~ 2,
         11 ~ 2,
         19 ~ 2,
         13 ~ 3,
         NA_real_ ~ NA,
         TRUE ~ 4)

ukfull <- ukfull %>% 
  mutate(DONCOD = factor(DONCOD, labels = c("Trauma", "CVA", "Cerebral Anoxia", "Other"))) 

#Donor graft types
ukfull <- ukfull %>% 
  mutate(GRAFT_TYPE = case_when(
    ORGAN == 40 ~ 0, #whole liver
    ORGAN == 41 ~ 1, #left lobe
    ORGAN == 42 ~ 2, #right lobe
    ORGAN == 46 ~ 3, #left lateral segment
    ORGAN == 47 ~ 4, #liver segment IV
    TRUE ~ NA_real_
  ))

#Donor CMV
ukfull <- ukfull %>% mutate(
  DCMV = case_when(
    ukfull$DCMV == 1 ~ 1, #negative 
    ukfull$DCMV == 2 ~ 2, #positive 
    ukfull$DCMV == 3 ~ 3, #indeterminate
    ukfull$DCMV == 4 ~ 4, #repeat reactive
    ukfull$DCMV == 5 ~ 5, #confirmed positive
    ukfull$DCMV == 6 ~ 6, #test result awaited/sent for testing
    ukfull$DCMV == 7 ~ 7, #not tested
    ukfull$DCMV == 8 ~ 8, #not reported
    ukfull$DCMV == 9 ~ 9, #unknown
    ukfull$DCMV == 10 ~ 10, #non-specific reactivity
    TRUE ~ NA_real_)) 

#Recipient
#Ethnicity
ukfull$RETHNIC <- recode(
  ukfull$RETHNIC,
  1 ~ 1, #white
  2 ~ 2, #asian
  3 ~ 3, #black
  4 ~ 4, #chinese
  6 ~ 6, #mixed - specify
  7 ~ 7, #other - specify
  8 ~ 8, #not reported
  9 ~ 9, #unknown
  TRUE ~ NA_real_
)

#Renal support
ukfull$RREN_SUP <- recode(
  ukfull$RREN_SUP,
  8 ~ NA,
  9 ~ NA,
  3 ~ 0, #not required
  1 ~ 1, #haemodialysis
  2 ~ 2, #filtration
)

#Functional status
ukfull$RLIFE <- recode(ukfull$RLIFE,
                       7 ~ NA,
                       8 ~ NA,
                       9 ~ NA,
                       1 ~ 1, #Able to carry out normal activity without restriction
                       2 ~ 2, #Only restricted in physically strenuous activity
                       3 ~ 3, #Can move freely. Capable of self care. Unable to do any form of work
                       4 ~ 4, #Only capable of limited self care. Confined mostly to bed or chair
                       5 ~ 5, #Completely reliant on nursing/medical care
                       7 ~ 7 #Aged five years or less - irrelevant
)


#Encephalopathy
ukfull$RENCEPH <- recode(ukfull$RENCEPH,
                         0 ~ 0, #not encephalopathic
                         1 ~ 1, #grade 1
                         2 ~ 2, #grade 2
                         3 ~ 3, #grade 3
                         4 ~ 4, #grade 4
                         8 ~ NA,
                         9 ~ NA
)

#Recipient HCV status
ukfull$RANTI_HCV <- recode(ukfull$RANTI_HCV,
                                                      8 ~ NA, #not reported
                                                      9 ~ NA,  #unknown
                                                      1 ~ 1, #negative 
                                                      2 ~ 2, #positive 
                                                      3 ~ 3, #indeterminate
                                                      4 ~ 4, #repeat reactive
                                                      5 ~ 5, #confirmed positive
                                                      6 ~ 6, #test result awaited/sent for testing
                                                      7 ~ 7, #not tested
                                                      10 ~ 10 #non-specific reactivity
)



#Causes of death
ukfull <- ukfull %>% mutate(
  COD_cardiovascular = case_when(
    RCOD == 511 ~ 1,
    RCOD2 == 511 ~ 1,
    RCOD == 512 ~ 1,
    RCOD2 == 512 ~ 1,
    RCOD == 517 ~ 1,
    RCOD2 == 517 ~ 1,
    RCOD == 514 ~ 1,
    RCOD2 == 514 ~ 1,
    RCOD == 515 ~ 1,
    RCOD2 == 515 ~ 1,
    RCOD == 516 ~ 1,
    RCOD2 == 516 ~ 1,
    RCOD == 518 ~ 1,
    RCOD2 == 518 ~ 1,
    RCOD == 513 ~ 1,
    RCOD2 == 513 ~ 1,
    RCOD == 576 ~ 1,
    RCOD2 == 576 ~ 1,
    RCOD == 519 ~ 1,
    RCOD2 == 519 ~ 1,
    RCOD == 521 ~ 1,
    RCOD2 == 521 ~ 1,
    RCOD == 522 ~ 1,
    RCOD2 == 522 ~ 1,
    RCOD == 524 ~ 1,
    RCOD2 == 524 ~ 1,
    RCOD == 525 ~ 1,
    RCOD2 == 525 ~ 1,
    RCOD == 526 ~ 1,
    RCOD2 == 526 ~ 1,
    RCOD == 527 ~ 1,
    RCOD2 == 527 ~ 1,
    RCOD == 528 ~ 1,
    RCOD2 == 528 ~ 1,
    RCOD == 523 ~ 1,
    RCOD2 == 523 ~ 1,
    RCOD == 529 ~ 1,
    RCOD2 == 529 ~ 1,
    TRUE ~ 0))

ukfull <- ukfull %>% mutate(
  COD_malignancy = case_when(
    RCOD == 548 ~ 1,
    RCOD2 == 548 ~ 1,
    RCOD == 549 ~ 1,
    RCOD2 == 549 ~ 1,
    RCOD == 566 ~ 1,
    RCOD2 == 566 ~ 1,
    RCOD == 567 ~ 1,
    RCOD2 == 567 ~ 1,
    RCOD == 568 ~ 1,
    RCOD2 == 568 ~ 1,
    RCOD == 573 ~ 1,
    RCOD2 == 573 ~ 1,
    RCOD == 574 ~ 1,
    RCOD2 == 574 ~ 1,
    RCOD == 563 ~ 1,
    RCOD2 == 563 ~ 1,
    RCOD == 564 ~ 1,
    RCOD2 == 564 ~ 1,
    TRUE ~ 0))

ukfull <- ukfull %>% mutate(
  COD_infection = case_when(
    RCOD == 530 ~ 1,
    RCOD2 == 530 ~ 1,
    RCOD == 531 ~ 1,
    RCOD2 == 531 ~ 1,
    RCOD == 532 ~ 1,
    RCOD2 == 532 ~ 1,
    RCOD == 533 ~ 1,
    RCOD2 == 533 ~ 1,
    RCOD == 534 ~ 1,
    RCOD2 == 534 ~ 1,
    RCOD == 535 ~ 1,
    RCOD2 == 535 ~ 1,
    RCOD == 536 ~ 1,
    RCOD2 == 536 ~ 1,
    RCOD == 537 ~ 1,
    RCOD2 == 537 ~ 1,
    RCOD == 538 ~ 1,
    RCOD2 == 538 ~ 1,
    RCOD == 539 ~ 1,
    RCOD2 == 539 ~ 1,
    TRUE ~ 0))

ukfull <- ukfull %>% mutate(
  COD_liverrelated = case_when(
    RCOD == 541 ~ 1,
    RCOD2 == 541 ~ 1,
    RCOD == 542 ~ 1,
    RCOD2 == 542 ~ 1,
    RCOD == 543 ~ 1,
    RCOD2 == 543 ~ 1,
    RCOD == 544 ~ 1,
    RCOD2 == 544 ~ 1,
    RCOD == 545 ~ 1,
    RCOD2 == 545 ~ 1,
    RCOD == 546 ~ 1,
    RCOD2 == 546 ~ 1,
    RCOD == 590 ~ 1,
    RCOD2 == 590 ~ 1,
    RCOD == 561 ~ 1,
    RCOD2 == 561 ~ 1,
    RCOD == 575 ~ 1,
    RCOD2 == 575 ~ 1,
    TRUE ~ 0))

ukfull <- ukfull %>% mutate(
  COD_other = case_when(
    RCOD == 562 ~ 1,
    RCOD2 == 562 ~ 1,
    RCOD == 571 ~ 1,
    RCOD2 == 571 ~ 1,
    RCOD == 572 ~ 1,
    RCOD2 == 572 ~ 1,
    RCOD == 570 ~ 1,
    RCOD2 == 570 ~ 1,
    RCOD == 547 ~ 1,
    RCOD2 == 547 ~ 1,
    RCOD == 554 ~ 1,
    RCOD2 == 554 ~ 1,
    RCOD == 520 ~ 1,
    RCOD2 == 520 ~ 1,
    RCOD == 577 ~ 1,
    RCOD2 == 577 ~ 1,
    RCOD == 578 ~ 1,
    RCOD2 == 578 ~ 1,
    RCOD == 569 ~ 1,
    RCOD2 == 569 ~ 1,
    RCOD == 579 ~ 1,
    RCOD2 == 579 ~ 1,
    RCOD == 500 ~ 1,
    RCOD2 == 500 ~ 1,
    RCOD == 595 ~ 1,
    RCOD2 == 595 ~ 1,
    RCOD == 598 ~ 1,
    RCOD2 == 598 ~ 1,
    RCOD == 599 ~ 1,
    RCOD2 == 599 ~ 1,
    RCOD == 588 ~ 1,
    RCOD2 == 588 ~ 1,
    RCOD == 581 ~ 1,
    RCOD2 == 581 ~ 1,
    RCOD == 582 ~ 1,
    RCOD2 == 582 ~ 1,
    RCOD == 551 ~ 1,
    RCOD2 == 551 ~ 1,
    RCOD == 552 ~ 1,
    RCOD2 == 552 ~ 1,
    RCOD == 553 ~ 1,
    RCOD2 == 553 ~ 1,
    TRUE ~ 0))

ukfull <- ukfull %>% mutate(COD_categorized = case_when(
  COD_cardiovascular == 1 ~ 1,
  COD_malignancy == 1 ~ 2,
  COD_infection == 1 ~ 3,
  COD_liverrelated == 1 ~ 4,
  COD_other == 1 ~ 5
)) %>% 
  mutate(COD_categorized = factor(COD_categorized, labels = c("Cardiovascular", "Infection", "Liver-related", "Other")))

#Create a country variable 
ukfull <- ukfull %>% mutate(
  COUNTRY = case_when(
    PCENS >= 0 ~ "UK"
  )
)

#Canadian data**
#Donor graft type
corrdata <- corrdata %>% mutate(
  GRAFT_TYPE = case_when(
    ORGAN_TYPE_CODE == 20 ~ 0, #Liver
    ORGAN_TYPE_CODE == 21 ~ 1, #Liver left lobe
    ORGAN_TYPE_CODE == 22 ~ 2, #Liver right lobe
    ORGAN_TYPE_CODE == 23 ~ 3, #liver lateral segment
    ORGAN_TYPE_CODE == 29 ~ 9, #liver two (from conversion
    TRUE ~ NA_real_)
  )

#CMV status donor
corrdata <- corrdata %>% mutate(
  DCMV = case_when(
    corrdata$CMV_FLAG_donor == "P" ~ "Positive",
    corrdata$CMV_FLAG_donor == "N" ~ "Negative",
    corrdata$CMV_FLAG_donor == "U" ~ "Unknown/Missing" #Consider making this NA
    TRUE ~ NA_real_)) 

#Ethnicity
corrdata <- corrdata %>% mutate(
  RETHNIC = case_when(
    Racial_Origin == "01" ~ 1, #caucasian
    Racial_Origin == "02" ~ 2, #asian
    Racial_Origin == "03" ~ 3, #black
    Racial_Origin == "05" ~ 5, #indian subcontinent
    Racial_Origin == "08" ~ 8, #pacific islander
    Racial_Origin == "09" ~ 9, #aboriginal
    Racial_Origin == "10" ~ 10, #mid east arabian
    Racial_Origin == "11" ~ 11, #latin american
    Racial_Origin == "98" ~ NA_real_, #unknown
    Racial_Origin == "99" ~ 99, #other
    TRUE ~ NA_real_
  )
)


#Recipient blood group
corrdata <- corrdata %>% mutate(
  RBG = case_when(
    corrdata$BLOOD_TYPE_CODE == "O" ~ 1,
    corrdata$BLOOD_TYPE_CODE == "A" ~ 2,
    corrdata$BLOOD_TYPE_CODE == "B" ~ 3,
    corrdata$BLOOD_TYPE_CODE == "AB" ~ 4,
    corrdata$BLOOD_TYPE_CODE == "U" ~ 5, #unknown
    is.na(corrdata$BLOOD_TYPE_CODE) ~ NA_real_
  ))%>% 
  mutate(RBG = factor(RBG, labels = c("0", "A", "B", "AB", "Unknown")))

#Recipient HCV status
corrdata <- corrdata %>% mutate(
  RANTI_HCV = case_when(
    corrdata$HEPATITIS_C_FLAG == "P" ~ "Positive",
    corrdata$HEPATITIS_C_FLAG == "N" ~ "Negative",
    corrdata$HEPATITIS_C_FLAG == "U" ~ "Unknown/Missing", #Consider making this NA
    is.na(corrdata$HEPATITIS_C_FLAG) ~ NA_character_)) %>%
  mutate(RANTI_HCV = factor(RANTI_HCV))


#Causes of death
corrdata <- corrdata %>% mutate(DEATH_CAUSE_CODE = as.numeric(DEATH_CAUSE_CODE))

corrdata <- corrdata %>% mutate(
  COD_cardiovascular = case_when(
    DEATH_CAUSE_CODE == 11 ~ 1,
    DEATH_CAUSE_CODE == 11 ~ 1,
    DEATH_CAUSE_CODE == 12 ~ 1,
    DEATH_CAUSE_CODE == 12 ~ 1,
    DEATH_CAUSE_CODE == 17 ~ 1,
    DEATH_CAUSE_CODE == 17 ~ 1,
    DEATH_CAUSE_CODE == 14 ~ 1,
    DEATH_CAUSE_CODE == 14 ~ 1,
    DEATH_CAUSE_CODE == 15 ~ 1,
    DEATH_CAUSE_CODE == 15 ~ 1,
    DEATH_CAUSE_CODE == 16 ~ 1,
    DEATH_CAUSE_CODE == 16 ~ 1,
    DEATH_CAUSE_CODE == 18 ~ 1,
    DEATH_CAUSE_CODE == 18 ~ 1,
    DEATH_CAUSE_CODE == 13 ~ 1,
    DEATH_CAUSE_CODE == 13 ~ 1,
    DEATH_CAUSE_CODE == 21 ~ 1,
    DEATH_CAUSE_CODE == 21 ~ 1,
    DEATH_CAUSE_CODE == 22 ~ 1,
    DEATH_CAUSE_CODE == 22 ~ 1,
    DEATH_CAUSE_CODE == 30 ~ 1,
    DEATH_CAUSE_CODE == 30 ~ 1,
    DEATH_CAUSE_CODE == 24 ~ 1,
    DEATH_CAUSE_CODE == 24 ~ 1,
    DEATH_CAUSE_CODE == 25 ~ 1,
    DEATH_CAUSE_CODE == 25 ~ 1,
    DEATH_CAUSE_CODE == 26 ~ 1,
    DEATH_CAUSE_CODE == 26 ~ 1,
    DEATH_CAUSE_CODE == 27 ~ 1,
    DEATH_CAUSE_CODE == 27 ~ 1,
    DEATH_CAUSE_CODE == 28 ~ 1,
    DEATH_CAUSE_CODE == 28 ~ 1,
    DEATH_CAUSE_CODE == 23 ~ 1,
    DEATH_CAUSE_CODE == 23 ~ 1,
    DEATH_CAUSE_CODE == 29 ~ 1,
    DEATH_CAUSE_CODE == 29 ~ 1,
    DEATH_CAUSE_CODE == 55 ~ 1,
    DEATH_CAUSE_CODE == 55 ~ 1,
    DEATH_CAUSE_CODE == 73 ~ 1,
    DEATH_CAUSE_CODE == 73 ~ 1,
    DEATH_CAUSE_CODE == 56 ~ 1,
    DEATH_CAUSE_CODE == 56 ~ 1,
    DEATH_CAUSE_CODE == 57 ~ 1,
    DEATH_CAUSE_CODE == 57 ~ 1,
    DEATH_CAUSE_CODE == 71 ~ 1,
    DEATH_CAUSE_CODE == 71 ~ 1,
    TRUE ~ 0))

corrdata <- corrdata %>% mutate(
  COD_malignancy = case_when(
    DEATH_CAUSE_CODE == 66 ~ 1,
    DEATH_CAUSE_CODE == 66 ~ 1,
    DEATH_CAUSE_CODE == 67 ~ 1,
    DEATH_CAUSE_CODE == 67 ~ 1,
    DEATH_CAUSE_CODE == 63 ~ 1,
    DEATH_CAUSE_CODE == 63 ~ 1,
    DEATH_CAUSE_CODE == 64 ~ 1,
    DEATH_CAUSE_CODE == 64 ~ 1,
    TRUE ~ 0))

corrdata <- corrdata %>% mutate(
  COD_infection = case_when(
    DEATH_CAUSE_CODE == 3 ~ 1,
    DEATH_CAUSE_CODE == 3 ~ 1,
    DEATH_CAUSE_CODE == 4 ~ 1,
    DEATH_CAUSE_CODE == 4 ~ 1,
    DEATH_CAUSE_CODE == 5 ~ 1,
    DEATH_CAUSE_CODE == 5 ~ 1,
    DEATH_CAUSE_CODE == 6 ~ 1,
    DEATH_CAUSE_CODE == 6 ~ 1,
    DEATH_CAUSE_CODE == 7 ~ 1,
    DEATH_CAUSE_CODE == 7 ~ 1,
    DEATH_CAUSE_CODE == 8 ~ 1,
    DEATH_CAUSE_CODE == 8 ~ 1,
    DEATH_CAUSE_CODE == 9 ~ 1,
    DEATH_CAUSE_CODE == 9 ~ 1,
    DEATH_CAUSE_CODE == 10 ~ 1,
    DEATH_CAUSE_CODE == 10 ~ 1,
    DEATH_CAUSE_CODE == 31 ~ 1,
    DEATH_CAUSE_CODE == 31 ~ 1,
    DEATH_CAUSE_CODE == 32 ~ 1,
    DEATH_CAUSE_CODE == 32 ~ 1,
    DEATH_CAUSE_CODE == 33 ~ 1,
    DEATH_CAUSE_CODE == 33 ~ 1,
    DEATH_CAUSE_CODE == 36 ~ 1,
    DEATH_CAUSE_CODE == 36 ~ 1,
    DEATH_CAUSE_CODE == 37 ~ 1,
    DEATH_CAUSE_CODE == 37 ~ 1,
    DEATH_CAUSE_CODE == 34 ~ 1,
    DEATH_CAUSE_CODE == 34 ~ 1,
    DEATH_CAUSE_CODE == 35 ~ 1,
    DEATH_CAUSE_CODE == 35 ~ 1,
    DEATH_CAUSE_CODE == 38 ~ 1,
    DEATH_CAUSE_CODE == 38 ~ 1,
    DEATH_CAUSE_CODE == 39 ~ 1,
    DEATH_CAUSE_CODE == 39 ~ 1,
    TRUE ~ 0))

corrdata <- corrdata %>% mutate(
  COD_liverrelated = case_when(
    DEATH_CAUSE_CODE == 41 ~ 1,
    DEATH_CAUSE_CODE == 41 ~ 1,
    DEATH_CAUSE_CODE == 42 ~ 1,
    DEATH_CAUSE_CODE == 42 ~ 1,
    DEATH_CAUSE_CODE == 43 ~ 1,
    DEATH_CAUSE_CODE == 43 ~ 1,
    DEATH_CAUSE_CODE == 44 ~ 1,
    DEATH_CAUSE_CODE == 44 ~ 1,
    DEATH_CAUSE_CODE == 45 ~ 1,
    DEATH_CAUSE_CODE == 45 ~ 1,
    DEATH_CAUSE_CODE == 46 ~ 1,
    DEATH_CAUSE_CODE == 46 ~ 1,
    DEATH_CAUSE_CODE == 74 ~ 1,
    DEATH_CAUSE_CODE == 74 ~ 1,
    DEATH_CAUSE_CODE == 61 ~ 1,
    DEATH_CAUSE_CODE == 61 ~ 1,
    TRUE ~ 0))

corrdata <- corrdata %>% mutate(
  COD_other = case_when(
    DEATH_CAUSE_CODE == 2 ~ 1,
    DEATH_CAUSE_CODE == 2 ~ 1,
    DEATH_CAUSE_CODE == 20 ~ 1,
    DEATH_CAUSE_CODE == 20 ~ 1,
    DEATH_CAUSE_CODE == 62 ~ 1,
    DEATH_CAUSE_CODE == 62 ~ 1,
    DEATH_CAUSE_CODE == 68 ~ 1,
    DEATH_CAUSE_CODE == 68 ~ 1,
    DEATH_CAUSE_CODE == 69 ~ 1,
    DEATH_CAUSE_CODE == 69 ~ 1,
    DEATH_CAUSE_CODE == 70 ~ 1,
    DEATH_CAUSE_CODE == 70 ~ 1,
    DEATH_CAUSE_CODE == 40 ~ 1,
    DEATH_CAUSE_CODE == 40 ~ 1,
    DEATH_CAUSE_CODE == 47 ~ 1,
    DEATH_CAUSE_CODE == 47 ~ 1,
    DEATH_CAUSE_CODE == 48 ~ 1,
    DEATH_CAUSE_CODE == 48 ~ 1,
    DEATH_CAUSE_CODE == 59 ~ 1,
    DEATH_CAUSE_CODE == 59 ~ 1,
    DEATH_CAUSE_CODE == 75 ~ 1,
    DEATH_CAUSE_CODE == 75 ~ 1,
    DEATH_CAUSE_CODE == 19 ~ 1,
    DEATH_CAUSE_CODE == 19 ~ 1,
    DEATH_CAUSE_CODE == 49 ~ 1,
    DEATH_CAUSE_CODE == 49 ~ 1,
    DEATH_CAUSE_CODE == 77 ~ 1,
    DEATH_CAUSE_CODE == 77 ~ 1,
    DEATH_CAUSE_CODE == 90 ~ 1,
    DEATH_CAUSE_CODE == 90 ~ 1,
    DEATH_CAUSE_CODE == 0 ~ 1,
    DEATH_CAUSE_CODE == 0 ~ 1,
    DEATH_CAUSE_CODE == 99 ~ 1,
    DEATH_CAUSE_CODE == 99 ~ 1,
    DEATH_CAUSE_CODE == 81 ~ 1,
    DEATH_CAUSE_CODE == 81 ~ 1,
    DEATH_CAUSE_CODE == 82 ~ 1,
    DEATH_CAUSE_CODE == 82 ~ 1,
    DEATH_CAUSE_CODE == 50 ~ 1,
    DEATH_CAUSE_CODE == 50 ~ 1,
    DEATH_CAUSE_CODE == 51 ~ 1,
    DEATH_CAUSE_CODE == 51 ~ 1,
    DEATH_CAUSE_CODE == 52 ~ 1,
    DEATH_CAUSE_CODE == 52 ~ 1,
    DEATH_CAUSE_CODE == 53 ~ 1,
    DEATH_CAUSE_CODE == 53 ~ 1,
    DEATH_CAUSE_CODE == 54 ~ 1,
    DEATH_CAUSE_CODE == 54 ~ 1,
    TRUE ~ 0
  ))

corrdata <- corrdata %>% mutate(COD_categorized = case_when(
  COD_cardiovascular == 1 ~ 1, 
  COD_malignancy == 1 ~ 2,
  COD_infection == 1 ~ 3,
  COD_liverrelated == 1 ~ 4,
  COD_other == 1 ~ 5
)) %>%
  mutate(COD_categorized = factor(COD_categorized, labels = c("Cardiovascular", "Malignancy", "Infection", "Liver-related", "Other")))

