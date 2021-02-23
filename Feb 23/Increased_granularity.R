#UK data
#renaming with _txp2 suffix
ukwl <- ukwl %>%
  dplyr::rename_all(paste0, "_wl")

ukwl <- ukwl %>% 
  rename(REGID=regid_wl)

ukfull <- left_join(ukfull, ukwl, by="REGID")

#Filtering data
#Count number of missing in each variable
sapply(ukfull, function(transplant_date) sum(is.na(transplant_date)))

#Limit year
ukfull <- ukfull %>% filter(TX_YR <= 2018)

#Drop patient with missing patient or graft info
ukfull <- ukfull %>% 
  filter(PSURV >= 0 & GSURV >= 0)

#Drop pediatric patients
ukfull <- ukfull %>%
  filter(RAGE >= 18)

#Limit to first-time liver only transplants
ukfull <- ukfull %>%
  filter(GRAFT_NO == 1 & 
           TX_TYPE < 80 &
           TX_METHOD < 2 &
           ORG_TXD < 4)

#General: Filtering, recoding and renaming
#Transplant date
ukfull$transplant_date <- dmy(ukfull$transplant_date)
ukfull$reg_date_wl <- dmy(ukfull$reg_date_wl)
ukfull$WAITLIST_TIME <- difftime(ukfull$transplant_date, ukfull$reg_date_wl, units = c("days"))
ukfull$WAITLIST_TIME <- as.numeric(ukfull$WAITLIST_TIME)

#Liver cancer patients 
ukfull <- ukfull %>%
  rename(PLD = RCSPLD1,
         PLD2 = RCSPLD2, 
         PLD3 = RCSPLD3)

ukfull <- ukfull %>% 
  mutate(LC = case_when(
    PLD >= 443 & PLD <= 447 ~ 1,
    PLD2 >= 443 & PLD2 <= 447 ~ 1,
    PLD3 >= 443 & PLD3 <= 447 ~ 1,
    TRUE ~ 0))

#Donor: Filtering, recoding and renaming**
ukfull <- ukfull %>%
  filter(DGRP < 4)

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

#Donor BMI
ukfull$DBMI <- 
  recode(ukfull$DBMI,
         100 %thru% hi ~ NA, 
         lo %thru% 9.99 ~ NA, 
         TRUE ~ copy)

#Donor sex
ukfull$DSEX <- 
  recode(ukfull$DSEX,
         8 ~ NA,
         9 ~ NA,
         1 ~ copy,
         2 ~ 0
  )

ukfull <- ukfull %>% 
  mutate(DSEX = factor(DSEX, labels = c("Female", "Male"))) 

#Blood group
ukfull <- ukfull %>% 
  mutate(BLD_GP_MATCH = factor(ABOMATCH, labels = c("Identical", "Compatible", "Incompatible"))) 

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

ukfull$CIT <- recode(ukfull$CIT,
                     8888 ~ NA,
                     9909 ~ NA,
                     9999 ~ NA,
                     TRUE ~ copy
)

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

ukfull <- ukfull %>% mutate(
  RSEX = case_when(
    RSEX == 1 ~ 1,
    RSEX == 2 ~ 0
  )
) %>% 
  mutate(RSEX = factor(RSEX, labels = c("Female", "Male")))

#Renal support
ukfull$RREN_SUP <- recode(
  ukfull$RREN_SUP,
  8 ~ NA,
  9 ~ NA,
  3 ~ 0, #not required
  1 ~ 1, #haemodialysis
  2 ~ 2, #filtration
)

#BMI (at registration)
ukfull <- ukfull %>% mutate(
  BMI = RWEIGHT/(RHEIGHT/100)^2
)

#Ventilatory support
ukfull$RVENT <- recode(
  ukfull$RVENT,
  8 ~ NA,
  9 ~ NA,
  1 ~ 0,
  2 ~ 1
)

ukfull <- ukfull %>% 
  mutate(RVENT = factor(RVENT, labels = c("Not ventilated", "Ventilated")))

#Previous abdominal surgery
ukfull$RAB_SURGERY <- recode(
  ukfull$RAB_SURGERY,
  8 ~ NA,
  9 ~ NA,
  2 ~ 1,
  1 ~ 0
)

ukfull <- ukfull %>% 
  mutate(RAB_SURGERY = factor(RAB_SURGERY, labels = c("No", "Yes")))

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


#Ascites
ukfull$RASCITES <- recode(ukfull$RASCITES,
                          8 ~ NA,
                          9 ~ NA,
                          1 ~ 0,
                          2 ~ 1
)

ukfull <- ukfull %>%
  mutate(RASCITES = factor(RASCITES, labels = c("No ascites", "Ascites")))

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

#Blood group
ukfull <- ukfull %>%
  mutate(RBG = factor(RBG, labels = c("0", "A", "B", "AB")))

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





#Recipient albumin
ukfull$RALBUMIN <- recode(ukfull$RALBUMIN,
                          88 ~ NA,
                          99 ~ NA,
                          lo %thru% 6.999 ~ NA,
                          60.001 %thru% hi ~ NA,
                          TRUE ~ copy
)

#Convert albumin unit
ukfull <- ukfull %>% mutate(
  RALBUMIN = RALBUMIN/10
)

#Recipient bilirubin
ukfull$RBILIRUBIN <- recode(ukfull$RBILIRUBIN,
                            8888 ~ NA,
                            9999 ~ NA,
                            TRUE ~ copy
)

#Recalculate RBILI
ukfull <- ukfull %>% mutate(
  RBILIRUBIN = RBILIRUBIN/17.1
)

#Recipient creatinine
ukfull$RCREAT <- recode(ukfull$RCREAT,
                        8888 ~ NA,
                        9999 ~ NA,
                        TRUE ~ copy
)

#Convert umol/l to mg/dl
ukfull <- ukfull %>%
  mutate(RCREAT = RCREAT*0.0113)

#Cancer and acute
ukfull <- ukfull %>%
  mutate(UKT_PLDGRP = case_when(
    PLD >= 440 & PLD <= 447 ~ 1,
    PLD2 >= 440 & PLD2 <= 447 ~ 1,
    PLD3 >= 440 & PLD3 <= 447 ~ 1,
    PLD == 427 ~ 2,
    PLD == 428 ~ 2,
    PLD >= 430 & PLD <= 435 ~ 2,
    PLD >= 437 & PLD <= 439 ~2,
    PLD == 455 ~ 2,
    PLD2 == 427 ~ 2,
    PLD2 == 428 ~ 2,
    PLD2 >= 430 & PLD2 <= 435 ~ 2,
    PLD2 >= 437 & PLD2 <= 439 ~2,
    PLD2 == 455 ~ 2,
    PLD3 == 427 ~ 2,
    PLD3 == 428 ~ 2,
    PLD3 >= 430 & PLD3 <= 435 ~ 2,
    PLD3 >= 437 & PLD3 <= 439 ~2,
    PLD3 == 455 ~ 2,
    PLD == 424 ~ 3,
    PLD2 == 424 ~ 3,
    PLD3 == 424 ~3,
    PLD == 414 ~ 4,
    PLD2 == 414 ~ 4,
    PLD3 == 414 ~ 4,
    PLD == 413 ~ 5,
    PLD == 436 ~ 5,
    PLD2 == 413 ~ 5,
    PLD2 == 436 ~ 5,
    PLD3 == 413 ~ 5,
    PLD3 == 436 ~ 5,
    PLD == 411 ~ 6,
    PLD2 == 411 ~ 6,
    PLD3 == 411 ~ 6,
    PLD == 419 ~ 7,
    PLD2 == 419 ~ 7,
    PLD3 == 419 ~ 7,
    PLD == 412 ~ 8,
    PLD == 417 ~ 8,
    PLD2 == 412 ~ 8,
    PLD2 == 417 ~ 8,
    PLD3 == 412 ~ 8,
    PLD3 == 417 ~ 8,
    PLD == 415 ~ 9,
    PLD == 422 ~ 9,
    PLD == 426 ~ 9,
    PLD == 450 ~ 9,
    PLD == 452 ~ 9,
    PLD == 454 ~ 9,
    PLD == 456 ~ 9,
    PLD == 457 ~ 9,
    PLD == 458 ~ 9,
    PLD == 461 ~ 9,
    PLD == 462 ~ 9,
    PLD == 464 ~ 9,
    PLD == 465 ~ 9,
    PLD == 466 ~ 9,
    PLD == 467 ~ 9,
    PLD == 468 ~ 9,
    PLD == 469 ~ 9,
    PLD == 470 ~ 9,
    PLD == 483 ~ 9,
    PLD2 == 415 ~ 9,
    PLD2 == 422 ~ 9,
    PLD2 == 426 ~ 9,
    PLD2 == 450 ~ 9,
    PLD2 == 452 ~ 9,
    PLD2 == 454 ~ 9,
    PLD2 == 456 ~ 9,
    PLD2 == 457 ~ 9,
    PLD2 == 458 ~ 9,
    PLD2 == 461 ~ 9,
    PLD2 == 462 ~ 9,
    PLD2 == 464 ~ 9,
    PLD2 == 465 ~ 9,
    PLD2 == 466 ~ 9,
    PLD2 == 467 ~ 9,
    PLD2 == 468 ~ 9,
    PLD2 == 469 ~ 9,
    PLD2 == 470 ~ 9,
    PLD2 == 483 ~ 9,
    PLD3 == 415 ~ 9,
    PLD3 == 422 ~ 9,
    PLD3 == 426 ~ 9,
    PLD3 == 450 ~ 9,
    PLD3 == 452 ~ 9,
    PLD3 == 454 ~ 9,
    PLD3 == 456 ~ 9,
    PLD3 == 457 ~ 9,
    PLD3 == 458 ~ 9,
    PLD3 == 461 ~ 9,
    PLD3 == 462 ~ 9,
    PLD3 == 464 ~ 9,
    PLD3 == 465 ~ 9,
    PLD3 == 466 ~ 9,
    PLD3 == 467 ~ 9,
    PLD3 == 468 ~ 9,
    PLD3 == 469 ~ 9,
    PLD3 == 470 ~ 9,
    PLD3 == 483 ~ 9,
    PLD == 410 ~ 10,
    PLD == 416 ~ 10,
    PLD == 418 ~ 10,
    PLD == 420 ~ 10,
    PLD == 421 ~ 10,
    PLD == 423 ~ 10,
    PLD == 425 ~ 10,
    PLD == 448 ~ 10,
    PLD == 451 ~ 10,
    PLD == 453 ~ 10,
    PLD == 459 ~ 10,
    PLD == 460 ~ 10,
    PLD == 463 ~ 10,
    PLD == 471 ~ 10,
    PLD == 472 ~ 10,
    PLD == 473 ~ 10,
    PLD == 474 ~ 10,
    PLD == 475 ~ 10,
    PLD == 476 ~ 10,
    PLD == 477 ~ 10,
    PLD == 478 ~ 10,
    PLD == 479 ~ 10,
    PLD == 480 ~ 10,
    PLD == 481 ~ 10,
    PLD == 482 ~ 10,
    PLD == 484 ~ 10,
    PLD == 485 ~ 10,
    PLD == 486 ~ 10,
    PLD == 498 ~ 10,
    PLD == 499 ~ 10,
    PLD2 == 410 ~ 10,
    PLD2 == 416 ~ 10,
    PLD2 == 418 ~ 10,
    PLD2 == 420 ~ 10,
    PLD2 == 421 ~ 10,
    PLD2 == 423 ~ 10,
    PLD2 == 425 ~ 10,
    PLD2 == 448 ~ 10,
    PLD2 == 451 ~ 10,
    PLD2 == 453 ~ 10,
    PLD2 == 459 ~ 10,
    PLD2 == 460 ~ 10,
    PLD2 == 463 ~ 10,
    PLD2 == 471 ~ 10,
    PLD2 == 472 ~ 10,
    PLD2 == 473 ~ 10,
    PLD2 == 474 ~ 10,
    PLD2 == 475 ~ 10,
    PLD2 == 476 ~ 10,
    PLD2 == 477 ~ 10,
    PLD2 == 478 ~ 10,
    PLD2 == 479 ~ 10,
    PLD2 == 480 ~ 10,
    PLD2 == 481 ~ 10,
    PLD2 == 482 ~ 10,
    PLD2 == 484 ~ 10,
    PLD2 == 485 ~ 10,
    PLD2 == 486 ~ 10,
    PLD2 == 498 ~ 10,
    PLD2 == 499 ~ 10,
    PLD3 == 410 ~ 10,
    PLD3 == 416 ~ 10,
    PLD3 == 418 ~ 10,
    PLD3 == 420 ~ 10,
    PLD3 == 421 ~ 10,
    PLD3 == 423 ~ 10,
    PLD3 == 425 ~ 10,
    PLD3 == 448 ~ 10,
    PLD3 == 451 ~ 10,
    PLD3 == 453 ~ 10,
    PLD3 == 459 ~ 10,
    PLD3 == 460 ~ 10,
    PLD3 == 463 ~ 10,
    PLD3 == 471 ~ 10,
    PLD3 == 472 ~ 10,
    PLD3 == 473 ~ 10,
    PLD3 == 474 ~ 10,
    PLD3 == 475 ~ 10,
    PLD3 == 476 ~ 10,
    PLD3 == 477 ~ 10,
    PLD3 == 478 ~ 10,
    PLD3 == 479 ~ 10,
    PLD3 == 480 ~ 10,
    PLD3 == 481 ~ 10,
    PLD3 == 482 ~ 10,
    PLD3 == 484 ~ 10,
    PLD3 == 485 ~ 10,
    PLD3 == 486 ~ 10,
    PLD3 == 498 ~ 10,
    PLD3 == 499 ~ 10,
  ))

ukfull <- ukfull %>% 
  mutate(UKT_PLDGRP = factor(UKT_PLDGRP, labels = c("Cancer", "Acute", "HCV", "PSC", "HBV", "PBC", "ALD", "AID", "Metabolic", "Others")))

ukfull <- ukfull %>% mutate(
  ALF = case_when(
    PLD == 427 ~ 1,
    PLD == 428 ~ 1,
    PLD >= 430 & PLD <= 435 ~ 1,
    PLD >= 437 & PLD <= 439 ~1,
    PLD == 455 ~ 1,
    PLD2 == 427 ~ 1,
    PLD2 == 428 ~ 1,
    PLD2 >= 430 & PLD2 <= 435 ~ 1,
    PLD2 >= 437 & PLD2 <= 439 ~1,
    PLD2 == 455 ~ 1,
    PLD3 == 427 ~ 1,
    PLD3 == 428 ~ 1,
    PLD3 >= 430 & PLD3 <= 435 ~ 1,
    PLD3 >= 437 & PLD3 <= 439 ~1,
    PLD3 == 455 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  HCV = case_when(
    PLD == 424 ~ 1,
    PLD2 == 424 ~ 1,
    PLD3 == 424 ~1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  PSC = case_when(
    PLD == 414 ~ 1,
    PLD2 == 414 ~ 1,
    PLD3 == 414 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  HBV = case_when(
    PLD == 413 ~ 1,
    PLD == 436 ~ 1,
    PLD2 == 413 ~ 1,
    PLD2 == 436 ~ 1,
    PLD3 == 413 ~ 1,
    PLD3 == 436 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  PBC = case_when(
    PLD == 411 ~ 1,
    PLD2 == 411 ~ 1,
    PLD3 == 411 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  ALD = case_when(
    PLD == 419 ~ 1,
    PLD2 == 419 ~ 1,
    PLD3 == 419 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  AID = case_when(
    PLD == 412 ~ 1,
    PLD == 417 ~ 1,
    PLD2 == 412 ~ 1,
    PLD2 == 417 ~ 1,
    PLD3 == 412 ~ 1,
    PLD3 == 417 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  MET = case_when(
    PLD == 415 ~ 1,
    PLD == 422 ~ 1,
    PLD == 426 ~ 1,
    PLD == 450 ~ 1,
    PLD == 452 ~ 1,
    PLD == 454 ~ 1,
    PLD == 456 ~ 1,
    PLD == 457 ~ 1,
    PLD == 458 ~ 1,
    PLD == 461 ~ 1,
    PLD == 462 ~ 1,
    PLD == 464 ~ 1,
    PLD == 465 ~ 1,
    PLD == 466 ~ 1,
    PLD == 467 ~ 1,
    PLD == 468 ~ 1,
    PLD == 469 ~ 1,
    PLD == 470 ~ 1,
    PLD == 483 ~ 1,
    PLD2 == 415 ~ 1,
    PLD2 == 422 ~ 1,
    PLD2 == 426 ~ 1,
    PLD2 == 450 ~ 1,
    PLD2 == 452 ~ 1,
    PLD2 == 454 ~ 1,
    PLD2 == 456 ~ 1,
    PLD2 == 457 ~ 1,
    PLD2 == 458 ~ 1,
    PLD2 == 461 ~ 1,
    PLD2 == 462 ~ 1,
    PLD2 == 464 ~ 1,
    PLD2 == 465 ~ 1,
    PLD2 == 466 ~ 1,
    PLD2 == 467 ~ 1,
    PLD2 == 468 ~ 1,
    PLD2 == 469 ~ 1,
    PLD2 == 470 ~ 1,
    PLD2 == 483 ~ 1,
    PLD3 == 415 ~ 1,
    PLD3 == 422 ~ 1,
    PLD3 == 426 ~ 1,
    PLD3 == 450 ~ 1,
    PLD3 == 452 ~ 1,
    PLD3 == 454 ~ 1,
    PLD3 == 456 ~ 1,
    PLD3 == 457 ~ 1,
    PLD3 == 458 ~ 1,
    PLD3 == 461 ~ 1,
    PLD3 == 462 ~ 1,
    PLD3 == 464 ~ 1,
    PLD3 == 465 ~ 1,
    PLD3 == 466 ~ 1,
    PLD3 == 467 ~ 1,
    PLD3 == 468 ~ 1,
    PLD3 == 469 ~ 1,
    PLD3 == 470 ~ 1,
    PLD3 == 483 ~ 1,
    TRUE ~ 0
  )
)

ukfull <- ukfull %>% mutate(
  OTH = case_when(
    PLD == 410 ~ 1,
    PLD == 416 ~ 1,
    PLD == 418 ~ 1,
    PLD == 420 ~ 1,
    PLD == 421 ~ 1,
    PLD == 423 ~ 1,
    PLD == 425 ~ 1,
    PLD == 448 ~ 1,
    PLD == 451 ~ 1,
    PLD == 453 ~ 1,
    PLD == 459 ~ 1,
    PLD == 460 ~ 1,
    PLD == 463 ~ 1,
    PLD == 471 ~ 1,
    PLD == 472 ~ 1,
    PLD == 473 ~ 1,
    PLD == 474 ~ 1,
    PLD == 475 ~ 1,
    PLD == 476 ~ 1,
    PLD == 477 ~ 1,
    PLD == 478 ~ 1,
    PLD == 479 ~ 1,
    PLD == 480 ~ 1,
    PLD == 481 ~ 1,
    PLD == 482 ~ 1,
    PLD == 484 ~ 1,
    PLD == 485 ~ 1,
    PLD == 486 ~ 1,
    PLD == 498 ~ 1,
    PLD == 499 ~ 1,
    PLD2 == 410 ~ 1,
    PLD2 == 416 ~ 1,
    PLD2 == 418 ~ 1,
    PLD2 == 420 ~ 1,
    PLD2 == 421 ~ 1,
    PLD2 == 423 ~ 1,
    PLD2 == 425 ~ 1,
    PLD2 == 448 ~ 1,
    PLD2 == 451 ~ 1,
    PLD2 == 453 ~ 1,
    PLD2 == 459 ~ 1,
    PLD2 == 460 ~ 1,
    PLD2 == 463 ~ 1,
    PLD2 == 471 ~ 1,
    PLD2 == 472 ~ 1,
    PLD2 == 473 ~ 1,
    PLD2 == 474 ~ 1,
    PLD2 == 475 ~ 1,
    PLD2 == 476 ~ 1,
    PLD2 == 477 ~ 1,
    PLD2 == 478 ~ 1,
    PLD2 == 479 ~ 1,
    PLD2 == 480 ~ 1,
    PLD2 == 481 ~ 1,
    PLD2 == 482 ~ 1,
    PLD2 == 484 ~ 1,
    PLD2 == 485 ~ 1,
    PLD2 == 486 ~ 1,
    PLD2 == 498 ~ 1,
    PLD2 == 499 ~ 1,
    PLD3 == 410 ~ 1,
    PLD3 == 416 ~ 1,
    PLD3 == 418 ~ 1,
    PLD3 == 420 ~ 1,
    PLD3 == 421 ~ 1,
    PLD3 == 423 ~ 1,
    PLD3 == 425 ~ 1,
    PLD3 == 448 ~ 1,
    PLD3 == 451 ~ 1,
    PLD3 == 453 ~ 1,
    PLD3 == 459 ~ 1,
    PLD3 == 460 ~ 1,
    PLD3 == 463 ~ 1,
    PLD3 == 471 ~ 1,
    PLD3 == 472 ~ 1,
    PLD3 == 473 ~ 1,
    PLD3 == 474 ~ 1,
    PLD3 == 475 ~ 1,
    PLD3 == 476 ~ 1,
    PLD3 == 477 ~ 1,
    PLD3 == 478 ~ 1,
    PLD3 == 479 ~ 1,
    PLD3 == 480 ~ 1,
    PLD3 == 481 ~ 1,
    PLD3 == 482 ~ 1,
    PLD3 == 484 ~ 1,
    PLD3 == 485 ~ 1,
    PLD3 == 486 ~ 1,
    PLD3 == 498 ~ 1,
    PLD3 == 499 ~ 1,
    TRUE ~ 0
  )
)

#Make a NASH yesno
ukfull <- ukfull %>% mutate(
  NASH = case_when(
    PLD == 426 ~ 1,
    PLD2 == 426 ~ 1,
    PLD3 == 426 ~ 1,
    primary_liver_disease_wl == 426 ~ 1,
    secondary_liver_disease_wl == 426 ~ 1,
    tertiary_liver_disease_wl == 426 ~ 1,
    TRUE ~ 0
  )
)

#Make a HCC yesno 
ukfull <- ukfull %>% mutate(
  HCC = case_when(
    PLD >= 440 & PLD <= 447 ~ 1,
    PLD2 >= 440 & PLD2 <= 447 ~ 1,
    PLD3 >= 440 & PLD3 <= 447 ~ 1,
    primary_liver_disease_wl >= 440 & primary_liver_disease_wl <= 447 ~ 1,
    secondary_liver_disease_wl >= 440 & secondary_liver_disease_wl <= 447 ~ 1,
    tertiary_liver_disease_wl >= 440 & tertiary_liver_disease_wl <= 447 ~ 1,
    TRUE ~ 0
  )
)

#Combined HCC
ukfull <- ukfull %>% mutate(
  HCC_combined = case_when(
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "HCV" ~ 1,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "PSC" ~ 2,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "HBV" ~ 3,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "PBC" ~ 4,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "ALD" ~ 5,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "AID" ~ 6,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "Metabolic" ~ 7,
    ukfull$HCC == 1 & ukfull$UKT_PLDGRP == "Others" ~ 8,
    TRUE ~ 8
  )
) %>% mutate(
  HCC_combined = factor(HCC_combined, labels = c("HCV-associated", "PSC-associated", "HBV-associated", "PBC-associated", "ALD-associated", "AID-associated", "MET-associated", "Other"))
)

#Combined HCC
ukfull <- ukfull %>% mutate(
  HCC_combined = case_when(
    ukfull$HCC == 1 & ukfull$PLD == 424 ~ 1,
    ukfull$HCC == 1 & ukfull$PLD2 == 424  ~ 1,
    ukfull$HCC == 1 & ukfull$PLD3 == 424 ~ 1,
    ukfull$HCC == 1 & ukfull$PLD ==  414 ~ 2,
    ukfull$HCC == 1 & ukfull$PLD2 ==  414 ~ 2,
    ukfull$HCC == 1 & ukfull$PLD3 == 414 ~ 2,
    ukfull$HCC == 1 & ukfull$PLD == 413 ~ 3,
    ukfull$HCC == 1 & ukfull$PLD == 436 ~ 3,
    ukfull$HCC == 1 & ukfull$PLD2 == 413 ~ 3,
    ukfull$HCC == 1 & ukfull$PLD2 == 436 ~ 3,
    ukfull$HCC == 1 & ukfull$PLD3 == 413 ~ 3,
    ukfull$HCC == 1 & ukfull$PLD3 == 436 ~ 3,
    ukfull$HCC == 1 & ukfull$PLD == 411 ~ 4,
    ukfull$HCC == 1 & ukfull$PLD2 == 411 ~ 4,
    ukfull$HCC == 1 & ukfull$PLD2 == 411 ~ 4,
    ukfull$HCC == 1 & ukfull$PLD == 419 ~ 5,
    ukfull$HCC == 1 & ukfull$PLD2 == 419 ~ 5,
    ukfull$HCC == 1 & ukfull$PLD3 == 419 ~ 5,
    ukfull$HCC == 1 & ukfull$PLD == 412 ~ 6,
    ukfull$HCC == 1 & ukfull$PLD == 417 ~ 6,
    ukfull$HCC == 1 & ukfull$PLD2 == 412 ~ 6,
    ukfull$HCC == 1 & ukfull$PLD2 == 417 ~ 6,
    ukfull$HCC == 1 & ukfull$PLD3 == 412 ~ 6,
    ukfull$HCC == 1 & ukfull$PLD3 == 417 ~ 6,
    ukfull$HCC == 1 & ukfull$PLD == 415 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 422 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 426 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 450 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 452 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 454 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 456 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 457 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 458 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 461 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 462 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 464 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 465 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 466 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 467 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 468 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 469 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 470 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD == 483 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 415 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 422 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 426 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 450 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 452 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 454 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 456 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 457 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 458 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 461 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 462 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 464 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 465 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 466 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 467 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 468 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 469 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 470 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD2 == 483 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 415 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 422 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 426 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 450 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 452 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 454 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 456 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 457 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 458 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 461 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 462 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 464 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 465 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 466 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 467 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 468 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 469 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 470 ~ 7,
    ukfull$HCC == 1 & ukfull$PLD3 == 483 ~ 7,
    TRUE ~ 8
  )
) %>% mutate(
  HCC_combined = factor(HCC_combined, labels = c("HCV-associated", "PSC-associated", "HBV-associated", "PBC-associated", "ALD-associated", "AID-associated", "MET-associated", "Other"))
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

ukfull <- ukfull %>% mutate(COD_cardiovascular = factor(COD_cardiovascular, labels = c("Cardiovascular COD", "Noncardiovascular COD")))

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

ukfull <- ukfull %>% mutate(COD_malignancy = factor(COD_malignancy, labels = c("Malignancy-related COD", "Nonmalignancy-related COD")))

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

ukfull <- ukfull %>% mutate(COD_infection = factor(COD_infection, labels = c("Infection COD", "Noninfection COD")))

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

ukfull <- ukfull %>% mutate(COD_liverrelated = factor(COD_liverrelated, labels = c("Liver disease or graft failure COD", "Nonliverdisease or graft failure COD")))

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

ukfull <- ukfull %>% mutate(COD_other = factor(COD_other, labels = c("Other COD", "Non-Other (previously categorized) COD")))

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

ukfull <- ukfull %>% mutate(TRANSPLANT_UNIT = factor(transplant_unit))

ukformerge <- ukfull %>% select(REGID, TX_YR, RAGE, GSURV, GCENS, PSURV, PCENS, LC, DAGE, DTYPE, DONCOD, DBMI, DSEX, DCMV, BLD_GP_MATCH, GRAFT_TYPE, CIT, RSEX, RETHNIC, BMI, WAITLIST_TIME, TRANSPLANT_UNIT, MELD, RREN_SUP, RVENT, RAB_SURGERY, RLIFE, RASCITES, RENCEPH, RBG, RANTI_HCV, RALBUMIN, RINR, RBILIRUBIN, RCREAT, COUNTRY, HCC_combined, UKT_PLDGRP, NASH, COD_cardiovascular, COD_malignancy, COD_infection, COD_liverrelated, COD_other, HCC, HCV, PSC, HBV, PBC, ALD, AID, MET, OTH)


#Canadian data**
corrdata <- corrdata %>% mutate(TRANSPLANT_UNIT = factor(UHN))

corrdata <- corrdata %>% 
  rename(REGID=RECIPIENT_ID)

#Keep analysis from 2008 onwards
corrdata <- corrdata %>% filter(
  Year >= 2008
)

#Rename TX_YEAR variable
corrdata <- corrdata %>%
  rename(TX_YR = Year)

#Keep only adult patients
corrdata <- corrdata %>% filter(
  Age >= 18
)

corrdata <- corrdata %>% 
  rename(RAGE = Age)

#Limit to first-time liver only transplants (multivisc already excluded)
corrdata <- corrdata %>% filter(
  Graft_num1 == 1
)

corrdata <- corrdata %>% filter(
  gtime_days >= 0
)

corrdata <- corrdata %>% filter(
  time_lastfu_days >= 0
)

corrdata <- corrdata %>% 
  rename(GSURV = gtime_days,
         GCENS = Gstatus_txp,
         PSURV = time_lastfu_days,
         PCENS = Status
  )

#Donor characteristics
#Donor age
corrdata <- corrdata %>% rename(
  DAGE = age_years_donor
)

corrdata$DAGE <- as.numeric(corrdata$DAGE)

#Donor type
corrdata$DCD <- 
  recode(corrdata$donor_death_type_code_donor,
         c(1~1,
           0~0,
           TRUE~NA
         ))

corrdata$LDLT <- 
  recode(corrdata$Allograft_type,
         c(1~0,
           2~1,
           3~0
         ))

corrdata <- corrdata %>% mutate(
  DTYPE = case_when(
    DCD == 1 ~ 1,
    LDLT == 1 ~ 2,
    TRUE ~ 0
  )
)

corrdata <- corrdata %>% mutate(
  DTYPE = factor(DTYPE, labels = c("DBD", "DCD", "LDLT")) 
)

#Cause of death
corrdata <- corrdata %>% mutate(
  DONCOD = case_when(
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "03" ~ 1,
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "04" ~ 1,
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "02" ~ 2,
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "07" ~ 2,
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "08" ~ 2,
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "10" ~ 2,
    corrdata$DEATH_CAUSE_CADAVER_CODE_donor == "01" ~ 3,
    TRUE ~ 4
  )
)

corrdata <- corrdata %>% mutate(
  DONCOD = factor(DONCOD, labels = c("Trauma", "CVA", "Cerebral Anoxia", "Other")) 
)

#BMI
corrdata <- corrdata %>% rename(DBMI = Donor_BMI)

corrdata$DBMI <- as.numeric(corrdata$DBMI)

#Donor sex
corrdata$DSEX <- 
  recode(corrdata$SEX_CODE,
         c(1~1,
           0~0
         ))

corrdata <- corrdata %>% mutate(
  DSEX = factor(DSEX, labels = c("Female", "Male"))
)


#Donor ABO match not appropriately coded

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

#CIT
corrdata$COLD_ISC_TIME_MINUTES <- as.numeric(corrdata$COLD_ISC_TIME_MINUTES)

corrdata <- corrdata %>% mutate(
  CIT = COLD_ISC_TIME_MINUTES
)

unique(corrdata$CMV_FLAG_donor)

#CMV status donor
corrdata <- corrdata %>% mutate(
  DCMV = case_when(
    corrdata$CMV_FLAG_donor == "P" ~ "Positive",
    corrdata$CMV_FLAG_donor == "N" ~ "Negative",
    corrdata$CMV_FLAG_donor == "U" ~ "Unknown/Missing" #Consider making this NA
    TRUE ~ NA_real_)) 

#Recipient characteristics
#Gender
corrdata <- corrdata %>% mutate(
  RSEX = case_when(
    SEX_CODE == 1 ~ 1,
    SEX_CODE == 0 ~ 0
  ) 
)

corrdata <- corrdata %>% mutate(
  RSEX = factor(RSEX, labels = c("Female", "Male"))
)

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

#Recipient BMI
corrdata <- corrdata %>% rename(
  BMI = RECIPIENT_BMI
)
corrdata$BMI <- as.numeric(corrdata$BMI)

#Waiting list
corrdata$Waitlist_days <- as.numeric(corrdata$Waitlist_days)

corrdata <- corrdata %>% rename(
  WAITLIST_TIME = Waitlist_days
)

#Recipient MELD score
corrdata <- corrdata %>% rename(
  MELD = MELD_Score
)

corrdata$MELD <- as.numeric(corrdata$MELD)

#Renal support
corrdata$RREN_SUP <- 
  recode(corrdata$Prior_Dialysis,
         c(1~1,
           0~0
         ))

corrdata <- corrdata %>% mutate(
  RREN_SUP = factor(RREN_SUP, labels = c("No pre-tx support", "Pre-tx support"))
)

#Ventilator status
corrdata <- corrdata %>% mutate(
  RVENT = case_when(
    corrdata$MEDICAL_STATUS_CODE_clean == "18" ~ 1,
    corrdata$MEDICAL_STATUS_CODE_clean == "05" ~ 0,
    corrdata$MEDICAL_STATUS_CODE_clean == "11" ~ 0,
    corrdata$MEDICAL_STATUS_CODE_clean == "12" ~ 0,
    corrdata$MEDICAL_STATUS_CODE_clean == "16" ~ 0,
    corrdata$MEDICAL_STATUS_CODE_clean == "17" ~ 0,
    corrdata$MEDICAL_STATUS_CODE_clean == "19" ~ 0,
    TRUE ~ NA_real_
  )  
) %>%
  mutate(
    RVENT = factor(
      RVENT, labels = 
        c("Not ventilated", "Ventilated"
        )))

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

#INR
corrdata <- corrdata %>% mutate(
  RINR = INR_TX
)

corrdata$RINR <- as.numeric(corrdata$RINR)

#Bilirubin
corrdata <- corrdata %>% rename(
  RBILIRUBIN = SERUM_BILIRUBIN_TX
)

corrdata$RBILIRUBIN <- as.numeric(corrdata$RBILIRUBIN)

#Convert to mg/dl
corrdata <- corrdata %>% mutate(
  RBILIRUBIN = RBILIRUBIN/17.1
)

#Creatinine
corrdata <- corrdata %>% rename(
  RCREAT = CREATININE_TX
)

corrdata$RCREAT <- as.numeric(corrdata$RCREAT)

#Convert umol/l to mg/dl
corrdata <- corrdata %>%
  mutate(RCREAT = RCREAT*0.0113)

#Country code
corrdata <- corrdata %>% mutate(
  COUNTRY = case_when(
    PCENS >= 0 ~ "CAN"
  )
)

#Disease Etiology
corrdata$ORGAN_DIAGNOSIS_CODE_A <- as.numeric(corrdata$ORGAN_DIAGNOSIS_CODE_A)
corrdata$Secondary_diagnosis <- as.numeric(corrdata$Secondary_diagnosis)
corrdata$Tertiary <- as.numeric(corrdata$Tertiary)
corrdata$Quarternary <- as.numeric(corrdata$Quarternary)
str(corrdata$Secondary_diagnosis)

#NASH
corrdata <- corrdata  %>% 
  mutate(NASH = case_when(
    ORGAN_DIAGNOSIS_CODE_A == 64 ~ 1,
    Secondary_diagnosis == 64 ~ 1,
    Tertiary == 64 ~ 1,
    Quarternary == 64 ~ 1,
    TRUE ~ 0
  ))

#HCC
corrdata <- corrdata %>% mutate(
  HCC = case_when(
    ORGAN_DIAGNOSIS_CODE_A == 16 ~ 1,
    Secondary_diagnosis == 16 ~ 1,
    Tertiary == 16 ~ 1,
    Quarternary == 16 ~ 1,
    TRUE ~ 0
  )
)

#ALF
corrdata <- corrdata %>% mutate(
  ALF = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 4 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 5 ~ 1, 
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 35 ~ 1, 
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 47 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 56 ~ 1, 
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 58 ~ 1, 
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 61 ~ 1,
    corrdata$Secondary_diagnosis == 4 ~ 1, 
    corrdata$Secondary_diagnosis == 5 ~ 1, 
    corrdata$Secondary_diagnosis == 35 ~ 1, 
    corrdata$Secondary_diagnosis == 47 ~ 1, 
    corrdata$Secondary_diagnosis == 56 ~ 1,
    corrdata$Secondary_diagnosis == 58 ~ 1, 
    corrdata$Secondary_diagnosis == 61 ~ 1,
    corrdata$Tertiary == 4 ~ 1, 
    corrdata$Tertiary == 5 ~ 1, 
    corrdata$Tertiary == 35 ~ 1, 
    corrdata$Tertiary == 47 ~ 1, 
    corrdata$Tertiary == 56 ~ 1, 
    corrdata$Tertiary == 58 ~ 1, 
    corrdata$Tertiary == 61 ~ 1,
    corrdata$Quarternary == 4 ~ 1, 
    corrdata$Quarternary == 5 ~ 1, 
    corrdata$Quarternary == 35 ~ 1, 
    corrdata$Quarternary == 47 ~ 1, 
    corrdata$Quarternary == 56 ~ 1, 
    corrdata$Quarternary == 58 ~ 1, 
    corrdata$Quarternary == 61 ~ 1,
    TRUE ~ 0
  )
)

#HCV
corrdata <- corrdata %>% mutate(
  HCV = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 60 ~ 1,
    corrdata$Secondary_diagnosis == 60 ~ 1,
    corrdata$Tertiary == 60 ~ 1,
    corrdata$Quarternary == 60 ~ 1,
    TRUE ~ 0
  )
)

#PSC
corrdata <- corrdata %>% mutate(
  PSC =  case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 11 ~ 1,
    corrdata$Secondary_diagnosis == 11 ~ 1,
    corrdata$Tertiary == 11 ~ 1,
    corrdata$Quarternary == 11 ~ 1,
    TRUE ~ 0
  )
)

#HBV
corrdata <- corrdata %>% mutate(
  HBV = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 43 ~ 1,
    corrdata$Secondary_diagnosis == 43 ~ 1,
    corrdata$Tertiary == 43 ~ 1,
    corrdata$Quarternary == 43 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 2 ~ 1,
    corrdata$Secondary_diagnosis == 2 ~ 1,
    corrdata$Tertiary == 2 ~ 1,
    corrdata$Quarternary == 2 ~ 1,
    TRUE ~ 0
  )
)

#PBC
corrdata <- corrdata %>% mutate(
  PBC = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 7 ~ 1,
    corrdata$Secondary_diagnosis == 7 ~ 1,
    corrdata$Tertiary == 7 ~ 1,
    corrdata$Quarternary == 7 ~ 1,
    TRUE ~ 0
  )
)

#Alcoholic
corrdata <- corrdata %>% mutate(
  ALD = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 9 ~ 1,
    corrdata$Secondary_diagnosis == 9 ~ 1,
    corrdata$Tertiary == 9 ~ 1,
    corrdata$Quarternary == 9 ~ 1,
    TRUE ~ 0
  )
)

#Autoimmune and cryptogenic
corrdata <- corrdata %>% mutate(
  AID = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 8 ~ 1,
    corrdata$Secondary_diagnosis == 8 ~ 1,
    corrdata$Tertiary == 8 ~ 1,
    corrdata$Quarternary == 8 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 6 ~ 1,
    corrdata$Secondary_diagnosis == 6 ~ 1,
    corrdata$Tertiary == 6 ~ 1,
    corrdata$Quarternary == 6 ~ 1,
    TRUE ~ 0
  )
)

#Metabolic liver disease
corrdata <- corrdata %>% mutate(
  MET = case_when(
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 64 ~ 1,
    corrdata$Secondary_diagnosis == 64 ~ 1,
    corrdata$Tertiary == 64 ~ 1,
    corrdata$Quarternary == 64 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 20 ~ 1,
    corrdata$Secondary_diagnosis == 20 ~ 1,
    corrdata$Tertiary == 20 ~ 1,
    corrdata$Quarternary == 20 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 22 ~ 1,
    corrdata$Secondary_diagnosis == 22 ~ 1,
    corrdata$Tertiary == 22 ~ 1,
    corrdata$Quarternary == 22 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 23 ~ 1,
    corrdata$Secondary_diagnosis == 23 ~ 1,
    corrdata$Tertiary == 23 ~ 1,
    corrdata$Quarternary == 23 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 34 ~ 1,
    corrdata$Secondary_diagnosis == 34 ~ 1,
    corrdata$Tertiary == 34 ~ 1,
    corrdata$Quarternary == 34 ~ 1,
    corrdata$ORGAN_DIAGNOSIS_CODE_A == 34 ~ 1,
    corrdata$Secondary_diagnosis == 34 ~ 1,
    corrdata$Tertiary == 34 ~ 1,
    corrdata$Quarternary == 34 ~ 1,
    TRUE ~ 0
  )
)

#Other 
corrdata <- corrdata %>% mutate(
  OTH = case_when(
    HCC == 0 & ALF == 0 & HCV == 0 & PSC == 0 & HBV == 0 & PBC == 0 & ALD == 0 & AID == 0 & MET == 0 ~ 1,
    TRUE ~ 0
  )
)


corrdata <- corrdata %>% mutate(
  UKT_PLDGRP = case_when(
    HCC == 1 ~ 1,
    ALF == 1 ~ 2,
    HCV == 1 ~ 3,
    PSC == 1 ~ 4,
    HBV == 1 ~ 5,
    PBC == 1 ~ 6,
    ALD == 1 ~ 7,
    AID == 1 ~ 8,
    MET == 1 ~ 9,
    OTH == 1 ~ 10
  )
) %>%
  mutate(UKT_PLDGRP = factor(UKT_PLDGRP, labels = c("HCC", "ALF", "HCV", "PSC", "HBV", "PBC", "ALD", "AID", "Metabolic", "Others")))

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

corrdata %>% select(DEATH_CAUSE_CODE, COD_cardiovascular, COD_infection, COD_liverrelated, COD_other, COD_categorized) %>% View()
