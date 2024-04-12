# INSTALL & LOAD PACKAGES -------------------------------------------------

library(tidyverse)
library(psyntur)
library(reader)
library(car)
library(lm.beta)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(webshot2)
library(magick)


# CLEANING THE DATA SET ---------------------------------------------------

# DELETING ITEMS

clean_total_data <- select(Total_Dataset,
                           -start,
                           -end,
                           -accessed,
                           -completed,
                           -accessed.base,
                           -completed.base,
                           -consent,
                           -heard.study.flyers,
                           -heard.study.sub,
                           -heard.study.email,
                           -heard.study.word,
                           -heard.study.kijiji,
                           -heard.study.fb,
                           -heard.study.twit,
                           -heard.study.insta,
                           -heard.study.othersm,
                           -heard.study.other,
                           -heard.study.other2,
                           -province.residence,
                           -makeup,
                           -alc2,
                           -alc3,
                           -alc4,
                           -alc5,
                           -alc6,
                           -alc.mean,
                           -lq1,
                           -lq2,
                           -lq3,
                           -lq4,
                           -lq5,
                           -lq6,
                           -lq7,
                           -lq8,
                           -tipm.EX2.r,
                           -tipm.AG2.r,
                           -tipm.CONS2.r,
                           -tipm.ES2.r,
                           -tipm.OE2.r,
                           -tipm.EX.mean,
                           -tipm.AG.mean,
                           -tipm.CONS.mean,
                           -tipm.ES.mean,
                           -tipm.OE.mean,
                           -spss1.r,
                           -spss4.r,
                           -spss5.r,
                           -spss.mean,
                           -rstpq.FIL1,
                           -rstpq.FIL2,
                           -rstpq.FIL3,
                           -rstpq.FIL4,
                           -rstpq.FIL5,
                           -guilt.mean,
                           -fear.mean,
                           -host.mean,
                           -dep.mean)

# RENAMING ITEMS

clean_total_data <- rename(clean_total_data,
                           "panas.NEG1" = "panas1",
                           "panas.NEG2" = "panas2",
                           "panas.NEG3" = "panas3",
                           "panas.NEG4" = "panas4",
                           "panas.NEG5" = "panas5",
                           "panas.NEG6" = "panas6",
                           "panas.NEG7" = "panas7",
                           "panas.NEG8" = "panas8",
                           "panas.NEG9" = "panas9",
                           "panas.NEG10" = "panas10",
                           "panas.POS1" = "panas11",
                           "panas.POS2" = "panas12",
                           "panas.POS3" = "panas13",
                           "panas.POS4" = "panas14",
                           "panas.POS5" = "panas15",
                           "panas.POS6" = "panas16",
                           "panas.POS7" = "panas17",
                           "panas.POS8" = "panas18",
                           "panas.POS9" = "panas19",
                           "panas.POS10" = "panas20")

# RECODING NEGATIVELY KEYED ITEMS

total_data_recoded <- mutate(clean_total_data,
                             tipm.EX2 = re_code(tipm.EX2, 1:7, 7:1),
                             tipm.AG1 = re_code(tipm.AG1, 1:7, 7:1),
                             tipm.CONS2 = re_code(tipm.CONS2, 1:7, 7:1),
                             tipm.ES1 = re_code(tipm.ES1, 1:7, 7:1),
                             tipm.OE2 = re_code(tipm.OE2, 1:7, 7:1),
                             spss1 = re_code(spss1, 1:4, 4:1),
                             spss4 = re_code(spss4, 1:4, 4:1),
                             spss5 = re_code(spss5, 1:4, 4:1))

# REORDERING COLUMNS

total_data <- total_data_recoded %>% 
  relocate(guilt2, .after=guilt1) %>%
  relocate(guilt3, .after=guilt2) %>%
  relocate(fear2, .after=fear1) %>%
  relocate(fear3, .after=fear2) %>%
  relocate(host2, .after=host1) %>%
  relocate(host3, .after=host2) %>%
  relocate(posi3, .before=posi4) %>%
  relocate(posi2, .before=posi3) %>%
  relocate(posi1, .before=posi2) %>% 
  relocate(tipm.EX2, .after=tipm.EX1) %>%
  relocate(tipm.AG2, .after=tipm.AG1) %>%
  relocate(tipm.CONS2, .after=tipm.CONS1) %>%
  relocate(tipm.ES2, .after=tipm.ES1) %>%
  relocate(tipm.OE2, .after=tipm.OE1) %>%
  relocate(rstpq.MPI1, .after=sex.other) %>%
  relocate(rstpq.MPI2, .after=rstpq.MPI1) %>%
  relocate(rstpq.MPI3, .after=rstpq.MPI2) %>%
  relocate(rstpq.MPI4, .after=rstpq.MPI3) %>%
  relocate(rstpq.MPI5, .after=rstpq.MPI4) %>%
  relocate(rstpq.W1, .after=rstpq.MPI5) %>%
  relocate(rstpq.W2, .after=rstpq.W1) %>%
  relocate(rstpq.W3, .after=rstpq.W2) %>%
  relocate(rstpq.W4, .after=rstpq.W3) %>%
  relocate(rstpq.W5, .after=rstpq.W4) %>%
  relocate(rstpq.OT1, .after=rstpq.W5) %>%
  relocate(rstpq.OT2, .after=rstpq.OT1) %>%
  relocate(rstpq.OT3, .after=rstpq.OT2) %>%
  relocate(rstpq.OT4, .after=rstpq.OT3) %>%
  relocate(rstpq.OT5, .after=rstpq.OT4) %>% 
  relocate(rstpq.OT6, .after=rstpq.OT5) %>%
  relocate(rstpq.OT7, .after=rstpq.OT6) %>%
  relocate(rstpq.BD1, .after=rstpq.OT7) %>%
  relocate(rstpq.BD2, .after=rstpq.BD1) %>%
  relocate(rstpq.BD3, .after=rstpq.BD2) %>%
  relocate(rstpq.BD4, .after=rstpq.BD3) %>%
  relocate(rstpq.BD5, .after=rstpq.BD4) %>%
  relocate(rstpq.BD6, .after=rstpq.BD5) %>%
  relocate(rstpq.RI1, .after=rstpq.BD6) %>%
  relocate(rstpq.RI2, .after=rstpq.RI1) %>%
  relocate(rstpq.RI3, .after=rstpq.RI2) %>%
  relocate(rstpq.RI4, .after=rstpq.RI3) %>%
  relocate(rstpq.RI5, .after=rstpq.RI4) %>%
  relocate(rstpq.RI6, .after=rstpq.RI5) %>% 
  relocate(rstpq.RI7, .after=rstpq.RI6) %>%
  relocate(rstpq.GDP1, .after=rstpq.RI7) %>%
  relocate(rstpq.GDP2, .after=rstpq.GDP1) %>%
  relocate(rstpq.GDP3, .after=rstpq.GDP2) %>%
  relocate(rstpq.GDP4, .after=rstpq.GDP3) %>%
  relocate(rstpq.GDP5, .after=rstpq.GDP4) %>%
  relocate(rstpq.GDP6, .after=rstpq.GDP5) %>%
  relocate(rstpq.GDP7, .after=rstpq.GDP6) %>%
  relocate(rstpq.RR1, .after=rstpq.GDP7) %>%
  relocate(rstpq.RR2, .after=rstpq.RR1) %>%
  relocate(rstpq.RR3, .after=rstpq.RR2) %>%
  relocate(rstpq.RR4, .after=rstpq.RR3) %>% 
  relocate(rstpq.RR5, .after=rstpq.RR4) %>%
  relocate(rstpq.RR6, .after=rstpq.RR5) %>%
  relocate(rstpq.RR7, .after=rstpq.RR6) %>%
  relocate(rstpq.RR8, .after=rstpq.RR7) %>%
  relocate(rstpq.RR9, .after=rstpq.RR8) %>%
  relocate(rstpq.RR10, .after=rstpq.RR9) %>%
  relocate(rstpq.I1, .after=rstpq.RR10) %>%
  relocate(rstpq.I2, .after=rstpq.I1) %>%
  relocate(rstpq.I3, .after=rstpq.I2) %>%
  relocate(rstpq.I4, .after=rstpq.I3) %>% 
  relocate(rstpq.I5, .after=rstpq.I4) %>%
  relocate(rstpq.I6, .after=rstpq.I5) %>%
  relocate(rstpq.I7, .after=rstpq.I6) %>%
  relocate(rstpq.I8, .after=rstpq.I7) %>%
  relocate(rstpq.DF1, .after=rstpq.I8) %>%
  relocate(rstpq.DF2, .after=rstpq.DF1) %>%
  relocate(rstpq.DF3, .after=rstpq.DF2) %>%
  relocate(rstpq.DF4, .after=rstpq.DF3) %>%
  relocate(rstpq.DF5, .after=rstpq.DF4) %>%
  relocate(rstpq.DF6, .after=rstpq.DF5) %>% 
  relocate(rstpq.DF7, .after=rstpq.DF6) %>%
  relocate(rstpq.DF8, .after=rstpq.DF7) %>%
  relocate(rstpq.FL1, .after=rstpq.DF8) %>%
  relocate(rstpq.FL2, .after=rstpq.FL1) %>%
  relocate(rstpq.FL3, .after=rstpq.FL2) %>%
  relocate(rstpq.PA1, .after=rstpq.FL3) %>%
  relocate(rstpq.PA2, .after=rstpq.PA1) %>%
  relocate(rstpq.FZ1, .after=rstpq.PA2) %>%
  relocate(rstpq.FZ2, .after=rstpq.FZ1) %>%
  relocate(rstpq.FZ3, .after=rstpq.FZ2) %>%
  relocate(rstpq.FZ4, .after=rstpq.FZ3) %>%
  relocate(rstpq.FZ5, .after=rstpq.FZ4) %>% 
  relocate(rstpq.P1, .after=rstpq.FZ5) %>%
  relocate(rstpq.P2, .after=rstpq.P1) %>%
  relocate(rstpq.P3, .after=rstpq.P2) %>%
  relocate(rstpq.P4, .after=rstpq.P3) %>%
  relocate(rstpq.P5, .after=rstpq.P4) %>%
  relocate(rstpq.P6, .after=rstpq.P5) %>%
  relocate(rdmq.SOC2, .after=rdmq.SOC1) %>%
  relocate(rdmq.SOC3, .after=rdmq.SOC2) %>% 
  relocate(rdmq.SOC4, .after=rdmq.SOC3) %>%
  relocate(rdmq.SOC5, .after=rdmq.SOC4) %>%
  relocate(rdmq.CA2, .after=rdmq.CA1) %>% 
  relocate(rdmq.CA3, .after=rdmq.CA2) %>%
  relocate(rdmq.CA4, .after=rdmq.CA3) %>%
  relocate(rdmq.CD1, .after=rdmq.CA4) %>%
  relocate(rdmq.CD2, .after=rdmq.CD1) %>%
  relocate(rdmq.CD3, .after=rdmq.CD2) %>%
  relocate(rdmq.CD4, .after=rdmq.CD3) %>%
  relocate(rdmq.CD5, .after=rdmq.CD4) %>%
  relocate(rdmq.CD6, .after=rdmq.CD5) %>%
  relocate(rdmq.CD7, .after=rdmq.CD6) %>%
  relocate(rdmq.CD8, .after=rdmq.CD7) %>%
  relocate(rdmq.CD9, .after=rdmq.CD8) %>%
  relocate(rdmq.EN2, .after=rdmq.EN1) %>%
  relocate(rdmq.EN3, .after=rdmq.EN2) %>% 
  relocate(rdmq.EN4, .after=rdmq.EN3) %>%
  relocate(rdmq.EN5, .after=rdmq.EN4) %>%
  relocate(rdmq.CON2, .after=rdmq.CON1) %>%
  relocate(rdmq.CON3, .after=rdmq.CON2) %>% 
  relocate(rdmq.CON4, .after=rdmq.CON3) %>%
  relocate(rdmq.CON5, .after=rdmq.CON4)


# DEMOGRAPHICS ------------------------------------------------------------

# COUNTING GENDER

sex_by_id <- total_data %>%
  group_by(id) %>%
  summarise(sex = unique(sex))

sum(sex_by_id$sex=='0')   #female
sum(sex_by_id$sex=='1')   #male

if(any(!is.na(total_data$sex.other))) {
  print("There is data in the column.")
} else {
  print("The column is empty.")
}


# LOOKING AT MISSING DATA -------------------------------------------------

colSums(is.na(total_data))


# CALCULATING AGGREGATE SCORES --------------------------------------------

# CALCULATING TOTAL SCORES FOR SUBSCALES OF NEGATIVE AFFECT

negative_affect <- total_scores(total_data,
                                id = id,
                           guilt = starts_with('guilt'),
                           fear = starts_with('fear'),
                           hostility = starts_with('host'),
                           depressed_affect = starts_with('dep'),
                           .method = 'sum_like')

# MERGING NEGATIVE AFFECT SUBSCALES

negative_affect <- negative_affect %>%
  mutate(negative_affect = rowSums(select(., guilt, 
                                          fear, 
                                          hostility, 
                                          depressed_affect)))

# CALCULATING MEANS OF THE BIG 5 SUBSCALES, SOCIAL SUPPORT & ALCOHOL USE (per day)

means_part1 <- total_scores(total_data,
                           extraversion = starts_with('tipm.EX'),
                           agreeableness = starts_with('tipm.AG'),
                           conscientiousness = starts_with('tipm.CONS'),
                           emotional_stability = starts_with('tipm.ES'),
                           openness = starts_with('tipm.OE'),
                           perceived_social_support = starts_with('spss'),
                           alcohol_use = drink,
                           alcohol_consumption_baseline = alc1)

# SELECTING PREVIOUSLY CALCULATED COLUMNS OF MEANS FROM THE ORIGINAL DATAFRAME

means_part2 <- total_data %>%
  select(ends_with("mean"))

# MERGING THE 3 DATAFRAMES

total_data_means <- cbind(means_part2, means_part1, negative_affect)

# GROUPING BY ID 

total_data_means <- total_data_means %>%
  group_by(id) %>%
  summarise(self_oriented_perfectionism = mean(mps.SOP.mean, na.rm = TRUE),
            socially_prescribed_perfectionism = mean(mps.SPP.mean, na.rm = TRUE),
            other_oriented_perfectionism = mean(mps.OOP.mean, na.rm = TRUE),
            perfectionism_cognitions = mean(pc.mean, na.rm = TRUE),
            perfectionistic_self_presentation = mean(psp.mean, na.rm = TRUE),
            alcohol_use = mean(alcohol_use, na.rm = TRUE),
            social_motives = mean(social.mean, na.rm = TRUE),
            enhancement_motives = mean(enhancement.mean, na.rm = TRUE),
            coping_motives = mean(coping.mean, na.rm = TRUE),
            conformity_motives = mean(conformity.mean, na.rm = TRUE),
            alcohol_problems = mean(apc.mean, na.rm = TRUE),
            social_anxiety = mean(ssa.mean, na.rm = TRUE),
            satisfaction_with_life = mean(swl.mean, na.rm = TRUE),
            negative_affect = mean(negative_affect, na.rm = TRUE),
            positive_affect = mean(posi.mean, na.rm = TRUE),
            extraversion = mean(extraversion, na.rm = TRUE),
            agreeableness = mean(agreeableness, na.rm = TRUE),
            conscientiousness = mean(conscientiousness, na.rm = TRUE),
            emotional_stability = mean(emotional_stability, na.rm = TRUE),
            openness = mean(openness, na.rm = TRUE),
            binge_eating = mean(bes.mean, na.rm = TRUE),
            perceived_social_support = mean(perceived_social_support, na.rm = TRUE),
            behavioural_inhibition_system_baseline = mean(rstpq.BIS.mean, na.rm = TRUE),
            behavioural_approach_system_baseline = mean(rstpq.BAS.mean, na.rm = TRUE),
            defensive_fight_baseline = mean(rstpq.DF.mean, na.rm = TRUE),
            fight_flight_freeze_system_baseline = mean(rstpq.FFF.mean, na.rm = TRUE),
            panic_baseline = mean(rstpq.P.mean, na.rm = TRUE),
            rigid_baseline = mean(rigid.mean, na.rm = TRUE),
            self_critical_baseline = mean(self.critical.mean, na.rm = TRUE),
            narcissistic_baseline = mean(narcissistic.mean, na.rm = TRUE),
            alcohol_consumption_baseline = mean(alcohol_consumption_baseline, na.rm = TRUE),
            alcohol_problem_index_baseline = mean(rapi.mean, na.rm = TRUE),
            social_motives_baseline = mean(rdmq.SOC.mean, na.rm = TRUE),
            coping_anxiety_motives_baseline = mean(rdmq.CA.mean, na.rm = TRUE),
            coping_depression_motives_baseline = mean(rdmq.CD.mean, na.rm = TRUE),
            enhancement_motives_baseline = mean(rdmq.EN.mean, na.rm = TRUE),
            conformity_motives_baseline = mean(rdmq.CON.mean, na.rm = TRUE),
            negative_affect_baseline = mean(negativeaffect.mean, na.rm = TRUE),
            positive_affect_baseline = mean(positiveaffect.mean, na.rm = TRUE))



# CALCULATING DESCRIPTIVE STATISTICS --------------------------------------

descriptive_stats <- describe_across(total_data_means, 
                variables = everything(), 
                functions = list(avg = mean_xna, stdev = sd_xna),
                pivot = TRUE)

# ROUNDING THE RESULTS TO 2 DECIMALS

descriptive_stats <- describe_across(total_data_means, 
                                     variables = everything(), 
                                     functions = list(avg = mean_xna, stdev = sd_xna),
                                     pivot = TRUE) %>%
  mutate(across(avg:stdev, ~ round(., 2)))



# CREATING A DESCRIPTIVE STATISTICS TABLE --------------------------------------------------------

# MODYFYING VARIABLE NAMES

new_variable_names <- c("ID", "Self-oriented perfectionism", "Socially prescribed perfectionism", "Other-oriented perfectionism",
                    "Perfectionism cognitions", "Perfectionistic self-presentation", "Alcohol use", "Social motives",
                    "Enhancement motives", "Coping motives", "Conformity motives", "Alcohol problems", "Social anxiety",
                    "Satisfaction with life", "Negative affect", "Positive affect", "Extraversion", "Agreeableness",
                    "Conscientiousness", "Emotional stability", "Openness", "Binge eating", "Perceived social support",
                    "Behavioural inhibition system", "Behavioural approach system", "Defensive fight", 
                    "Fight-flight-freeze system", "Panic", "Rigid", "Self-critical", "Narcissistic", "Alcohol consumption",
                    "Alcohol problem index", "Social motives baseline", "Coping anxiety motives baseline", 
                    "Coping depression motives baseline", "Enhancement motives baseline", "Conformity motives baseline",
                    "Negative affect baseline", "Positive affect baseline")

descriptive_stats$variable <- new_variable_names

# EXCLUDING THE 'ID' VARIABLE

descriptive_stats <- descriptive_stats[-1, ]

# RENAMING THE COLUMNS

colnames(descriptive_stats) <- c("Variable", "Mean", "SD")

# CREATING THE TABLE

table <- descriptive_stats %>%
  select(Variable, Mean, SD) 



# MULTIPLE LINEAR REGRESSION ----------------------------------------------

model_total <- lm(alcohol_use ~ self_oriented_perfectionism + socially_prescribed_perfectionism + other_oriented_perfectionism +
                  perfectionism_cognitions + perfectionistic_self_presentation + social_motives + enhancement_motives +
                  coping_motives + conformity_motives + alcohol_problems + social_anxiety + satisfaction_with_life +
                  negative_affect + positive_affect + extraversion + agreeableness + conscientiousness + emotional_stability +
                  openness + binge_eating + perceived_social_support + behavioural_inhibition_system_baseline +
                  behavioural_approach_system_baseline + defensive_fight_baseline + fight_flight_freeze_system_baseline +
                  panic_baseline + rigid_baseline + self_critical_baseline + narcissistic_baseline + alcohol_consumption_baseline +
                  alcohol_problem_index_baseline + social_motives_baseline + coping_anxiety_motives_baseline +
                  coping_depression_motives_baseline + enhancement_motives_baseline + conformity_motives_baseline +
                  negative_affect_baseline + positive_affect_baseline, data = total_data_means) 

# CONFIDENCE INTERVALS LRM ----------------------------------------------------

confint(model_total)

# MULTICOLLINEARITY LRM -------------------------------------------------------

vif(model_total)

# STANDARDIZED COEFFICIENTS LRM -----------------------------------------------

model_total_standardized <- lm.beta(model_total)




# FORWARD-BACKWARD STEPWISE REGRESSION ---------------------------------------

model_step <- step(model_total)

# CONFIDENCE INTERVALS OF STEPWISE REGRESSION MODEL --------------------------

confint(model_step)

# MULTICOLLINEARITY SRM -------------------------------------------------------

vif(model_step)

# STANDARDIZED COEFFICIENTS SRM -----------------------------------------------

model_step_standardized <- lm.beta(model_step)




# MLR MODEL OF SEPARATE ITEMS (of significant variables) ---------------------------------------------

# CALCULATING MEANS OF SEPARATE ITEMS OF STATISTICALLY SIGNIFICANT SCALES (GROUPED BY PARTICIPANT ID)

separate_items_means <- total_data %>%
  group_by(id) %>%
  summarise(
    alcohol_use = mean(drink, na.rm = TRUE),
    mps.OOP1_mean = mean(mps.OOP1, na.rm = TRUE),
    mps.OOP2_mean = mean(mps.OOP2, na.rm = TRUE),
    mps.OOP3_mean = mean(mps.OOP3, na.rm = TRUE),
    mps.OOP4_mean = mean(mps.OOP4, na.rm = TRUE),
    mps.OOP5_mean = mean(mps.OOP5, na.rm = TRUE),
    pc1_mean = mean(pc1, na.rm = TRUE),
    pc2_mean = mean(pc2, na.rm = TRUE),
    pc3_mean = mean(pc3, na.rm = TRUE),
    conformity1_mean = mean(conformity1, na.rm = TRUE),
    conformity2_mean = mean(conformity2, na.rm = TRUE),
    conformity3_mean = mean(conformity3, na.rm = TRUE),
    posi1_mean = mean(posi1, na.rm = TRUE),
    posi2_mean = mean(posi2, na.rm = TRUE),
    posi3_mean = mean(posi3, na.rm = TRUE),
    posi4_mean = mean(posi4, na.rm = TRUE),
    posi5_mean = mean(posi5, na.rm = TRUE),
    tipm.AG1_mean = mean(tipm.AG1, na.rm = TRUE),
    tipm.AG2_mean = mean(tipm.AG2, na.rm = TRUE),
    tipm.ES1_mean = mean(tipm.ES1, na.rm = TRUE),
    tipm.ES2_mean = mean(tipm.ES2, na.rm = TRUE),
    spss1_mean = mean(spss1, na.rm = TRUE),
    spss2_mean = mean(spss2, na.rm = TRUE),
    spss3_mean = mean(spss3, na.rm = TRUE),
    spss4_mean = mean(spss4, na.rm = TRUE),
    spss5_mean = mean(spss5, na.rm = TRUE),
    spss6_mean = mean(spss6, na.rm = TRUE),
    rstpq.MPI1_mean = mean(rstpq.MPI1, na.rm = TRUE),
    rstpq.MPI2_mean = mean(rstpq.MPI2, na.rm = TRUE),
    rstpq.MPI3_mean = mean(rstpq.MPI3, na.rm = TRUE),
    rstpq.MPI4_mean = mean(rstpq.MPI4, na.rm = TRUE),
    rstpq.MPI5_mean = mean(rstpq.MPI5, na.rm = TRUE),
    rstpq.W1_mean = mean(rstpq.W1, na.rm = TRUE),
    rstpq.W2_mean = mean(rstpq.W2, na.rm = TRUE),
    rstpq.W3_mean = mean(rstpq.W3, na.rm = TRUE),
    rstpq.W4_mean = mean(rstpq.W4, na.rm = TRUE),
    rstpq.W5_mean = mean(rstpq.W5, na.rm = TRUE),
    rstpq.OT1_mean = mean(rstpq.OT1, na.rm = TRUE),
    rstpq.OT2_mean = mean(rstpq.OT2, na.rm = TRUE),
    rstpq.OT3_mean = mean(rstpq.OT3, na.rm = TRUE),
    rstpq.OT4_mean = mean(rstpq.OT4, na.rm = TRUE),
    rstpq.OT5_mean = mean(rstpq.OT5, na.rm = TRUE),
    rstpq.OT6_mean = mean(rstpq.OT6, na.rm = TRUE),
    rstpq.OT7_mean = mean(rstpq.OT7, na.rm = TRUE),
    rstpq.BD1_mean = mean(rstpq.BD1, na.rm = TRUE),
    rstpq.BD2_mean = mean(rstpq.BD2, na.rm = TRUE),
    rstpq.BD3_mean = mean(rstpq.BD3, na.rm = TRUE),
    rstpq.BD4_mean = mean(rstpq.BD4, na.rm = TRUE),
    rstpq.BD5_mean = mean(rstpq.BD5, na.rm = TRUE),
    rstpq.BD6_mean = mean(rstpq.BD6, na.rm = TRUE),
    rstpq.P1_mean = mean(rstpq.P1, na.rm = TRUE),
    rstpq.P2_mean = mean(rstpq.P2, na.rm = TRUE),
    rstpq.P3_mean = mean(rstpq.P3, na.rm = TRUE),
    rstpq.P4_mean = mean(rstpq.P4, na.rm = TRUE),
    rstpq.P5_mean = mean(rstpq.P5, na.rm = TRUE),
    rstpq.P6_mean = mean(rstpq.P6, na.rm = TRUE),
    btps.SOP1_mean = mean(btps.SOP1, na.rm = TRUE),
    btps.SOP2_mean = mean(btps.SOP2, na.rm = TRUE),
    btps.SOP3_mean = mean(btps.SOP3, na.rm = TRUE),
    btps.SOP4_mean = mean(btps.SOP4, na.rm = TRUE),
    btps.SOP5_mean = mean(btps.SOP5, na.rm = TRUE),
    btps.SWC1_mean = mean(btps.SWC1, na.rm = TRUE),
    btps.SWC2_mean = mean(btps.SWC2, na.rm = TRUE),
    btps.SWC3_mean = mean(btps.SWC3, na.rm = TRUE),
    btps.SWC4_mean = mean(btps.SWC4, na.rm = TRUE),
    btps.SWC5_mean = mean(btps.SWC5, na.rm = TRUE),
    alc1_mean = mean(alc1, na.rm = TRUE),
    rapi1_mean = mean(rapi1, na.rm = TRUE),
    rapi2_mean = mean(rapi2, na.rm = TRUE),
    rapi3_mean = mean(rapi3, na.rm = TRUE),
    rapi4_mean = mean(rapi4, na.rm = TRUE),
    rapi5_mean = mean(rapi5, na.rm = TRUE),
    rapi6_mean = mean(rapi6, na.rm = TRUE),
    rapi7_mean = mean(rapi7, na.rm = TRUE),
    rapi8_mean = mean(rapi8, na.rm = TRUE),
    rapi9_mean = mean(rapi9, na.rm = TRUE),
    rapi10_mean = mean(rapi10, na.rm = TRUE),
    rapi11_mean = mean(rapi11, na.rm = TRUE),
    rapi12_mean = mean(rapi12, na.rm = TRUE),
    rapi13_mean = mean(rapi13, na.rm = TRUE),
    rapi14_mean = mean(rapi14, na.rm = TRUE),
    rapi15_mean = mean(rapi15, na.rm = TRUE),
    rapi16_mean = mean(rapi16, na.rm = TRUE),
    rapi17_mean = mean(rapi17, na.rm = TRUE),
    rapi18_mean = mean(rapi18, na.rm = TRUE),
    rapi19_mean = mean(rapi19, na.rm = TRUE),
    rapi20_mean = mean(rapi20, na.rm = TRUE),
    rapi21_mean = mean(rapi21, na.rm = TRUE),
    rapi22_mean = mean(rapi22, na.rm = TRUE),
    rapi23_mean = mean(rapi23, na.rm = TRUE),
    rdmq.SOC1_mean = mean(rdmq.SOC1, na.rm = TRUE),
    rdmq.SOC2_mean = mean(rdmq.SOC2, na.rm = TRUE),
    rdmq.SOC3_mean = mean(rdmq.SOC3, na.rm = TRUE),
    rdmq.SOC4_mean = mean(rdmq.SOC4, na.rm = TRUE),
    rdmq.SOC5_mean = mean(rdmq.SOC5, na.rm = TRUE),
    rdmq.CA1_mean = mean(rdmq.CA1, na.rm = TRUE),
    rdmq.CA2_mean = mean(rdmq.CA2, na.rm = TRUE),
    rdmq.CA3_mean = mean(rdmq.CA3, na.rm = TRUE),   
    rdmq.CA4_mean = mean(rdmq.CA4, na.rm = TRUE),
    rdmq.CON1_mean = mean(rdmq.CON1, na.rm = TRUE),  
    rdmq.CON2_mean = mean(rdmq.CON2, na.rm = TRUE),
    rdmq.CON3_mean = mean(rdmq.CON3, na.rm = TRUE),  
    rdmq.CON4_mean = mean(rdmq.CON4, na.rm = TRUE),
    rdmq.CON5_mean = mean(rdmq.CON5, na.rm = TRUE),  
    panas.NEG1_mean = mean(panas.NEG1, na.rm = TRUE),
    panas.NEG2_mean = mean(panas.NEG2, na.rm = TRUE),  
    panas.NEG3_mean = mean(panas.NEG3, na.rm = TRUE),
    panas.NEG4_mean = mean(panas.NEG4, na.rm = TRUE),  
    panas.NEG5_mean = mean(panas.NEG5, na.rm = TRUE),
    panas.NEG6_mean = mean(panas.NEG6, na.rm = TRUE),  
    panas.NEG7_mean = mean(panas.NEG7, na.rm = TRUE),
    panas.NEG8_mean = mean(panas.NEG8, na.rm = TRUE),  
    panas.NEG9_mean = mean(panas.NEG9, na.rm = TRUE),
    panas.NEG10_mean = mean(panas.NEG10, na.rm = TRUE),  
    panas.POS1_mean = mean(panas.POS1, na.rm = TRUE),
    panas.POS2_mean = mean(panas.POS2, na.rm = TRUE),  
    panas.POS3_mean = mean(panas.POS3, na.rm = TRUE),
    panas.POS4_mean = mean(panas.POS4, na.rm = TRUE),
    panas.POS5_mean = mean(panas.POS5, na.rm = TRUE),
    panas.POS6_mean = mean(panas.POS6, na.rm = TRUE),  
    panas.POS7_mean = mean(panas.POS7, na.rm = TRUE),
    panas.POS8_mean = mean(panas.POS8, na.rm = TRUE),  
    panas.POS9_mean = mean(panas.POS9, na.rm = TRUE),
    panas.POS10_mean = mean(panas.POS10, na.rm = TRUE),
    .groups = 'drop'
  )

# REMOVING MISSING VALUES

separate_items_means_na <- na.omit(separate_items_means)

# MULTIPLE LINEAR REGRESSION MODEL

separate_items_model <- lm(alcohol_use ~ mps.OOP1_mean + mps.OOP2_mean + mps.OOP3_mean + mps.OOP4_mean + mps.OOP5_mean + 
                             pc1_mean + pc2_mean + pc3_mean + conformity1_mean + conformity2_mean + conformity3_mean + 
                             posi1_mean + posi2_mean + posi3_mean + posi4_mean + posi5_mean + tipm.AG1_mean + tipm.AG2_mean + 
                             tipm.ES1_mean + tipm.ES2_mean + spss1_mean + spss2_mean + spss3_mean + spss4_mean + spss5_mean + 
                             spss6_mean + rstpq.MPI1_mean + rstpq.MPI2_mean + rstpq.MPI3_mean + rstpq.MPI4_mean + rstpq.MPI5_mean + 
                             rstpq.W1_mean + rstpq.W2_mean + rstpq.W3_mean + rstpq.W4_mean + rstpq.W5_mean + rstpq.OT1_mean + 
                             rstpq.OT2_mean + rstpq.OT3_mean + rstpq.OT4_mean + rstpq.OT5_mean + rstpq.OT6_mean + rstpq.OT7_mean + 
                             rstpq.BD1_mean + rstpq.BD2_mean + rstpq.BD3_mean + rstpq.BD4_mean + rstpq.BD5_mean + rstpq.BD6_mean + 
                             rstpq.P1_mean + rstpq.P2_mean + rstpq.P3_mean + rstpq.P4_mean + rstpq.P5_mean + rstpq.P6_mean + 
                             btps.SOP1_mean + btps.SOP2_mean + btps.SOP3_mean + btps.SOP4_mean + btps.SOP5_mean + btps.SWC1_mean + 
                             btps.SWC2_mean + btps.SWC3_mean + btps.SWC4_mean + btps.SWC5_mean + alc1_mean + rapi1_mean + 
                             rapi2_mean + rapi3_mean + rapi4_mean + rapi5_mean + rapi6_mean + rapi7_mean + rapi8_mean + rapi9_mean + 
                             rapi10_mean + rapi11_mean + rapi12_mean + rapi13_mean + rapi14_mean + rapi15_mean + rapi16_mean + 
                             rapi17_mean + rapi18_mean + rapi19_mean + rapi20_mean + rapi21_mean + rapi22_mean + rapi23_mean + 
                             rdmq.SOC1_mean + rdmq.SOC2_mean + rdmq.SOC3_mean + rdmq.SOC4_mean + rdmq.SOC5_mean + rdmq.CA1_mean + 
                             rdmq.CA2_mean + rdmq.CA3_mean + rdmq.CA4_mean + rdmq.CON1_mean + rdmq.CON2_mean + rdmq.CON3_mean + 
                             rdmq.CON4_mean + rdmq.CON5_mean + panas.NEG1_mean + panas.NEG2_mean + panas.NEG3_mean + panas.NEG4_mean + 
                             panas.NEG5_mean + panas.NEG6_mean + panas.NEG7_mean + panas.NEG8_mean + panas.NEG9_mean + panas.NEG10_mean + 
                             panas.POS1_mean + panas.POS2_mean + panas.POS3_mean + panas.POS4_mean + panas.POS5_mean + panas.POS6_mean + 
                             panas.POS7_mean + panas.POS8_mean + panas.POS9_mean + panas.POS10_mean, data = separate_items_means_na)

# CONFIDENCE INTERVALS ----------------------------------------------------

confint(separate_items_model)

# MULTICOLLINEARITY LRM -------------------------------------------------------

vif(separate_items_model)

# STANDARDIZED COEFFICIENTS LRM -----------------------------------------------

separate_items_model_standardized <- lm.beta(separate_items_model)



# STEPWISE REGRESSION MODEL -----------------------------------------------

separate_items_step_model <- step(separate_items_model)

# CONFIDENCE INTERVALS SRM ----------------------------------------------------

confint(separate_items_step_model)

# MULTICOLLINEARITY SRM -------------------------------------------------------

vif(separate_items_step_model)

# STANDARDIZED COEFFICIENTS SRM -----------------------------------------------

separate_items_step_model_standardized <- lm.beta(separate_items_step_model)



