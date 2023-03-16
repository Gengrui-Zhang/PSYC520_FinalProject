# Load packages
library(lavaan)
library(lme4)
library(haven)
library(semTools)
library(semPlot)
library(dplyr)
library(polycor)
library(labelled)
library(psych)
library(foreign)
library(ggplot2)
library(tidyr)
library(reshape)
library(fastDummies)
library(corrplot)
library(GPArotation)
library(DiagrammeR)

# Read data
df <- as.data.frame(read_sav("~/Desktop/Fall 2022/PM 603/Final Project/Generations.sav"), label)
df <- zap_formats(zap_labels(df))
# Deleted cases for straight (n = 11) and other sexuality (n = 70)

# Stage 1: Conduct exploratory and confirmatory factor analysis to determine the model of drug use

# EFA: Suggest a 2-factor model
df_drug_w1 <- df %>% select(W1Q90:W1Q100)
names_drug_w1 <- names(df_drug_w1)
df_drug_w1 <- as.data.frame(matrix(sapply(df_drug_w1, as.numeric), ncol = 11))
names(df_drug_w1) <- names_drug_w1

pc <- hetcor(df_drug_w1, ML=TRUE)
faPC <- fa(r=pc$correlations, nfactors=2, n.obs=nrow(df_drug_w1), rotate="varimax")
fa.diagram(faPC)

# CFA: Very good fit
cfa_drug_w1 <- "D_Consump =~ W1Q90 + W1Q91 + W1Q92 + W1Q93
                D_Past =~ W1Q94 + W1Q95 + W1Q96 + W1Q97 + W1Q98 + 
                          W1Q99 + W1Q100
                Drug_use =~ D_Consump + D_Past"
fit_drug_w1 <- cfa(
                  cfa_drug_w1, 
                  data = df_drug_w1,
                  ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                              "W1Q94", "W1Q95", "W1Q96", "W1Q97",
                              "W1Q98", "W1Q99", "W1Q100", "W1Q100")
                )

summary(fit_drug_w1)
fitmeasures(fit_drug_w1)
semPaths(fit_drug_w1)

cfa_drug_w1_2 <- "
                  Drug_use =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 +
                              W1Q94 + W1Q95 + W1Q96 + W1Q97 + W1Q98 + 
                              W1Q99 + W1Q100
                "
fit_drug_w1_2 <- cfa(
  cfa_drug_w1_2, 
  data = df_drug_w1,
  ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
              "W1Q94", "W1Q95", "W1Q96", "W1Q97",
              "W1Q98", "W1Q99", "W1Q100", "W1Q100")
)

summary(fit_drug_w1_2)
fitmeasures(fit_drug_w1_2)
semPaths(fit_drug_w1_2)

# Stage 4: Assessing community connectedness, adverse childhood experience, everyday discrimination, 
#          outness, felt stigma, internalizaed homophobia, social support

# Community
drug_demo <- df %>% select(STUDYID,
                           W1GENDER,
                           W1SEXMINID,
                           SCREEN_RACE,
                           GEDUC1,
                           W1POVERTY,
                           W1Q90:W1Q100) %>%
  mutate(W1GENDER = W1GENDER,
         W1ORI = W1SEXMINID,
         W1RACE = SCREEN_RACE,
         W1EDU = GEDUC1,
         W1POVERTY = W1POVERTY)

drug_comm <- df %>% 
  select(W1Q53:W1Q59) %>%
  mutate(W1Q53 = factor(W1Q53),
         W1Q54 = factor(W1Q54),
         W1Q55 = factor(W1Q55),
         W1Q56 = factor(W1Q56),
         W1Q57 = factor(W1Q57),
         W1Q58 = factor(W1Q58),
         W1Q59 = factor(W1Q59)) %>%
  as.data.frame()
drug_comm <- cbind(drug_demo, drug_comm)

drug_ace <- df %>% 
  select(W1Q151:W1Q161) %>% 
  mutate(W1Q151 = replace(W1Q151, W1Q151 == 3|W1Q151 == 4, NA),
         W1Q152 = replace(W1Q152, W1Q152 == 3|W1Q152 == 4, NA),
         W1Q153 = replace(W1Q153, W1Q153 == 3|W1Q153 == 4, NA),
         W1Q154 = replace(W1Q154, W1Q154 == 3|W1Q154 == 4, NA),
         W1Q155 = replace(W1Q155, W1Q155 == 3|W1Q155 == 4, NA),
         W1Q156 = replace(W1Q156, W1Q156 == 3|W1Q156 == 4, NA),
         W1Q157 = replace(W1Q157, W1Q157 == 3|W1Q157 == 4, NA),
         W1Q158 = replace(W1Q158, W1Q158 == 3|W1Q158 == 4, NA),
         W1Q159 = replace(W1Q159, W1Q159 == 3|W1Q159 == 4, NA),
         W1Q160 = replace(W1Q160, W1Q160 == 3|W1Q160 == 4, NA),
         W1Q161 = replace(W1Q161, W1Q161 == 3|W1Q161 == 4, NA)) %>%
  as.data.frame()
drug_ace <- cbind(drug_comm, drug_ace)

# Everyday Discrimination

drug_evedis <- df %>% select(W1Q144A:W1Q144I)
drug_evedis <- cbind(drug_ace, drug_evedis) %>%
  as.data.frame()

# CFA

cfa_evedis <- "
            discrimination =~ W1Q144A + W1Q144B + W1Q144C + W1Q144D + W1Q144E +
                              W1Q144F + W1Q144G + W1Q144H + W1Q144I
            "
fit_evedis <- sem(cfa_evedis, 
                  drug_evedis,
                  ordered = c("W1Q144A", "W1Q144B", "W1Q144C", "W1Q144D", "W1Q144E", "W1Q144F", 
                              "W1Q144G", "W1Q144H", "W1Q144I"))
summary(fit_evedis)
fitmeasures(fit_evedis)

# Outness

drug_out <- df %>% select(W1Q123A:W1Q123D)
drug_out <- cbind(drug_evedis, drug_out) %>%
  as.data.frame()

cfa_out <- "outness =~ W1Q123A + W1Q123B + W1Q123C + W1Q123D"
fit_out <- sem(cfa_out, 
               drug_out,
               ordered = c("W1Q123A", "W1Q123B", "W1Q123C", "W1Q123D"))
summary(fit_out)
fitmeasures(fit_out)

# Felt stigma

drug_stigma <- df %>% select(W1Q125:W1Q127)
drug_stigma <- cbind(drug_out, drug_stigma) %>%
  as.data.frame() 

cfa_stigma <- "stigma =~ W1Q125 + W1Q126 + W1Q127"
fit_stigma <- sem(cfa_stigma, 
                  drug_stigma)
summary(fit_stigma, standardized = TRUE, rsquare = TRUE)
fitmeasures(fit_stigma)

# Homophobia

drug_phobia <- df %>% select(W1Q128:W1Q132)
drug_phobia <- cbind(drug_stigma, drug_phobia) %>%
  as.data.frame()

cfa_phobia <- "phobia =~ W1Q128 + W1Q129 + W1Q130 + W1Q131 + W1Q132"
fit_phobia <- sem(cfa_phobia, 
                  drug_phobia)
summary(fit_phobia, standardized = TRUE, rsquare = TRUE)
fitmeasures(fit_phobia)

# Social support

drug_support <- df %>% select(W1Q164A:W1Q164L)
drug_support <- cbind(drug_support, drug_phobia) %>%
  as.data.frame()

cfa_support <- "soc_spt =~ W1Q164A + W1Q164B + W1Q164C + W1Q164D + W1Q164E +
                           W1Q164F + W1Q164G + W1Q164H + W1Q164I + W1Q164J +
                           W1Q164K + W1Q164L"
fit_support <- cfa(cfa_support, 
                  drug_support,
                  ordered = c("W1Q164A", "W1Q164B", "W1Q164C", "W1Q164D", "W1Q164E",
                              "W1Q164F", "W1Q164G", "W1Q164H", "W1Q164I", "W1Q164J",
                              "W1Q164K", "W1Q164L"))
summary(fit_support, standardized = TRUE, rsquare = TRUE)
fitmeasures(fit_support)

# Social well-being

drug_well <- df %>% 
  select(W1Q04:W1Q18) 
drug_well <- cbind(drug_support, drug_well)

cfa_well <- "well_being =~ W1Q04 + W1Q05 + W1Q06 + W1Q07 + W1Q08 + W1Q09 + 
                              W1Q10 + W1Q11 + W1Q12 + W1Q13 + W1Q14 + 
                              W1Q15 + W1Q16 + W1Q17 + W1Q18"
fit_well <- cfa(cfa_well, 
                drug_well,
                ordered = c("W1Q04", "W1Q05", "W1Q06", "W1Q07", "W1Q08", "W1Q09",
                            "W1Q10", "W1Q11", "W1Q12", "W1Q13", "W1Q14",
                            "W1Q15", "W1Q16", "W1Q17", "W1Q18"))
summary(fit_well, standardized = TRUE, rsquare = TRUE)
fitmeasures(fit_well)

# Mental disability

drug_mental <- df %>% select(W1Q77A:W1Q77F)
drug_mental <- cbind(drug_well, drug_mental) %>%
  as.data.frame()

cfa_mental <- "mental =~ W1Q77A + W1Q77B + W1Q77C + W1Q77D + W1Q77E + W1Q77F"
fit_mental <- sem(cfa_mental, 
                  drug_mental)
summary(fit_mental, standardized = TRUE, rsquare = TRUE)
fitmeasures(fit_mental)

# Stage 5: Simple Hypotheses
# Hypothesis 1: adverse childhood experience is positively associated with drug use
# Not significant

model_ace_drug <- "
                  # Drug Use
                  drug_freq =~ W1Q90 + W1Q91 + W1Q92 + W1Q93
                  drug_past =~ W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                               W1Q98 + W1Q99 + W1Q100
                  drug_intention =~ drug_freq + drug_past
                  
                  # ACE
                  ace_hurt =~ W1Q152 + W1Q153 + W1Q154 + W1Q156 + W1Q157 + W1Q158
                  ace_sexual =~ W1Q159 + W1Q160 + W1Q161
                  ace =~ ace_hurt + ace_sexual
                  
                  drug_intention ~ ace
                  "

fit_ace_drug <- sem(model_ace_drug, 
                    drug_mental,
                    std.lv = TRUE,
                    meanstructure = TRUE)
summary(fit_ace_drug, 
        standardized = TRUE, 
        rsquare = TRUE)
fitmeasures(fit_ace_drug)
semPaths(fit_ace_drug)

# Hypothesis 2: Community connectedness is positively associated with drug use
# Not significant

model_comm_drug <- "
                  # Drug Use
                  drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                               W1Q98 + W1Q99 + W1Q100

                  # Community connectedness
                  comm =~ W1Q53 + W1Q54 + W1Q55 + W1Q56 + W1Q57 + W1Q58 + W1Q59
                  
                  # Regression
                  drug_intention ~ comm
                  "

fit_comm_drug <- sem(model_comm_drug, 
                     drug_mental,
                    ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                "W1Q98", "W1Q99", "W1Q100",
                                "W1Q53", "W1Q54", "W1Q55",
                                "W1Q56", "W1Q57", "W1Q58", "W1Q59"),
                    std.lv = TRUE,
                    meanstructure = TRUE)
summary(fit_comm_drug, 
        standardized = TRUE, 
        rsquare = TRUE, 
        )
fitmeasures(fit_comm_drug)
semPaths(fit_comm_drug)

# Hypothesis 3: Everyday discrimination is positively associated with drug use
# Significant

model_disc_drug <- "
                  # Drug Use
                  drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                               W1Q98 + W1Q99 + W1Q100
                  
                  # Everyday Discrimination
                  discrimination =~ W1Q144A + W1Q144B + W1Q144C + W1Q144D + W1Q144E +
                                    W1Q144F + W1Q144G + W1Q144H + W1Q144I
                  
                  drug_intention ~ discrimination
                  "

fit_disc_drug <- sem(model_disc_drug, 
                     drug_mental,
                     ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                 "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                 "W1Q98", "W1Q99", "W1Q100",
                                 "W1Q144A", "W1Q144B", "W1Q144C", "W1Q144D", "W1Q144E",
                                 "W1Q144F", "W1Q144G", "W1Q144H", "W1Q144I"),
                     std.lv = TRUE,
                     meanstructure = TRUE)
summary(fit_disc_drug, 
        standardized = TRUE, 
        rsquare = TRUE, 
)
fitmeasures(fit_disc_drug)
semPaths(fit_disc_drug)

# Hypothesis 4: Felt stigma is positively associated with drug use
# Not significant

model_stig_drug <- "
                  # Drug Use
                  drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                               W1Q98 + W1Q99 + W1Q100
                  
                  # Felt stigma
                  stigma =~ W1Q125 + W1Q126 + W1Q127
                  
                  drug_intention ~ stigma
                  "

fit_stig_drug <- sem(model_stig_drug, 
                     drug_mental,
                     ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                 "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                 "W1Q98", "W1Q99", "W1Q100",
                                 "W1Q125", "W1Q126", "W1Q127"),
                     std.lv = TRUE,
                     meanstructure = TRUE)
summary(fit_stig_drug, 
        standardized = TRUE, 
        rsquare = TRUE
)
fitmeasures(fit_stig_drug)
semPaths(fit_stig_drug)

# Hypothesis 5: Internalized homophobia is positively associated with drug use
# Not significant

model_phob_drug <- "
                  # Drug Use
                  drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                               W1Q98 + W1Q99 + W1Q100
                  
                  # Homomphobia
                  phobia =~ W1Q128 + W1Q129 + W1Q130 + W1Q131 + W1Q132
                  
                  drug_intention ~ phobia
                  "

fit_phob_drug <- sem(model_phob_drug, 
                     drug_mental,
                     ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                 "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                 "W1Q98", "W1Q99", "W1Q100",
                                 "W1Q128", "W1Q129", "W1Q130", "W1Q131", "W1Q132"),
                     std.lv = TRUE,
                     meanstructure = TRUE)
summary(fit_phob_drug, 
        standardized = TRUE, 
        rsquare = TRUE
)
fitmeasures(fit_phob_drug)
semPaths(fit_phob_drug)

# Hypothesis 6: Outness is positively associated with drug use
# Not significant

model_out_drug <- "
                  # Drug Use
                  drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                                    W1Q98 + W1Q99 + W1Q100
                  
                  # Outness
                  outness =~ W1Q123A + W1Q123B + W1Q123C + W1Q123D
                  
                  drug_intention ~ outness
                  "

fit_out_drug <- sem(model_out_drug, 
                    drug_mental,
                     ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                "W1Q98", "W1Q99", "W1Q100",
                                "W1Q123A", "W1Q123B", "W1Q123C", "W1Q123D"),
                     std.lv = TRUE,
                     meanstructure = TRUE)
summary(fit_out_drug, 
        standardized = TRUE, 
        rsquare = TRUE
)
fitmeasures(fit_out_drug)
semPaths(fit_out_drug)

# Hypothesis 7: Social support is negatively assoicated with drug use
# Close to significant

model_support_drug <- "
                      # Drug Use
                      drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                                        W1Q98 + W1Q99 + W1Q100
                      
                      # Social support
                      soc_spt =~ W1Q164A + W1Q164B + W1Q164C + W1Q164D + W1Q164E +
                                 W1Q164F + W1Q164G + W1Q164H + W1Q164I + W1Q164J +
                                 W1Q164K + W1Q164L
                      
                      drug_intention ~ soc_spt
                      "

fit_support_drug <- sem(model_support_drug, 
                        drug_mental,
                    ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                "W1Q98", "W1Q99", "W1Q100",
                                "W1Q164A", "W1Q164B", "W1Q164C", "W1Q164D", "W1Q164E",
                                "W1Q164F", "W1Q164G", "W1Q164H", "W1Q164I", "W1Q164J",
                                "W1Q164K", "W1Q164L"),
                    std.lv = TRUE,
                    meanstructure = TRUE)
summary(fit_support_drug, 
        standardized = TRUE, 
        rsquare = TRUE
)

fitmeasures(fit_support_drug)
semPaths(fit_support_drug)

# Hypothesis 8: Social well-being is negatively assoicated with drug use
model_well_drug <- "
                      # Drug Use
                      drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                                        W1Q98 + W1Q99 + W1Q100
                      
                      # Social well-being
                      well_being =~ W1Q04 + W1Q05 + W1Q06 + W1Q07 + W1Q08 + W1Q09 + 
                              W1Q10 + W1Q11 + W1Q12 + W1Q13 + W1Q14 + 
                              W1Q15 + W1Q16 + W1Q17 + W1Q18
                      
                      drug_intention ~ well_being
                      "

fit_well_drug <- sem(model_well_drug, 
                     drug_mental,
                        ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                    "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                    "W1Q98", "W1Q99", "W1Q100",
                                    "W1Q04", "W1Q05", "W1Q06", "W1Q07", "W1Q08", "W1Q09",
                                    "W1Q10", "W1Q11", "W1Q12", "W1Q13", "W1Q14",
                                    "W1Q15", "W1Q16", "W1Q17", "W1Q18"),
                        std.lv = TRUE,
                        meanstructure = TRUE)
summary(fit_well_drug, 
        standardized = TRUE, 
        rsquare = TRUE
)

fitmeasures(fit_well_drug)
semPaths(fit_well_drug)

# Hypothesis 9: Mental disability is positively associated with drug use

model_mental_drug <- "
                      # Drug Use
                      drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                                        W1Q98 + W1Q99 + W1Q100
                      
                      # Mental
                      mental =~ W1Q77A + W1Q77B + W1Q77C + W1Q77D + W1Q77E + W1Q77F
                      
                      drug_intention ~ mental
                      "

fit_mental_drug <- sem(model_mental_drug, 
                     drug_mental,
                     ordered = c("W1Q90", "W1Q91", "W1Q92", "W1Q93",
                                 "W1Q94", "W1Q95", "W1Q96", "W1Q97", 
                                 "W1Q77A", "W1Q77B", "W1Q77C",
                                 "W1Q77D", "W1Q77E", "W1Q77F"),
                     std.lv = TRUE,
                     meanstructure = TRUE)
summary(fit_mental_drug, 
        standardized = TRUE, 
        rsquare = TRUE
)

fitmeasures(fit_mental_drug)
semPaths(fit_mental_drug)


# Stage 6: Full model
# Model 1: Mediation model
# Hypotheses: Perceived oppression is positively assoicated with drug use, mediated by community connectedness and social support

drug_mental[, c("W1Q53", "W1Q54", "W1Q55",
                 "W1Q56", "W1Q57", "W1Q58", 
                 "W1Q59")] <- lapply(drug_mental[,c("W1Q53", "W1Q54", "W1Q55",
                                                     "W1Q56", "W1Q57", "W1Q58", 
                                                     "W1Q59")], ordered)

model_med1_drug <- "
                      # Drug Use: DV
                      drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                                        W1Q98 + W1Q99 + W1Q100
                                        
                      # Everyday Discrimination
                      discrimination =~ W1Q144A + W1Q144B + W1Q144C + W1Q144D + W1Q144E +
                                        W1Q144F + W1Q144G + W1Q144H + W1Q144I
                                 
                      # Social well-being
                      well_being =~ W1Q04 + W1Q05 + W1Q06 + W1Q07 + W1Q08 + W1Q09 + 
                              W1Q10 + W1Q11 + W1Q12 + W1Q13 + W1Q14 + 
                              W1Q15 + W1Q16 + W1Q17 + W1Q18
                              
                      # Mental disability
                      mental =~ W1Q77A + W1Q77B + W1Q77C + W1Q77D + W1Q77E + W1Q77F
                      
                      # Stigma
                      stigma =~ W1Q125 + W1Q126 + W1Q127
                      
                      # Homophobia
                      phobia =~ W1Q128 + W1Q129 + W1Q130 + W1Q131 + W1Q132
                      
                      # Outness
                      outness =~ W1Q123A + W1Q123B + W1Q123C + W1Q123D
                      
                      # Social support
                      soc_spt =~ W1Q164A + W1Q164B + W1Q164C + W1Q164D + W1Q164E +
                                 W1Q164F + W1Q164G + W1Q164H + W1Q164I + W1Q164J +
                                 W1Q164K + W1Q164L
                                 
                      drug_intention ~ discrimination + well_being + mental
                      mental ~ discrimination + soc_spt + stigma + phobia
                      well_being ~ discrimination + soc_spt + outness
                      "

fit_med1_drug <- sem(model_med1_drug, 
                     drug_mental,
                     std.lv = TRUE,
                     meanstructure = TRUE)
summary(fit_med1_drug, 
        standardized = TRUE, 
        rsquare = TRUE)

fitmeasures(fit_med1_drug)
semPaths(fit_med1_drug)

# Model 2: Latent interaction model

W1AGE <- df$W1AGE
drug_mental <- cbind(drug_mental, W1AGE)

model_int_drug <- "
                      drug_intention =~ W1Q90 + W1Q91 + W1Q92 + W1Q93 + W1Q94 + W1Q95 + W1Q96 + W1Q97 + 
                                        W1Q98 + W1Q99 + W1Q100
                                        
                      discrimination =~ W1Q144A + W1Q144B + W1Q144C + W1Q144D + W1Q144E +
                                        W1Q144F + W1Q144G + W1Q144H + W1Q144I
                                 
                      well_being =~ W1Q04 + W1Q05 + W1Q06 + W1Q07 + W1Q08 + W1Q09 + 
                              W1Q10 + W1Q11 + W1Q12 + W1Q13 + W1Q14 + 
                              W1Q15 + W1Q16 + W1Q17 + W1Q18
                              
                      mental =~ W1Q77A + W1Q77B + W1Q77C + W1Q77D + W1Q77E + W1Q77F
                                 
                      drug_intention ~ discrimination + mental + well_being + mental:well_being
                      "

fit_int_drug <- upi(data = drug_mental,
                    model = model_int_drug)

summary(fit_int_drug,
        standardized = TRUE)
fitmeasures(fit_int_drug)



