################################################################################
#
# Clean all distiller Data
#
# Purpose 
#
# Sneha Nicholson, Fri Mar 15 13:52:48 2024 
#
################################################################################

{
  library(magrittr)
  library(plotly)
  library(plyr)
  library(flexdashboard)
  library(dplyr)
  library(openxlsx)
  library(data.table)
  library(tidyr)
  library(revtools)
  library(stringr)
  library(googlesheets4)
  library(tidyverse)
  library(data.table)
  library(readxl)
  library(ggplot2)
  library(gridExtra)
  library(cowplot)
  library(RColorBrewer)
  library(maptools)
  library(ggpubr)
  library(pdftools)
  library(ggh4x, lib='/ihme/homes/snehai/rlibs/')
  library(shiny)
  library(pander)
  library(purrr)
  library(doParallel)
  library(parallel)
  library(foreach)
  library(patchwork)
  invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) 
  #source("/ihme/code/st_gpr/central/stgpr/r_functions/utilities/utility.r")
  date <- substr(gsub(" |:","-",Sys.time()),1,10)
  user <- Sys.getenv(x="USER")
} #libraries


#define paths and pull in files

param_path <- "file_name"
input_path <- "file_name"
output_path <- "file_name"
gbd2021_path <- "file_name"
clean_path <- "file_name"



param_file <- read_excel(paste0(param_path, "/distiller_to_R_params.xlsx"))
cause_file <- read_csv(paste0(param_path, "/cause_params.csv"))
files_list <- list.files(input_path, pattern = ".xlsx")

#define risk-specific parameters
rei_id <- 102
risk_id <- "drugs_alcohol"
bundle_id <- 9023
risk_type <- "continuous" #continuous or dichotomous


################################################################################
# pull in all raw files
raw <- data.table()

for(file in files_list){
  print(file)
  temporary <- read_excel(paste0(input_path, "/", file))
  names_df <- read.csv(paste0(param_path, "/distiller_names.csv"))
  old_names <- names_df$old
  new_names <- names_df$new
  
  col_list <- grep("...", names(temporary))
  if(length(col_list) > 0){
  for(c in col_list){
    temporary <- temporary %>% mutate(mut = case_when(
      is.na(c) ~ temporary[[b]],
      TRUE ~ c
    ))
  }
    }
  
  temporary <- setnames(temporary, old_names, new_names, skip_absent = TRUE)
  temporary <- temporary %>% select(-contains("del"))
  
  raw <- rbind(raw, temporary, fill = TRUE)
}
################################################################################
# initial outcome cleaning (stage1)
################################################################################
step1 <- copy(raw)

#duplicate headneck by sub outcome
repl <- c("combined, please specify","oral/mouth","head &amp; neck cancer unspecified", "esophageal", "oropharyngeal", "larynx", "nasopharynx", "pharynx", "hypopharyngeal")
vals <- c("neo_headneck_combined", "neo_mouth", "neo_headneck_unspecified", "neo_esophageal", "neo_oropharyngeal", "neo_larynx", "neo_nasopharynx", "neo_pharynx", "neo_hypopharyngeal")

hnc_sub <- step1 %>% filter(acause == "neo_headneck")
hnc_sub$outcome_hnc_sub <- replace(hnc_sub$outcome_hnc_sub, hnc_sub$outcome_hnc_sub %in% repl, vals)


hnc_sub <- hnc_sub %>% mutate(acause = case_when(
  acause %like% "neo_headneck" ~ outcome_hnc_sub,
  TRUE ~ acause
 ))
 
step1 <- step1 %>% rbind(hnc_sub, fill=TRUE) 

################################################################################
# in-depth column cleaning (stage2)
################################################################################

step2 <- copy(step1)
# remove unnecessary columns

step2 <- step2 %>% select(-contains("del"))

###############################################################################
# create necessary variables (stage 3)
###############################################################################
step3 <- copy(step2)

step3$seq <- seq.int(nrow(step3))
step3$rei_id <- rei_id 
step3$risk_id <- risk_id
step3$bundle_id <- bundle_id
step3$bundle_version_id <- 0
step3$study_id <- step3$study_id
step3$risk_type <- risk_type
step3$bc_new <- 1

step3 <- step3[!is.na("mean")] #remove rows with missing mean

###############################################################################
# fix any weird acauses (stage 4), convert to g/day
###############################################################################
step4 <- copy(step3)

step4 <- step4 %>% mutate(acause = case_when(
  acause %like% "neo_colon" ~ "neo_colorectal",
  acause %like% "neo_rectum" ~ "neo_colorectal",
  acause %like% "neo_headneck" ~ "neo_headneck",
  acause %like% "diabetes" ~ "diabetes_typ2",
  TRUE ~ acause))

# convert to gday (stage 4)
to_convert <- copy(step4)

tbl <- table(to_convert$exp_quant_unit, is.na(to_convert$quant_convert))
print(tbl)
quant_fix <- to_convert[, c("seq", "study_id", "acause", "exp_quant_lower", "exp_quant_upper", "unexp_quant_lower", "unexp_quant_upper", "exp_quant_unit", "quant_convert")]
quant_fix <- subset(quant_fix, is.na(quant_convert))

quant_fix <- quant_fix %>% mutate(quant_convert_fix = case_when(
  exp_quant_unit %like% "drink" ~ 14,
  exp_quant_unit %like% "gram" ~ 1,
  exp_quant_unit %like% "g/" ~ 1,
  exp_quant_unit %like% "unit" ~ 12,
  TRUE ~ 1)) %>% select(-c("quant_convert"))

quant_fixed <- merge(to_convert, quant_fix, by = c("seq", "study_id", "acause", "exp_quant_lower", "exp_quant_upper", "unexp_quant_lower", "unexp_quant_upper", "exp_quant_unit"), all.x = TRUE, fill = TRUE)
quant_fixed$quant_convert <- ifelse(is.na(quant_fixed$quant_convert), quant_fixed$quant_convert_fix, quant_fixed$quant_convert)

quant_fixed <- quant_fixed %>% select(-c("quant_convert_fix"))
################################################################################
tbl <- table(quant_fixed$exp_freq_unit, is.na(quant_fixed$freq_convert))
print(tbl)
freq_fix <- quant_fixed[, c("seq", "study_id", "acause", "exp_freq_lower", "exp_freq_upper", "unexp_freq_lower", "unexp_freq_upper", "exp_freq_unit", "freq_convert")]
freq_fix <- freq_fix %>% mutate(freq_convert_fix = case_when(
  exp_freq_unit %like% "per week" ~ 7,
  exp_freq_unit %like% "month" ~ 30,
  exp_freq_unit %like% "year" ~ 365,
  exp_freq_unit %like% "day" ~ 1,
  TRUE ~ 1)) %>% select(-c("freq_convert"))

freq_fixed <- merge(quant_fixed, freq_fix, by = c("seq", "study_id", "acause", "exp_freq_lower", "exp_freq_upper", "unexp_freq_lower", "unexp_freq_upper", "exp_freq_unit"), all.x = TRUE, fill = TRUE)
freq_fixed$freq_convert <- ifelse(is.na(freq_fixed$freq_convert), freq_fixed$freq_convert_fix, freq_fixed$freq_convert)

freq_fixed <- freq_fixed %>% select(-c("freq_convert_fix"))

converted <- as.data.table(freq_fixed)

#finish conversion to gday
converted$quant_convert <- as.numeric(converted$quant_convert)
converted$freq_convert <- as.numeric(converted$freq_convert)
converted$exp_quant_lower <- as.numeric(converted$exp_quant_lower)
converted$exp_quant_upper <- as.numeric(converted$exp_quant_upper)
converted$unexp_quant_lower <- as.numeric(converted$unexp_quant_lower)
converted$unexp_quant_upper <- as.numeric(converted$unexp_quant_upper)


converted <- converted[, convert_factor := (quant_convert/freq_convert)]
converted <- converted[, alt_risk_lower := (exp_quant_lower*convert_factor)]
converted <- converted[, alt_risk_upper := (exp_quant_upper*convert_factor)]
converted <- converted[, ref_risk_lower := (unexp_quant_lower*convert_factor)]
converted <- converted[, ref_risk_upper := (unexp_quant_upper*convert_factor)]
converted <- converted[, risk_unit := "gday"]

################################################################################
# fix exposures
###############################################################################
step5 <- copy(converted)

step5 <- as.data.table(step5)
step5$mean <- as.numeric(step5$mean)
step5$ln_rr <- log(step5$mean)
step5$upper <- as.numeric(step5$upper)
step5$lower <- as.numeric(step5$lower)
step5 <- step5[is.na(lower), lower := 0]
step5$ln_rr_se <- (log(step5$upper) - log(step5$lower))/(1.96*2)

#room to manually outlier if necessary
step5$is_outlier <- 0

#manually outlier here
step5$is_outlier[step5$alt_risk_lower > 150] <- 1  

step5 <- step5 %>% mutate(is_outlier = case_when(
  study_id %in% c("528704","528772") ~1,
  TRUE ~ 0))



step5 <- step5[is_outlier == 0] #drops any nids that are selected for outliering

#fix endpoint bias covariate
step5[, bc_incidence := ifelse(outcome_type == "Incidence", 0, 1)]
step5[, bc_mortality := ifelse(outcome_type == "Mortality", 0, 1)]

# recode alcohol subtypes
recoded_data <- step5 %>% mutate(alcohol_type = case_when(alcohol_type %like% "Any" ~ "bc_alcohol_type_any",
                                                            is.na(alcohol_type) ~ "bc_alcohol_type_any",
                                                            alcohol_type %like% "Beer" ~ "bc_alcohol_type_beer",
                                                            alcohol_type %like% "wine" ~ "bc_alcohol_type_wine",
                                                            alcohol_type %like% "Spirits" ~ "bc_alcohol_type_liquor",
                                                            alcohol_type %like% "liquor" ~ "bc_alcohol_type_liquor",
                                                            alcohol_type %like% "vodka" ~ "bc_alcohol_type_vodka",
                                                            alcohol_type %like% "unspecified" ~ "bc_alcohol_type_unspecified") %>% as.factor) %>% 
  pivot_wider(names_from = alcohol_type, values_from = alcohol_type, values_fn = length, values_fill = 0)

write.csv(recoded_data, paste0(output_path, "all_outcomes_processed_2.csv"))
################################################################################
# get data ready for mrbrt (stage 6)
################################################################################

stage6 <- copy(recoded_data)

cause_file <- read_csv(paste0(param_path, "/cause_params.csv"))
mr_data <- merge(stage6, cause_file, by = "acause")

bc_names <- names(mr_data) %>% str_subset("bc_")
bc_names <- bc_names[!bc_names %like% c("_split")]

mrbrt_names <- c("seq",  "rei_id", "risk_id", "cause_id", "acause", "study_id", "bundle_id", "bundle_version_id", "risk_type",
                 "risk_unit", "ln_rr", "ln_rr_se", "ref_risk_lower", "ref_risk_upper", "alt_risk_lower", "alt_risk_upper", bc_names)

mr_cleaned <- mr_data[, mrbrt_names]
mr_cleaned <- mr_cleaned %>% mutate(ref_risk_lower = ifelse(is.na(ref_risk_lower), 0, ref_risk_lower),
                                    ref_risk_upper = ifelse(is.na(ref_risk_upper), 0, ref_risk_upper),
                                    alt_risk_lower = ifelse(is.na(alt_risk_lower), 1, alt_risk_lower),
                                    alt_risk_upper = ifelse(is.na(alt_risk_upper), 150, alt_risk_upper))

################################################################################
# bind on old data (stage7)
###############################################################################
old_data <- fread(paste0(cleaned_csv_path, "/old_data.csv"))

stage7 <- rbind(old_data, mr_cleaned, fill=TRUE)

write.csv(stage7, paste0(output_path, "all_outcomes_preMRBRT_2.csv"))


################################################################################
# separate data by outcome and prep for mrbrt
################################################################################
stage8 <- copy(stage7)

outcome_list <- unique(stage8$acause)

for(o in outcome_list){
  print(o)
  data <- stage8[stage8$acause == o]
  fname <- paste0("drugs_alcohol-", o)
  
  #drop missing ln_rr values
  data <- data[ln_rr != "NA",]
  
  #drop weird ln_rr_se values
  data <- data[ln_rr_se >0,]
  data <- data[ln_rr_se <1,]
  data <- data[ln_rr_se != "NA",]
  
  print("Data points with nonsensical ln_rr_se values are now removed.")
  
  #make sure relevant vals are numeric
  data[, alt_risk_lower := as.numeric(alt_risk_lower)]
  data[, alt_risk_upper := as.numeric(alt_risk_upper)]
  
  print("alt_risk_lower and alt_risk_upper are now numeric.")
  
  #check to make sure lower is always less than upper
  data[, check_alt := alt_risk_lower < alt_risk_upper]
  if (sum(data$check_alt) > 1){
    print("Warning: alt_risk_lower is not always less than alt_risk_upper.")
  }
  data[, check_ref := ref_risk_lower < ref_risk_upper]
  if (sum(data$check_ref) > 1){
    print("Warning: ref_risk_lower is not always less than ref_risk_upper.")
  }
  
  recode <- data %>%
    mutate(across(c(alt_risk_lower, alt_risk_upper), ~ ifelse(alt_risk_lower > alt_risk_upper, alt_risk_upper, alt_risk_lower))) %>% 
    mutate(across(c(ref_risk_lower, ref_risk_upper), ~ ifelse(ref_risk_lower > ref_risk_upper, ref_risk_upper, ref_risk_lower))) %>% 
    select(-check_alt, -check_ref)
  print("alt_risk_lower and ref_risk_lower are now less than their respective upper bounds.")
  
  #remove any columns with all 0s or 1s
  check_df <- copy(recode)
  bc_names <- colnames(check_df)[names(check_df) %like% "bc_"]
  check_df <- check_df[,..bc_names]
  
  zero_list<- colnames(check_df)[apply(check_df, 2, function(x) all(x==0))]
  one_list <- colnames(check_df)[apply(check_df, 2, function(x) all(x==1))]

  drop_list <- c(zero_list, one_list)
  
  #find and remove duplicated columns
  
  dup_list <- colnames(check_df)[apply(check_df, 2, function(x) duplicated(as.list(x)))]
  dup_list <- dup_list[!is.na(dup_list)]
  drop_list <- append(drop_list, dup_list)
  
  data <- recode[, !..drop_list]
  
  print("Columns with all 0s or 1s are now removed.")
  print("Duplicated columns are now removed.")
  
  assign(paste0(fname, "_debug"), copy(data))
  
  #write out files to input
  data <- data %>% select(-V1) %>% mutate(seq = row_number())
  data <- data %>% select(seq, everything())
  
  write.csv(data, file = paste0(save_dir, "/04_input/", fname, ".csv"))
  write.csv(data, paste0(save_dir, "/05_data_and_results/data/", fname, ".csv"), row.names = FALSE, quote = FALSE)

  print("Data is ready for mr_brt!")
}

# now run BPRF pipeline in the terminal
# ------------------------------------------------------------------------------
# 
# https://hub.ihme.washington.edu/display/MSCA/The+Burden+of+Proof+Pipeline#
#
# please double-check if the correct settings.yaml file is stored in the data folder.

# Log onto the cluster and run an interactive job (please see the Slurm documentation here). For example, you could use:
#    srun -J bop_run --mem=2G -c 1 -t 24:00:00 -A proj_team -p i.q --pty bash
# 
# Please replace {project_account_name} with the account name of your project (i.e., proj_{team_abbreviation}).
# 
# Run the following command to create a bash session:
#    bash --rcfile /mnt/team/msca/pub/miniconda3/bashrc
# 
# Run the following command to access the escore-2022 environment:
#   conda activate bop-dev
# 
# Run pwd to see what working directory you are in and then run cd {path} to navigate to the folder in which the data folder with the settings.yaml file is saved.
# 
# cd /snfs1/WORK/05_risk/risks/TEAM/sub_risks/alcohol/gbd_2022/evidence_score/05_data_and_results/
#
# Run the following command: 
# continuous_pipeline -i ./data -o ./results -p drugs_alcohol-cirrhosis drugs_alcohol-diabetes_typ2 drugs_alcohol-cvd_stroke_isch drugs_alcohol-cvd_stroke_cerhem

# ------------------------------------------------------------------------------


################################################################################
# output summary sources data table
#################################################################################

new <- stage8 %>% select(acause, bc_new, study_id) %>% filter(bc_new == 1)
old <- stage8 %>% select(acause, bc_new, study_id) %>% filter(bc_new == 0)

n <- new %>% group_by(acause) %>% summarise(new = length(unique(study_id)))

o <- old %>% group_by(acause) %>% summarise(old = length(unique(study_id)))

summary_tbl  <- merge(n, o, by = "acause", all = TRUE)
print(summary_tbl)

write.csv(summary_tbl, paste0(save_dir, "/05_data_and_results/summary_table.csv"), row.names = FALSE, quote = FALSE)
