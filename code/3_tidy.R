# PROGRESS {{{ ====

# This file is for disparate columns, so we can at least identify which ones we will need to massage into a correct definition

### TO DO
	# Look at file called sample-coding-indices.sas
	# Create new summary variables that match their pattern

### DEFINITIONS
	# Clinical covariates
	# Demographics: age, sex, smoking, alcohol
	# Obesity: weight, height, metabolic syndrome

# }}}

# Workspace setup {{{ ====

# Will need to "source" the intake files" to bring up the right R object / variables
# Can do this in RStudio

# }}}

# Demographic Tidying {{{ ====

# New table for recoding
df <- combined_data

# Sex (male = 1, female = 0), may need transgender category
df$sex[df$sex == 1] <- 1 # Males = 1
df$sex[df$sex == 2] <- 0 # Females recoded from 2 to 0

# Age categories
df$agecat[df$age < 45] <- 0
df$agecat[df$age >= 45 & df$age < 65] <- 1
df$agecat[df$age >= 65] <- 2
df$agecat[is.na(df$age)] <- NA

# Education Categories

# }}}

# Merge files {{{ ====

# Merge both datasets will require them to have the same columns
# Then they can be merged by rows
c1_merge <- cohort1_data[svar]
c2_merge <- cohort2_data[svar]
combined_data <- bind_rows(c1_merge, c2_merge)

# }}}


