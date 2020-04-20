# PROGRESS {{{ ====

# This file is for disparate columns, so we can at least identify which ones we will need to massage into a correct definition

### TO DO
	# Look at file called sample-coding-indices.sas
	# Create new summary variables that match their pattern

# }}}

# Workspace setup {{{ ====

# Will need to "source" the intake files" to bring up the right R object / variables
# Can do this in RStudio

# }}}

# Demographic Tidying {{{ ====

# Sex (male = 1, female = 0), may need transgender category

# Age categories

# Education Categories

# }}}

# Merge files {{{ ==== 

# Merge both datasets will require them to have the same columns
# Then they can be merged by rows
c1_merge <- cohort1_data[svar]
c2_merge <- cohort2_data[svar]
combined_data <- bind_rows(c1_merge, c2_merge)

# }}}


