# Define the list of characteristics with multiple specific conditions
source("code/R/functions/validate_conditions.R")

conditions_by_type <- list(
  foods = list(
    excluded_code_groups = c(11, 17, 18, 19),
    specific_exclusions = list( #works with a single list or many
      list(code_group = 28, code_cultu = c("SNE", "SBO", "ROS", "MRS"))
    )
  ),
  feeds = list(
    excluded_code_groups = c(11, 18),
    specific_exclusions = list(
      list(code_group = 17, code_cultu = c("BOP"))
    )
  ),
  farms = list(
    excluded_code_groups = NA,
    specific_exclusions = NA
  )
)

# Apply validation
lapply(conditions_by_type, validate_conditions)