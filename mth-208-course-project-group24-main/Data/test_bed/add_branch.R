
# Load necessary libraries
library(readxl)
library(dplyr)
setwd("/home/atulya/Desktop/MTH208/mth-208-course-project-group24/ShinyApp/test_bed")
# # Load the RData file
load("students.Rdata")
# # Read the Excel file
excel_data <- read_excel("branches.xlsx")

# # View a sample of the loaded data to understand column names
head(excel_data)

# # Assuming 'Branch' is the column in the Excel file and 'roll' is the ID to merge on:
# # Map full branch names to abbreviations (create a mapping as required)
branch_mapping <- data.frame(
  full_name = c("Aerospace Engineering","Biological Sciences and Bioengineering","Chemical Engineering","Chemistry","Civil Engineering","Computer Science and Engineering","Earth Science","Economics","Electrical Engineering","Materials Science and Engineering","Mathematics and Scientific Computing","Mathematics and Statistics","Mechanical Engineering","Physics"),  # Replace with actual names
  abbreviation = c("BT-AE", "BT-BSBE", "BT-CHE", "BS-CHM", "BT-CE", "BT-CSE", "BS-ES", "BS-ECO", "BT-EE", "BT-MSE", "BS-MTH", "BS-MTH", "BT-ME", "BS-PHY")  # Replace with actual abbreviations
)

excel_data <- excel_data %>%
  left_join(branch_mapping, by = c("Department" = "full_name")) %>%  # Replace "Department" if your column name is different
  mutate(Department = coalesce(abbreviation, Department)) %>%        # Update with abbreviation where available
  select(-abbreviation)  # Remove extra column if no longer needed


students <- students %>%
  left_join(excel_data %>% select(roll, Department), by = "roll") %>%  # Join by roll number
  mutate(branch = if_else(branch == "", Department, branch)) %>%       # Replace empty string with Department
  select(-Department)  # Remove the extra Department column from excel_data if not needed



gender_map <- read.csv("gender_mapping.csv")

students <- students %>%
  left_join(gender_map %>% select(roll, Gender), by = "roll") %>%  # Join by roll number
  mutate(gender = Gender) %>%       # Replace empty string with Department
  select(-Gender)  # Remove the extra Department column from excel_data if not needed
print(n = 100, students)

save(students, file = "new_students.Rdata")


