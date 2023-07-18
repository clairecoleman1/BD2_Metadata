########################################################################################################
# General functions and settings
########################################################################################################

{
  library(data.table)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(ggsci)
  library(ggpubr)
  library(plotly)
  library(synapser)
  library(UpSetR)
  library(stringr)
  synLogin()
  
####################################
####################################
# Synapse paths
SYNAPSE = list(root = "",
               mssm_metadata = ""
               UPitt_metadata = "")

medications_atod = list(
  "B" = "benzodiazepines", "C" = "anticonvulsants", "D" = "antidepressants", "L" = "lithium", "N" = "no_medications", "O" = "other_medication_s", "P" = "antipsychotic",  "U" = "unknown")

UPitt_Metadata <- read.csv("./Documents/BD2/UPitt_BD2_78CC.csv")
head(UPitt_Metadata)

MSSM_Metadata <- read.csv("./Documents/BD2/MSSM_BD2_15C.csv")
head(MSSM_Metadata)
MSSM_Metadata$BD.type
UPitt_Metadata$BD_class

# Fixing MSSM - all cases (so far)
MSSM_Metadata$Case_Control <- "Case"
MSSM_Metadata
nrow(MSSM_Metadata)
nrow(UPitt_Metadata)
158/2
#rename columns
colnames(UPitt_Metadata)[colnames(UPitt_Metadata) == "Case"] <- "SubNum"
UPitt_Metadata
colnames(UPitt_Metadata)[colnames(UPitt_Metadata) == "BD_class"] <- "BD.Type"

UPitt_Metadata
MSSM_Metadata

# Replace spaces with underscores in the "BD.type" column
MSSM_Metadata$BD.type <- gsub(" ", "_", MSSM_Metadata$BD.type)
UPitt_Metadata$BD.Type <- gsub(" ", "_", UPitt_Metadata$BD.Type)
head(MSSM_Metadata)
head(UPitt_Metadata)
tail(UPitt_Metadata)
tail(MSSM_Metadata)
View(UPitt_Metadata)

# merging and renaming

library(dplyr)

# Assuming you have loaded the MSSM_Metadata and UPitt_Metadata dataframes
UPitt_Metadata$SubNum <- as.character(UPitt_Metadata$SubNum)
MSSM_Metadata$SubNum <- as.character(MSSM_Metadata$SubNum)
# Get the common columns
common_cols <- intersect(colnames(MSSM_Metadata), colnames(UPitt_Metadata))
common_cols

# rename as not recognized
colnames(MSSM_Metadata)[colnames(MSSM_Metadata) == "BD.type"] <- "BD.Type"
common_cols <- intersect(colnames(MSSM_Metadata), colnames(UPitt_Metadata))
common_cols

# Get the columns unique to UPitt_Metadata
upitt_only_cols <- setdiff(colnames(UPitt_Metadata), colnames(MSSM_Metadata))
upitt_only_cols
# Get the columns unique to MSSM_Metadata
mssm_only_cols <- setdiff(colnames(MSSM_Metadata), colnames(UPitt_Metadata))
mssm_only_cols

library(dplyr)
# Rename the mssm-only columns by adding "mssm_" prefix
MSSM_Metadata_renamed <- MSSM_Metadata %>%
  rename_with(~paste0("MSSM_", .), all_of(mssm_only_cols))
MSSM_Metadata_renamed
# Rename the upitt-only columns by adding "upitt_" prefix
UPitt_Metadata_renamed <- UPitt_Metadata %>%
  rename_with(~paste0("UPitt_", .), all_of(upitt_only_cols))
UPitt_Metadata_renamed



# Combine the common columns and renamed dataframes
merged_df <- bind_rows(UPitt_Metadata_renamed, MSSM_Metadata_renamed)

# View the merged dataframe
merged_df
View(merged_df)

# visualiation 
# sex
category_counts <- table(UPitt_Metadata$Sex)
category_counts
pie(category_counts, labels = names(category_counts))
title("Distribution of Sex in UPitt Data")

# race
category_counts <- table(UPitt_Metadata$Race)
category_counts
pie(category_counts, labels = names(category_counts))
title("Distribution of Race in UPitt Data")


# case or control
category_counts <- table(merged_df$Case_Control)
category_counts
pie(category_counts, labels = names(category_counts))
title("Distribution of Cases and Controls in UPitt and MSSM Data Combined")

# Remove leading and trailing spaces in the "case or control" column
merged_df$Case_Control <- trimws(merged_df$Case_Control)
category_counts

# case or control
category_counts <- table(merged_df$Case_Control)
category_counts
pie(category_counts, labels = names(category_counts))
title("Distribution of Cases and Controls in UPitt and MSSM Data Combined")

head(merged_df)


# Define the bin width
bin_width <- 5

# Determine the range of the age data
age_min <- min(UPitt_Metadata$Age)
age_max <- max(UPitt_Metadata$Age)

# Calculate the number of bins based on the bin width
num_bins <- ceiling((age_max - age_min) / bin_width)

# Create the histogram with 5-year bins
hist(UPitt_Metadata$Age, breaks = seq(age_min, age_max, by = bin_width),
     main = "Age Distribution in UPitt Cohort", xlab = "Age", ylab = "Frequency",
     col = "lightblue", border = "white", xlim = c(age_min, age_max + bin_width))

UPitt_Metadata
num_bins
# Create a boxplot of the "Subject.RIN" column in UPitt_Metadata
boxplot(UPitt_Metadata$Subject.RIN, main = "RIN",
        xlab = "Subject.RIN", ylab = "RIN Value", col = "lightpink")

# Write merged_df to a CSV file
write.csv(merged_df, "BD2_Metadata_UPitt_7979_MSSM_15.csv", row.names = TRUE)

read.csv("BD2_Metadata_UPitt_7979_MSSM_15.csv")



