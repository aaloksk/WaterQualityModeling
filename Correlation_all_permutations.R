library(corrplot)
library(dplyr)
library(reshape2)
library("gtools")

#Working directory
path <- 'C:\\Users\\Aalok\\OneDrive - lamar.edu\\000Water_Q_Modelling\\WD\\pivotT'
setwd(path)

#Import data
df1 <- read.csv('Pivot_15367.csv')
df2 <- read.csv('Pivot_10607.csv')
df3 <- read.csv('Pivot_10602.csv')
df4 <- read.csv('Pivot_10599.csv')
df5 <- read.csv('Pivot_20774.csv')
df6 <- read.csv('Pivot_10575.csv')
df7 <- read.csv('Pivot_10570.csv')
df8 <- read.csv('Pivot_10566.csv')
df9 <- read.csv('Pivot_10563.csv')



df_combined <- bind_rows(df1, df2, df3,df4,df5,df6,df7,df8,df9, .id = "source")
df_combined <- df_combined[, -c(1, 2)]

#Calculate correlation
corr_full <- cor(df_combined, use = "pairwise.complete.obs")

# Get the count of complete cases for each pair of columns
complete_cases_count <- colSums(complete.cases(df_combined))



# Reshape the correlation matrix to a long format
cor_long <- melt(corr_full, varnames = c("Parameter1", "Parameter2"), value.name = "correlation")

# Remove rows with NA values
cor_long_fil <- na.omit(cor_long)

# Remove rows where x and y have the same value
cor_long_fil2 <- cor_long_fil[cor_long_fil$Parameter1 != cor_long_fil$Parameter2, ]

write.csv(cor_long_fil2, 'All_correlation.csv')
