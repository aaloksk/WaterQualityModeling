

library(corrplot)

#Working directory
path <- 'C:\\Users\\Aalok\\OneDrive - lamar.edu\\000Water_Q_Modelling\\WD'
setwd(path)

#Import data
df <- read.csv('7.Pivort_10607.csv')

#Removing date
pars_df <- df[,-1]
colnames(pars_df)

#For Flow
my_data <- pars_df[complete.cases(pars_df$DEPTH.OF.BOTTOM.OF.WATER.BODY.AT.SAMPLE.SITE), ]
fil_df2 <- my_data[, colSums(is.na(my_data)) <= 2]
colnames(fil_df2) #Hereko
fil_df2 <- fil_df2[,c(1,2,3,4,7,8,9,10,11,12)] #Specific tyo nikaaleko
colnames(fil_df2) <- c('Alk', 'Days_since_PPT', 'Depth','E_Coli','Nitrogen', 'DO','pH','TNR','SC','Temp') #Rename haaneko



# extract the first row as a vector 
row1 <- fil_df2[, 3] #Flow or depth

# calculate the correlation between row1 and all other rows
correlations <- cor(fil_df2[, -3], row1, use = "pairwise.complete.obs")
round(correlations,3)



####Correlation
cor_matrix <- cor(fil_df2, use = "pairwise.complete.obs")
cor_matrix
par(mfrow=c(1,1))

# Create a lower triangular matrix with NA in the upper triangle
lower_tri <- cor_matrix
lower_tri[upper.tri(cor_matrix)] <- NA

# Melt the lower triangular matrix and remove NA values
library(reshape2)
melted_cor <- melt(lower_tri, na.rm = TRUE)

# Create a correlation heatmap using ggplot2
ggplot(data = melted_cor, aes(x=Var2, y=Var1, fill=value, label = round(value, 2))) +
  geom_tile() +
  geom_text(color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Correlation Heatmap - Station 10607")
