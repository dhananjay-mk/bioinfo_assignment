# Assignment 1

# Task 4
## 1
sqrt(10)
# ans:3.162278
## 2
log2(32)
# 5
## 3
sum(1:1000)
# 500500
# 4
sum(seq(2,1000,by=2))
# answer:250500
## 5
## finding combinations
choose(100,2)
# 4950
## 6
## finding permutations with formula
choose(100, 3) * factorial(3)
# 970200

# Task 5
# 1
df=CO2
# 2
help(CO2)
#The CO2 data frame has 84 rows and 5 columns of data from an experiment on the cold tolerance of the grass species Echinochloa crus-galli.
colnames(df)
# 3
summary(df$uptake)
# answer median = 28.3, mean = 27.21



# Task 6
## 1
ratio_mean_median=function(vec){mean(vec)/median(vec)}
## 2
mean_skip_minmax=function(vec){
  # using tidyverse discard_at function to discard min and max values
  library(tidyverse)
  vec=discard_at(vec,which.max(vec))
  vec=discard_at(vec,which.min(vec))
  return(mean(vec, na.rm = T))
}


##### Task 7
install.packages("remotes")
library(remotes)
install_url("http://emotion.utu.fi/wp-content/uploads/2019/11/nummenmaa_1.0.tar.gz",dependencies=TRUE)
library(nummenmaa)

# 1.a
df=read.csv2('magic_guys.csv', sep = ',')
View(df)
par(mfrow=c(1,2)) # creating panels
# histogram selecting length for species jedi
p1=hist(as.numeric(df$length[df$species=='jedi']),
        breaks = seq(50,300,by=20),
        main='height jedi',xlab='length',col='green')
# histogram selecting length for species sith
p2=hist(as.numeric(df$length[df$species=='sith']), 
        main='height sith',xlab='length',col='red',
        breaks = seq(50,300,by=20))
dev.off()
# using ggplot
library(ggplot2)
# plot using geom_histogram and specifying breaks
ggplot(df, aes(x=as.numeric(length),fill=species)) + 
  geom_histogram(alpha = 0.7, breaks=seq(50,300,by=20), 
                 position = 'identity',linetype = 1)+
  theme_light()+
  scale_fill_discrete()+
  labs(x="Length", y="Counts")

# 1.b
par(mfrow=c(2,1))
# boxplots
boxplot(as.numeric(df$length[df$species=='jedi']),
        main='Length jedi',xlab='legth',col='lightgreen',horizontal=T, alpha=0.1)
boxplot(as.numeric(df$length[df$species=='sith']),
        main='Length sith',xlab='legth',col='lightpink', horizontal = T)
dev.off()
# using geom_boxplot
ggplot(df, aes(x=as.numeric(length),species) ) + 
  geom_boxplot(alpha=0.5,aes(fill=species))+
  theme_light()+
  scale_fill_brewer(palette = 'Dark2')+
  labs(x="Length", y="Species", main='Distribution of length for species')
# c
# I think it depends on how the plot is intended to be shared. For web or presentations png or svg can be used.
# for reports pdf or svg are better. 
#Png may loose image quality upon scaling where as pdf/svg can maintain the quality.


## 2
df=read.csv2('microarray_data.tab',sep = '\t', na.strings = "")
# convert strings to numbers as NA
df=as.data.frame(sapply(df, function(x) as.numeric(x)))
View(df)
# a
dim(df)
# b
nullrows=sapply(df, function(x) sum(is.na(x)))
boxplot(nullrows, main='distribution of null values in data', col='cyan',
        alpha=0.5, ylab='null rows', xlab='genes')

#c
# using sapply to calculate the percentage for nulls in each column
percentages=sapply(nullrows,function(x)x/553*100)
percentages[percentages>10]
percentages[percentages>20]
percentages[percentages>50]

# d
# using sapply; for each column check if value is na and replace with mean using if-else
df2 = sapply(df, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
View(df2)

# 3 (not sure?)
dfco=CO2
View(dfco)
#
library(ggplot2)
library(gridExtra)
# boxplots using ggplot
p1=ggplot(dfco, aes(uptake,Type),)+geom_boxplot(col='magenta')+
  theme_minimal()+
  scale_fill_distiller()
p2=ggplot(dfco, aes(uptake,Treatment))+geom_boxplot(col='green')+
  theme_minimal()+
  scale_fill_distiller()
grid.arrange(p1,p2)


##### Task 8
devtools::install_github("hirscheylab/tidybiology")
library(tidybiology)
library(tidyverse)
library(ggplot2)
library(gridExtra)
# a
df=chromosome
colnames(df)
# using dplr summarise function to calculate the stats. We select the columns by name.
# combine mean, median and max as a list
summary_stats2 <- df %>%
  summarise(across(c(variations, protein_codinggenes, mi_rna), list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))) #

print(summary_stats2)
# b (not sure what are the relevent size columns)
ggplot(df, aes(id,length_mm)) +
  geom_point(alpha=0.50, size=3) +
  theme_light()
# c
p1=ggplot(df, aes(length_mm, protein_codinggenes),color=protein_codinggenes) +
  geom_point(alpha=0.50, size=3) +
  geom_smooth(method=lm, color='red') +
  theme_minimal() +
  labs(title="Correlations", 
       x="Lenght mm", y = "protein_codinggenes")
p2=ggplot(df, aes(length_mm, mi_rna),color=mi_rna) +
  geom_point(alpha=0.50, size=3) +
  geom_smooth(method=lm, color='blue') +
  theme_minimal() +
  labs(title="Correlations", 
       x="Lenght mm", y = "mi rna")
grid.arrange(p1,p2)

# d
df=proteins
View(df)
names(df)
summary_stats3 <- df %>%
  summarise(across(c(length, mass), list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))) #

print(summary_stats3)
# plotting - linear regression and histogram
p3=ggplot(df, aes(length, mass)) +
  geom_point(alpha=0.30, size=2, position = 'identity') +
  geom_smooth(method=lm,col='orange') +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2")+
  labs(title="Regression", 
       x="Length", y = "Mass")

p4=ggplot(df, aes(length), fill(mass)) +
  geom_histogram(alpha=0.70, fill='orange') +
  theme_minimal() +
  scale_fill_brewer(palette="Accent")+
  labs(title="Histogram", 
       x="Length", y = "Mass")
grid.arrange(p3,p4)


