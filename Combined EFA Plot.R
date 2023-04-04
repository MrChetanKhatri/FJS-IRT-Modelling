library(psych)
library(tidyverse)

#Code for this combined plot is adapted from the following paper:
#Sakaluk, J. K., & Short, S. D. (2016). A Methodological Review of Exploratory Factor Analysis in Sexuality Research: Used Practices, Best Practices, and Data Analysis Resources. Journal of Sex Research.

#Read in data
basic <- read_excel("basic data.xlsx")
#create subset of only items
fjsdata <- subset(basic, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12"))

#Parallel analysis
parallel <- fa.parallel(fjsdata, fm='minres', fa ='fa', n.iter=5000,
                        main="Parallel Analysis Scree Plot of Forgotten Joint Score")

#Create data frame from observed eigenvalue data
obs <- data.frame(parallel$fa.values)
obs$type <- c('Observed Data')
obs$num <- c(row.names(obs))
obs$num <- as.numeric(obs$num)
colnames(obs) <- c('eigenvalue', 'type', 'num')

#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile <- apply(parallel$values,2,function(x) quantile(x,.95))
min <- as.numeric(nrow(obs))
min <- (4*min) - (min-1)
max <- as.numeric(nrow(obs))
max <- 4*max
percentile1 <- percentile[min:max]

#Create data frame called 'sim' with simulated eigenvalue data
sim <- data.frame(percentile1)
sim$type <- c('Simulated Data (95th %ile)')
sim$num <- c(row.names(obs))
sim$num <- as.numeric(sim$num)
colnames(sim) <- c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat <- rbind(obs,sim)

apatheme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated 

#Plot with dashed line at 1, to denote kaiser
ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  geom_line()+
  geom_point(size=4)+
  scale_y_continuous(name='Eigenvalue')+
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  scale_shape_manual(values=c(16,1)) +
  apatheme +
  geom_hline(yintercept=1, linetype = 'dashed') + #this line represented Kaiser 1 over rule
  labs(title="Scree Plot, Kaiser 1 over and Parallel Analysis of Forgotten Joint Score")
