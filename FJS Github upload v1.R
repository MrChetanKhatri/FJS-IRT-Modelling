library(tidyverse) 
library(readxl)
library(janitor)
library(finalfit)
library(DACF)
library(cowplot)
library(grid)
library(psych)
library(REdaS)
library(paran)
library(EFA.dimensions)
library(lavaan)
library(mokken)
library(mirt)
library(lordif)

basic <- read_excel("basic data.xlsx")

fjsdata <- subset(basic, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12"))

#FJS Score
basic <- basic %>% 
  rowwise() %>% 
  mutate(
    complete = sum(!is.na(c(FJSN1,FJSN2,FJSN3,FJSN4,FJSN5,FJSN6,FJSN7,FJSN8,FJSN9,FJSN10,FJSN11,FJSN12))),
    FJS_score_raw = sum(c(FJSN1,FJSN2,FJSN3,FJSN4,FJSN5,FJSN6,FJSN7,FJSN8,FJSN9,FJSN10,FJSN11,FJSN12), na.rm=TRUE),
    FJS_score = 100-(FJS_score_raw/complete)*25)
#Remember high scores (100) means high function, high level of forgetting joint

# score is invalid if more than 4 is missing
basic[basic$complete<8,"FJS_score"]<-NA

#### Missing data ####
#Count missing data (count NA)
na_count <-sapply(basic, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
#add column for percentage of missing data
na_count <- na_count %>% 
  mutate(
    percentage = na_count/ nrow(basic) *100)

#Patterns of missing
library(finalfit)

basic %>% 
  ff_glimpse()

fjsdata %>% 
  ff_glimpse()

fjsdata %>% 
  missing_plot()

fjsdata %>% 
  missing_pattern()

#### Plot Sumscore FJS ####

#In this datamatrix, the column ASSSESS refers to whether participant was pre- or post-operative
basic %>% 
  ggplot(aes(x=FJS_score, fill=factor(ASSESS))) +
  geom_density(alpha = 0.5)  +
  theme(legend.position = "bottom") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1") + 
  labs(title='Distribution of Pre- and Post-Operative Forgotten Joint Score', x= 'Forgotten Joint Score', y= 'Frequency') 

#### Floor and Ceiling Effects ####

##### Sum FJS Whole Data ######

#Calculate 0 and 100
v <- tabyl(basic$FJS_score)

v <- v %>%
  mutate(
    Percentage = percent*100) 

#Floor 10
basic$f.10 <- case_when(basic$FJS_score <= 10 ~ 0,
                        basic$FJS_score >10 ~ 1)

tabyl(basic$f.10)

#Floor 15
basic$f.15 <- case_when(basic$FJS_score <= 15 ~ 0,
                        basic$FJS_score > 15 ~ 1)

tabyl(basic$f.15)

#Ceiling 10

basic$c.10 <- case_when(basic$FJS_score < 90 ~ 0,
                        basic$FJS_score >= 90 ~ 1)
tabyl(basic$c.10)

basic$c.15 <- case_when(basic$FJS_score < 85 ~ 0,
                        basic$FJS_score >= 85 ~ 1)

tabyl(basic$c.15)

##### Sum FJS Preop #####

#scores 0-100

v.1 <- tabyl(preop_basic$FJS_score)

v.1 <- v.1 %>%
  mutate(
    Percentage = percent*100) 

#Floor 10
preop_basic$f.10 <- case_when(preop_basic$FJS_score <= 10 ~0,
                              preop_basic$FJS_score >10 ~1)

tabyl(preop_basic$f.10)

#Floor 15
preop_basic$f.15 <- case_when(preop_basic$FJS_score <= 15 ~0,
                              preop_basic$FJS_score > 15 ~1)

tabyl(preop_basic$f.15)

#Ceiling 10

preop_basic$c.10 <- case_when(preop_basic$FJS_score < 90 ~ 0,
                              preop_basic$FJS_score >= 90 ~1)

tabyl(preop_basic$c.10)

preop_basic$c.15 <- case_when(preop_basic$FJS_score < 85 ~ 0,
                              preop_basic$FJS_score >= 85 ~1)

tabyl(preop_basic$c.15)

##### Sum FJS Postop #####

#scores 0-100
v.2 <- tabyl(postop_basic$FJS_score)

v.2 <- v.2 %>%
  mutate(
    Percentage = percent*100) 

#Floor 10
postop_basic$f.10 <- case_when(postop_basic$FJS_score <= 10 ~ 0,
                               postop_basic$FJS_score >10 ~ 1)

tabyl(postop_basic$f.10)

#Floor 15
postop_basic$f.15 <- case_when(postop_basic$FJS_score <= 15 ~ 0,
                               postop_basic$FJS_score > 15 ~ 1)

tabyl(postop_basic$f.15)

#Ceiling 10

postop_basic$c.10 <- case_when(postop_basic$FJS_score < 90 ~ 0,
                               postop_basic$FJS_score >= 90 ~ 1)
tabyl(postop_basic$c.10)

postop_basic$c.15 <- case_when(postop_basic$FJS_score < 85 ~ 0,
                               postop_basic$FJS_score >= 85 ~ 1)

tabyl(postop_basic$c.15)

#### Exploratory Factor Analysis ####
#Prepare data for analysis
fjsdata <- subset(mydata, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12"))
#remove NA to clean database
fjsdata <- na.omit(fjsdata)

##### KMO & Bartletts #####
bart_spher(fjsdata) #Bartletts test of spehricity. This should be significant
KMO(fjsdata) #Kaiser-Meyer-Olkin measure, want this to be >0.7

#Determine eigenvalues
EV_FJS <- eigen(cor(fjsdata))
#Plot it (Scree plot)
plot(EV_FJS$values, ylab="Eigenvalue", xlab="Factor")
lines(EV_FJS$values, col="black")
title(main="Scree plot of Eigenvalues for Forgotten Joint Score")
axis(side=1, at=c(0:12))

#Velicers minimum average partial (MAP) Test
MAP(fjsdata, corkind= 'polychoric')

#Parallel Analysis
fa.parallel(fjsdata, fm='minres', fa ='fa', n.iter = 5000,
            main= "Parallel Analysis Scree Plot of Forgotten Joint Score")

#Factor loading
names_fjs<- names(fjsdata)
f1 <- fa(fjsdata,1,rotate="oblimin")
Load <- loadings(f1)
Loadprint(Load,sort=FALSE,digits=2,cutoff=0.01)
plot(Load, type="n", ylim=c(0,1))
title(main="Factor loadings for Forgotten Joint Score")
text(Load,labels=names(fjsdata))

####  Confirmatory Factor Analysis #### 
fjsdata.model <- 'fjsdata=~ FJSN1+FJSN2+FJSN3+FJSN4+FJSN5+FJSN6+FJSN7+FJSN8+FJSN9+FJSN10+FJSN11+FJSN12' #Define the model
fit.fjsdata <- cfa(fjsdata.model, data=fjsdata)
summary(fit.fjsdata, fit.measures=T, standardized=TRUE)

#### SEM Plot ####
library(semPlot)
semPaths(fit.fjsdata, style = 'mx')

#### Mokken #### 

coefH(fjsdata) 
aisp(fjsdata) 
summary(check.monotonicity(fjsdata))

fjs.iio <- check.iio(fjsdata)
summary(fjs.iio)

#### Graded Response Model ####

#create pre and post-op data
preop_basic <- filter(basic, ASSESS == 1)
postop_basic <- filter(basic, ASSESS == 2)

#get FJS data only
pre.op.fjsdata <- subset(preop_basic, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12"))
post.op.fjsdata <- subset(postop_basic, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12"))

#remove rows with full 12 NA
pre.op.fjsdata <- pre.op.fjsdata[rowSums(is.na(pre.op.fjsdata)) != ncol(pre.op.fjsdata), ]
post.op.fjsdata <- post.op.fjsdata[rowSums(is.na(post.op.fjsdata)) != ncol(post.op.fjsdata), ]

#create GRM models
FJSgrm <- mirt(post.op.fjsdata, model=1, itemtype='graded', SE=TRUE)
saveRDS(FJSgrm, "FJSgrm.RDS")
FJSgrm <- readRDS("FJSgrm.RDS")

##### Gain EAP scores ####

f.pre <- fscores(FJSgrm, response.pattern = pre.op.fjsdata)[,1]
f.post <- fscores(FJSgrm)[,1]

score <- c(f.pre, f.post)
time <- c(rep("Pre-operative", 609), rep("Post-operative", 630))

df <- cbind(score, time) %>% as.data.frame()
df$score <- as.numeric(df$score)

#plot new EAP scores
ggplot(df, aes(x = score, fill = time)) +
  geom_density(alpha=0.5)  +
  scale_fill_brewer(palette= "Set1") +
  theme_minimal() + 
  labs(title='Distribution of GRM FJS Score')

#change the EAP scores score to go from 0-100
minimum_theta <- fscores(grm, response.pattern = rep(0,12))[,1]
maximum_theta <- fscores(grm, response.pattern = rep(4,12))[,1]

pre.op <- 100 - (round((100*(f.pre - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
post.op <- 100 - (round((100*(f.post - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))

score <- c(pre.op, post.op)
time <- c(rep("Pre-operative", 609), rep("Post-operative", 630))

FJS.EAP.df <- cbind(score, time) %>% as.data.frame()
FJS.EAP.df$score <- as.numeric(FJS.EAP.df$score)

##### Plot GRM score #####
ggplot(FJS.EAP.df, aes(x = score, fill = time)) +
  geom_density(alpha=0.5) +
  scale_fill_brewer(palette= "Set1") +
  theme_minimal() + 
  labs(title='Distribution of GRM FJS', x='GRM FJS Score', y='count')

#Relationship of GRM to FJS sumscare
postop %>% 
  ggplot(aes(x=FJS_score, y=score)) +
  geom_point(colour="blue", alpha=0.5) +
  theme_bw() +
  labs(title="Correlation of FJS to GRM FJS score", x="FJS", y="GRM FJS") +
  stat_cor(method = "pearson", aes(label = ..r.label..), label.x = 3, label.y = 90) #Pearson correlation coefficient

#add in R^2
stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")),label.x = 3)

##### GRM Model eval #####

summary(grm)

coef(FJSgrm, IRTpars=TRUE)

M2(FJSgrm, na.rm=TRUE)

# itemfit looks at how each item is fitting and behaving in the model
itemfit(FJSgrm, na.rm=TRUE)

# look at threshold
plot(FJSgrm, type="trace")

#### Local Dependency ####

# Local dependency using Yen's Q3, with values under .2 suppressed
residuals(grm, type="Q3", digits=2, suppress=+.2)

#### GRM FJS Floor and Ceiling ####

##### Load data #####
FJS.grm <- readRDS("FJS.EAP.df.RDS")

FJS.grm <- FJS.grm %>% 
  rename("FJS_score" = "score")

##### Create pre and post op data #####

preop_FJS.grm <- filter(FJS.grm, time == "Pre-op")
postop_FJS.grm <- filter(FJS.grm, time == "Post-op")

#Calculate 0 and 100
tabyl(FJS.grm$FJS_score)

#Floor 10
FJS.grm$f.10 <- case_when(FJS.grm$FJS_score <= 10 ~ 0,
                          FJS.grm$FJS_score >10 ~ 1)

tabyl(FJS.grm$f.10)

#Floor 15
FJS.grm$f.15 <- case_when(FJS.grm$FJS_score <= 15 ~ 0,
                          FJS.grm$FJS_score > 15 ~ 1)

tabyl(FJS.grm$f.15)

#Ceiling 10

FJS.grm$c.10 <- case_when(FJS.grm$FJS_score < 90 ~ 0,
                          FJS.grm$FJS_score >= 90 ~ 1)
tabyl(FJS.grm$c.10)

#Ceiling 15
FJS.grm$c.15 <- case_when(FJS.grm$FJS_score < 85 ~ 0,
                          FJS.grm$FJS_score >= 85 ~ 1)

tabyl(FJS.grm$c.15)

#### GRM Preop ####

#scores 0-100

tabyl(preop_FJS.grm$FJS_score)

#Floor 10
preop_FJS.grm$f.10 <- case_when(preop_FJS.grm$FJS_score <= 10 ~0,
                                preop_FJS.grm$FJS_score >10 ~1)

tabyl(preop_FJS.grm$f.10)

#Floor 15
preop_FJS.grm$f.15 <- case_when(preop_FJS.grm$FJS_score <= 15 ~0,
                                preop_FJS.grm$FJS_score > 15 ~1)

tabyl(preop_FJS.grm$f.15)

#Ceiling 10

preop_FJS.grm$c.10 <- case_when(preop_FJS.grm$FJS_score < 90 ~ 0,
                                preop_FJS.grm$FJS_score >= 90 ~1)

tabyl(preop_FJS.grm$c.10)

#Ceiilng 15
preop_FJS.grm$c.15 <- case_when(preop_FJS.grm$FJS_score < 85 ~ 0,
                                preop_FJS.grm$FJS_score >= 85 ~1)

tabyl(preop_FJS.grm$c.15)

#### GRM Postop ####

#scores 0-100
tabyl(postop_FJS.grm$FJS_score)

#Floor 10
postop_FJS.grm$f.10 <- case_when(postop_FJS.grm$FJS_score <= 10 ~ 0,
                                 postop_FJS.grm$FJS_score >10 ~ 1)

tabyl(postop_FJS.grm$f.10)

#Floor 15
postop_FJS.grm$f.15 <- case_when(postop_FJS.grm$FJS_score <= 15 ~ 0,
                                 postop_FJS.grm$FJS_score > 15 ~ 1)

tabyl(postop_FJS.grm$f.15)

#Ceiling 10

postop_FJS.grm$c.10 <- case_when(postop_FJS.grm$FJS_score < 90 ~ 0,
                                 postop_FJS.grm$FJS_score >= 90 ~ 1)
tabyl(postop_FJS.grm$c.10)

#Ceiling 15
postop_FJS.grm$c.15 <- case_when(postop_FJS.grm$FJS_score < 85 ~ 0,
                                 postop_FJS.grm$FJS_score >= 85 ~ 1)

tabyl(postop_FJS.grm$c.15)

#### Differential Item Functioning ####

##### Sex #####
fjsdataSex <- subset(mydata, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12", "Gender"))
fjsdataSex <- na.omit(fjsdataSex)
fjsdataSex <- as.data.frame(fjsdataSex)
#Run DIF
SexDIF <- lordif(fjsdataSex[,1:12], fjsdataSex[,13], criterion = "R2", pseudo.R2 = "Nagelkerke")
SexDIF

summary(genDIF)
#Plot DIF results
#Note no DIF in FJS therefore not plotted
plot(SexDIF, labels = c('male', 'female')) 

#####Age#####
mydata$agecat1 <- cut(mydata$age, breaks=c(60,61), right = FALSE)

mydata$age1 <- case_when(mydata$age <= 60 ~ 0, 
                         mydata$age >= 61 ~ 1) 

tabyl(mydata$age1) #n=171<60YO (13%)

fjsdata.age <- subset(mydata, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12", "age1"))
fjsdata.age <- na.omit(fjsdata.age)
fjsdata.age <- as.data.frame(fjsdata.age)

ageDIF <- lordif(fjsdata.age[,1:12], fjsdata.age[,13], criterion = "R2", pseudo.R2 = "Nagelkerke")
ageDIF

summary(ageDIF)

plot(ageDIF, labels = c('>60', '<60'))


##### BMI #####

mydata$BMI1 <- case_when(mydata$BMI <30 ~0, 
                         mydata$BMI >= 30 ~1)

fjsdata.BMI <- subset(mydata, select=c("FJSN1", "FJSN2", "FJSN3", "FJSN4", "FJSN5", "FJSN6", "FJSN7", "FJSN8", "FJSN9", "FJSN10", "FJSN11", "FJSN12", "BMI1"))
fjsdata.BMI <- na.omit(fjsdata.BMI)
fjsdata.BMI <- as.data.frame(fjsdata.BMI)

BMIDIF <- lordif(fjsdata.BMI[,1:12], fjsdata.BMI[,13], criterion = "R2", pseudo.R2 = "Nagelkerke")

BMIDIF

summary(BMDIF)

plot(BMIDIF, labels = c('<30', '<= 30'))
