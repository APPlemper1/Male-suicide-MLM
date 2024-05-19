
library(tidyverse)
library(janitor)
library(stringr)
library(zoo)
library(tidyr)
library(lme4)
library(bestNormalize)
library(lme4)
library(lmerTest)
library(nortest)
library(lattice)
library(lmtest)
library(ggplot2)
library(car)
library(LMERConvenienceFunctions)
library(minpack.lm)
library(dplyr)


#------------------------------------------------------------------------------------------------------------#
#-------------------------------- Multilevel modeeling of regional predictors of suicide --------------------#
#-------------------------------- in England and wales-------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------#
#-------------------------------- STAGE  1: CLEANING DATA----------------------------------------------------# 


# ---cleaning raw data regarding unemployment levels in females 

raw_f<-read.csv("data/f_unemp2.csv")

# removing unnecessary columns 

f_df<- raw_f[-c(1,2,13,14), -c(1,2)]

#changing collum name

colnames(f_df)[1]<- "Area"

# converting the data to long format 

f_df<- f_df %>% 
  pivot_longer(cols = -Area, names_to = "quater", values_to = "unemp_rate" )

# making a collum called year wich contains the numeric information from the quater column 

f_df<- f_df %>%  
  mutate(Year = str_extract(quater, "\\d{4}"))

# taking the mean for each year/region using the group by function 

f_df<- f_df %>% group_by(Year, Area) %>% summarise(unemp_rate = mean(unemp_rate))


#-------compleating the above prosess for males 



raw_m<-read.csv("data/m_unemp2.csv")


m_df<- raw_m[-c(1,2,13,14), -c(1,2)]


colnames(m_df)[1]<- "Area"


m_df<- m_df %>% 
  pivot_longer(cols = -Area, names_to = "quater", values_to = "unemp_rate" )


m_df<- m_df %>%  
  mutate(Year = str_extract(quater, "\\d{4}"))


m_df<- m_df %>% group_by(Year, Area) %>% summarise(unemp_rate = mean(unemp_rate))



#---------# now i am going to comin both dataframes together. 
#adding a sex collum to both datasets. 

f_df$Sex <- "Female"

m_df$Sex <- "Male"

c_unemp <- rbind(f_df, m_df)

# removing the 2023 data (this is not a compleate estimate)

year_remove23<- 2023

c_unemp <- c_unemp %>% filter(Year != year_remove23)


# ----------Now I am going to read in the data for my suicide rates veriable  


# reading in data 

raw_sr <- read.csv("data/suic_rates.csv")

# droping the first 2 collums 

sr_df<- raw_sr[-c(1,2),]

# and making the first row the title 

sr_df<- sr_df %>% row_to_names(row_number = 1) 

# removing the collums i dont want 

sr_df<- sr_df %>% select("Sex", contains("Area of usual residence"), contains("per 100,000"))

# and changing the names of the collums

colnames(sr_df)<- gsub("..Rate.per.100.000...note.4.", "", colnames(sr_df))

colnames(sr_df)[2] = "Area"


# converting the data into long format 

sr_df<- sr_df %>%
  pivot_longer(c( '2021','2020', '2019', '2018', '2017', '2016', '2015', '2014', '2013', 
                  '2012', '2011', '2010', '2009','2008', '2007', '2006', '2005', '2004', 
                  '2003', '2002', '2001', '2000', '1999', '1998', '1997', '1996','1995', 
                  '1994', '1993', '1992', '1991', '1990', '1989', '1988', '1987', '1986', 
                  '1985', '1984', '1983', '1982', '1981' ), names_to = "Year", values_to = "suic_rates") 


# getting rid of the "presons" catagory.


sr_df1<- sr_df %>% filter(Sex != "Persons")

#---------------------------- cleaning data from wales 

raw_srw<- read.csv("data/suic_rates_w.csv")

# doping the first 2 columns 

srw_df<- raw_srw[-c(1,2),]


# and making the first row the title 

srw_df<- srw_df %>% row_to_names(row_number = 1)

# removing the collums i dont want 

srw_df<- srw_df %>% select(contains("Area of usual residence"), contains("per 100,000"), 
                           contains("Year of death registration"))

# and changing the names of the collums

colnames(srw_df)<- gsub("..Rate.per.100.000...note.4.", "", colnames(srw_df))

colnames(srw_df)[1] = "Area"

colnames(srw_df)[5] = "Year"

# droping the persons collum

srw_df1 <- srw_df[ , -2]

# now giving rates and sex their own collum 

srw_df1 <- srw_df1 %>% pivot_longer(cols = c(Males, Females), names_to = "Sex", values_to = "suic_rates")


#-------------- now im going to combine the datasets so far together 

# first combining england and wales suicde rates 

all_suic <- bind_rows(srw_df1, sr_df1)

# next combining suicide rates with unemployment 

# first im going to make sure the cell values match in each data set 

c_unemp$Area <- gsub("\\s+$","", c_unemp$Area)
all_suic$Sex<- gsub("Males", "Male", all_suic$Sex)
all_suic$Sex<- gsub("Females", "Female", all_suic$Sex)
c_unemp$Area[c_unemp$Area == "Yorkshire and the Humber"] <- "Yorkshire and The Humber"

suic_unemp<- merge(all_suic, c_unemp, by = c("Sex", "Area", "Year"))

# saving as .csv 

write.csv(suic_unemp, "data\\suic_unemp.csv")

#-------------------------------------------cleaning data on beifits claiments-----------------#

# reading in data 
raw_fben_wb<- read.csv("data/benif/f_wb_benif.csv")

# cleaning data set 
# 
# removing columns i dont want 

fben_wb<- raw_fben_wb[-c(1:5, 7, 392:415), ]

fben_wb1<- subset(fben_wb, select = c(Claimant.Count...seasonally.adjusted, X.1, X.3, X.5, X.7, X.9, X.11, X.13, X.15, X.17, X.19))

# keeping colums i want 
colnames(fben_wb1) <- c("Date", "East", "East Midlands", "London", "North East", "North West", 
                        "South East", "South West", "Wales", "West Midlands", "Yorkshire and The Humber" )
# removing first row 

fben_wb1<- fben_wb1[-1, ]

#converting to long format 

fben_wb1 <- fben_wb1 %>% pivot_longer(cols = -Date, names_to = "Area", values_to = "benclmwb")


# altering date and creating yearly averages 

fben_wb1$Date <- as.yearmon(fben_wb1$Date, "%b-%y")

fben_wb1$Year <- as.integer(format(fben_wb1$Date, "%Y"))



# changing the benifits veriable to numeric 

fben_wb1$benclmwb <- as.numeric(fben_wb1$benclmwb)

#taking yearly averages 

fben_wb1 <- fben_wb1 %>% 
  group_by(Year, Area) %>% 
  summarise(Benclm_wb=mean(benclmwb))

# Adding sex colum to data frame 


fben_wb1$Sex <- "Female"


# ---------------------------- importing in male dataset 

# reading in data 
raw_mben_wb<- read.csv("data/benif/m_wb_benif.csv")

# cleaning data 

mben_wb<- raw_mben_wb[-c(1:5, 7, 392:415), ]

mben_wb1<- subset(mben_wb, select = c(Claimant.Count...seasonally.adjusted, X.1, X.3, X.5, X.7, X.9, X.11, X.13, X.15, X.17, X.19))


colnames(mben_wb1) <- c("Date", "East", "East Midlands", "London", "North East", "North West", 
                        "South East", "South West", "Wales", "West Midlands", "Yorkshire and The Humber" )

mben_wb1<- mben_wb1[-1, ]

#converting to long format 

mben_wb1 <- mben_wb1 %>% pivot_longer(cols = -Date, names_to = "Area", values_to = "benclmwb")


# altering date and creating yearly averages 

mben_wb1$Date <- as.yearmon(mben_wb1$Date, "%b-%y")

mben_wb1$Year <- as.integer(format(mben_wb1$Date, "%Y"))


# changing the benifits veriable to numeric 

mben_wb1$benclmwb <- as.numeric(mben_wb1$benclmwb)

#########

#taking yearly averages 

mben_wb1 <- mben_wb1 %>% 
  group_by(Year, Area) %>% 
  summarise(Benclm_wb=mean(benclmwb))

# Adding sex colum to data frame 


mben_wb1$Sex <- "Male"

# ----------combining the two datasets together 


ben_wb<- bind_rows(fben_wb1, mben_wb1)

#exporting as .csv

write.csv(ben_wb, "data\\benifwb.csv")


#---------- adding benefits data to main data set 

ben_wb$Year<- as.numeric(ben_wb$Year)
suic_unemp$Year <- as.numeric(suic_unemp$Year)
suic_unemp$suic_rates<- as.numeric(suic_unemp$suic_rates)

maindf<- merge(suic_unemp, ben_wb, by = c("Sex", "Area", "Year"))

write.csv(maindf, "data/maindf.csv")

#-------------- adding the data for rates of drug spesisific deaths 

# reading in data 
raw_drug<- read.csv("data/raw_drug.csv")

# removing unnessacary colums 

drug_df<- raw_drug[ , c(1, 3, 4, 11, 12, 17, 18)]

# changing colum names 

colnames(drug_df)[1] = "Year"

# clappsing collums into one 

drug_df<- drug_df %>% unite('Area', X.1:X.2)

# removing the _ from the cells #

drug_df <-  drug_df %>% 
  mutate(Area = gsub("_", "", Area))

# renaming the rates collums temporarely 

colnames(drug_df)[4] = "male_rate"
colnames(drug_df)[6] = "fem_rate"

# selecting the collums i want 

drug_df <- drug_df %>% 
  select(Year, Area, male_rate, fem_rate)

# removing blank rows 
drug_df <- drug_df %>% 
  filter_all(any_vars(!grepl("^\\s*$", .)))

#removing the first row

drug_df<- drug_df[-1, ]

# filling in the year column 

my_years<- rep(2021:1993, each = 11)

as.data.frame(my_years)

drug_df$Year1<- my_years

# deleting the original year column and changing the name of my new year column

drug_df<- drug_df[ ,-1]

colnames(drug_df)[4] <- "Year"

# removing all columns for England 

rm_eng<- "England"

drug_df1<- drug_df[!apply(drug_df, 1, function(row)any(grepl(rm_eng, row))),]


# ----------creating a separate df for men and women 

m_drug<- drug_df1[, c(1,2,4)]

# adding a sex column 

m_drug$Sex<- "Male"

# changing the column name from male rate to drug deth 

colnames(m_drug)[2]<- "Drug_death"


# repeating the same process for females 

f_drug <- drug_df1[, c(1,3,4)]

f_drug$Sex <- "Female"

colnames(f_drug)[2]<- "Drug_death"

# ----- merging datasets together 

drg_dth<-bind_rows(f_drug, m_drug)

# making sure the yorkshire veriable is named the same as in the main df 


targ<- "Yorkshire and the Humber"
repl<- "Yorkshire and The Humber"

drg_dth$Area <- gsub(targ, repl, drg_dth$Area, ignore.case = TRUE)


#----- adding this to the main df 

maindf<- merge(maindf, drg_dth, by = c("Area", "Year", "Sex"))

# making this vaeriable numeric 

maindf$Drug_death<- as.numeric(maindf$Drug_death)

# adding in an alterntive year veriable which starts at 0 and inreases by 1 

offset<- min(maindf$Year)
maindf$CYear<- maindf$Year - offset

# spliting this dataset

split_main<- split(maindf, maindf$Sex)

male_main<- split_main$Male
fem_main<- split_main$Female

# --------------------------- cleaning alcohol use deaths -------------------#
# reading in data 
raw_alc<- read.csv("data/raw_alc.csv")

# getting rid of cllums i dont need 
alc_dth<- raw_alc[-c(1:3), c(2,3,4,6)]

# making the top row the collum names 
alc_dth<- alc_dth %>% 
  row_to_names(row_number = 1)

# changing the collum names 

colnames(alc_dth)[1]<- "Area"
colnames(alc_dth)[2]<- "Sex"
colnames(alc_dth)[3]<- "Year"
colnames(alc_dth)[4]<- "Alc_death"

# removing rows that contain 'persons' 

rm_per<- "Persons"

alc_dth1<- alc_dth[!apply(alc_dth, 1, function(row)any(grepl(rm_per, row))),]

# removing rows that are contain data on UK Scotland etc 

alch_dth2<- subset(alc_dth1, Area != "Northern Ireland" & Area!="Scotland" & Area !="England" & Area != ""
                   & Area != "England and Wales" & Area != "United Kingdom" )

# changing the name of the yorkshire veible so it matces the other one 
targ<- "Yorkshire and the Humber"
repl<- "Yorkshire and The Humber"

alch_dth2$Area <- gsub(targ, repl, alch_dth2$Area, ignore.case = TRUE)

unique(alch_dth2$Area)

# ------adding this to the main df 

# making sure the cell values for catgorcal veriables match 

alch_dth2$Year <- as.numeric(alch_dth2$Year)

alch_dth2 <- alch_dth2 %>% 
  mutate(Sex = gsub("Females", "Female", Sex)) %>% 
  mutate(Sex = gsub("Males", "Male", Sex))

maindf1<- merge(maindf, alch_dth2, by = c("Area", "Year", "Sex"))

# making sure IV's are numeric 

maindf1$Drug_death <- as.numeric(maindf1$Drug_death)
maindf1$Alc_death <- as.numeric(maindf1$Alc_death)

# adding in an alterntive year veriable which starts at 0 and inreases by 1 

offset<- min(maindf1$Year)
maindf1$CYear<- maindf1$Year - offset

# ----spliting the main df into males and females 

split_main1 <- split(maindf1, maindf1$Sex)

male_main1<- split_main1$Male
fem_main1<- split_main1$Female

#----------------------------STAGE 2: VALIDATING PROXY MESURES----------------------------#
#-----------------------------------------------------------------------------------------#
# -----------------------cleaning the validation mesures -------#

# cleaning alcohol use over the last week 

# removing data i dont need 
alch_LW<- read.csv("data/proxy/alch_LW.csv")

alch_LW<- alch_LW[c(49:61), c("X2a","X.17")]

# renaming columns 

colnames(alch_LW)[1] = "Year"
colnames(alch_LW)[2] = "Exeed_perc"

# removing the spaces and numbers from the Year collum 

alch_LW$Year<- substr(gsub("[^0-9]", "", alch_LW$Year), 1, 4)

alch_LW$Year<- as.integer(alch_LW$Year)
alch_LW$Exeed_perc <- as.numeric(alch_LW$Exeed_perc)

# --------------cleaning drug use in the last year data 

drg_LY<- read.csv("data/proxy/DU_Lyear.csv")

drg_LY<- drg_LY[c(7,31), -c(25:27) ]

drg_LY<-row_to_names(drg_LY,1)

drg_LY<- drg_LY[ , -1]

my_years<- c(1995,1997,1999, 2001:2019,2021)

colnames(drg_LY) = paste(my_years)

drg_LY<- drg_LY %>% pivot_longer(cols =  everything(), names_to = "Year", values_to = "druguse_LY")

drg_LY$Year<- as.integer(drg_LY$Year)
drg_LY$druguse_LY <- as.numeric(drg_LY$druguse_LY)

# joining the two datasets used so far 

all_data <- full_join(alch_LW, drg_LY, by ="Year") %>% arrange(Year)



# -------------------reading in the opiet and crack use data 

OCU<- read.csv("data/proxy/OCU.csv")

all_data<- full_join(all_data, OCU, by= "Year") %>% arrange(Year)

# -------------------------reading in treatment presentation data 

pres_treat<- read.csv("data/proxy/pres_treat.csv")

pres_treat<- pres_treat[ , c(1, 2, 25)]

# selecting only the alcohol sections of the DF 

alcch_select <- pres_treat %>%
  filter(grepl("^Alcohol$", X, ignore.case = TRUE)& X.23!= "")

alcch_select <- alcch_select %>%
  filter(grepl("", X.23, ignore.case = TRUE))

# adding the year to df 

my_years2<- c(2005:2021)

alcch_select$Year <- paste(my_years2) 

alch_pres<- alcch_select[ , -c(1,2)]

colnames(alch_pres)[1] = "Alc_present"
alch_pres$Year<- as.integer(alch_pres$Year)

# joining to other data set 

all_data<- full_join(all_data, alch_pres, by = "Year") %>% arrange(Year)


#---------------- cleaning the raw drug death data set to take only the values for persons for England 


per_drg_deth<- raw_drug[ , c(1, 3, 6)]

# changing colum names 

colnames(per_drg_deth)[1] = "Year"
colnames(per_drg_deth)[2] = "Area"


# renaming the rates columns temporarily 

colnames(per_drg_deth)[3] = "p_drug_deth"

# removing blank rows 
per_drg_deth <- per_drg_deth %>% 
  filter_all(any_vars(!grepl("^\\s*$", .)))

# keeping only data for england 

Eng<- "England"

per_drg_dethE<- per_drg_deth %>% 
  filter(str_detect(Area, fixed(Eng)))

# removing the area collum 
per_drg_dethE<- per_drg_dethE[ , -2]

# changing year to interger 

per_drg_dethE$Year<- as.integer(per_drg_dethE$Year)


# adding this to the df with the validation mesures 

all_data_E<- full_join(all_data, per_drg_dethE, by = "Year")

# remoiving the comma from the mesures 

col_to_clean<- c("OCU","OU","CU","Alc_present")

all_data_E<- all_data_E %>%
  mutate_at(vars(col_to_clean), ~ as.numeric(gsub(",", "", .)))


# making sure the mesures are numeric 
conv_cols<- c("Exeed_perc","druguse_LY","OCU", "OU", "CU", "Alc_present", "p_drug_deth")
all_data_E <- all_data_E %>% 
  mutate_at(vars(conv_cols), as.numeric)

# ----------- adding alcohol use to the data set for England 


Eng<- "England"
alch_deathE<- alc_dth %>% 
  filter(str_detect(Area, fixed(Eng)))

alch_deathE<- alch_deathE %>% filter(Sex == "Persons")

alch_deathE<- alch_deathE %>% filter(Area == "England")


# removing the first two collums 

alch_deathE<- alch_deathE[ , -c(1,2)]

# making the veriables interger and numeric 

alch_deathE$Year<- as.integer(alch_deathE$Year)
alch_deathE$Alc_death<- as.numeric(alch_deathE$Alc_death)

#merging with the validation dataframe for england 

all_data_E<- full_join(all_data_E, alch_deathE, by = "Year") %>% arrange(Year)


# -------checking normality

ks_tests<- lapply(all_data_E, function(column) lillie.test(column))
print(ks_tests)

#---------- running correlations


# creating correlation matrix 

require(Hmisc)
require(flextable)
require(officer)

mulcor<- rcorr(as.matrix(all_data_E))

mulcorr<-as.data.frame(mulcor$r)

ft <- flextable(mulcorr)

doc <- read_docx()

doc <- doc %>%
  body_add_flextable(value = ft)

print(doc, target = "correlation_table.docx")



#---------------------------Stage 3: preforming pre-model building assumption checks-----------------------# 

#----------------------------- multicoliniarity-----------------------# 

cormat_data<- data.frame(maindf1$Benclm_wb, maindf1$Drug_death, maindf1$Alc_death)

mulcor2<- rcorr(as.matrix(cormat_data))

mulcor2r<- as.data.frame(mulcor2$r)

ft2 <- flextable(mulcor2r)

doc <- read_docx()

doc <- doc %>%
  body_add_flextable(value = ft2)

print(doc, target = "correlation_table2.docx")

# Based on this I will create a coposite verieble for drug and alchohol deaths to remove the muticoliniarity issue 

maindf1$alc_drg_dth<- maindf1$Drug_death + maindf1$Alc_death

# running corelations again 

cor.test(maindf1$Benclm_wb,  maindf1$alc_drg_dth)



#--------------------------------- asssesing liniarity ----------------------------# 


#-----------examining benifit clainments on suicide rates 

maindf1 %>% 
  ggplot(mapping = aes(x = Benclm_wb, y = suic_rates)) +
  geom_point()

# looking at this for each sex  

maindf1_fil<- maindf1 %>%
  filter(Sex %in% head(unique(Sex), n = 2))

maindf1_fil%>% 
  ggplot() +
  geom_point(mapping = aes(x = Benclm_wb, y = suic_rates)) +
  facet_wrap(~ Sex)

# graphing each sex one plane with regression lines 

maindf1%>% 
  group_by(Sex) %>% 
  ggplot(mapping = aes(x = Benclm_wb, y = suic_rates, colour = factor(Sex))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE, fullrange = TRUE)

# examining liniarity for sex and area (benclm on sucic rates) 

maindf1_fil2<- maindf1 %>%
  filter(Sex %in% head(unique(Sex))) %>%  
  filter(Area %in% head(unique(Area)))

maindf1_fil2 %>% 
  ggplot() +
  geom_point(mapping = aes(x = Benclm_wb, y = suic_rates)) +
  facet_wrap(~ Sex*Area)

#------------------------- asssesing liniarity of drug and alchohol deaths on suic rates 

maindf1 %>% 
  ggplot(mapping = aes(x = alc_drg_dth, y = suic_rates)) +
  geom_point()

# examining this for each sex 

maindf1_fil%>% 
  ggplot() +
  geom_point(mapping = aes(x = alc_drg_dth, y = suic_rates)) +
  facet_wrap(~ Sex)

# graphing each sex one plane with regression lines 

maindf1%>% 
  group_by(Sex) %>% 
  ggplot(mapping = aes(x = alc_drg_dth, y = suic_rates, colour = factor(Sex))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE, fullrange = TRUE)

# examining liniarity drug and alc deaths on suic rates by sex and area 

maindf1_fil2 %>% 
  ggplot() +
  geom_point(mapping = aes(x = alc_drg_dth, y = suic_rates)) +
  facet_wrap(~ Sex*Area)


#----------------- assesing liniarity of CYear on suicide rates


ggplot(maindf1, aes(x= CYear, y = suic_rates))+
  geom_point() # non liniar relationship here 

# graphing each sex one plane with regression lines 

maindf1%>% 
  group_by(Sex) %>% 
  ggplot(mapping = aes(x = CYear, y = suic_rates, colour = factor(Sex))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE, fullrange = TRUE) # this seems to suggest a rendom effet for sex could be enterd 

# across sex and area 

ggplot(maindf1, aes(x = CYear, y = suic_rates))+
  geom_point()+
  facet_grid(Sex ~ Area)

# it looks like we have a non liniar relationship between suicide rates and time for each sex, may have to fit a sine wave or somthing here. 

# Assesing the distibution of the predictors 

hist(maindf1$suic_rates)
hist(maindf1$alc_drg_dth)
hist(maindf1$Benclm_wb) 
# these are all non normal, could potetioly be problematic 


# ------------------------------------- Stage 4: inital model building----------------------------------------# 

NullSL<- lm(suic_rates ~ 1, data = maindf1)
summary(NullSL)

# making residual plot 

par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(NullSL), ylab='Residuals',xlab='Index')


#Fit a null model with random intercepts for Sex#

Null1<- lmer(suic_rates ~ 1 + (1|Sex), data = maindf1, REML = FALSE)
summary(Null1)
ranef(Null1)


# create catipiller plot  

dotplot(ranef(Null1, condVar = TRUE), strip = F)

# making residual plot 


# calcuclate log likelyhhod ratio 

anova(Null1, NullSL)


# creating ICc function 

ICC.Model<-function(Model.Name) {
  tau.Null<-as.numeric(lapply(summary(Model.Name)$varcor, diag))
  sigma.Null <- as.numeric(attr(summary(Model.Name)$varcor, "sc")^2)
  ICC.Null <- tau.Null/(tau.Null+sigma.Null)
  return(ICC.Null)
}

# calculate ICC 
ICC.Model(Null1)

# ---------------- creating null model with random intercepts for area ---

Null2<- lmer(suic_rates ~ 1 + (1 |Area), data = maindf1, REML = FALSE)
summary(Null2)
ranef(Null2)

# create catipiller plot of random effects 

dotplot(ranef(Null2, condVar = TRUE), strip = F)

# making residual plot 

par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(Null2), ylab='Residuals',xlab='Index')

# calcuclate log likelyhhod ratio against the null single intercept model 

anova(Null2, NullSL)


# calculate ICC 

ICC.Model(Null2)

#------------------------ repeat this prosess for rand effs for sex and area ------------

Null3<- lmer(suic_rates ~ 1 + (1 |Sex:Area), data = maindf1, REML = FALSE)
summary(Null3)
ranef(Null3)


# create catipiller plot of random effects 

dotplot(ranef(Null3, condVar = TRUE), strip = F)

# making residual plot 

par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(Null3), ylab='Residuals',xlab='Index')

# calcuclate log likelyhhod ratio against the null single intercept model 

anova(Null3, NullSL)

# and the intercepts model for sex 

anova(Null3, Null1)

# calculate ICC 

ICC.Model(Null3)

# -------creating model with main effects (controlling for year) and random intercepts only for area and sex 

mainef_mod<- lmer(suic_rates ~ CYear+ alc_drg_dth + Benclm_wb + Sex + Area + (1|Sex:Area), data = maindf1)
summary(mainef_mod)
ranef(mainef_mod)

# making residual plot 

par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(mainef_mod), ylab='Residuals',xlab='Index')

# calcuclate log likelyhhod ratio against intercepts model for sex and area 

anova(mainef_mod, Null3)

# calculate ICC 

ICC.Model(mainef_mod)

# -------creating model with main effects moderated by sex (controlling for year) and random intercepts only for area and sex 


mainef_mod2<- lmer(suic_rates ~ CYear:Sex + alc_drg_dth + Benclm_wb +Sex + Area + 
                     (1|Sex:Area), data = maindf1)
summary(mainef_mod2)
ranef(mainef_mod2)

# calcuclate log likelyhhod ratio against previous model

anova(mainef_mod2, mainef_mod)


# -------- creating model with random effects sructure for all predicots (maximal model)

# reordring london to refernce catagory 

maindf1$Area<- as.factor(maindf1$Area)
maindf1$Area <- relevel(maindf1$Area , ref = "London")


MLM1<- lmer(suic_rates ~ CYear+ alc_drg_dth + Benclm_wb+ Sex + Area +(1+ CYear | Sex:Area) +
              (1+ alc_drg_dth | Sex:Area) + (1+ Benclm_wb | Sex:Area)
            , data = maindf1)

summary(MLM1)
ranef(MLM1) # is singular 
# estimating slopes and intercepts speratyly  

MLM2<- lmer(suic_rates ~ CYear+ alc_drg_dth + Benclm_wb+ Sex + Area +(1| Sex:Area) + (0 + CYear | Sex:Area)+ 
              (1|Sex:Area) +( 0 + alc_drg_dth | Sex:Area) + 
              (1| Sex:Area) + (0+ Benclm_wb |Sex:Area), data = maindf1)

summary(MLM2)
ranef(MLM2) #is singular 


# removing alchol and drug deaths from random effets and running model again 

MLM3<- lmer(suic_rates ~ CYear+ alc_drg_dth + Benclm_wb+ Sex + Area:Sex +
              (1| Sex:Area) + (0 + CYear | Sex:Area)+ 
              (1| Sex:Area) + (0+ Benclm_wb |Sex:Area), data = maindf1)


summary(MLM3)
ranef(MLM3) # non convergance 

# both predictors with same intercept 

MLM3<- lmer(suic_rates ~ CYear+ alc_drg_dth + Benclm_wb+ Sex + Area +  
              (1 + CYear+ Benclm_wb| Sex:Area), data = maindf1)

summary(MLM3)
ranef(MLM3)

#atempting to examine this as a moderation for the effects of sex  


MLM4<- lmer(suic_rates ~ CYear:Sex+ alc_drg_dth:Sex + Benclm_wb:Sex+
              Sex+ Area + (1 + CYear|Area:Sex), data = maindf1)

summary(MLM4)
ranef(MLM4)

anova(MLM4, Null3)
anova(MLM4, mainef_mod)



#------------------------------ stage 5: preforming futher assumption checks on the eligiable model---------------------------------#


# ---------- testing normaility of residuals ---------------------# 

resid_MLM4 <- resid(MLM4)
hist(resid_MLM4)
qqnorm(resid_MLM4)
qqline(resid_MLM4)
plot(density(resid_MLM4))
lillie.test(resid_MLM4)

# creating dataset removing outliers 3 SD + / - residual mean 

maindf_OR<- romr.fnc(MLM4, maindf1, trim = 3.0)

maindf2<- as.data.frame(maindf_OR$data)

# running model again without outliers 

MLM5<- lmer(suic_rates ~ CYear:Sex+ alc_drg_dth:Sex + Benclm_wb:Sex+ Sex + 
              Area + (1 + CYear|Area:Sex), data = maindf2)
summary(MLM5)
ranef(MLM5) 

MLM5<- lmer(suic_rates ~ CYear:Sex+ alc_drg_dth:Sex + Benclm_wb:Sex+ Sex + 
              Area + (1|Area:Sex) + (0 + CYear|Area:Sex), data = maindf2)
summary(MLM5)
ranef(MLM5) 

# testing normality of resifuals again 

L1resid_MLM5 <- resid(MLM5)
hist(L1resid_MLM5)
qqnorm(L1resid_MLM5)
qqline(L1resid_MLM5)
plot(density(L1resid_MLM5))
lillie.test(L1resid_MLM5)

# level one assumptions of homoscdactiity
# alc drug death  

maindf2 %>% 
  ggplot(aes(x = alc_drg_dth, y = L1resid_MLM5))+
  geom_point()+
  labs(x= 'alcdrugdeath', y = 'resid') # this has been violated 

# ben clm 

maindf2 %>% 
  ggplot(aes(x = Benclm_wb, y = L1resid_MLM5))+
  geom_point()+
  labs(x= 'benclm', y = 'resid')  # also dprobably violated (borderline)



# ------  homoscedastsicity(level 1 residuals independent of level 2 predictors)


maindf2$Area_sex <- paste(maindf2$Area, maindf2$Sex, sep = " ")

subgrp_data<- split(maindf2, maindf2$Area_sex)



for (group_data in subgrp_data) {
  model <- lm(suic_rates ~ CYear, data = group_data)
  plot(model$fitted.values, model$residuals,  main = unique(group_data$Area_sex), ylim = c(-4,4))
}  # this looks more or less ok, some outliers but spread is roughly constant and random

# -------BP test 

for(group_data in subgrp_data) {
  model <- lm(suic_rates ~ CYear  , data = group_data)
  print(unique(group_data$Area_sex))
  print(bptest(model)) # one is below .05 sig, homoscedasticity of level 2 is therefore  violated in one case 
}


#---------homogeneity of variance 

# creating absolute values for model residuals 

maindf2$L1resid_abs<- abs(L1resid_MLM5)

# squaring these 

maindf2$L1resid_absp2<- maindf2$L1resid_abs^2

# testing assumptions 
levene_mod<- lm(L1resid_absp2 ~ Area_sex, data = maindf2)
anova(levene_mod) # violated


## examine plots 

dotplot(resid(MLM5) ~ Area_sex, data = maindf2)
# looks like there is quite a bit of variation in residuals between groups


# ----------  normality of level 2 residuals 
# extracting level 2 slope residuals 

L2_data <- maindf2 %>% 
  group_by(Area, Sex) %>%  
  mutate(
    mean_suic = mean(suic_rates), 
    mean_LBC = mean(Benclm_wb)
  ) %>% 
  select(Area, Sex, mean_suic, mean_LBC ) %>% 
  unique() 


L2_data$slope_resid<- ranef(MLM5)$'Area:Sex'[, 1]

L2_data %>% 
  ggplot(aes(x= slope_resid)) + 
  geom_histogram(binwidth = .2)

L2_data %>% 
  ggplot(aes(sample = slope_resid)) + 
  stat_qq()

lillie.test(L2_data$slope_resid)

shapiro.test(L2_data$slope_resid) # looks fine 

#------  level 1 and level 2 residuals are independent 

# working out how many cases are in each group 

n_per_group <- maindf2 %>% 
  group_by(Area, Sex) %>% 
  select(Area, Sex) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  unlist()

# adding slope  residuals to the main df and repeating these by the number of level 1 cases


maindf2$slope_resid<- rep(L2_data$slope_resid, times = n_per_group)

# --- testing wether level 1 and 2 residuals are correlated 

maindf2 %>% 
  ggplot( aes(x = L1resid_MLM5, y = slope_resid)) +
  geom_point() # looks fine 

# level 2 residuals independant of level 1 predicitors

maindf2 %>% 
  ggplot(aes(x  = slope_resid, y = Benclm_wb))+
  geom_point()

cor.test(maindf2$slope_resid, maindf2$Benclm_wb)# also fine

maindf2 %>% 
  ggplot(aes(x  = slope_resid, y = Benclm_wb))+
  geom_point()

cor.test(maindf2$slope_resid, maindf2$Benclm_wb) # this is ok 

# ---------- final model 

FMOD1<- lmer(suic_rates ~ CYear:Sex+ alc_drg_dth:Sex + Benclm_wb:Sex+ Sex + 
               Area + (1|Sex:Area) + (0 + CYear|Sex:Area), data = maindf2)
summary(FMOD1)
ranef(FMOD1)

# creating results tables 

coefficients <- coef(summary(FMOD1))
std_errors <- sqrt(diag(vcov(FMOD1)))
p_values <- summary(FMOD1)$coefficients[, "Pr(>|t|)"]
df <- summary(FMOD1)$coefficients[, "df"]
t_values <- summary(FMOD1)$coefficients[, "t value"]

# Combine coefficients and standard errors into a data frame
summary_df1 <- data.frame(
  Effect = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  Std_Error = std_errors,
  Degrees_of_Freedom = df,
  T_Value = t_values,
  P_Value = p_values,
  stringsAsFactors = FALSE
)

summary_df1<- round(summary_df1[, 2:6], digits = 3)
summary_df1<-cbind(Effect = rownames(coefficients), summary_df1 )



random_effects1 <- as.data.frame(ranef(FMOD1)$'Sex:Area')

random_effects1<- round(random_effects1, digits = 3)

random_effects1<- cbind(Region = rownames(ranef(FMOD1)$'Sex:Area'), random_effects1)

require(officer)

doc <- read_docx()

# Add a title
doc <- doc %>%
  body_add("Main  Effects Summary", style = "heading 1")

# Add the summary table
doc <- doc %>%
  body_add(summary_df1)  

doc <- doc %>%
  body_add("Random Effects Summary", style = "heading 1")

doc <- doc%>%
  body_add(random_effects1)

# Save the document
print(doc, target = "results1.docx")






####---------- ----------------------------------------------------------------------------------------------------------------####
####---------------------------------------------------------------------------------------------------------------------------####
####--------------------------running ayanslsis again on split data set for men only and re running assumption checks----------####



#--------------------------- preforming pre-model building assumption checks-----------------------#

#----------------------------- multicoliniarity-----------------------# 

cormat_data2<- data.frame(male_main1$Benclm_wb, male_main1$Drug_death, male_main1$Alc_death, male_main1$Year)

cor(cormat_data2) 


# Based on this I will create a composite verieble for drug and alchohol deaths to remove the muticoliniarity issue 


male_main1$ADD<- male_main1$Drug_death + male_main1$Alc_death

cormat_data3<- data.frame(male_main1$Benclm_wb, male_main1$ADD,  male_main1$Year)

cor(cormat_data3)

cor.test(male_main1$ADD, male_main1$Benclm_wb) # this looks like it has solved the multicolinarity issue somewhat 

#--------------------------------- asssesing liniarity ----------------------------# 

#-----------examining benifit clainments on suicide rates 

male_main1 %>% 
  ggplot(mapping = aes(x = Benclm_wb, y = suic_rates)) +
  geom_point() # looks great 

#  graphing each Area one plane with regression lines

male_main1%>% 
  group_by(Area) %>% 
  ggplot(mapping = aes(x = Benclm_wb, y = suic_rates, colour = factor(Area))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE, fullrange = TRUE)

#  producing seperate plots 

male_main_fil<- male_main1%>%
  filter(Area %in% unique(Area))

male_main_fil %>% 
  ggplot() +
  geom_point(mapping = aes(x = Benclm_wb, y = suic_rates)) +
  facet_wrap(~ Area) # this looks good, more or less liniar, but possibly a few outliers 

#------------------------- asssesing liniarity of drug and alchohol deaths on suic rates 

male_main1 %>% 
  ggplot(mapping = aes(x = ADD, y = suic_rates)) +
  geom_point() # yep this is good, again mebe an outlier or two 

# examining this for each Area 

male_main_fil %>%  
  ggplot() +
  geom_point(mapping = aes(x = ADD, y = suic_rates)) +
  facet_wrap(~ Area) # all look prety liniar 

# graphing each area one plane with regression lines 

male_main1 %>%  
  group_by(Area) %>% 
  ggplot(mapping = aes(x = ADD, y = suic_rates, colour = factor(Area))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE, fullrange = TRUE) # again posibly some outliers but all looks good otherwise


#--------------------------------------------- assesing liniarity of CYear on suicide rates

ggplot(male_main1, aes(x= CYear, y = suic_rates))+
  geom_point() # definatly non liniar will try and fit a curve to this 

# across area 

ggplot(male_main1, aes(x = CYear, y = suic_rates))+
  geom_point()+
  facet_wrap(~Area) # these look sort of liniar but could be an issue, posible violation 

# ------------------------------------looking at fiting a sine wave to the year data

# option 1: fit a moderate sine wave 

sine1<-lm(suic_rates ~ sin(CYear), data = male_main1)
summary(sine1)

sinedf1<- data.frame( CYear = male_main1$CYear , suic_rates = male_main1$suic_rates, fit = predict(sine1))

ggplot(sinedf1, aes(x = CYear, y = suic_rates))+
  geom_point(color = "blue") +
  geom_line(aes(y= fit), color = "red", size = 1)


# option 2: make a better fitting wave using a graph 


sine2<- nls(suic_rates ~ A * sin(B*CYear+ phi) + C , start = list(A = 2 , B = 0.6, phi = 1.5,
                                                                  C = 0), data = male_main1)
summary(sine2)

sinedf2<- data.frame( CYear = male_main1$CYear , suic_rates = male_main1$suic_rates, fit = predict(sine2))

ggplot(sinedf2, aes(x = CYear, y = suic_rates))+
  geom_point(color = "blue") +
  geom_line(aes(y= fit), color = "red", size = 1)


#option n3: preforming parameter optimization to find the sine wave that fits my data the best 

# create function to calculate sum of squared differences 

CYear<- male_main1$CYear
suic_rates<- male_main1$suic_rates

objective_function<- function(params, CYear, suic_rates) { 
  A<- A <- params[1]
  B <- params[2]
  phi <- params[3]
  C <- params[4]
  predicted <- A * sin(B * CYear + phi) + C
  sum((predicted - suic_rates)^2)
}

# enter initial parameter values 

initial_params <- c(A = 2 , B = 0.6, phi = 1.5, C = 0)

#preform optimization 

opt_result <- optim(par = initial_params, fn = objective_function, 
                    CYear = male_main1$CYear , suic_rates = male_main1$suic_rates)

# Extract optimized parameters
optimized_params <- opt_result$par

# Fit a sine wave with optimized parameters to the data
optimized_model <- nls(suic_rates ~ A * sin(B * CYear + phi) + C, start = list(A = optimized_params[1], B = optimized_params[2], 
                                                                               phi = optimized_params[3], C = optimized_params[4]))
summary(optimized_model)

# Create a data frame for plotting

opt_df <- data.frame(CYear = CYear, suic_rates = suic_rates, fit = predict(optimized_model))

# Plot the data and fitted sine wave using ggplot2

ggplot(opt_df, aes(x = CYear, y= suic_rates)) +
  geom_point(color = "blue") +
  geom_line(aes(y = fit), color = "red", size = 1) +
  labs(title = "Optimized Fitting of Sine Wave")

# create sine wave function 
sine_wave <- function(x, A, B, phi, C) {
  A * sin(B * x + phi) + C
}
# ------------------------------------- inital model building----------------------------------------# 

NullSL_Male<- lm(suic_rates ~ 1, data = male_main1)
summary(NullSL_Male)

# making residual plot 

par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(NullSL_Male), ylab='Residuals',xlab='Index')

#-Fit a null model with random intercepts for Area

Null1_M<- lmer(suic_rates ~ 1 + (1|Area), data = male_main1)
summary(Null1_M)
ranef(Null1_M)

# create catipiller plot  

dotplot(ranef(Null1_M, condVar = TRUE), strip = F) # this looks like quite a bit of variation 

# making residual plot 

par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(Null1_M), ylab='Residuals',xlab='Index')

# calcuclate log likelyhhod ratio 

anova(Null1_M, NullSL_Male)

# calculate ICC 

ICC.Model(Null1_M)

# -------creating model with main effects and random intercepts only for area (keeping year as liniar)

mainef_mod_M<- lmer(suic_rates ~ CYear+ ADD + Benclm_wb + (1|Area) , data = male_main1)
summary(mainef_mod_M)
ranef(mainef_mod_M)

# making residual plot
par(mfrow=c(1,1), bty='n',mar = c(5, 4, .1, .1), cex=1.1, pch=16)
plot(resid(mainef_mod_M), ylab='Residuals',xlab='Index')

# calcuclate log likelyhhod ratio against intercepts model for area 

anova(mainef_mod_M,Null1_M)

ICC.Model(mainef_mod_M)

# ------------ using sine wave (OPTIMISED PARAMETERS) and looking at fit 

CY_op_sin<- sine_wave(CYear, A = 0.7761745, B = 0.3285576, 
                      phi = -10.2833392, C = 15.9711753 )

sine3_mod_M<- lmer(suic_rates ~ CY_op_sin + ADD + Benclm_wb + (1|Area) , data = male_main1)
summary(sine3_mod_M)
ranef(sine3_mod_M)

anova(mainef_mod_M,sine3_mod_M) 

ICC.Model(sine3_mod_M) 


# building maximal model with sine curve for main efffect of CYear 


MLM1_M<- lmer(suic_rates ~ CY_op_sin+ ADD + Benclm_wb  + (1+ CYear|Area)+ 
                (1+ ADD|Area) + (1+ Benclm_wb|Area) , data = male_main1)
summary(MLM1_M)
ranef(MLM1_M) # is singualr 


#removing alcho and drug death from random effets 

MLM2_M<- lmer(suic_rates ~ CY_op_sin+ ADD + Benclm_wb  + (1+ CYear|Area)+ 
                (1+ Benclm_wb|Area) , data = male_main1)
summary(MLM2_M)
ranef(MLM2_M) # singularity 

# cobing both predictors into the same effects structure 

MLM3_M<- lmer(suic_rates ~ CY_op_sin+ ADD + Benclm_wb  + (1+ CYear+ Benclm_wb|Area) 
              , data = male_main1)
summary(MLM3_M)
ranef(MLM3_M) 


anova(sine3_mod_M, MLM3_M)

#-------------  Assumptions 
#--------------  checking normaility of residuals 

male_main1$L1resid <- resid(MLM3_M)
hist(male_main1$L1resid)
qqnorm(male_main1$L1resid)
qqline(male_main1$L1resid)
lillie.test(male_main1$L1resid) # this is great !!

#------------ level one assumptions of homoscdactiity
# alc drug death  

male_main1 %>% 
  ggplot(aes(x = ADD, y = L1resid))+
  geom_point()+
  labs(x= 'alcdrugdeath', y = 'resid') # looks violated 

# on benifit claiments 

male_main1 %>% 
  ggplot(aes(x = Benclm_wb, y = L1resid))+
  geom_point()+
  labs(x= 'benclm', y = 'resid') # this looks fine 

#checking notrmaility of ADD and transform if needed 
hist(male_main1$ADD)

hist(log(male_main1$ADD))
lillie.test(log(male_main1$ADD))

hist(male_main1$ADD)


result<- boxcox(male_main1$ADD)
male_main1$BXCX_ADD<- predict(result)
hist(male_main1$BXCX_ADD)
lillie.test(male_main1$BXCX_ADD)

#running model again with transformed predictor 

MLM4_M<- lmer(suic_rates ~ CY_op_sin+ BXCX_ADD + Benclm_wb  + (1+ CYear+ Benclm_wb|Area) 
              , data = male_main1)
summary(MLM4_M)
ranef(MLM4_M)

anova(sine3_mod_M, MLM4_M)

# --------- checking normality of residuals  

male_main1$L1resid <- resid(MLM4_M)
hist(male_main1$L1resid)
qqnorm(male_main1$L1resid)
qqline(male_main1$L1resid)
lillie.test(male_main1$L1resid) 

# Checking hetroscedacticity 

# alc drug death  

male_main1 %>% 
  ggplot(aes(x = BXCX_ADD, y = L1resid))+
  geom_point()+
  labs(x= 'alcdrugdeath', y = 'resid') # this looks good now 

#--------level 2 predictor assumptions----------# 

## ------  homoscedastsicity(level 1 residuals independent of level 2 predictors)


subgrp_data2<- split(male_main1, male_main1$Area)

for (group_data in subgrp_data2) {
  model <- lm(suic_rates ~ CYear + Benclm_wb , data = group_data)
  plot(model$fitted.values, model$residuals,  main = unique(group_data$Area), ylim = c(-4,4))
} 
# looks more or less fine, some veriation is spread between groups

for(group_data in subgrp_data2) {
  model <- lm(suic_rates ~ CYear + Benclm_wb , data = group_data)
  print(unique(group_data$Area))
  print(bptest(model)) # only 2 significant values but scatterlots dont reveal hetroscedacticity 
}

# ----------homogeneity of variance 

# creating absolute values for model residuals 
male_main1$l1resid_abs <- abs(male_main1$L1resid)

male_main1$l1resid_abs_2 <- male_main1$l1resid_abs^2

# testing assumptions 
levene_mod<- lm(l1resid_abs_2 ~ Area, data = male_main1)
anova(levene_mod) 

## examine plots 
dotplot(resid(MLM4_M) ~ Area, data = male_main1)

#independence of L2 residuals 

# extracting level 2 slope and intercept residuals 

L2_data_M <- male_main1 %>% 
  group_by(Area, Sex) %>%  
  mutate(
    mean_suic = mean(suic_rates), 
    mean_BC = mean(Benclm_wb)
  ) %>% 
  select(Area, Sex, mean_suic, mean_BC ) %>% 
  unique() 


L2_data_M$int_resid<- ranef(MLM4_M)$'Area'[, 1]

L2_data_M$Y_slope_resid<- ranef(MLM4_M)$'Area'[, 2]

L2_data_M$BC_slope_resid<- ranef(MLM4_M)$'Area'[, 3]

# print  corelation matrix 

cor(L2_data_M[ , 3:7])

#----------  normality of level 2 residuals 

# intercepts 

L2_data_M %>% 
  ggplot(aes(x= int_resid)) + 
  geom_histogram(binwidth = .40)  

L2_data_M%>% 
  ggplot(aes(sample = int_resid)) + 
  stat_qq() 

lillie.test( L2_data_M$int_resid) 
shapiro.test( L2_data_M$int_resid)# this is fine 


# slopes 

# year 

L2_data_M %>% 
  ggplot(aes(x= Y_slope_resid)) + 
  geom_histogram(binwidth = .15)

L2_data_M%>% 
  ggplot(aes(sample = Y_slope_resid)) + 
  stat_qq() # violated 

lillie.test( L2_data_M$Y_slope_resid)
shapiro.test( L2_data_M$Y_slope_resid) # violated 

#benclm 
L2_data_M %>% 
  ggplot(aes(x= BC_slope_resid)) + 
  geom_histogram(binwidth = .15)

L2_data_M%>% 
  ggplot(aes(sample = BC_slope_resid)) + 
  stat_qq() # violated 

lillie.test(L2_data_M$BC_slope_resid)
shapiro.test(L2_data_M$BC_slope_resid) # this is fine 


# ------  level 1 and level 2 residuals are independent 

# working out how many cases are in each group 

n_per_group2 <- male_main1 %>% 
  group_by(Area) %>% 
  select(Area) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  unlist()

# adding intercepts residuals to the main df and repeating these by the number of level 1 cases 

male_main1$int_resid<- rep(L2_data_M$int_resid, times = n_per_group2)

# same prosess for slopes 
# # year 

male_main1$Y_slope_resid<- rep(L2_data_M$Y_slope_resid, times = n_per_group2)

# benclm 
# 
male_main1$BC_slope_resid<- rep(L2_data_M$BC_slope_resid, times = n_per_group2)


# --- testing wether level 1 and 2 residuals are correlated 

# intercepts 

male_main1 %>% 
  ggplot(aes(x= L1resid, y= int_resid))+
  geom_point()

cor.test(male_main1$L1resid, male_main1$int_resid) # this looks fine 

# slopes 
# year 
male_main1 %>% 
  ggplot(aes(x= L1resid, y= Y_slope_resid))+
  geom_point()

cor.test(male_main1$L1resid, male_main1$Y_slope_resid) # this is fine 

# benclm 

male_main1 %>% 
  ggplot(aes(x= L1resid, y= BC_slope_resid))+
  geom_point()

cor.test(male_main1$L1resid, male_main1$BC_slope_resid)


# level 1 resisduals are independant of level 2 predictors and vice versa 

# level 1 residuals independant of level 2 predictors 

male_main1$mean_BCWB<- rep(L2_data_M$mean_BC, times = n_per_group2)

male_main1 %>% 
  ggplot(aes(x=L1resid , y= mean_BCWB))+
  geom_point()

cor.test(male_main1$L1resid, male_main1$mean_BCWB) # this is fine 

# level 2 residuals independant of level 1 predicitors 

# l2 resid independent of benifit claimants 
# intercepts 

male_main1 %>% 
  ggplot(aes(x  = int_resid, y = Benclm_wb))+
  geom_point()

cor.test(male_main1$int_resid, male_main1$Benclm_wb) # looks ok howevr some knid of trend apears to be going on 

# same prosess for slopes 
# year
male_main1 %>% 
  ggplot(aes(x  = Y_slope_resid, y = Benclm_wb))+
  geom_point()

cor.test(male_main1$Y_slope_resid, male_main1$Benclm_wb) # looks like there is a slight trend here 

# benclm 

male_main1 %>% 
  ggplot(aes(x  = BC_slope_resid, y = Benclm_wb))+
  geom_point()

cor.test(male_main1$BC_slope_resid, male_main1$Benclm_wb) # again a slight trend here 


# l2 resid independant of drug and alc deaths 
# intercepts 

male_main1 %>% 
  ggplot(aes(x  = int_resid, y = BXCX_ADD))+
  geom_point()

cor.test(male_main1$int_resid, male_main1$BXCX_ADD) #definate trend here 

# same prosess for slopes 

# year
male_main1 %>% 
  ggplot(aes(x  = Y_slope_resid, y = BXCX_ADD))+
  geom_point()

cor.test(male_main1$Y_slope_resid, male_main1$BXCX_ADD) # these are  corelated !!!!

# benclm 

male_main1 %>% 
  ggplot(aes(x  = BC_slope_resid, y = BXCX_ADD))+
  geom_point()

cor.test(male_main1$BC_slope_resid, male_main1$BXCX_ADD) # correlated 

# -------------------------------- final model -------------------------# 


F_Mod_M<- lmer(suic_rates ~ CY_op_sin+ BXCX_ADD + Benclm_wb  + (1+ CYear+ Benclm_wb|Area) 
               , data = male_main1)
summary(F_Mod_M)
ranef(F_Mod_M)

# creating results tables 

coefficients <- coef(summary(F_Mod_M))
std_errors <- sqrt(diag(vcov(F_Mod_M)))
p_values <- summary(F_Mod_M)$coefficients[, "Pr(>|t|)"]
df <- summary(F_Mod_M)$coefficients[, "df"]
t_values <- summary(F_Mod_M)$coefficients[, "t value"]

# Combine coefficients and standard errors into a data frame
summary_df2 <- data.frame(
  Effect = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  Std_Error = std_errors,
  Degrees_of_Freedom = df,
  T_Value = t_values,
  P_Value = p_values,
  stringsAsFactors = FALSE
)

summary_df2<- round(summary_df2[ ,2:6], digits = 3)

summary_df2<- cbind(Effect = rownames(coefficients), summary_df2)



random_effects2 <- as.data.frame(ranef(F_Mod_M)$'Area')

random_effects2<- round(random_effects2, digits = 3)

random_effects2<- cbind(Region = rownames(ranef(F_Mod_M)$'Area'), random_effects2)

require(officer)

doc <- read_docx()

# Add a title
doc <- doc %>%
  body_add("Main  Effects Summary", style = "heading 1")

# Add the summary table
doc <- doc %>%
  body_add(summary_df2)  

doc <- doc %>%
  body_add("rand Effects Summary", style = "heading 1")

doc <- doc%>%
  body_add(random_effects2)

# Save the document
print(doc, target = "results2.docx")

# -----------------------------------------Making graphs for the report -------------------------------# 

# creating GGplot barplot  of mean suiide rates for england and wales. 

# creating DF 
m_suic<- maindf %>%
  group_by(Area, Sex) %>% 
  summarise(suic = mean(suic_rates))


m_suic$Area<- factor(m_suic$Area, levels = c( "North East","North West","Yorkshire and The Humber","Wales","East Midlands","West Midlands", 
                                              "East", "London", "South East", "South West" ))
write.csv(m_suic, "data\\Mean_suic1.csv")

# creating plot 

Mean_suic1<- ggplot(m_suic, aes(x= Area, y= suic, fill = Sex))+ 
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  labs(
    x = "Region",
    y = "Suicide Rate",
    fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# creating line chart of suicide rates for north east, ewales and london 

wales_suic<- subset(maindf, Area == "Wales" )

NE_suic<- subset(maindf, Area == "North East" )

LDN_suic <- subset(maindf, Area  == "London")

ggplot(wales_suic, aes(x = Year, y = suic_rates, color= Sex, group = Sex))+
  geom_point( size = 3)+
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed", size = 1.2) + 
  labs( x = "Year",
        y = "Suicide Rate")+
  theme_minimal()+
  theme(plot.title = element_text(size = 16, face = "bold"),  
        axis.title = element_text(size = 14),  
        axis.text = element_text(size = 12),   
        legend.title = element_blank(),        
        legend.text = element_text(size = 12))

ggplot(NE_suic, aes(x = Year, y = suic_rates, color= Sex, group = Sex))+
  geom_point( size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed", size = 1.2) + 
  labs( x = "Year",
        y = "Suicide Rate")+
  theme_minimal() +  
  theme(plot.title = element_text(size = 16, face = "bold"),  
        axis.title = element_text(size = 14),  
        axis.text = element_text(size = 12),   
        legend.title = element_blank(),        
        legend.text = element_text(size = 12)) 

ggplot(LDN_suic, aes(x = Year, y = suic_rates, color= Sex, group = Sex))+
  geom_point( size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed", size = 1.2) + 
  labs( x = "Year",
        y = "Suicide Rate")+
  theme_minimal() +  
  theme(plot.title = element_text(size = 16, face = "bold"),  
        axis.title = element_text(size = 14),  
        axis.text = element_text(size = 12),   
        legend.title = element_blank(),        
        legend.text = element_text(size = 12))


# making tables of drug and alchol deaths 

library(flextable)
library(officer)

alch_dth2$Alc_death<- as.numeric(alch_dth2$Alc_death)
drg_dth$Drug_death<-as.numeric(drg_dth$Drug_death)

M_alc_dth<- alch_dth2 %>% group_by(Area, Sex) %>%summarise(mean_A_dth = mean(Alc_death))

M_drg_dth<- drg_dth %>%  group_by(Area, Sex) %>% summarise(mean_D_dth = mean(Drug_death))

# combining both datasets to make a single deaths from substance abuse composite veritable 

mean_SA<- left_join( M_drg_dth, M_alc_dth)

mean_SA$SA_dth<- mean_SA$mean_D_dth + mean_SA$mean_A_dth

# graphing mean alchol deaths 

ggplot(mean_SA, aes(x= Area, y= mean_A_dth, fill = Sex))+ 
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  labs(
    x = "Region",
    y = "Alcohol-related Deaths",
    fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graphig mean drug deaths 

ggplot(mean_SA, aes(x= Area, y= mean_D_dth, fill = Sex))+ 
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  labs(
    x = "Region",
    y = "Drug-related Deaths ",
    fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# graphing mean deaths from all substance abuse 

ggplot(mean_SA, aes(x= Area, y= SA_dth, fill = Sex))+ 
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  labs(
    x = "Region",
    y = "Substance abuse related Deaths 
     per 100K",
    fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(mean_SA, "data/mean_SA.csv")
