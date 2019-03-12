#install.packages('boxr')
library(boxr)
library(dplyr)
library(memisc)
library(stringr)
library(tidyr)
library(gtools)
library(data.table)
library(caret)
library(GGally)
library(corrplot)
devtools::source_gist("4959237")

box_auth()
box_setwd(64535783500)
box_ls(dir_id = box_getwd())

#Data readin

###########################################################################
####### Survey Keys to merge the sales data with email and survey data ####
####### Input files: survey_key and old_survey Output_file: survey_key ####
###########################################################################

survey_key<- box_search("Survey Key.csv") %>% box_read()
#rm(survey_key)

# Initial Control Totals
summary(unique(survey_key$ANON.Contract))
summary(unique(survey_key$ANON.IPN))

# Missing values
blank<-survey_key[survey_key$ANON.IPN=="",]
blank<-survey_key[survey_key$ANON.Contract=="",]
#rm(blank)

#There are 3 missing values which needs to be mapped from the old survey data

#Readin old survey data
old_survey<- box_search("Final Survey Themes Masked without Original Keys.csv") %>% box_read()

#Replace missing with NA
library(stringr)
#is.na(survey_key$ANON.IPN) <- survey_key$ANON.IPN==''
#is.na(survey_key$ANON.Contract) <- survey_key$ANON.Contract==''
#or
#blank$ANON.Contract[blank$ANON.Contract==""]<-NA
#blank$ANON.Contract[which(is.na(blank$ANON.Contract))]

#Map from old survey
survey_key$ANON.Contract <-
  ifelse(
    survey_key$ANON.Contract == "",
    old_survey$ANON.Contract[trimws(old_survey$ResponseId) == trimws(survey_key$ResponseId)],
    survey_key$ANON.Contract
  )

survey_key$ANON.IPN <-
  ifelse(
    survey_key$ANON.IPN == "",
    old_survey$ANON.IPN[trimws(old_survey$ResponseId) == trimws(survey_key$ResponseId)],
    survey_key$ANON.IPN
  )

# Check for duplicates
dups<-survey_key[duplicated(survey_key),]

#Check for missing values
blank<-survey_key[survey_key$ANON.Contract=="",]
#rm(blank)

# Unique values
unique.ipn<- survey_key[!str_count(survey_key$ANON.IPN,',')==0,]

# Control totals check
pins<-sapply(strsplit(survey_key$ANON.IPN,","),FUN=function(x){length(x[x!="Null"])})
sum(pins)
contracts<-sapply(strsplit(survey_key$ANON.Contract,","),FUN=function(x){length(x[x!="Null"])})
sum(contracts)
summary(unique(survey_key$ANON.Contract))
summary(unique(survey_key$ANON.IPN))


###########################################################################
####### Sales and policy data at IPN and Contract aggregation level #######
### Input files: sales_data and add_sales Output_file: sales_policy_data ##
###########################################################################

sales_data<- box_search("Anon Sales and Policy Data 2019-03-06.csv") %>% box_read()

#Check for null values in Product Line
sales_data_null<-sales_data[sales_data$Product.Line=="",]

#filter NA rows
sales_data_miss <- colSums(is.na(sales_data)|sales_data == '')
as.data.frame(sales_data_miss)

#Additional Sales data
add_sales<- box_search("Missing Policies 3.6.19.csv") %>% box_read()

#Replace NA
is.na(add_sales$ANON.IPN) <- add_sales$ANON.IPN==''

# Map values from survey key 
add_sales$ANON.IPN <-
  ifelse(
    is.na(add_sales$ANON.IPN)==TRUE,
    survey_key$ANON.IPN[trimws(add_sales$ANON.Contract) %in% trimws(survey_key$ANON.Contract)],
    add_sales$ANON.IPN
  )

#For time being
add_sales[7,30]<- "IPN0000766303"


### Combine the sales data with add sales
names(sales_data)
names(add_sales)

sales_policy_data<-rbind(sales_data,add_sales)

# Duplicates
dups<-sales_policy_data[duplicated(sales_policy_data),]
unique.pin<- sales_policy_data[!str_count(sales_policy_data$ANON.IPN,',')==0,]

###########################################################################
############## Survey data has email list and survey combined #############
##### Input files: Survey and email lists     Output_file:             ####
###########################################################################

##MOHIT CODE##
survey_data <- box_search("Final Survey Themes Masked without Original Keys 2_19_2019.csv")%>% box_read()
survey_data_old <- box_search("Final Survey Themes Masked without Original Keys.csv") %>% box_read()

#Finding missing values
survey_data_missing <- colSums(is.na(survey_data)|survey_data == '')
survey_data_missing_df <- as.data.frame(survey_data_missing)

#Finding against which response ids, records not available
survey_data_missing_which <- survey_data[survey_data$ANON.Contract=="",]

#Filter 11 rows from 50 variables table(Final Survey Themes.. 2_19_2019) having null email data
d1 <- filter(survey_data, is.na(survey_data$ANON.IPN)|survey_data$ANON.IPN == '')
View(d1)

#Select response id from d1
d2 <- d1 %>% dplyr::select(ResponseId)
View(d2)

#Filter 40 variable table(Final survey themes...) with response id in d2
d3 <-survey_data_old %>% filter(ResponseId %in% d2$ResponseId)
View(d3)

#Select ANON.IPN and ANON.Contract from d3

d4 <- d3 %>% dplyr::select(ANON.IPN, ANON.Contract)

#Filter 10 variable(old email data) table on anon.ipn and anon.contract columns
#to the ANON.IPN and ANON.Contract against the 11 response ids
d5 <- email_list_old %>% filter(ANON.IPN %in% d4$ANON.IPN & ANON.Contract %in% d4$ANON.Contract)
View(d5) 

#joining d3 and d5
d6 <- left_join(d3, d5, by =  "ANON.Contract")
dplyr::select(d6, ResponseId, ANON.Contract, ANON.IPN)
#removing duplicated columns 
d6 <- d6[,-c(42,45,51)]
View(d6)


#Changing names of the columns for uniformity
library(data.table)
setnames(d6, old=c("RP.x","IND.x","ANON.IPN.x"), new=c("RP", "IND", "ANON.IPN"))
View(d6)


#Adding 2 missing columns not present in the email and the old survey data
d7 <- d6 %>% mutate(Q_URL = '', Entity = '') 
View(d7)

#Removing observations for which email data is not given 
survey_data1 <- survey_data[-c(425,950,1080,1468,2314,3978,4912,5060,6302,6335,6829),]
Survey <- 
  
  #adding/merging the required obtained records from email data with the survey data
  survey_data_final <- rbind(survey_data1, d7)

#reconfirming if there are any records missing against the earlier founded 11 
survey_data_final_missing_which <- survey_data_final[survey_data_final$ANON.Contract=="",]


###########################################################################
############## Sales data combine with survey key #############
##### Input files: sales_policy_data and survey_key   Output_file:             ####
###########################################################################
###Dharahas Code##########


###list of unique ipins from response id table
Ipinlist <- unique(survey_key$ANON.IPN)

sales_sur_1 <-  sales_policy_data[sales_policy_data$ANON.IPN %in% Ipinlist, ]
##yes they do exist now separate them to single level

####aggregation of duplicates into 1 row
#aggregation function
agg_fun <- function(x)
{
  x = paste(unique(x), collapse = ",")
}


#groupby to unique level
#mutate by applying fun2 function
#this merges unique values and concatenates the unique values into string
#doing this for analysis purpose only one hot encoding can be done on this as
#well but for initial aggregation we can use this
#then after we have 1 data set we can split and put in different columns
# for better understanding of the code 
# add filter(n() == 8) %>% in between group by and 
#mutate function
repeatedipindata <-
  sales_sur_1 %>%
  group_by(ANON.Contract,ANON.IPN) %>%
  mutate_all(agg_fun)





#remove duplicates generated
sales_sur_merge_ready <- repeatedipindata[!duplicated(repeatedipindata), ]

#merge with survey key 
check2 <- merge(survey_key, sales_sur_merge_ready, by=c("ANON.IPN","ANON.Contract"))


length(unique(check2$ResponseId))
##6854 observations are found right now which are unique

#extract that one response id where that combination does not exist
lol <- survey_key %>% filter(!(ResponseId %in% check2$ResponseId))
##findout from kelly :O


##to aggregate the above file "check2" to level of response Id's what we can do is
##just use the code for repeatedipin data but 
##group by responseid and then do the mutate all with same function
##remove dulicates just like in "sales_sur_merge_ready"

###########################################################################
############## combine sales data and survey data #########################
######## Input files: Survey and email lists     Output_file:  ############
###########################################################################

#Anusha code
View(check2)


# convert to factors ------------------------------------------------------

merge_ready_copy <- check2
merge_ready_copy$Coverage.Status...ACORD.Name <- as.factor(merge_ready_copy$Coverage.Status...ACORD.Name)
merge_ready_copy$Product.Line <- as.factor(merge_ready_copy$Product.Line)
merge_ready_copy$Product.Type <- as.factor(merge_ready_copy$Product.Type)
merge_ready_copy$Bill.Frequency <- as.factor(merge_ready_copy$Bill.Frequency)
merge_ready_copy$Billing.Mode <- as.factor(merge_ready_copy$Billing.Mode)
merge_ready_copy$Policy.Client.Role.Type <- as.factor(merge_ready_copy$Policy.Client.Role.Type)
merge_ready_copy$Owner.Age.Bin <- as.factor(merge_ready_copy$Owner.Age.Bin)
merge_ready_copy$Owner.Gender <- as.factor(merge_ready_copy$Owner.Gender)
merge_ready_copy$Agent.Type <- as.factor(merge_ready_copy$Agent.Type)
merge_ready_copy$Agent.Tenure.Bin <- as.factor(merge_ready_copy$Agent.Tenure.Bin)
merge_ready_copy$Agent.Change <- as.factor(merge_ready_copy$Agent.Change)
merge_ready_copy$Agent.Age.Bin <- as.factor(merge_ready_copy$Agent.Age.Bin)
merge_ready_copy$Agent.Gender <- as.factor(merge_ready_copy$Agent.Gender)
merge_ready_copy$Owner.Gender <- as.factor(merge_ready_copy$Owner.Gender)
merge_ready_copy$Policy.Tenure.Bin <- as.factor(merge_ready_copy$Policy.Tenure.Bin)
merge_ready_copy$Insured.Gender <- as.factor(merge_ready_copy$Insured.Gender)
merge_ready_copy$Insured.Underwriting.Tobacco.Code <- as.factor(merge_ready_copy$Insured.Underwriting.Tobacco.Code)
merge_ready_copy$Insured.Issue.Age.Bin <- as.factor(merge_ready_copy$Insured.Issue.Age.Bin)
merge_ready_copy$Same.OI <- as.factor(merge_ready_copy$Same.OI)
merge_ready_copy$Client.Country <- as.factor(merge_ready_copy$Client.Country)
merge_ready_copy$Annualized.Premium <- as.double(merge_ready_copy$Annualized.Premium)
merge_ready_copy$Coverage.Face.Amount <- as.double(merge_ready_copy$Coverage.Face.Amount)

# Convert to factors survey data ------------------------------------------


survey_data_copy <- survey_data_final
survey_data_copy$Progress <- as.factor(survey_data_copy$Progress)
survey_data_copy$Finished <- as.factor(survey_data_copy$Finished)
survey_data_copy$Q1_NPS_GROUP <- as.factor(survey_data_copy$Q1_NPS_GROUP)
survey_data_copy$Q7 <- as.factor(survey_data_copy$Q7)
survey_data_copy$RP <- as.factor(survey_data_copy$RP)
survey_data_copy$Entity <- as.factor(survey_data_copy$Entity)

survey_data_copy$Q1 <- ordered(survey_data_copy$Q1, levels = 0:10)
survey_data_copy$Q4_1 <- ordered(survey_data_copy$Q4_1, levels = 1:7)
survey_data_copy$Q10_1 <- ordered(survey_data_copy$Q10_1, levels = 1:7)
survey_data_copy$Q6_1 <- ordered(survey_data_copy$Q6_1, levels = 1:7)

#one hot encoding
###################################################################################
#dmy <- dummyVars( "~ Q1", data = survey_data_copy)
#trnsf <- data.frame(predict(dmy, newdata = survey_data_copy))
#trnsf
merge_survey_sales <- inner_join(survey_data_copy, merge_ready_copy,
                                 by ="ResponseId")
View(merge_survey_sales)
nrow(merge_survey_sales)

View(sales_data_miss)


merge_survey_sales %>%
  group_by(ResponseId)


# Plots -------------------------------------------------------------------
filter(survey_data_copy, Q1_NPS_GROUP != "NA" & Q1_NPS_GROUP != "") %>%
  ggplot(aes(Q1_NPS_GROUP)) + 
  geom_bar(fill = "steelblue", width = 0.2) +
  xlab("NPS Groups")

filter(survey_data_copy, Q1 >=0) %>%
  ggplot(aes(Q1)) + 
  geom_bar(fill = "steelblue") +
  facet_grid(. ~ na.omit(Q1_NPS_GROUP))+
  xlab("NPS Score")

filter(merge_survey_sales, Owner.Age.Bin != "NA" & Q1 != "NA") %>%
  dplyr::select(Owner.Age.Bin, Q1_NPS_GROUP,ResponseId) %>%
  group_by(ResponseId) %>% 
  unique() %>%
  ggplot(aes(Owner.Age.Bin, fill = Q1_NPS_GROUP)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("tomato2", "slategray3", "steelblue"))+
  labs(x = "Owner Age Bin",
       fill = "NPS Group") 

filter(merge_survey_sales, Agent.Gender != "NA"& Agent.Gender != "" &
         Q1_NPS_GROUP != "NA" & Q1_NPS_GROUP != "") %>%
  dplyr::select(Agent.Gender, Q1_NPS_GROUP, ResponseId) %>%
  group_by(ResponseId) %>%
  unique() %>%
  ggplot(aes(Agent.Gender)) +
  geom_density(color = "Darkseagreen")


filter(merge_survey_sales, Agent.Gender != "NA"& Agent.Gender != "" &
         Q1_NPS_GROUP != "NA" & Q1_NPS_GROUP != "") %>%
  dplyr::select(Agent.Gender, Q1_NPS_GROUP, ResponseId) %>%
  group_by(ResponseId) %>%
  unique() %>%
  ggplot(aes(Q1_NPS_GROUP, fill = Agent.Gender)) +
  geom_bar(width = 0.5)+
  scale_fill_manual(values = c("tomato2", "slategray3", "steelblue", "grey")) +
  xlab("Q1 NPS GROUP")

filter(merge_survey_sales, Agent.Gender != "NA"& Agent.Gender != "" &
         Q1_NPS_GROUP != "NA" & Q1_NPS_GROUP != "") %>%
  dplyr::select(Agent.Gender, Q1_NPS_GROUP, ResponseId) %>%
  
  corr_data <- dplyr::select(merge_survey_sales,Q1_NPS_GROUP, 
                             Annualized.Premium,Life.Policies, DI.Policies, AN.Policies, Coverage.Face.Amount)
ggcorr(unique(corr_data))
ggpairs(corr_data)

filter(merge_survey_sales, Q1_NPS_GROUP == "Detractor") %>%
  group_by(ResponseId)%>%
  dplyr::select( ResponseId,Main.Agency.State )%>%
  unique() %>%
  ggplot(aes(Main.Agency.State)) +
  geom_bar(fill = "steelblue") +
  coord_flip()

test <- filter(merge_survey_sales) %>%
  group_by(ResponseId, Main.Agency.State)%>%
  dplyr::select(Main.Agency.State,Q1_NPS_GROUP)%>%
  unique() %>%
  ggplot(aes(Main.Agency.State, fill = Q1_NPS_GROUP)) +
  geom_bar() +
  coord_flip()
test$ResponseId <- NULL
group_by(test, Main.Agency.State) %>% sort()

filter(merge_survey_sales, Life.Policies > 0) %>%
  group_by(ResponseId)%>%
  dplyr::select( ResponseId,Coverage.Face.Amount, Main.Agency.State)%>%
  unique() %>%
  ggplot(aes(Main.Agency.State, Coverage.Face.Amount)) +
  geom_boxplot(fill = "steelblue")
