install.packages('boxr')
library(boxr)
library(dplyr)
library(memisc)
library(stringr)
library(tidyr)
library(gtools)
library(data.table)
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
rm(survey_key)

# Initial Control Totals
summary(unique(survey_key$ANON.Contract))
summary(unique(survey_key$ANON.IPN))

# Missing values
blank<-survey_key[survey_key$ANON.IPN=="",]
blank<-survey_key[survey_key$ANON.Contract=="",]
rm(blank)

#There are 3 missing values which needs to be mapped from the old survey data

#Readin old survey data
old_survey<- box_search("Final Survey Themes Masked without Original Keys.csv") %>% box_read()

#Replace missing with NA
library(stringr)
is.na(survey_key$ANON.IPN) <- survey_key$ANON.IPN==''
is.na(survey_key$ANON.Contract) <- survey_key$ANON.Contract==''
#or
blank$ANON.Contract[blank$ANON.Contract==""]<-NA
blank$ANON.Contract[which(is.na(blank$ANON.Contract))]

#Map from old survey
survey_key$ANON.Contract <-
  ifelse(
    survey_key$ANON.Contract == "",
    old_survey$ANON.Contract[trim(old_survey$ResponseId) == trim(survey_key$ResponseId)],
    survey_key$ANON.Contract
  )

survey_key$ANON.IPN <-
  ifelse(
    survey_key$ANON.IPN == "",
    old_survey$ANON.IPN[trim(old_survey$ResponseId) == trim(survey_key$ResponseId)],
    survey_key$ANON.IPN
  )

# Check for duplicates
dups<-survey_key[duplicated(survey_key),]

#Check for missing values
blank<-survey_key[survey_key$ANON.Contract=="",]
rm(blank)

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


survey<- box_search("Final Survey Themes Masked without Original Keys 2_19_2019.csv") %>% box_read()

pins<-sapply(strsplit(survey$ANON.Contract,","),FUN=function(x){length(x[x!="Null"])})
sum(pins)
x2<-separate_rows(x1,ANON.IPN, sep = ",", convert = FALSE)
sum(ipins)
colnames(survey)
survey.unique.pin<- survey[!str_count(survey$ANON.IPN,',')==0,]
blank_sr_data<-survey[survey$ANON.Contract == "",]

########## Email list #############

email_List <- box_search("Final Email List Anonymized 2_19_2019.csv") %>% box_read()

sum(email_List$AN.Policies)

x<- email_List[!str_count(email_List$ANON.IPN,',')==0,]

x<- subset(x, select = -c(RP_COUNT_POLICY,RP_COUNT_PLAN,Life.Policies,AN.Policies,DI.Policies,IND_COUNT_POLICY,IND_COUNT_IPN))
x.contract.list<-sapply(strsplit(x$ANON.Contract,","),FUN=function(x){x[x!="Null"]})
x1<-separate_rows(x,ANON.Contract, sep = ",", convert = FALSE)
x2<-separate_rows(x1,ANON.IPN, sep = ",", convert = FALSE)
x2[duplicated(x2)]

y<- email_List[str_count(email_List$ANON.IPN,',')==0,]

email_List_Pin <- email_List$ANON.Contract
pins<-sapply(strsplit(email_List_Pin,","),FUN=function(x){length(x[x!="Null"])})
sum(pins)


email_List_iPin <- email_List$ANON.IPN
ipins<-sapply(strsplit(email_List_iPin,","),FUN=function(x){length(x[x!="Null"])})
sum(ipins)
x<-email_List[email_List$ANON.Contract]

###########################################################################
############## Sales data combine with survey key #############
##### Input files: Survey and email lists     Output_file:             ####
###########################################################################








###########################################################################
############## combine sales data and survey data #########################
######## Input files: Survey and email lists     Output_file:  ############
###########################################################################

summary(survey)

str(sales_data)
summary(survey$ResponseId)
table(survey$Q1_NPS_GROUP,survey$Q1)
(as.Date(Email_List$LAST_LOGIN_DATE))
str(Email_List)
attach(Email_List)
rowSums(is.na())
table(sales_data$Product.Type)
41012
sum(Email_List$IND_COUNT_POLICY,na.rm=T)
table(Email_List$RP)
unique(Email_List$IND)
table(Email_List$IND)
table(sum(Life.Policies),sum(DI.Policies),sum(AN.Policies))
table(((sales_data$Client.Zip.Code.5digit)))
sum(email_List$IND_COUNT_POLICY=="")
unique(survey$IND)
table(survey$IND)

table(Q3_Relationship,
Q3_Tech,
Q3_Product,
Q3_Service,
Q3_Ethics,
Q3_Communication,
Q3_Ease,
Q3_Company,
Q3_HO_Service,
Q3_Performance_Fee)

attach(sales_data)
table((Product.Line))
table(sales_data$Policy.Client.Role.Type)
