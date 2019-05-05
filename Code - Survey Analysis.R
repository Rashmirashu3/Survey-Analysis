#install.packages('boxr')
library(boxr)
library(dplyr)
library(plyr)
library(memisc)
library(stringr)
library(tidyr)
library(gtools)
library(data.table)
library(caret)
library(randomForest)
library(GGally)
library(corrplot)
library(data.table)
#install below packages
library(qdap)
library(mltools)
library(fastDummies)
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
### Input files: Survey and email lists  Output_file: survey_data_final ###
###########################################################################

##MOHIT CODE##
survey_data <- box_search("Final Survey Themes Masked without Original Keys 2_19_2019.csv")%>% box_read()
survey_data_old <- box_search("Final Survey Themes Masked without Original Keys.csv") %>% box_read()
##25/03 code######
email_list_old <- box_search("Final Email List Anonymized.csv") %>% box_read()
##################
#Finding missing values
survey_data_missing <- colSums(is.na(survey_data)|survey_data == '')
survey_data_missing_df <- as.data.frame(survey_data_missing)

#Finding against which response ids, records not available
survey_data_missing_which <- survey_data[survey_data$ANON.Contract=="",]

#Filter 11 rows from 50 variables table(Final Survey Themes.. 2_19_2019) having null email data
d1 <- filter(survey_data, is.na(survey_data$ANON.IPN)|survey_data$ANON.IPN == '')

#Select response id from d1
d2 <- d1 %>% dplyr::select(ResponseId)

#Filter 40 variable table(Final survey themes...) with response id in d2
d3 <-survey_data_old %>% filter(ResponseId %in% d2$ResponseId)

#Select ANON.IPN and ANON.Contract from d3
d4 <- d3 %>% dplyr::select(ANON.IPN, ANON.Contract)

#Filter 10 variable(old email data) table on anon.ipn and anon.contract columns
#to the ANON.IPN and ANON.Contract against the 11 response ids
d5 <- email_list_old %>% filter(ANON.IPN %in% d4$ANON.IPN & ANON.Contract %in% d4$ANON.Contract)

#joining d3 and d5
d6 <- left_join(d3, d5, by =  "ANON.Contract")

#removing duplicated columns 
d6 <- d6[,!names(d6) %in% c("RP.y","IND.y","ANON.IPN.y")]

#Changing names of the columns for uniformity
setnames(d6, old=c("RP.x","IND.x","ANON.IPN.x"), new=c("RP", "IND", "ANON.IPN"))

#Adding 2 missing columns not present in the email and the old survey data
d7 <- d6 %>% mutate(Q_URL = '', Entity = '') 

#Removing observations for which email data is not given 
survey_data1<-survey_data[!survey_data$ANON.IPN=="",]

#adding/merging the required obtained records from email data with the survey data
survey_data_final <- rbind(survey_data1, d7)

#reconfirming if there are any records missing against the earlier founded 11 
survey_data_final_missing_which <- survey_data_final[survey_data_final$ANON.Contract=="",]


###########################################################################
############## Sales data combine with survey key #############
##### Input files: sales_policy_data and survey_key   Output_file:####
###########################################################################
###Dharahas Code##########

##merge add home agency
home_agency<- box_search("Home Agencies by Agent.csv") %>% box_read()
#read new data 
sales_policy_data <- sales_policy_data%>%merge(home_agency,by = "ANON.AgentID")
length(setdiff(sales_data_home$ANON.AgentID,sales_policy_data$ANON.AgentID))
unique(sales_policy_data$Product.Line)


##code on 4/7/2019# changing amount columns by splitting to di an and life
#life policy amount coulumns####
##to be changed later to a better code format###

sales_policy_data$life.Annualized.Premium <-
  ifelse(
    !is.na(sales_policy_data$Annualized.Premium) &
      sales_policy_data$Product.Line == "IND LIFE",
    sales_policy_data$Annualized.Premium,
    0
  )

sales_policy_data$life.Net.Cash.Value <-
  ifelse(
    !is.na(sales_policy_data$Net.Cash.Value) &
      sales_policy_data$Product.Line == "IND LIFE",
    sales_policy_data$Net.Cash.Value,
    0
  )

sales_policy_data$life.Coverage.Face.Amount <-
  ifelse(
    !is.na(sales_policy_data$Coverage.Face.Amount) &
      sales_policy_data$Product.Line == "IND LIFE",
    sales_policy_data$Coverage.Face.Amount,
    0
  )

sales_policy_data$life.Account.Value <-
  ifelse(
    !is.na(sales_policy_data$Account.Value) &
      sales_policy_data$Product.Line == "IND LIFE",
    sales_policy_data$Account.Value,
    0
  )

##############################################################
#di#########
sales_policy_data$di.Annualized.Premium <-
  ifelse(
    !is.na(sales_policy_data$Annualized.Premium) &
      sales_policy_data$Product.Line == "IND DI",
    sales_policy_data$Annualized.Premium,
    0
  )

sales_policy_data$di.Net.Cash.Value <-
  ifelse(
    !is.na(sales_policy_data$Net.Cash.Value) &
      sales_policy_data$Product.Line == "IND DI",
    sales_policy_data$Net.Cash.Value,
    0
  )

sales_policy_data$di.Coverage.Face.Amount <-
  ifelse(
    !is.na(sales_policy_data$Coverage.Face.Amount) &
      sales_policy_data$Product.Line == "IND DI",
    sales_policy_data$Coverage.Face.Amount,
    0
  )

sales_policy_data$di.Account.Value <-
  ifelse(
    !is.na(sales_policy_data$Account.Value) &
      sales_policy_data$Product.Line == "IND DI",
    sales_policy_data$Account.Value,
    0
  )

###############################################
######Annuity#############################
sales_policy_data$ann.Annualized.Premium <-
  ifelse(
    !is.na(sales_policy_data$Annualized.Premium) &
      sales_policy_data$Product.Line == "IND ANNUITY",
    sales_policy_data$Annualized.Premium,
    0
  )

sales_policy_data$ann.Net.Cash.Value <-
  ifelse(
    !is.na(sales_policy_data$Net.Cash.Value) &
      sales_policy_data$Product.Line == "IND ANNUITY",
    sales_policy_data$Net.Cash.Value,
    0
  )

sales_policy_data$ann.Coverage.Face.Amount <-
  ifelse(
    !is.na(sales_policy_data$Coverage.Face.Amount) &
      sales_policy_data$Product.Line == "IND ANNUITY",
    sales_policy_data$Coverage.Face.Amount,
    0
  )

sales_policy_data$ann.Account.Value <-
  ifelse(
    !is.na(sales_policy_data$Account.Value) &
      sales_policy_data$Product.Line == "IND ANNUITY",
    sales_policy_data$Account.Value,
    0
  )

################################3
###########rp###############
sales_policy_data$rp.Annualized.Premium <-
  ifelse(
    !is.na(sales_policy_data$Annualized.Premium) &
      sales_policy_data$Product.Line == "RETIREMENT PLANS",
    sales_policy_data$Annualized.Premium,
    0
  )

sales_policy_data$rp.Net.Cash.Value <-
  ifelse(
    !is.na(sales_policy_data$Net.Cash.Value) &
      sales_policy_data$Product.Line == "RETIREMENT PLANS",
    sales_policy_data$Net.Cash.Value,
    0
  )


sales_policy_data$rp.Coverage.Face.Amount <-
  ifelse(
    !is.na(sales_policy_data$Coverage.Face.Amount) &
      sales_policy_data$Product.Line == "RETIREMENT PLANS",
    sales_policy_data$Coverage.Face.Amount,
    0
  )

sales_policy_data$rp.Account.Value <-
  ifelse(
    !is.na(sales_policy_data$Account.Value) &
      sales_policy_data$Product.Line == "RETIREMENT PLANS",
    sales_policy_data$Account.Value,
    0
  )

##################################################################
###Modify zipcode to create regions 0-9 region details is in url:
####https://www.unitedstateszipcodes.org/images/zip-codes/zip-codes.png
#######################################################################

sales_policy_data$Client.zip.region <-
  ifelse(
    !is.na(sales_policy_data$Client.Zip.Code.5digit) &
      nchar(sales_policy_data$Client.Zip.Code.5digit) >= 5,
    substring(sales_policy_data$Client.Zip.Code.5digit, 1, 1),
    0
  )


sales_policy_data$Client.zip.region <-
  ifelse(
    !is.na(sales_policy_data$Client.Zip.Code.5digit),
    sales_policy_data$Client.zip.region,
    NA
  )
unique(sales_policy_data$Client.zip.region)

sales_policy_data$Coverage.Status.code <-
  ifelse(
    !is.na(sales_policy_data$Coverage.Status...ACORD.Name) &
      sales_policy_data$Coverage.Status...ACORD.Name == "INFORCE",
    1,
    0
  )


################################AgeBins from 5 to 10 buckets#############################

binning_function <- function(x)
{
  x <- gsub(".*[,]([^]]+)[]].*", "\\1",x)
  x <- as.numeric(x)
  x <- as.character(cut(x, breaks = seq(10, 200, by = 10)))
}

age_bin_list <- c()
test_bin <- sales_policy_data

sales_policy_data$Owner.Age.Bin <- binning_function(sales_policy_data$Owner.Age.Bin)
sales_policy_data$Agent.Age.Bin <- binning_function(sales_policy_data$Agent.Age.Bin)
sales_policy_data$Agent.Tenure.Bin <- binning_function(sales_policy_data$Agent.Tenure.Bin)
sales_policy_data$Policy.Tenure.Bin <- binning_function(sales_policy_data$Policy.Tenure.Bin)


##################################################################
dim(sales_policy_data)

##drop the amount and zip code columns as we have already grouped them
sales_policy_data <-
  subset(
    sales_policy_data,
    select = -c(
      Account.Value,
      Coverage.Face.Amount,
      Net.Cash.Value,
      Annualized.Premium,
      Client.Zip.Code.5digit
    )
  )

dim(sales_policy_data)



###list of unique ipins from response id table
Ipinlist <- unique(survey_key$ANON.IPN)


###filter data wrtipinlist
sales_sur_1 <-  sales_policy_data[sales_policy_data$ANON.IPN %in% Ipinlist, ]

###############################################################################################
##one hot encode preprocessing##################

###convert blank strings to NA

sales_sur_1 <-
  sales_sur_1 %>% mutate_if(is.character, funs(na_if(., "")))

str(sales_policy_data)
######################################
#billing mode####
#get top  billing modes with summarise and then change rest into other
x<- sales_sur_1 %>% dplyr::group_by(Billing.Mode) %>% dplyr::summarise(count = n()) %>% dplyr::arrange()
#we see summarise gives that direct bill eft and no bill have highest so lets change all other categories into other
top_mode_list <- c("DIRECT BILL","EFT","NO BILL","LIST BILL")
sales_sur_1$Billing.Mode <- ifelse(sales_sur_1$Billing.Mode%in%top_mode_list, sales_sur_1$Billing.Mode,"OTHER")
##we have only 5 categories now########
unique(sales_sur_1$Billing.Mode)

#agent type grouping########
#get top  billing modes with summarise and then change rest into other
summary_ag_type <- as.data.frame(sales_sur_1 %>% dplyr::group_by(Agent.Type) %>% dplyr::summarise(count = n()) %>% dplyr::arrange(desc(count)))
#we see summarise gives that direct bill eft and no bill have highest so lets change all other categories into other
rownames(summary_ag_type) <- NULL
top_ag_list <- unlist(summary_ag_type[1:9,1])
sales_sur_1$Agent.Type <- ifelse(sales_sur_1$Agent.Type%in%top_ag_list, sales_sur_1$Agent.Type,"OTHER")
##we have 10 categories now########
#confirm number with jennifer###


columns_list <- c(
  "Main.Agency.Distribution.Region",
  "Agent.Type",
  "Billing.Mode",
  "Bill.Frequency",
  "Product.Line",
  "Product.Type",
  "Insured.Underwriting.Tobacco.Code"
)


#####Perform One Hot Encoding Here#######
sales_sur_2 <-
  dummy_cols(
    sales_sur_1,
    select_columns = columns_list,
    remove_first_dummy = TRUE
  )

str(sales_sur_2)

dim(sales_sur_2)
###########################################################################3

#groupby to unique level
#mutate by applying fun2 function
#this merges unique values and concatenates the unique values into string
#doing this for analysis purpose only one hot encoding can be done on this as
#well but for initial aggregation we can use this
#then after we have 1 data set we can split and put in different columns
# for better understanding of the code 
# add filter(n() == 8) %>% in between group by and 
#mutate function

####aggregation of duplicates into 1 row
#aggregation function
agg_fun <- function(x)
{
  x = paste(sort(unique(x)), collapse = ",")
}

agg_fun2 <- function(x)
{
  x = sum(unique(x))
}


agg_fun3 <- function(x)
{
  x = max(unique(x))
}




sales_sur_2 <- sales_sur_2 %>% mutate_if(is.character,as.factor)
sales_sur_2 <- sales_sur_2 %>% mutate_if(is.factor,as.character)
unique(sales_sur_2$Agent.Tenure.Bin)

repeatedipindata <-
  sales_sur_2 %>%
  group_by(ANON.Contract,ANON.IPN) %>%
  mutate_if(is.character,agg_fun)
x <- paste(sort(unique(sales_sur_2$Coverage.Status...ACORD.Name)), collapse = ",")

repeatedipindata <-
  repeatedipindata %>%
  group_by(ANON.Contract,ANON.IPN) %>%
  mutate_if(is.numeric,agg_fun2)

repeatedipindata <-
  repeatedipindata %>%
  group_by(ANON.Contract,ANON.IPN) %>%
  mutate_if(is.integer,agg_fun3)



#remove duplicates generated
sales_sur_merge_ready <- repeatedipindata[!duplicated(repeatedipindata), ]

#merge with survey key 
check2 <- merge(survey_key, sales_sur_merge_ready, by=c("ANON.IPN","ANON.Contract"))

str(survey_key)
length(unique(check2$ResponseId))
##6854 observations are found right now which are unique

#extract that one response id where that combination does not exist
lol <- survey_key %>% filter(!(ResponseId %in% check2$ResponseId))
##findout from kelly

######################new code 25/03##########################

check2 <- check2 %>% mutate_if(is.character, funs(gsub(",NA","",.)))
check2 <- check2 %>% mutate_if(is.character, funs(gsub("NA,","",.)))




#convert character "NAs" into actual N/a
check2 <-
  check2 %>% mutate_if(is.character, funs(na_if(., "NA")))

check2 <-
  check2 %>% mutate_if(is.character, funs(na_if(., "")))


#remove amount columns and same.OI which is unnecessary
survey_key_no_amounts <-
  check2[,!names(check2) %in% c("Account.Value",
                                "Annualized.Premium",
                                "Net.Cash.Value",
                                "Coverage.Face.Amount",
                                "Same.OI")]


#group by and aggregating to response id level
unique_level_Rid_dup <- 
  survey_key_no_amounts %>%
  group_by(ResponseId) %>%
  mutate_if(is.character,agg_fun)


unique_level_Rid_dup <-
  unique_level_Rid_dup %>%
  group_by(ResponseId) %>%
  mutate_if(is.numeric,agg_fun2)


unique_level_Rid_dup <-
  unique_level_Rid_dup %>%
  group_by(ResponseId) %>%
  mutate_if(is.integer,agg_fun3)



#remove dups
unique_level_Rid <- unique_level_Rid_dup[!duplicated(unique_level_Rid_dup), ]
#remove same repeated columns from sales final data
survey_data_final <- survey_data_final[,!names(survey_data_final) %in% c("ANON.IPN","ANON.Contract")]

unique_level_Rid <- unique_level_Rid %>% mutate_if(is.integer,as.character)


###########################################################################
############## combine sales data and survey data(Dharahas Code) ##########
######## Input files: Survey and email list   Output_file: Final_data #####
###########################################################################

#merge the data
final_merge_data_set <- merge(unique_level_Rid,survey_data_final,by = "ResponseId")

#change NA's appended into blank string ##hapens for zipcode of client alone as it has NA value
final_merge_data_set <- final_merge_data_set %>% mutate_if(is.character, funs(gsub(",NA","",.)))
final_merge_data_set <- final_merge_data_set %>% mutate_if(is.character, funs(gsub("NA,","",.)))

Final_data<-final_merge_data_set


View(Final_data)
str(Final_data)
dim(Final_data)

#change policy client role type to owner,payor,owner payor
for ( i in 1:nrow(Final_data))
{
  row <- Final_data[i,]
  if(grepl("owner",tolower(row$Policy.Client.Role.Type)))
  {
    if(grepl("payor",tolower(row$Policy.Client.Role.Type)))
    {
      Final_data[i,]$Policy.Client.Role.Type = "Payor Owner"
    }
    else
    {
      Final_data[i,]$Policy.Client.Role.Type = "Owner"
    }
  }
  else if(grepl("payor",tolower(row$Policy.Client.Role.Type)))
  {
    Final_data[i,]$Policy.Client.Role.Type = "Payor"
  }
  else
  {
    Final_data[i,]$Policy.Client.Role.Type = "Other"
  }
  Final_data[i,]$Owner.Age.Bin <- Final_data[i,]$Owner.Age.Bin %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% max()
  Final_data[i,]$Agent.Age.Bin <- Final_data[i,]$Agent.Age.Bin %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% max()
  Final_data[i,]$Agent.Tenure.Bin  <- Final_data[i,]$Agent.Tenure.Bin %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% max()
  Final_data[i,]$Policy.Tenure.Bin <- Final_data[i,]$Policy.Tenure.Bin %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% max()
  
  Final_data[i,]$Home.Agency.State <- gsub( ",.*$", "", Final_data[i,]$Home.Agency.State )
  }
Final_data$Owner.Age.Bin <- binning_function(Final_data$Owner.Age.Bin)
Final_data$Agent.Age.Bin <- binning_function(Final_data$Agent.Age.Bin)
Final_data$Agent.Tenure.Bin <- binning_function(Final_data$Agent.Tenure.Bin)
Final_data$Policy.Tenure.Bin <- binning_function(Final_data$Policy.Tenure.Bin)



##drop the columns we encoded
Final_data$Main.Agency.State <-substring(Final_data$Main.Agency.State, 1, 2)
Final_data$Home.Agency.State <-substring(Final_data$Home.Agency.State, 1, 2)
Final_data$Client.zip.region <-substring(Final_data$Client.zip.region, 1, 1)

table(Final_data$Home.Agency.State)


Final_data1<-  subset(
    Final_data,
    select = -c(
      Main.Agency.Distribution.Region,
      Main.Agency.State,
      Agent.Type,
      Billing.Mode,
      Bill.Frequency,
      Product.Line,
      Product.Type,
      Insured.Underwriting.Tobacco.Code,
      Coverage.Status...ACORD.Name,
      StartDate,
      EndDate,
      Progress,
      Finished,
      RecordedDate,
      ResponseId,
      DistributionChannel,
      UserLanguage,
      Q_URL,
      LAST_LOGIN_DATE,
      RP_COUNT_POLICY,
      Insured.Gender,
      Insured.Issue.Age.Bin,
      IND_COUNT_POLICY,
      IND_COUNT_IPN,
      Client.Country
    )
  )

table(Final_data1$Main.Agency.State)
table(Final_data1$Home.Agency.State)

dim(Final_data1)
names(Final_data1)

table(Final_data1$Policy.Client.Role.Type)



#######  End of Data Wrangling ###########

#######################################################################################################################################
##############################################       Data Analysis       ##############################################################
#######################################################################################################################################

#Please include any analysis part below in the same format

#######################################################################################################################################
############################################# 1. Analysis - Key drivers  (Rashmi code) #################################################
#######################################################################################################################################

test<-survey_data_final %>% dplyr::select(Q1_NPS_GROUP,Q3_Relationship,Q3_Tech,Q3_Product,Q3_Service,Q3_Ethics,Q3_Communication,Q3_Ease,Q3_Company,Q3_HO_Service,Q3_Performance_Fee,Q7_Service,Q7_Product,Q7_Price,Q7_Communication,Q7_Resources,Q7_Administration,Q7_Relationship,Q7_Technology) 

TEST2<- test%>% mutate_all(function(x) ifelse(x%in%"X", 1,x))

TEST3<- TEST2 %>% mutate_at(vars(-Q1_NPS_GROUP), as.numeric) %>% mutate_all(function(x) ifelse(x%in%NA, 0,x))

#subset into promoters

prom_test<-TEST3 %>% filter(Q1_NPS_GROUP=="Promoter")

prom<-colSums(prom_test[,!colnames(prom_test)%in% c("Q1_NPS_GROUP")])

prom<-as.data.frame(prom)

#subset into neutrals

neu_test<-TEST3 %>% filter(Q1_NPS_GROUP=="Passive")

neu<-colSums(neu_test[,!colnames(neu_test)%in% c("Q1_NPS_GROUP")])
neu<-as.data.frame(neu)

#subset into detractors

det_test<-TEST3 %>% filter(Q1_NPS_GROUP=="Detractor")

det<-colSums(det_test[,!colnames(det_test)%in% c("Q1_NPS_GROUP")])
det<-as.data.frame(det)

fin_drivers<-cbind(prom,det,neu)

#######################################################################################################################################
############################################# 2. Random forest - Variable Importance on Survey data (Rashmi code) ###################################
#######################################################################################################################################

require(randomForest)

#Columns excluded for analysis
list_col<-c("ResponseId",	"ANON.IPN",	"ANON.Contract",	"ANON.Agency",	"ANON.AgentID", "Q1","Duration (in seconds)","Q4_1","Q10_1","Q6_1","Q7","Q8","Q3_Masked","Q8 - Topics","Client.Country","Q3_Relationship","Q3_Tech","Q3_Product","Q3_Service","Q3_Ethics","Q3_Communication","Q3_Ease","Q3_Company","Q3_HO_Service","Q3_Performance_Fee", "Q7_Service","Q7_Product","Q7_Price","Q7_Communication","Q7_Resources","Q7_Administration","Q7_Relationship","Q7_Technology")

#Check data types for selected columns
str(Final_data1[, !(names(Final_data1) %in% list_col)])

## Note: Check if your dependent var has missing values or nulls
#Remove missing rows of dependent column
Final_data1<-Final_data1[!Final_data1$Q1_NPS_GROUP=="",]

#Final data needed for random forest
rfdata<-Final_data1[,!(names(Final_data1) %in% list_col)]

## Note: Below step is data manipulation and needs to be done as per the data requirement
# Replace X in certain columns with 1's and replace NA's with 0

#LISTT<-c("Q3_Relationship","Q3_Tech","Q3_Product","Q3_Service","Q3_Ethics","Q3_Communication","Q3_Ease","Q3_Company","Q3_HO_Service","Q3_Performance_Fee", "Q7_Service","Q7_Product","Q7_Price","Q7_Communication","Q7_Resources","Q7_Administration","Q7_Relationship","Q7_Technology")
#rfdata[,names(rfdata) %in% LISTT]<- rfdata[,names(rfdata) %in% LISTT] %>% dplyr::mutate_all(function(x)ifelse(x=="X",1,0))

rfdata2<-rfdata %>% mutate_all(function(x) ifelse(x%in%NA, 0,x))
rfdata2$RP_COUNT_PLAN<-ifelse(rfdata2$RP=="N",0,rfdata2$RP_COUNT_PLAN)

table(rfdata2$RP_COUNT_PLAN,rfdata2$RP)
unique(rfdata2$RP_COUNT_PLAN)

##Here everything needs to be either INT or Factor 

#Convert character columns to factors
rfdata2$Entity<-ifelse(rfdata2$Entity=="","FALSE",rfdata2$Entity)
table(rfdata2$Entity)

library(dplyr)
rfdata2=rfdata2 %>% mutate_if(is.character, as.factor)

str(rfdata2)
dim(rfdata2)

names(rfdata2) <- make.names(names(rfdata2), unique=TRUE)
sum(is.na(rfdata2))


#Fitting Random forest
##Note: Please remove any fields that are overly dependent sometimes it might not help 
survey.rf=randomForest(Q1_NPS_GROUP ~ . , data = rfdata2[,], na.action=na.omit)
survey.rf

#Tally rf result with the actuals
table(rfdata2$Q1_NPS_GROUP)

#For Classification problem - errors in each class and Black line- OOB error
plot(survey.rf)

#Plot for variable importance
library(caret)
varImpPlot(survey.rf,sort = T, n.var=10, main="Top 10 - Variable Importance")

#Variable Importance
var.imp = data.frame(importance(survey.rf,type=2))

####Now, detractors vs others


rfdata2$Q1_NPS_MODIFIED<-ifelse(rfdata2$Q1_NPS_GROUP=="Detractor","1","0")
str(rfdata2)
rfdata2=rfdata2 %>% dplyr::mutate_if(is.character, as.factor)


set.seed(123456)
## 75% of the sample size

smp_size <- floor(0.75 * nrow(rfdata2))

train_ind <- sample(seq_len(nrow(rfdata2)), size = smp_size)

train <- rfdata2[train_ind, ]
test <- rfdata2[-train_ind, ]


#Fitting Random forest
##Note: Please remove any fields that are overly dependent sometimes it might not help 
survey.rf2=randomForest(Q1_NPS_MODIFIED ~ . , data = train[,!names(train) %in% c("Q1_NPS_GROUP")], na.action=na.omit)
survey.rf2


#For Classification problem - errors in each class and Black line- OOB error
plot(survey.rf2)

#Plot for variable importance
library(caret)
varImpPlot(survey.rf2,sort = T, n.var=10, main="Top 10 - Variable Importance for Detractor (1) and others (0)")
table(Final_data$Product.Type)

#Variable Importance
var.imp2 = data.frame(importance(survey.rf2,type=2))

write.csv(var.imp2, "C:/Users/Rashu/Desktop/Class notes/Sem 2/flex 1/Grad case studies/rfdet.csv")

#In sample prediction

pred_train<-predict(survey.rf2, train, type="class")
a<-as.numeric(train$Q1_NPS_MODIFIED)
b<-as.numeric(pred_train)

#MSE
mean((a-b)^2)

#out of sample prediction

pred_test<-predict(survey.rf2, test, type="class")
X<-as.numeric(test$Q1_NPS_MODIFIED)
Y<-as.numeric(pred_test)

#MSPE
mean((X-Y)^2)

##logistic regression###
glm_model <- glm(Q1_NPS_MODIFIED ~ . , data = train[,!names(train) %in% c("Q1_NPS_GROUP")],family = "binomial")

predicted_values <- predict(glm_model, type = "response")

predict_binary <- as.numeric(predicted_values > 0.50)

Accuracy(train$Q1_NPS_MODIFIED,predict_binary)
##########################



######### Survey text analysis ###############

install.packages("textmineR")
library(textmineR)

# load survey data set from textmineR
str(survey_data)

# create a document term matrix 
dtm <- CreateDtm(doc_vec = survey_data$Q3_Masked, # character vector of documents
                 doc_names = survey_data$ResponseId, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2]

# Fit a Latent Dirichlet Allocation model
# note the number of topics is arbitrary here
# see extensions for more info

set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = 20,
                     iterations = 200, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 
# R-squared 
# - only works for probabilistic models like LDA and CTM
model$r2
#> [1] 0.2747765

# log Likelihood (does not consider the prior) 

plot(model$log_likelihood, type = "l")

# probabilistic coherence, a measure of topic quality
# this measure can be used with any topic model, not just probabilistic ones
summary(model$coherence)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.0060  0.1188  0.1543  0.1787  0.2187  0.4117

hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
head(t(model$top_terms))

# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")     


# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)

head(model$labels)
#>     label_1                      
#> t_1 "radiation_necrosis"         
#> t_2 "kda_fragment"               
#> t_3 "cardiovascular_disease"     
#> t_4 "mast_cell"                  
#> t_5 "radiation_necrosis"         
#> t_6 "laparoscopic_fundoplication"

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]


# predictions with gibbs
assignments <- predict(model, dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)
# predictions with dot
assignments_dot <- predict(model, dtm,
                           method = "dot")


# compare
barplot(rbind(assignments[10,], assignments_dot[10,]),
        col = c("red", "blue"), las = 2, beside = TRUE)
legend("topright", legend = c("gibbs", "dot"), col = c("red", "blue"), 
       fill = c("red", "blue"))
