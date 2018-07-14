setwd("C:/Users/mfrangos/Desktop/R")
library(dplyr) #Loads the dplyr library.
library(plyr)  #Allows for ddply functions 
options(max.print=1000000)

#Before running this code, delete filler/empty rows in csv files at the top and bottom rows to clean data. Also change to number font types to NUMBER
#Clean up Periods by replacing with zero. #CHECK IF column names are good


#This section Organizes leapahead csv files 
#------------------------------------------------------------------
##THIS SECTION READS LEAPAHEAD'S "Receivable Account Detail (ODS PROD)" Report
##

AllBalanceData = data.frame(read.csv("Input Tsaarev 7-09-18.csv")) #creates dataframe from leapahead file


#str(AllBalanceData$BALANCE)  #Checks the dataclass type
AllBalanceData$BALANCE #PRINT COLUMN



#Be sure to set the balance column as a "number" in excel to prevent errors.
StudentBalances=data.frame(rowsum(AllBalanceData$BALANCE,AllBalanceData$ID.1),row.names = NULL)   #Creates the StudentBalances dataframe. Our output.
str(AllBalanceData$BALANCE)

#Sums up all fees
TotalFees = c(rowsum(AllBalanceData$AMOUNT,AllBalanceData$ID.1),row.names = NULL)


#Removes duplicate names
NoDuplicatesNames = AllBalanceData[!duplicated(AllBalanceData$NAME),]
BalanceNames = NoDuplicatesNames$NAME


#Removes duplicate ZNumbers
NoDuplicatesZNumbers = AllBalanceData[!duplicated(AllBalanceData$ID.1),]
BalanceZNumbers = NoDuplicatesZNumbers$ID.1



#Starts merging our output stuff
BalanceOutputData =  data.frame(StudentBalances)
BalanceOutputData$ZNumber = BalanceZNumbers
BalanceOutputData$Names = BalanceNames

#Creates Payment Column
BalanceOutputData$TotalFees = TotalFees


#Renames columns
names(BalanceOutputData) = c("Balance", "ZNumber" , "Names","TotalFees")

#Creates percentage paid column
BalanceOutputData$PercentPaid = (TotalFees-BalanceOutputData$Balance)/TotalFees

#Renames columns
names(BalanceOutputData) = c("Balance", "ZNumber" , "Names","TotalFees","Percent Paid")



#write.csv(BalanceOutputData, file = "OUTPUTStudentBalances.csv",row.names=TRUE)     #Exports the output as a CSV file

 #END OF STUDENT BALANCES CSV PRINTER
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#FINANCIAL AID SECTION
#FINANCIAL AID SECTION
#THIS READS LEAPAHEAD'S "Financial Aid Award (ODS PROD)" Report

#Before running, delete filler/empty rows in csv file at the top and bottom rows.


#FINANCIAL AID SECTION
#FINANCIAL AID SECTION


FinancialAidData = data.frame(read.csv("Input RPAAWRD 7-9-18.csv")) #creates dataframe from leapahead file

FinancialAidData$X.AWARD_ACCEPT_AMOUNT #Print



#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
#This adds up all accepted aid amts by z#
TotalFinancialAidData=ddply(FinancialAidData,
                            .(ID),summarize
                            ,TotalAcceptedFa=sum(AWARD_ACCEPT_AMOUNT)
                            ,TotalOfferedFa=sum(AWARD_OFFER_AMOUNT)
                            ,TotalPaidFa=sum(AWARD_PAID_AMOUNT)
                            ,Number_of_Rewards=length(ID)) 
                                                                
               
names(TotalFinancialAidData) = c("ZNumber", "FA_Accepted", "FA_Offered", "FA_Paid", "ZnumInstances") #Renames the X.ID Column to ZNumber
TotalFinancialAidData$ZnumInstances = NULL #Deletes the glitch column

#SWITCH BELOW LINE FOR ABOVE TO ORGANIZE OUTPUT FINANCIAL AID DATA BY SOURCE OF FUNDS (subloan, gplus etc)
#ByAwardTypeData=ddply(FinancialAidData,.(X.ID,X.FUND_TITLE),summarize,sum=sum(X.AWARD_ACCEPT_AMOUNT),ZnumInstances=length(X.ID))
#names(ByAwardTypeData) = c("ZNumber", "FUND_TITLE", "sum", "ZnumInstances") #Renames the X.ID Column to ZNumber
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
FinAidOutputData =  data.frame(TotalFinancialAidData) #prepares output data

#SWITCH BELOW FOR ABOVE LINE TO OUTPUT FINANCIAL AID BY SOURCE OF FUNDS
#FinAidOutputData =  data.frame(ByAwardTypeData)    
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#


write.csv(FinAidOutputData, file = "FA OUTPUT.csv",row.names=TRUE)     #Exports the output as a CSV file
#END OF FINANCIAL AID SECTION


#-----------------------------------------------------------------------------------------------
#Merges both outputs into one dataframe
CombinedDF = merge(FinAidOutputData, BalanceOutputData, by="ZNumber",all=TRUE)


#write.csv(CombinedDF, file = "OutputData - Financial.Aid & Balances.csv",row.names=TRUE)  





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#Start of LeapAhead's "Academic Study Primary with GPA(ODS PROD)" Report parsing

AcademicStudyPrimaryData = data.frame(read.csv("Input AcademicStudyPrimaryData 7-9-18.csv")) #creates dataframe from leapahead file

FilteredAcademicStudyPrimaryData = ddply(AcademicStudyPrimaryData                  #Filters data to only include following columns.
                                            ,.(ID,MAJOR
                                            ,EMAIL_PREFERRED_ADDRESS
                                            ,VETERAN_TYPE_DESC)
                                         
                                         ,summarize,ZnumInstances="NULL")
                                                                    #^ First column, becomes what we filter by.                 ^ Summarize required to remove all other data.

names(FilteredAcademicStudyPrimaryData) = c("ZNumber","Major", "Email", "Veteran?", "DeleteCol") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.
FilteredAcademicStudyPrimaryData$DeleteCol = NULL #Deletes the glitch column


CombinedDF = merge(FilteredAcademicStudyPrimaryData, CombinedDF, by="ZNumber",all=TRUE) #Merges all of the 3 reports into one.


#write.csv(CombinedDF, file = "FinalOutputData - All Merged Data.csv",row.names=TRUE) 








#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#Start of LeapAhead's "Financial Aid Tracking Requirement (ODS PROD) - RRAAREQ" Report parsing
#RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ RRAAREQ 


RRAAREQ1.Data = data.frame(read.csv("Input FATR - RRAAREQ 7-9-18.csv")) #creates dataframe from leapahead file

#Selects the useful information for Entrance couneling column in final output
DL_EntranceCounceling = RRAAREQ1.Data %>%
  select(ID, REQUIREMENT_DESC, STATUS_DESC) %>%
  filter(REQUIREMENT_DESC == "Direct Loan Entrance")
  
#Renames columns
  names(DL_EntranceCounceling) = c("ZNumber","ElegibilityRequirement", "DirectLoanEntrance") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  

#filters out more stuff to merge  
DL_EntranceCounceling2 = DL_EntranceCounceling %>%
  select(ZNumber, DirectLoanEntrance)
    
    #Merges with final output file
CombinedDF = merge(DL_EntranceCounceling2 , CombinedDF, by="ZNumber",all=TRUE)
  
  
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

GPL_EntranceCounceling = RRAAREQ1.Data %>%
  select(ID, REQUIREMENT_DESC, STATUS_DESC) %>%
  filter(REQUIREMENT_DESC == "Grad PLUS Entrance")

#Renames columns
names(GPL_EntranceCounceling) = c("ZNumber","ElegibilityRequirement", "GradPLUSEntrance") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  

#filters out more stuff to merge  
GPL_EntranceCounceling2 = GPL_EntranceCounceling %>%
  select(ZNumber, GradPLUSEntrance)

#Merges with final output file
CombinedDF = merge(GPL_EntranceCounceling2 , CombinedDF, by="ZNumber",all=TRUE)
 


#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

Citizenship = RRAAREQ1.Data %>%
  select(ID, REQUIREMENT_DESC, STATUS_DESC) %>%
  filter(REQUIREMENT_DESC == "Citizenship")

#Renames columns
names(Citizenship) = c("ZNumber","ElegibilityRequirement", "Citizenship") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  

#filters out more stuff to merge  
Citizenship2 = Citizenship %>%
  select(ZNumber, Citizenship)

#Merges with final output file
CombinedDF = merge(Citizenship2 , CombinedDF, by="ZNumber",all=TRUE)




#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

SelectiveService = RRAAREQ1.Data %>%
  select(ID, REQUIREMENT_DESC, STATUS_DESC) %>%
  filter(REQUIREMENT_DESC == "Selective Service")

#Renames columns
names(SelectiveService) = c("ZNumber","ElegibilityRequirement", "SelectiveService") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  

#filters out more stuff to merge  
SelectiveService2 = SelectiveService %>%
  select(ZNumber, SelectiveService)

#Merges with final output file
CombinedDF = merge(SelectiveService2 , CombinedDF, by="ZNumber",all=TRUE)


#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

FAFSA = RRAAREQ1.Data %>%
  select(ID, REQUIREMENT_DESC, STATUS_DESC) %>%
  filter(REQUIREMENT_DESC == "Fed. Aid Application")

#Renames columns
names(FAFSA) = c("ZNumber","ElegibilityRequirement", "FAFSA") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  

#filters out more stuff to merge  
FAFSA2 = FAFSA %>%
  select(ZNumber, FAFSA)

#Merges with final output file
CombinedDF = merge(FAFSA2 , CombinedDF, by="ZNumber",all=TRUE)







#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#IMPORT COMMENTS FROM A FILE


Comment.Data = data.frame(read.csv("Comment Data and Other.csv")) #creates dataframe from leapahead file

CombinedDF = merge(Comment.Data , CombinedDF, by="ZNumber",all=TRUE)



#Reorder columns in this order
#CombinedDF = CombinedDF[,c(1,10,11,17,16,3,5,6,7,8,9,12,13,14,15,2,4)]

write.csv(CombinedDF, file = "FinalOutputData - All Merged Data.csv",row.names=TRUE)  
