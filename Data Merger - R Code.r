setwd("C:/Users/mfrangos/Desktop/R")
library(dplyr) #Loads the dplyr library.
library(plyr)  #Allows for ddply function
options(max.print=1000000)

#Before running this code, clean the data and ensure there are no null cells.
#Change Data types to numbers


#This section Organizes leapahead csv files 
#------------------------------------------------------------------
##THIS SECTION READS LEAPAHEAD'S "Receivable Account Detail (ODS PROD)" Report
##The Report must be sorted by X.iD when using leapahead ERP.

AllBalanceData = data.frame(read.csv("Input Tsaarev 7-27-18.csv")) #creates dataframe from leapahead file



#Prevents program from breaking by specifying column names ahead of time.
names(AllBalanceData) = c("X.ID",  "X.Line.Cnt","NAME","X.ID.1","X.ACADEMIC_PERIOD" ,"X.DETAIL_CODE","X.DETAIL_CODE_DESC", "X.DETAIL_CODE_TYPE",
                          "X.CATEGORY","X.CATEGORY_DESC","X.ENTRY_DATE","X.AMOUNT","X.BALANCE","X.TRANSACTION_DESC")


#str(AllBalanceData$BALANCE)  #Checks the dataclass type
AllBalanceData$X.BALANCE #PRINT COLUMN



#Be sure to set the balance columns as "number" dataclass in excel.
StudentBalances=data.frame(rowsum(AllBalanceData$X.BALANCE,AllBalanceData$X.ID.1),row.names = NULL)   #Creates the StudentBalances dataframe. Our output.
str(AllBalanceData$X.BALANCE)

#Sums up all fees
TotalFees = c(rowsum(AllBalanceData$X.AMOUNT,AllBalanceData$X.ID.1),row.names = NULL)


#Removes duplicate names
NoDuplicatesNames = AllBalanceData[!duplicated(AllBalanceData$NAME),]
BalanceNames = NoDuplicatesNames$NAME


#Removes duplicate ZNumbers
NoDuplicatesZNumbers = AllBalanceData[!duplicated(AllBalanceData$X.ID.1),]
BalanceZNumbers = NoDuplicatesZNumbers$X.ID.1



#Starts merging our output columns
BalanceOutputData =  data.frame(StudentBalances)
BalanceOutputData$ZNumber = BalanceZNumbers
BalanceOutputData$Names = BalanceNames

#Creates Payment Column
BalanceOutputData$TotalFees = TotalFees


#Renames columns
names(BalanceOutputData) = c("Balance", "ZNumber" ,"Names", "TotalFees")

#Creates percentage paid column
BalanceOutputData$PercentPaid = (TotalFees-BalanceOutputData$Balance)/TotalFees





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

#Before running, delete filler/empty rows in csv file at the top and bottom rows. Also change number font types to NUMBER
#Clean up Periods by replacing with zero

#FINANCIAL AID SECTION
#FINANCIAL AID SECTION


FinancialAidData = data.frame(read.csv("Input Financial Aid Award 7-27-18.csv")) #creates dataframe from leapahead file

FinancialAidData$AWARD_ACCEPT_AMOUNT #Print



#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
#This adds up all accepted aid amounts and sorts by unique id ("Z#")
TotalFinancialAidData=ddply(FinancialAidData,
                            .(ID),summarize
                            ,TotalAcceptedFa=sum(AWARD_ACCEPT_AMOUNT)
                            ,TotalOfferedFa=sum(AWARD_OFFER_AMOUNT)
                            ,TotalPaidFa=sum(AWARD_PAID_AMOUNT)
                            ,Number_of_Rewards=length(ID)) 
                                                                #^ Creates column                  #Counts how many z-Numbers ^^^^^

names(TotalFinancialAidData) = c("ZNumber", "FA_Accepted", "FA_Offered", "FA_Paid", "ZnumInstances") #Renames the X.ID Column to ZNumber
TotalFinancialAidData$ZnumInstances = NULL #Deletes the glitch column

#SWITCH BELOW FOR ABOVE TO ORGANIZE OUTPUT FINANCIAL AID DATA BY SOURCE OF FUNDS (subloan, gplus etc)
#ByAwardTypeData=ddply(FinancialAidData,.(X.ID,X.FUND_TITLE),summarize,sum=sum(X.AWARD_ACCEPT_AMOUNT),ZnumInstances=length(X.ID))
#names(ByAwardTypeData) = c("ZNumber", "FUND_TITLE", "sum", "ZnumInstances") #Renames the X.ID Column to ZNumber
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
FinAidOutputData =  data.frame(TotalFinancialAidData) #prepares output data

#SWITCH BELOW FOR ABOVE LINE TO OUTPUT FINANCIAL AID BY SOURCE OF FUNDS
#FinAidOutputData =  data.frame(ByAwardTypeData)    #THIS LINE CAN BE SWITCHED
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#



#File Export
#write.csv(FinAidOutputData, file = "FA OUTPUT.csv",row.names=TRUE)     #Exports the output as a CSV file
#END OF FINANCIAL AID SECTION


#-----------------------------------------------------------------------------------------------
#Merges both report outputs into one dataframe
CombinedDF = merge(FinAidOutputData, BalanceOutputData, by="ZNumber",all=TRUE)


#write.csv(CombinedDF, file = "OutputData - Financial.Aid & Balances.csv",row.names=TRUE)  





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#This section manipulates LeapAhead's "Academic Study Primary with GPA(ODS PROD)" Report

AcademicStudyPrimaryData = data.frame(read.csv("Input AcademicStudyPrimaryData 7-27-18.csv")) #creates dataframe from leapahead file

FilteredAcademicStudyPrimaryData = ddply(AcademicStudyPrimaryData                  #Filters data to only include following columns.
                                         ,.(ID,MAJOR
                                            ,EMAIL_PREFERRED_ADDRESS
                                            ,VETERAN_TYPE_DESC
                                         )
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
#This section manipulates LeapAhead's "Financial Aid Tracking Requirement (ODS PROD) - RRAAREQ" Report
#Do not use sorting when extracting from the Leapahead database.



RRAAREQ1.Data = data.frame(read.csv("Input FATR - RRAAREQ 7-27-18.csv")) #creates dataframe from leapahead file

#pREVENTS program from breaking by specifying column names ahead of time
names(RRAAREQ1.Data) = c("X.Line.Cnt"                  ,"X.AID_YEAR.1"                ,"X.AID_YEAR_DESC"            
                          ,"X.ID"                     , "X.NAME"                  ,    "X.REQUIREMENT"            ,   "X.REQUIREMENT_DESC"         
                        ,"X.SATISFIED_IND"            ,"X.STATUS"                 ,   "X.STATUS_DESC"             ,  "X.STATUS_DATE"              
                         ,"X.ESTABLISHED_DATE"          ,"X.PACKAGING_REQ_IND"      ,   "X.DISBURSEMENT_REQ_IND"    ,  "X.MEMO_IND"                 
                         ,"X.MESSAGE"                   ,"X.MESSAGE_DESC"            ,  "X.UNSATISFIED_PROM_NOTE_IND", "X.MORE_SPECIFIC_REQ_IND"    
                         ,"X.EMAIL_PREFERRED"           ,"X.EMAIL_PREFERRED_DESC"     , "X.EMAIL_PREFERRED_ADDRESS"  )




#Selects the useful information for Entrance couneling column in final output.
DL_EntranceCounceling = RRAAREQ1.Data %>%
  select(X.ID, X.REQUIREMENT_DESC, X.STATUS_DESC) %>%
  filter(X.REQUIREMENT_DESC == "Direct Loan Entrance")
  
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

#Switch ID to X.ID sometimes. As well as other column names.
GPL_EntranceCounceling = RRAAREQ1.Data %>%
  select(X.ID, X.REQUIREMENT_DESC, X.STATUS_DESC) %>%
  filter(X.REQUIREMENT_DESC == "Grad PLUS Entrance")

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
  select(X.ID, X.REQUIREMENT_DESC, X.STATUS_DESC) %>%
  filter(X.REQUIREMENT_DESC == "Citizenship")

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


#Switch ID to X.ID Sometimes
SelectiveService = RRAAREQ1.Data %>%
  select(X.ID, X.REQUIREMENT_DESC, X.STATUS_DESC) %>%
  filter(X.REQUIREMENT_DESC == "Selective Service")

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
  select(X.ID, X.REQUIREMENT_DESC, X.STATUS_DESC) %>%
  filter(X.REQUIREMENT_DESC == "Fed. Aid Application")

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
#LEAP AHEAD'S SATISFACTORY ACADEMIC PROGRESS ODS
#LEAP AHEAD'S SATISFACTORY ACADEMIC PROGRESS ODS
#LEAP AHEAD'S SATISFACTORY ACADEMIC PROGRESS ODS
#LEAP AHEAD'S SATISFACTORY ACADEMIC PROGRESS ODS


#Reads ERP Report
SapData = data.frame(read.csv("Input SAP 7-27-18.csv"))

#Premptively sets names of columns to prevent variable names from breaking the program
names(SapData) = c(                         "Line.Cnt",                    "ACADEMIC_PERIOD",             "ACADEMIC_PERIOD_DESC"       
                   ,"NAME" ,                       "ID.1" ,                       "AID_YEAR",                    "AID_YEAR_DESC"              
                   ,"SATISFACTORY_ACAD_PROG_CODE", "SATISFACTORY_ACAD_PROG_DESC")


UsefulSapData = SapData %>%
  select(SATISFACTORY_ACAD_PROG_DESC, ID.1)


names(UsefulSapData) = c("S.A.P", "ZNumber")


CombinedDF = merge(UsefulSapData , CombinedDF, by="ZNumber",all=TRUE)







#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

#Leap Ahead - Alternate Names or IDs (with Telephone) REPORT
#UNDER CONSTRUCTION


Spaiden.Data = data.frame(read.csv("Input Alt Names & Phones 7-18-18.csv"))

#names(Spaiden.Data) = c("SPRIDEN_NTYP_CODE",   "LineCnt, SPRIDEN_ID", "SPRIDEN_NTYP_CODE2" , "SPRIDEN_CHANGE_IND", "SPRIDEN_FIRST_NAME",
#         "SPRIDEN_MI",   "SPRIDEN_LAST_NAME",            "SPRIDEN_SURNAME_PREFIX",           "BIRTH_DATE",   "SPBPERS_CONFID_IND",         
#          "SPBPERS_DEAD_IND", "SPRIDEN_ENTITY_IND",         "SPRIDEN_ACTIVITY_DATE",   "SPRIDEN_USER",            "SPRIDEN_ORIGIN",     
#          "SPRTELE_SEQNO",       "SPRTELE_TELE_CODE",            "AreaCode",            "PhoneMidExtension",   "LONG_PHONE_EXT")


Phone.Data = Spaiden.Data %>%
  select (SPRIDEN_ID,SPRTELE_PHONE_AREA, SPRTELE_PHONE_NUMBER)

names(Phone.Data) = c("ZNumber", "AreaCode","PhoneMidExtension")



PhoneNumbers = data.frame(paste(Phone.Data$AreaCode, Phone.Data$PhoneMidExtension))
names(PhoneNumbers) = "PhoneNumber"


PhoneNumOutput = PhoneNumbers %>%
  select(PhoneNumber) %>%
  mutate(PhoneNumOutput, ZNumber = Phone.Data$ZNumber) 
 


CombinedDF = merge(PhoneNumOutput, CombinedDF)










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
