setwd("C:/Users/mfrangos2016/Desktop/R/Leap Ahead Data Merger")
library(dplyr) #Loads the dplyr library.
library(plyr)  #Allows for ddply functions

options(max.print=1000000)

AllBalanceData = data.frame(read.csv("Input Tsaarev 10-17-18.csv")) #creates dataframe from leapahead file



#Prevents program from breaking by specifying column names ahead of time.
names(AllBalanceData) = c("X.ID",  "X.Line.Cnt","NAME","X.ID.1","X.ACADEMIC_PERIOD" ,"X.DETAIL_CODE","X.DETAIL_CODE_DESC", "X.DETAIL_CODE_TYPE",
                          "X.CATEGORY","X.CATEGORY_DESC","X.ENTRY_DATE","X.AMOUNT","X.BALANCE","X.TRANSACTION_DESC")


FilteredFees1 = AllBalanceData %>%
select(X.ID.1, X.CATEGORY, X.AMOUNT,X.TRANSACTION_DESC ) %>%
filter(X.TRANSACTION_DESC =="Owl Card Replacement"| X.TRANSACTION_DESC =="FAU Campus Bookstore"| X.TRANSACTION_DESC =="Short Term Advance Processing "| X.TRANSACTION_DESC =="Student Health Services       "| X.TRANSACTION_DESC =="2118000864-FL-TEE51 "| X.TRANSACTION_DESC =="Student Health Services"| X.TRANSACTION_DESC =="eLearning Fee"| X.TRANSACTION_DESC =="Online MBA Fees"| X.TRANSACTION_DESC =="Executive Program Local Fees"| X.TRANSACTION_DESC =="Executive MBA Fees"| X.TRANSACTION_DESC =="EMBA-Instructional Materials"| X.TRANSACTION_DESC =="Professional MBA Fee"| X.TRANSACTION_DESC =="Online MHA Fees"| X.TRANSACTION_DESC =="Executive MHA Fees"| X.TRANSACTION_DESC =="EMHA-Instructional Materials"| X.TRANSACTION_DESC =="PMBA-Instructional Materials"| X.TRANSACTION_DESC =="Grad Student Orientation Fee"| X.TRANSACTION_DESC =="Master of Finance Fees"| X.TRANSACTION_DESC =="Tuition Differential"| X.TRANSACTION_DESC =="MSF-Instructional Materials"| X.TRANSACTION_DESC =="Financial Analyst Program Fees"| X.TRANSACTION_DESC =="Online BBA eLearning Fee"| X.TRANSACTION_DESC =="Five Payment Plan Set Up Fee"| X.TRANSACTION_DESC =="Three Installmt. Pymt Plan Fee"| X.TRANSACTION_DESC =="Six Payment Plan Set Up Fee"| X.TRANSACTION_DESC =="Four Payment Plan Set Up Fee"| X.TRANSACTION_DESC =="Install Plan Set Up Fee 50/50"| X.TRANSACTION_DESC =="Owl Card"| X.TRANSACTION_DESC =="Transportation Access Fee"| X.TRANSACTION_DESC =="Invalid Bank Account # Charge"| X.TRANSACTION_DESC =="Returned Check Serv. Charge 5%"| X.TRANSACTION_DESC =="Dishonored Wck Incorrect Acct"| X.TRANSACTION_DESC =="Dishonored Web Check 5%"| X.TRANSACTION_DESC =="Returned Check 5% Serv. Charge"| X.TRANSACTION_DESC =="Short Term Loan Advance       "| X.TRANSACTION_DESC =="Financial Aid Refund"| X.TRANSACTION_DESC =="Refund Distribution"| X.TRANSACTION_DESC =="Athletics Scholarship Advance"| X.TRANSACTION_DESC =="Short Term Advance"| X.TRANSACTION_DESC =="Out of State Tuition&Fees-GR"| X.TRANSACTION_DESC =="Out of State Non Resident-GR"| X.TRANSACTION_DESC =="Out of State Financial Aid-GR"| X.TRANSACTION_DESC =="Out of State Tuition&Fees-UG"| X.TRANSACTION_DESC =="Out of State Financial Aid-UG"| X.TRANSACTION_DESC =="Out of State Non Resident-UG"| X.TRANSACTION_DESC =="Out of State Tuition&FeesEL-GR"| X.TRANSACTION_DESC =="OutofState Financial Aid EL-GR"| X.TRANSACTION_DESC =="Out of State Non Res EL-GR"| X.TRANSACTION_DESC =="Out of State Tuition&FeesEL-UG"| X.TRANSACTION_DESC =="Out of State Non Res EL-UG"| X.TRANSACTION_DESC =="OutofState Financial Aid EL-UG"| X.TRANSACTION_DESC =="Fla.Res.Tuition&FeesEL-GR"| X.TRANSACTION_DESC =="Fla.Res.Tuition&Fees-GR"| X.TRANSACTION_DESC =="Fla.Res.Tuition&Fees-UG"| X.TRANSACTION_DESC =="Out of State Tuition Fee-GR"| X.TRANSACTION_DESC =="Out of State Tuition Fee EL-GR")

#These fees are posted as negative and must be subtracted correctly later.
FilteredFees2 =   AllBalanceData %>%
  select(X.ID.1, X.CATEGORY, X.AMOUNT,X.TRANSACTION_DESC ) %>%  
  filter(X.TRANSACTION_DESC =="Personal Check correction"| X.TRANSACTION_DESC =="VISA correction"| X.TRANSACTION_DESC =="Mastercard correction")

#Flips the sign to negative
FilteredFees2$X.AMOUNT = FilteredFees2$X.AMOUNT * -1

#merges the two data frames vertically
FilteredFees = rbind(FilteredFees1, FilteredFees2)         
         




#Be sure to set the balance column as a "number" in excel. This part of code sums up all the rows into one
StudentBalances=data.frame(rowsum(AllBalanceData$X.BALANCE,AllBalanceData$X.ID.1),row.names = NULL) 
#StudentBalances=data.frame(rowsum(FilteredFees$X.AMOUNT,FilteredFees$X.ID.1),row.names = NULL)   #Creates the StudentBalances dataframe. Our output.

str(AllBalanceData$X.BALANCE)

#Sums up all fees (OUTDATED)
#TotalFees = c(rowsum(FilteredFees$X.AMOUNT,FilteredFees$X.ID.1),row.names = NULL)


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


#Renames columns
names(BalanceOutputData) = c("Balance", "ZNumber" ,"Names")


#Creates Deposit Paid Column
Deposit = AllBalanceData %>%
  select(X.ID.1, X.CATEGORY, X.AMOUNT) %>%
  filter(X.CATEGORY == "DEP")

  #Sets names
  names(Deposit) = c("ZNumber", "TEMP.Col.Category", "Deposit")
  #Eliminates temporary column
  Deposit = Deposit %>% 
    select(ZNumber,Deposit)
  #Merges the rows by Z Number to remove duplicates
  Deposit = data.frame(aggregate(Deposit ~ZNumber , data=Deposit, FUN=sum))
  BalanceOutputData = merge(Deposit, BalanceOutputData, by="ZNumber", all = TRUE)
  #Removes NA cells
  BalanceOutputData[is.na(BalanceOutputData)] = 0 

  
  
#Creates Total Payments column (IN PROGRESS) LAST WORKING LINE
#BalanceOutputData$Payment = (TotalFees-BalanceOutputData$Balance-BalanceOutputData$Deposit)


  #Credits
FilteredPaymentCredits = AllBalanceData %>%
  select(X.ID.1, X.CATEGORY, X.AMOUNT,X.TRANSACTION_DESC ) %>%
  filter(X.TRANSACTION_DESC =="memorial healthcare"| X.TRANSACTION_DESC =="Other Party Check"| X.TRANSACTION_DESC =="Personal Check"
         | X.TRANSACTION_DESC =="Chapter 33 Wire"| X.TRANSACTION_DESC =="Flywire International Payment"|  X.TRANSACTION_DESC =="Discover"
         | X.TRANSACTION_DESC =="FAU Employees Educ Assistance"| X.TRANSACTION_DESC =="Business Executive Prog Ex"| X.TRANSACTION_DESC =="State Employee Res Exemption"
         | X.TRANSACTION_DESC =="Undergrad Studies Matric Exemp"| X.TRANSACTION_DESC =="Money Order 3"| X.TRANSACTION_DESC =="FA Refund Ck Voided-Crd to AR"
         | X.TRANSACTION_DESC =="Refund Ck Voided-Crd to AR"| X.TRANSACTION_DESC =="VISA"| X.TRANSACTION_DESC =="Mastercard" | X.TRANSACTION_DESC == "Web Check" 
         | X.TRANSACTION_DESC == "Florida Prepaid Wire Transfer " | X.TRANSACTION_DESC == "Direct Unsubsidized Loan" 
         | X.TRANSACTION_DESC == "Athletic Tuition Scholarship" |X.TRANSACTION_DESC == "Pre Post Season Expense" | X.TRANSACTION_DESC == "Direct Grad PLUS Loan" 
         | X.TRANSACTION_DESC == "American Express" | X.TRANSACTION_DESC == "Alternative Private Loan" | X.TRANSACTION_DESC == "External Scholarship 1" 
         | X.TRANSACTION_DESC == "Womens Soccer Scholarship" | X.TRANSACTION_DESC == "WSO Cost of Attendance" | X.TRANSACTION_DESC == "Mens Basketball Scholarship" 
         | X.TRANSACTION_DESC == "Miscellaneous FAU Award" | X.TRANSACTION_DESC == "Football Scholarship" | X.TRANSACTION_DESC == "FB Cost of Attendance" 
         | X.TRANSACTION_DESC == "Contract Payment" | X.TRANSACTION_DESC == "Football Off Campus" | X.TRANSACTION_DESC == "MBB Cost of Attendance") 
  

FilteredPaymentCredits =  data.frame(ddply(FilteredPaymentCredits,
                                           .(X.ID.1),summarize
                                           ,TotalCredits=sum(X.AMOUNT)))

   

FilteredPaymentDebits1 = AllBalanceData %>%
  select(X.ID.1, X.CATEGORY, X.AMOUNT,X.TRANSACTION_DESC ) %>%
  filter( X.TRANSACTION_DESC =="Invalid Bank Account # Charge"| X.TRANSACTION_DESC =="Returned Check Serv. Charge 5%"| X.TRANSACTION_DESC =="Dishonored Wck Incorrect Acct"| X.TRANSACTION_DESC =="Dishonored Web Check 5%"| X.TRANSACTION_DESC =="Returned Check 5% Serv. Charge"| X.TRANSACTION_DESC =="Short Term Loan Advance Processing "| X.TRANSACTION_DESC =="Financial Aid Refund"| X.TRANSACTION_DESC =="Refund Distribution"| X.TRANSACTION_DESC =="Short Term Advance")

#For negative credits (in otherwords a debit)
FilteredPaymentDebits2 = AllBalanceData %>%
  select(X.ID.1, X.CATEGORY, X.AMOUNT,X.TRANSACTION_DESC ) %>%
  filter(X.TRANSACTION_DESC =="VISA correction"| X.TRANSACTION_DESC =="Mastercard correction"| X.TRANSACTION_DESC =="Personal Check correction")
  
#Flips the sign to negative
FilteredPaymentDebits2$X.AMOUNT = FilteredPaymentDebits2$X.AMOUNT * -1

#merges the two data frames vertically
FilteredPaymentDebits = rbind(FilteredPaymentDebits1, FilteredPaymentDebits2) 

FilteredPaymentDebits =  data.frame(ddply(FilteredPaymentDebits,
                                           .(X.ID.1),summarize
                                           ,TotalDebits=sum(X.AMOUNT)))

DebitsAndCredits = merge(FilteredPaymentDebits,FilteredPaymentCredits, by="X.ID.1",all=TRUE)

#Removes NA cells and replaces with zero
DebitsAndCredits[is.na(DebitsAndCredits)] = 0 

DebitsAndCredits$TotalPayments = DebitsAndCredits$TotalCredits - DebitsAndCredits$TotalDebits



names(DebitsAndCredits) = c("ZNumber", "TotalDebits", "TotalCredits", "TotalPayments")


#Removes debits & Credits columns
DebitsAndCredits2 = data.frame(DebitsAndCredits$ZNumber,DebitsAndCredits$TotalPayments)
names(DebitsAndCredits2) = c("ZNumber","TotalPayments")


BalanceOutputData = merge(DebitsAndCredits2,BalanceOutputData, by="ZNumber", all = TRUE)



#Creates Total fees Column
BalanceOutputData$TotalFees = BalanceOutputData$Balance + BalanceOutputData$TotalPayments


#Creates percentage paid column
BalanceOutputData$PercentPaid = (BalanceOutputData$TotalFees-BalanceOutputData$Balance)/BalanceOutputData$TotalFees


#Removes NA cells
BalanceOutputData[is.na(BalanceOutputData)] = 0 


FinancialAidData = data.frame(read.csv("Input Financial Aid Award 10-17-18.csv")) #creates dataframe from leapahead file

FinancialAidData$AWARD_ACCEPT_AMOUNT #Print



#This adds up all accepted aid amts by z#
TotalFinancialAidData=ddply(FinancialAidData,
                            .(ID),summarize
                            ,TotalAcceptedFa=sum(AWARD_ACCEPT_AMOUNT)
                            ,TotalOfferedFa=sum(AWARD_OFFER_AMOUNT)
                            ,TotalPaidFa=sum(AWARD_PAID_AMOUNT)
                            ,TotalDeclinedFa=sum(AWARD_DECLINE_AMOUNT)
                            ,Number_of_Rewards=length(ID)) 
                                                                #^ Creates column                  #Counts how many z-Numbers ^^^^^

names(TotalFinancialAidData) = c("ZNumber", "FA_Accepted", "FA_Offered", "FA_Paid", "FA_Declined", "ZnumInstances") #Renames the X.ID Column to ZNumber
TotalFinancialAidData$ZnumInstances = NULL #Deletes the glitch column


FinAidOutputData =  data.frame(TotalFinancialAidData) #prepares output data



CombinedDF = merge(FinAidOutputData, BalanceOutputData, by="ZNumber",all=TRUE)


#write.csv(CombinedDF, file = "OutputData - Financial.Aid & Balances.csv",row.names=TRUE)  


#Reads data
AcademicStudyPrimaryData = data.frame(read.csv("Input AcademicStudyPrimaryData 10-17-18.csv")) #creates dataframe from leapahead file


#Filters data to only include the following columns. We're picking columns to keep.
FilteredAcademicStudyPrimaryData = ddply(AcademicStudyPrimaryData                  
                                         ,.(ID,MAJOR      #First column paramater is what we're filtering by, which is the ZNumber.
                                            ,EMAIL_PREFERRED_ADDRESS
                                            ,VETERAN_TYPE_DESC
                                            ,ACADEMIC_PERIOD_ADMITTED
                                            ,TOTAL_CREDITS_ENROLLED
                                         )
                                         ,summarize,ZnumInstances="NULL") 
   #         ^ Summarize required to remove all other data.


#Renames the X.ID Column to ZNumber. Prepares for Z# merge.
names(FilteredAcademicStudyPrimaryData) = c("ZNumber","Major", "Email", "Veteran?","PeriodAdmitted", "Ttl.Credits.Enrolled", "DeleteCol") 
FilteredAcademicStudyPrimaryData$DeleteCol = NULL #Deletes a glitch column


CombinedDF = merge(FilteredAcademicStudyPrimaryData, CombinedDF, by="ZNumber",all=TRUE) #Merges all of the 3 reports into one.


#write.csv(CombinedDF, file = "FinalOutputData - All Merged Data.csv",row.names=TRUE) 



RRAAREQ1.Data = data.frame(read.csv("Input FATR - RRAAREQ 10-17-18.csv")) #creates dataframe from leapahead file

#pREVENTS program from breaking by specifying column names ahead of times
names(RRAAREQ1.Data) = c("X.Line.Cnt"                  ,"X.AID_YEAR.1"                ,"X.AID_YEAR_DESC"            
                          ,"X.ID"                     , "X.NAME"                  ,    "X.REQUIREMENT"            ,   "X.REQUIREMENT_DESC"         
                        ,"X.SATISFIED_IND"            ,"X.STATUS"                 ,   "X.STATUS_DESC"             ,  "X.STATUS_DATE"              
                         ,"X.ESTABLISHED_DATE"          ,"X.PACKAGING_REQ_IND"      ,   "X.DISBURSEMENT_REQ_IND"    ,  "X.MEMO_IND"                 
                         ,"X.MESSAGE"                   ,"X.MESSAGE_DESC"            ,  "X.UNSATISFIED_PROM_NOTE_IND", "X.MORE_SPECIFIC_REQ_IND"    
                         ,"X.EMAIL_PREFERRED"           ,"X.EMAIL_PREFERRED_DESC"     , "X.EMAIL_PREFERRED_ADDRESS"  )

#Temporary fix
RRAAREQ1.Data = RRAAREQ1.Data[-c(23:27)]

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



#Selects the following columns and only keeps the rows with "Fed.Aid Application"
FAFSA = RRAAREQ1.Data %>%
  select(X.ID, X.REQUIREMENT_DESC, X.STATUS_DESC) %>%          #Selects columns
  filter(X.REQUIREMENT_DESC == "Fed. Aid Application")        #Filters rows

#Renames columns
names(FAFSA) = c("ZNumber","ElegibilityRequirement", "FAFSA") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  

#filters out more stuff to merge  
FAFSA2 = FAFSA %>%
  select(ZNumber, FAFSA)

#Merges with final output file
CombinedDF = merge(FAFSA2 , CombinedDF, by="ZNumber",all=TRUE)


#OTHER OUTSTANDING REQUIREMENT RRAAREQ COLUMN
All.FA.Requirements1 = RRAAREQ1.Data %>%
  select(X.ID, X.REQUIREMENT, X.STATUS) %>%          #Selects columns
  filter(  X.REQUIREMENT == "ADNMGP"
         | X.REQUIREMENT == "BNKRPT"
         | X.REQUIREMENT == "CERT"
         | X.REQUIREMENT == "CONCUR"
         | X.REQUIREMENT == "DEFLT"
         | X.REQUIREMENT == "DHS"
         | X.REQUIREMENT == "ENRHIS"
         | X.REQUIREMENT == "ENTR"
         | X.REQUIREMENT == "GPENTR"
         | X.REQUIREMENT == "HSCH"
         | X.REQUIREMENT == "ISIR"
         | X.REQUIREMENT == "ISIRDC"
         | X.REQUIREMENT == "ISIRRJ"
         | X.REQUIREMENT == "NFI13"
         | X.REQUIREMENT == "NFI16"
         | X.REQUIREMENT == "NOGRAD"
         | X.REQUIREMENT == "SERV"
         | X.REQUIREMENT == "SSA"
         | X.REQUIREMENT == "SSCARD"
         | X.REQUIREMENT == "TMALRT"
         | X.REQUIREMENT == "V5PX16"
         | X.REQUIREMENT == "V5SX16"
         | X.REQUIREMENT == "VDEPV4"
         | X.REQUIREMENT == "VDEPV5"
         | X.REQUIREMENT == "VENFI5"
         | X.REQUIREMENT == "VENFI6"
         | X.REQUIREMENT == "VENFI7"
         | X.REQUIREMENT == "VENFI8"
         | X.REQUIREMENT == "VENFI9"
         | X.REQUIREMENT == "VER5AI"
         | X.REQUIREMENT == "VER5AS"
         | X.REQUIREMENT == "VER5M9"
         | X.REQUIREMENT == "VER5NI"
         | X.REQUIREMENT == "VER5SP"
         | X.REQUIREMENT == "VINDV4"
         | X.REQUIREMENT == "VINDV5"
         )       

#Filters and keeps only the required financial aid items, as opposed to satisfied items.
All.FA.Requirements2 = RRAAREQ1.Data %>%
  select(X.ID, X.REQUIREMENT, X.STATUS) %>%          #Selects columns
  filter(  X.STATUS == "R" | X.STATUS == "D"| X.STATUS == "U" | X.STATUS == "V" | X.STATUS == "Z")




#Merges the requirement rows by Z Number and concatenates them.
All.FA.Requirements3 = data.frame(aggregate(X.REQUIREMENT ~X.ID , data=All.FA.Requirements2, paste, sep=",", collapse = ", "))





#Renames columns
names(All.FA.Requirements3) = c("ZNumber","All.FA.Requirements") #Renames the X.ID Column to ZNumber. Prepares for Z# merge.  


#Merges with final output file
CombinedDF = merge(All.FA.Requirements3 , CombinedDF, by="ZNumber",all=TRUE)







SapData = data.frame(read.csv("Input SAP 10-17-18.csv"))

#Premptively sets names of columns to prevent leapahead from breaking the program
names(SapData) = c(                         "Line.Cnt",                    "ACADEMIC_PERIOD",             "ACADEMIC_PERIOD_DESC"       
                   ,"NAME" ,                       "ID.1" ,                       "AID_YEAR",                    "AID_YEAR_DESC"              
                   ,"SATISFACTORY_ACAD_PROG_CODE", "SATISFACTORY_ACAD_PROG_DESC")


UsefulSapData = SapData %>%
  select(SATISFACTORY_ACAD_PROG_DESC, ID.1)


names(UsefulSapData) = c("S.A.P", "ZNumber")


CombinedDF = merge(UsefulSapData , CombinedDF, by="ZNumber",all=TRUE)


#Leap Ahead - Alternate Names or IDs (with Telephone) REPORT

#Reads data
Spaiden.Data = data.frame(read.csv("Input Alt Names & Phones 7-18-18.csv"))

#Deletes all data except the selected columns
Phone.Data = Spaiden.Data %>%
  select (SPRIDEN_ID,SPRTELE_PHONE_AREA, SPRTELE_PHONE_NUMBER)

#Sets names of columns
names(Phone.Data) = c("ZNumber", "AreaCode","PhoneMidExtension")


#Concatonates two columns into one column. Combines area code & rest of phone #
PhoneNumbers = data.frame(paste(Phone.Data$AreaCode, Phone.Data$PhoneMidExtension))
names(PhoneNumbers) = "PhoneNumber" #Changes name of column

#Creates Z# column and keeps phonenumber column from phone report
PhoneNumOutput = PhoneNumbers %>%
  select(PhoneNumber) %>% #Selects PhoneNumber column inside of PhoneNumOutput.
  mutate(PhoneNumOutput, ZNumber = Phone.Data$ZNumber) 

PhoneNumOutput = PhoneNumOutput[!duplicated(PhoneNumOutput["ZNumber"]),] #Removes duplicates from the data
 

#Merges data. NOTE: all.x = TRUE prevents extra phone number data to create new rows in CombinedDF
CombinedDF = merge(CombinedDF,PhoneNumOutput, by="ZNumber", all.x = TRUE)





#IMPORT COMMENTS FROM A FILE
Comment.Data = data.frame(read.csv("Comment Data and Other.csv")) #creates dataframe from leapahead file
merges comment data by Z#
CombinedDF = merge(Comment.Data , CombinedDF, by="ZNumber",all=TRUE)


CombinedDF = CombinedDF[!duplicated(CombinedDF["ZNumber"]),] #Removes duplicates from the data
rownames(CombinedDF) =  1:nrow(CombinedDF) #AutoNumbers the first column

#Reorder columns in this order
CombinedDF = CombinedDF[ ,c(match("ZNumber"            ,names(CombinedDF))
                           ,match("Major"              ,names(CombinedDF))
                           ,match("Names"              ,names(CombinedDF))
                           ,match("Email"              ,names(CombinedDF))
                           ,match("Balance"            ,names(CombinedDF))
                           ,match("TotalPayments"            ,names(CombinedDF))
                           ,match("Deposit"            ,names(CombinedDF))
                           ,match("PercentPaid"        ,names(CombinedDF))
                           ,match("TotalFees"          ,names(CombinedDF))       
                           ,match("PhoneNumber"        ,names(CombinedDF))
                           ,match("S.A.P"              ,names(CombinedDF))
                           ,match("FAFSA"              ,names(CombinedDF))
                           ,match("SelectiveService"   ,names(CombinedDF))
                           ,match("Citizenship"        ,names(CombinedDF))
                           ,match("GradPLUSEntrance"   ,names(CombinedDF))
                           ,match("DirectLoanEntrance" ,names(CombinedDF))
                           ,match("All.FA.Requirements",names(CombinedDF))
                           ,match("Veteran?"           ,names(CombinedDF))
                           ,match("FA_Accepted"        ,names(CombinedDF))
                           ,match("FA_Offered"         ,names(CombinedDF))
                           ,match("FA_Paid"            ,names(CombinedDF))
                           ,match("FA_Declined"            ,names(CombinedDF))
                           ,match("PeriodAdmitted"     ,names(CombinedDF))
                           ,match("Ttl.Credits.Enrolled"     ,names(CombinedDF))
                           
#                           ,match("Comment"            ,names(CombinedDF))
#                           ,match("Action"             ,names(CombinedDF))
)]


#Reads data
MOP.Data = data.frame(read.csv("Fall 2018 MOP and Fee.S.ExecPrograms.csv"))
MOP.Data = MOP.Data[!duplicated(MOP.Data["ZNumber"]),] #Removes duplicates from the data
CombinedDF = merge(CombinedDF, MOP.Data, by="ZNumber", all.x = TRUE)




write.csv(CombinedDF, file = "FinalOutput - All Merged Data.csv",row.names=TRUE)  
