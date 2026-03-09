#Code to export EPM data from MedPC Raw Files
#Created by Olivia Ortelli
#Version 2: 03/02/2026

# Load Libraries ----------------------------------------------------------
library("writexl")
library("ggplot2")
library("ggstance")
library("gridExtra")
library("utils")
library("tcltk")
library("dplyr")
library("medparser")

# Main Code Algorithm -----------------------------------------------------

#select your files
file = choose.files()

#Creating placeholders for all of the variables we want to store:
#data file metadata
ExperimentDates.list = list()
SubjectIDs.list = list()
ExperimentNumbers.list = list()
GroupNumbers.list = list()
BoxNumbers.list = list()

#unique IDs - which identifies each rat by the subject ID and date
UniqueIDs.list = list()

#E array : Total maze time in seconds
TotalTime.list = list()

#J array: Total junciton time in seconds
JunctionTime.list = list()

#C array: Closed Arm Stats
ClosedArmExplorations.list = list()
ClosedArmTime.list = list()
ClosedArmEntries.list = list()

#O array: Open Arm Stats
OpenArmExplorations.list = list()
OpenArmEntries.list = list()
OpenArmTime.list = list()

#L array: Latencies into each arm
LatencyArm1.list = list()
LatencyArm2.list = list()
LatencyArm3.list = list()
LatencyArm4.list = list()

LatencyOpen.list = list()
LatencyClosed.list = list()

#V array: Open/Closed + Junction Times
ClosedJunctionTime.list = list()
OpenJunctionTime.list = list()

#Percent Time Spent
ClosedTimePercent.list = list()
OpenTimePercent.list = list()

#Arrays
CarrayDF.list = list()
LarrayDF.list = list()
OarrayDF.list = list()


#Main Code Loop
for (i in 1:length(file))
{
  allText = readLines(file[i])
  
  #STAGE 1: GATHER METADATA
  #gather and store metadata in temporary variables
  RawExperimentDate = PullStringFromTextSearch(allText, "Start Date:")
  SubjectID = PullStringFromTextSearch(allText, "Subject:")
  ExperimentNumber = PullStringFromTextSearch(allText, "Experiment:")
  GroupNumber = PullStringFromTextSearch(allText, "Group:")
  BoxNumber = PullStringFromTextSearch(allText, "Box:")
  TotalTime = PullStringFromTextSearch(allText, "E:")
  JunctionTime = PullStringFromTextSearch(allText, "J:")
  
  #do any necessary processing on temp variables
  ExperimentDate = gsub("/", "-", RawExperimentDate) #replace the slashes in the date with dashes to make it R-friendly
  UniqueID = paste0(SubjectID, "_", ExperimentDate)
  
  #Now add the values of the temp variables to the larger lists, named by the Unique ID for this Animal/Date
  ExperimentDates.list[[UniqueID]] = ExperimentDate
  SubjectIDs.list[[UniqueID]] = SubjectID
  ExperimentNumbers.list[[UniqueID]] =  ExperimentNumber
  GroupNumbers.list[[UniqueID]] = GroupNumber
  BoxNumbers.list[[UniqueID]] = BoxNumber
  TotalTime.list[[UniqueID]] = round(as.numeric(TotalTime), 2)
  JunctionTime.list[[UniqueID]] = round(as.numeric(JunctionTime), 2)
  
  UniqueIDs.list[[UniqueID]] = UniqueID
  
  #STAGE 2: GATHER DATA FROM THE C ARRAY
  
  #identify the index where it says C:
  C_index = 1 + grep("C:", allText[-1], value = FALSE)
  
  C_index = 1 + grep("C:", allText[-1], value = FALSE) #grep searches for specified text in element of the vector and returns string (value = T) OR index (value = F)
  #[-1] eliminates first column so we don't have to worry about file path when searching for element.
  #1+ adds this column back so we don't lose info.
  L_index = 1 + grep("L:", allText[-1], value = FALSE)
  NumberOfRowsInC = (L_index) - (C_index + 1)
  
  #make 5 empty lists to store the C array data
  Ccolumn1.list = list()
  Ccolumn2.list = list()
  Ccolumn3.list = list()
  Ccolumn4.list = list()
  Ccolumn5.list = list()
  
  for (j in 1:NumberOfRowsInC)
  {
    CrowString = allText[C_index + j] #this will give you 1 long string with all of the content of that row
    CrowString.elements = strsplit(CrowString, " +")[[1]]  #this will break up that 1 long string into a vector of elements in the long string
    
    #now, for the row that we are looking at, we can add on the values of each column to our lists
    Ccolumn1.list = c(Ccolumn1.list, CrowString.elements[1])
    Ccolumn2.list = c(Ccolumn2.list, CrowString.elements[2])
    Ccolumn3.list = c(Ccolumn3.list, CrowString.elements[3])
    Ccolumn4.list = c(Ccolumn4.list, CrowString.elements[4])
    Ccolumn5.list = c(Ccolumn5.list, CrowString.elements[5])
    
  }
  
  #make the Carray dataframe from the collected column values
  CarrayDF.list[[UniqueID]] = data.frame(
    Ccol3 = unlist(Ccolumn3.list),
    Ccol4 = unlist(Ccolumn4.list),
    Ccol5 = unlist(Ccolumn5.list)
  )
  
  #get some data from our Carray dataframe
  ClosedArmExplorations = CarrayDF.list[[UniqueID]]$Ccol3[1]
  ClosedArmEntries = CarrayDF.list[[UniqueID]]$Ccol4[1]
  ClosedArmTime = CarrayDF.list[[UniqueID]]$Ccol5[1]
  
  ClosedArmExplorations.list[[UniqueID]] = round(as.numeric(ClosedArmExplorations), 0)
  ClosedArmEntries.list[[UniqueID]] = round(as.numeric(ClosedArmEntries), 0)
  ClosedArmTime.list[[UniqueID]] = round(as.numeric(ClosedArmTime), 2)
  
  #identify the index where it says O:
  O_index = 1 + grep("O:", allText[-1], value = FALSE)
  NumberOfRowsInL = (O_index) - (L_index + 1)
  
  #make 8 empty lists to store the L array data
  Lcolumn1.list = list()
  Lcolumn2.list = list()
  Lcolumn3.list = list()
  Lcolumn4.list = list()
  Lcolumn5.list = list()
  Lcolumn6.list = list()
  Lcolumn7.list = list()
  Lcolumn8.list = list()
  
  for (j in 1:NumberOfRowsInL)
  {
    LrowString = allText[L_index + j] #this will give you 1 long string with all of the content of that row
    LrowString.elements = strsplit(LrowString, " +")[[1]]  #this will break up that 1 long string into a vector of elements in the long string
    
    #now, for the row that we are looking at, we can add on the values of each column to our lists
    Lcolumn1.list = c(Lcolumn1.list, LrowString.elements[1])
    Lcolumn2.list = c(Lcolumn2.list, LrowString.elements[2])
    Lcolumn3.list = c(Lcolumn3.list, LrowString.elements[3])
    Lcolumn4.list = c(Lcolumn4.list, LrowString.elements[4])
    Lcolumn5.list = c(Lcolumn5.list, LrowString.elements[5])
    Lcolumn6.list = c(Lcolumn6.list, LrowString.elements[6])
    Lcolumn7.list = c(Lcolumn7.list, LrowString.elements[7])
  }
  
  #make the Larray dataframe from the collected column values
  LarrayDF.list[[UniqueID]] = data.frame(
    Lcol3 = unlist(Lcolumn3.list),
    Lcol4 = unlist(Lcolumn4.list),
    Lcol5 = unlist(Lcolumn5.list),
    Lcol6 = unlist(Lcolumn6.list),
    Lcol7 = unlist(Lcolumn7.list)
  )
  
  #get some data from our Larray dataframe
  LatencyArm1 = LarrayDF.list[[UniqueID]]$Lcol4[1]
  LatencyArm2 = LarrayDF.list[[UniqueID]]$Lcol5[1]
  LatencyArm3 = LarrayDF.list[[UniqueID]]$Lcol6[1]
  LatencyArm4 = LarrayDF.list[[UniqueID]]$Lcol7[1]
  
  LatencyArm1 = ifelse(as.numeric(LatencyArm1) == 0,
                       round(as.numeric(TotalTime), 2),
                       round(as.numeric(LatencyArm1), 2))
  LatencyArm2 = ifelse(as.numeric(LatencyArm2) == 0,
                       round(as.numeric(TotalTime), 2),
                       round(as.numeric(LatencyArm2), 2))
  LatencyArm3 = ifelse(as.numeric(LatencyArm3) == 0,
                       round(as.numeric(TotalTime), 2),
                       round(as.numeric(LatencyArm3), 2))
  LatencyArm4 = ifelse(as.numeric(LatencyArm4) == 0,
                       round(as.numeric(TotalTime), 2),
                       round(as.numeric(LatencyArm4), 2))
  
  LatencyOpen = round(as.numeric(min(LatencyArm1, LatencyArm3)), 2)
  LatencyClosed = round(as.numeric(min(LatencyArm2, LatencyArm4)), 2)
  
  LatencyArm1.list[[UniqueID]] = LatencyArm1
  LatencyArm2.list[[UniqueID]] = LatencyArm2
  LatencyArm3.list[[UniqueID]] = LatencyArm3
  LatencyArm4.list[[UniqueID]] = LatencyArm4
  LatencyOpen.list[[UniqueID]] = LatencyOpen
  LatencyClosed.list[[UniqueID]] = LatencyClosed
  
  #identify the index where it says R:
  R_index = 1 + grep("R:", allText[-1], value = FALSE)
  NumberOfRowsInO = (R_index) - (O_index + 1)
  
  #make 5 empty lists to store the O array data
  Ocolumn1.list = list()
  Ocolumn2.list = list()
  Ocolumn3.list = list()
  Ocolumn4.list = list()
  Ocolumn5.list = list()
  
  for (j in 1:NumberOfRowsInO)
  {
    OrowString = allText[O_index + j] #this will give you 1 long string with all of the content of that row
    OrowString.elements = strsplit(OrowString, " +")[[1]]  #this will break up that 1 long string into a vector of elements in the long string
    
    #now, for the row that we are looking at, we can add on the values of each column to our lists
    Ocolumn1.list = c(Ocolumn1.list, OrowString.elements[1])
    Ocolumn2.list = c(Ocolumn2.list, OrowString.elements[2])
    Ocolumn3.list = c(Ocolumn3.list, OrowString.elements[3])
    Ocolumn4.list = c(Ocolumn4.list, OrowString.elements[4])
    Ocolumn5.list = c(Ocolumn5.list, OrowString.elements[5])
  }
  
  #make the Larray dataframe from the collected column values
  OarrayDF.list[[UniqueID]] = data.frame(
    Ocol3 = unlist(Ocolumn3.list),
    Ocol4 = unlist(Ocolumn4.list),
    Ocol5 = unlist(Ocolumn5.list)
  )
  
  #get some data from our Carray dataframe
  OpenArmExplorations = OarrayDF.list[[UniqueID]]$Ocol3[1]
  OpenArmEntries = OarrayDF.list[[UniqueID]]$Ocol4[1]
  OpenArmTime = ifelse(OpenArmEntries > 0, OarrayDF.list[[UniqueID]]$Ocol5[1], TotalTime)
  
  OpenArmExplorations.list[[UniqueID]] = round(as.numeric(OpenArmExplorations), 0)
  OpenArmEntries.list[[UniqueID]] = round(as.numeric(OpenArmEntries), 0)
  OpenArmTime.list[[UniqueID]] = round(as.numeric(OpenArmTime), 2)
  
  #Junction Time
  ClosedJunctionTime = as.numeric(JunctionTime) + as.numeric(ClosedArmTime)
  OpenJunctionTime = as.numeric(JunctionTime) + as.numeric(OpenArmTime)
  
  ClosedJunctionTime.list[[UniqueID]] = ClosedJunctionTime
  OpenJunctionTime.list[[UniqueID]] = OpenJunctionTime
  
  #PercentTime
  ClosedTimePercent = round((as.numeric(ClosedArmTime) / as.numeric(TotalTime)) *
                              100, 2)
  OpenTimePercent = round((as.numeric(OpenArmTime) / as.numeric(TotalTime)) *
                            100, 2)
  
  ClosedTimePercent.list[[UniqueID]] = ClosedTimePercent
  OpenTimePercent.list[[UniqueID]] = OpenTimePercent
  
}

# Creating Summary Data Excel Sheet ---------------------------------------
SummaryData = data.frame(
  UniqueID = unlist(UniqueIDs.list),
  ExperimentDate = unlist(ExperimentDates.list),
  SubjectID = unlist(SubjectIDs.list),
  GroupNumber = unlist(GroupNumbers.list),
  BoxNumber = unlist(BoxNumbers.list),
  TotalTime = unlist(TotalTime.list),
  JunctionTime = unlist(JunctionTime.list),
  ClosedArmExplorations = unlist(ClosedArmExplorations.list),
  ClosedArmEntries = unlist(ClosedArmEntries.list),
  ClosedArmTime = unlist(ClosedArmTime.list),
  OpenArmExplorations = unlist(OpenArmExplorations.list),
  OpenArmEntries = unlist(OpenArmEntries.list),
  OpenArmTime = unlist(OpenArmTime.list),
  LatencyArm1 = unlist(LatencyArm1.list),
  LatencyArm2 = unlist(LatencyArm2.list),
  LatencyArm3 = unlist(LatencyArm3.list),
  LatencyArm4 = unlist(LatencyArm4.list),
  LatencyOpen = unlist(LatencyOpen.list),
  LatencyClosed = unlist(LatencyClosed.list),
  ClosedJunctionTime = unlist(ClosedJunctionTime.list),
  OpenJunctionTime = unlist(OpenJunctionTime.list),
  ClosedTimePercent = unlist(ClosedTimePercent.list),
  OpenTimePercent = unlist(OpenTimePercent.list)
)


ExcelDFList <- c(SummaryData <- list(SummaryData))

temporaryFile = paste0(tempfile(), ".xlsx")
write_xlsx(ExcelDFList, temporaryFile)
browseURL(temporaryFile)
