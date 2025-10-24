# Packages ----------------------------------------------------------------
library("shiny") #Required to run any Shiny app
library("writexl") #Write and export Excel files
library("medparser") #Pulls operant data from .txt files generated from MPC
library("ggplot2") #Data visualization
library("ggstance")
library("gridExtra") #Combining LP and Lick cumulative record plots
library("utils") #R utility functions
library("tcltk") #Pop ups for GUI interface - might be able to omit after Shiny app is built
library("dplyr") #Filtering and manipulating data
library("DT") #creating data tables
library("shinyFiles") #selecting directory to save images
library("shinyalert")

# Define UI ---------------------------------------------------------------
ui <- fluidPage(
  
  #Add a title panel
  titlePanel("Med PC File Analyzer: Response Requirement Program"),
  
  #Creating a navigation bar across the top of the page
  navbarPage(
    "Menu",
    tabPanel("Summary Table",
             # Make the layout a sidebarLayout
             sidebarLayout(
               # Inside the sidebarLayout, add a sidebarPanel
               sidebarPanel(
                 titlePanel("Summary Data"),
                 
                 numericInput(
                   "LPBoutThreshold",
                   label = h5("Time (sec) to determine LP bout: Pause in activity for this time will result in new LP bout being recorded."),
                   value = 20
                 ),
                 
                 numericInput(
                   "LickBoutThreshold",
                   label = h5("Time (sec) to determine lick bout: Pause in activity for this time will result in new lick bout being recorded."),
                   value = 20
                  ),
               
                 # Horizontal line
                 tags$hr(),
               
                 
                 h4("Options for Adjusting LP or Lick Values:"),
                 checkboxInput("RemoveLPsAfterLicks", "Remove LPs After Licks", FALSE),
                 checkboxInput("RemoveLicksBeforeLP", "Remove Licks Before LP", FALSE),
                 checkboxInput("NonContingentPresentation", "EPT: Non-Contingent Sipper Presentation", FALSE),
                 
                 # Horizontal line
                 tags$hr(),
                 
                 numericInput(
                   "DecimalforLP",
                   label = h5("Value following decimal in MedPC code to signify lever press"),
                   value = 200
                 ),
                 
                 # Horizontal line
                 tags$hr(),
                 
                 # Input: User-specified values
                 numericInput(
                   "MinimumNumberofLicksForaBout",
                   label = h5("Minimum Number of Licks For a Bout"),
                   value = 50
                 ),
                 numericInput(
                   "SipperPresentationTime",
                   label = h5("Only relevant for EPT with Non-Contingent Sipper Presentation: Time (min.) when sipper is presented"),
                   value = 20
                 ),
                 
                 # Horizontal line
                 tags$hr(),
                 
                 fileInput(
                   "file",
                   "Choose MedPC File(s)",
                   accept = "",
                   multiple = TRUE
                 ),
                 
               ),
               
               # Inside the sidebarLayout, add a mainPanel for displaying outputs
               mainPanel(DT::dataTableOutput("table"))
             )),
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Cumulative Record Details"),
                 selectizeInput("selected_uniqueID",
                                "Unique ID:",
                                choices = "Please Upload MedPC file(s)"),
                 # Horizontal line
                 tags$hr(),
                 
                 #Specify how to save plots to your computer
                 radioButtons(
                   "ftype",
                   "Select the File Type: Single CR Download",
                   c("png", "jpg", "pdf"),
                 ),
                 # Horizontal line
                 tags$hr(),
                 
                 shinyDirButton(
                   "folder",
                   "Select Directory to Save All Cumulative Records",
                   "Select a directory to save CRs:",
                   multiple = F,
                   buttonType = "default"
                 ),
                 verbatimTextOutput("filepaths"),
                 actionButton("SaveAllPlotsButton", "Save All CRs")
               ),
               mainPanel(
                 plotOutput("plot"),
                 downloadButton('downloadThisCR', "Download CR")
               )
             )),
    tabPanel("Bin Analyses",
             sidebarLayout(
               sidebarPanel(
                 #Type of microstructure data
                 titlePanel("Session Details"),
                 radioButtons(
                   "microstructure_type",
                   "Microstructure Data:",
                   c("Raw", "Percentage", "Cumulative Percentage"),
                 ),
                 
                 # Horizontal line
                 tags$hr(),
                 
                 numericInput(
                   "LPAccessLength",
                   label = h6("Time to Complete Response Requirement (sec.)"),
                   value = 1200
                 ),
                 numericInput(
                   "LPMicrostructure_BinLength",
                   label = h6("LP Microstructure Bin Length (sec.)"),
                   value = 60
                 ),
                 numericInput(
                   "LickAccessLength",
                   label = h6("Sipper Access Time (sec.)"),
                   value = 1200
                 ),
                 numericInput(
                   "LickMicrostructure_BinLength",
                   label = h6("Lick Microstructure Bin Length (sec.)"),
                   value = 60
                 ),
               ),
               mainPanel(tabsetPanel(
                 tabPanel(
                   "Lever Press Bins",
                   DT::dataTableOutput("lpbinstable")
                 ),
                 tabPanel(
                   "Lick Bins",
                   DT::dataTableOutput("lickbinstable")
                 ),
               ),)
             )),
    tabPanel("Bouts",
             sidebarLayout(sidebarPanel(
               titlePanel("Bouts"),
               selectizeInput("selected_uniqueID_forbouts",
                              "Unique ID:",
                              choices = "Please Upload MedPC file(s)"),
               numericInput(
                 "BoutLength",
                 label = h6("User-Defined LP Bout Length (sec.)"),
                 value = 20
               ),
             ),
               mainPanel(tabsetPanel(
                 tabPanel(
                   "Lever Presses Per Bout",
                   DT::dataTableOutput("lpsperbouttable")
                 ),
                 tabPanel("Licks Per Bout",
                          DT::dataTableOutput("licksperbouttable")),
               ), )
             )), 
                 
    tabPanel(
      "Information",
      h4(
        "[Place holder for citation information]"),
        uiOutput("OtherOptionsToNote"),
        h4(
          "Variables generated in the Summary Table are operationally defined below:"
        ),
        DT::dataTableOutput("vartable"),
    )
  )
)

# Define server logic -----------------------------------------------------
server <- function(input, output) {

  #Creating placeholders for all of the variables we want to store:
  #data file metadata
  ExperimentDates.list = list()
  SubjectIDs.list = list()
  ExperimentNumbers.list = list()
  GroupNumbers.list = list()
  BoxNumbers.list = list()
  
  #unique IDs - which identifies each rat by the subject ID and date
  UniqueIDs.list = list()
  
  #B array data of interest
  LeverPressBouts.list = list()
  TotalActualLeverPresses.list = list()
  LickBouts.list = list()
  TotalLicks.list = list()
  
  #Adjusted/clean variables
  RRLeverPresses.list = list()
  
  #Timestamps Of Interest
  StartTimes.list = list()
  FirstLeverPressTimes.list = list()
  LastLeverPressTimes.list = list()
  FirstLickTimes.list = list()
  LastLickTimes.list = list()
  
  #Seconds To Interested Action
  LatencyToFirstLP.list = list()
  SessionStartToLastLP.list = list()
  TimeBetweenFirstandLastLP.list = list()
  LatencyToFirstLick.list = list()
  TimeBetweenFirstandLastLick.list = list() 
  
  #Rate variables (minutes)
  LPRate_SessionStarttoLastLP.list = list() #LP/min
  LPRate_FirsttoLastLP.list = list()        #LP/min
  LickRate_FirsttoLastLick.list = list()    #Licks/min
  LickRate_TotalLickTime.list = list()      #Licks/min
  
  #Export Data Frame List
  HMSDataFrames.list = list()
  
  #Plots
  TogetherPlots.list = list()
  SeparatedPlots.list = list()
  
  #Accurate LickBoutTimes List
  AccurateLickBoutTimes.list = list()
  TotalLickTime.list = list()
  FirstBoutDuration_Percentage.list = list()
  FirstBoutLick_Percentage.list = list()
  FirstBoutRate.list = list()
  LickBout_Nontrivial.list = list()
  Lick50PercentTime.list = list()
  
  #Microstructure analyses
  BinnedLP.list = list()
  BinnedLicks.list = list()
  
  observe({
    if (!is.null(input$file)) {
      N_tables = length(input$file[,1])
        
      for (i in 1:N_tables) {
        allText <- readLines(input$file$datapath[i])
        #STAGE 1: GATHER METADATA
        #gather and store metadata in temporary variables
        RawExperimentDate = PullStringFromTextSearch(allText,"Start Date:")
        SubjectID = PullStringFromTextSearch(allText,"Subject:")
        ExperimentNumber = PullStringFromTextSearch(allText,"Experiment:")
        GroupNumber = PullStringFromTextSearch(allText,"Group:")
        BoxNumber = PullStringFromTextSearch(allText,"Box:")
        
        #do any necessary processing on temp variables
        ExperimentDate = gsub("/","-", RawExperimentDate) #replace the slashes in the date with dashes to make it R-friendly
        UniqueID=paste0(SubjectID,"_",ExperimentDate)
        
        #Now add the values of the temp variables to the larger lists, named by the Unique ID for this Animal/Date
        ExperimentDates.list[[UniqueID]] = ExperimentDate
        SubjectIDs.list[[UniqueID]] = SubjectID
        ExperimentNumbers.list[[UniqueID]] =  ExperimentNumber
        GroupNumbers.list[[UniqueID]] = GroupNumber
        BoxNumbers.list[[UniqueID]] = BoxNumber
        
        UniqueIDs.list[[UniqueID]] = UniqueID
        
        #STAGE 2: GATHER DATA FROM THE B ARRAY
        #identify the index where it says B:
        B_index = 1+grep("B:", allText[-1], value = FALSE) 
        
        #gather the data on B0, B1, B2, and B3 by pulling from the indices following the B_index and store in temp variables
        TotalActualLeverPresses = DecimalStringToInteger(PullStringFromIndex(allText,B_index+2))
        RRLeverPresses = TotalActualLeverPresses
        TotalLicks = DecimalStringToInteger(PullStringFromIndex(allText,B_index+4))
        
        #Now add the values of the temp variables to the larger lists, named by the Unique ID for this Animal/Date
        TotalActualLeverPresses.list[[UniqueID]] = TotalActualLeverPresses
        TotalLicks.list[[UniqueID]] = TotalLicks
        
        #STAGE 3: PULL OUT THE HMS ARRAY TIMESTAMPS AND ASSOCIATED C ARRAY DATA
        #identify the index where it says C:/H:/M:
        C_index = 1+grep("C:", allText[-1], value = FALSE)
        H_index = 1+grep("H:", allText[-1], value = FALSE)
        M_index = 1+grep("M:", allText[-1], value = FALSE)
        NumberOfTimestampsRecorded = (M_index-1)-(H_index+1) #calculate numbers of values in the HMS arrays
        
        #instantiate lists for the HMS and C: array data to be collected
        indices = list()
        timestamps = list()
        Cvalues = list()
        Cvalues_BeforeDecimal = list()
        Cvalues_AfterDecimal = list()
        
     #Creating progress bar while reading the data in   
      withProgress(
        message = "Reading Data", 
        value = 0,{
          
        for (j in 0:NumberOfTimestampsRecorded)
        {
          #add on to the indices and timestamps lists for the values at index j
          indices = c(indices, j)
          timestamps = c(timestamps, TimestampPull(allText,j))

          #start on the 3rd Cvalue so it aligns
          if(j==0|j==1)
          {
            Cvalues = c(Cvalues, NA)
            Cvalues_BeforeDecimal = c(Cvalues_BeforeDecimal, NA)
            Cvalues_AfterDecimal = c(Cvalues_AfterDecimal, NA)
          }
          else
          {
            RawCvalue = allText[C_index+j] #get the value of C at index J
            RawCvalue_elements = strsplit(RawCvalue," ")
            Cvalue = tail(RawCvalue_elements[[1]], n=1)
            Cvalue.elements =  strsplit(Cvalue,".", fixed=T)
            Cvalue_BeforeDecimal = strsplit(Cvalue,".", fixed=T)[[1]][1]
            Cvalue_AfterDecimal = strsplit(Cvalue,".", fixed=T)[[1]][2]
            Cvalues = c(Cvalues, Cvalue)
            Cvalues_BeforeDecimal = c(Cvalues_BeforeDecimal, Cvalue_BeforeDecimal)
            Cvalues_AfterDecimal = c(Cvalues_AfterDecimal, Cvalue_AfterDecimal)
          }
          incProgress(amount = 1/NumberOfTimestampsRecorded,
                      detail = paste0("Subject ",i," (", UniqueID, ")"," out of ", length(input$file[,1])," - Timestamp ", j, " out of ", NumberOfTimestampsRecorded))
                  }
        }
      )
        
        HMSDataFrames.list[[UniqueID]] = data.frame(Index = unlist(indices),
                                                    Timestamps = unlist(timestamps),
                                                    C_value = unlist(Cvalues),
                                                    C_value_BeforeDecimal = unlist(Cvalues_BeforeDecimal),
                                                    C_value_AfterDecimal = unlist(Cvalues_AfterDecimal))
        
        #add columns to the HMS data frame specifying the time
        HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds = as.double(HMSDataFrames.list[[UniqueID]]$C_value_BeforeDecimal)/10
        HMSDataFrames.list[[UniqueID]]$CumulativeTime_Seconds = cumsum(ifelse(is.na(HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds), 0, HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds))
        HMSDataFrames.list[[UniqueID]]$CumulativeTime_Minutes = round(HMSDataFrames.list[[UniqueID]]$CumulativeTime_Seconds/60, digits = 3)
        
        #Check in C_value_after decimal is 100 or 200, and check TimeSinceLast Event to assign LeverPress,Lick,LickBoutStart
        HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp = ifelse(HMSDataFrames.list[[UniqueID]]$C_value_AfterDecimal==input$DecimalforLP, "LeverPress", 
                                                                  ifelse(HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds>(input$LickBoutThreshold),
                                                                         "LickBoutStart","Lick"))
        HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
        
        #Detecting licks that occur before final LP 
        LPRows = which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "LeverPress")
        FirstLickRow = head(which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "Lick"), n=1)
        LPRowsAfterLeverPressFinish = LPRows[LPRows>FirstLickRow]

        #Removing LPs that occur after LP requirement is met/licks have occurred
        #Delete the rows that have "LP" after the first "Lick"
        
        #7/5/23 - could i combine these if statements into a larger one so there are 2 messages, one saying that these were detected and removed and another saying "consider selecting "Remove LPs After Licks"
        if(input$RemoveLPsAfterLicks){  
          LPRows = which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "LeverPress")
          FirstLickRow = head(which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "Lick"), n=1)
          LPRowsAfterLeverPressFinish = LPRows[LPRows>FirstLickRow]
          #Creating variable to store number of LPs that occur before first lick, in case rat plays with retracted lever after sipper extends
          if(!identical(LPRowsAfterLeverPressFinish, integer(0))) 
          {
            HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]][-LPRowsAfterLeverPressFinish, ]
            RRLeverPresses = length(HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"]) 
          }
        }
        RRLeverPresses.list[[UniqueID]] = RRLeverPresses
        
        #test if there are no Licks before the last lever press, create warning message if licks are present before final LP
        if(!identical(LPRowsAfterLeverPressFinish, integer(0))) 
        {
          warning1 <- "Lever press/es detected after first lick for"
          message1 <- paste(warning1, UniqueID)
          shinyalert(html = T, text = message1, type = "warning")
        }
        
        #Detecting licks that occur before final LP 
        LickRows = which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "Lick")
        FinalLeverPressRow = tail(which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "LeverPress"), n=1)
        LickRowsBeforeLeverPressFinish = LickRows[LickRows<FinalLeverPressRow]
  
        if(input$RemoveLicksBeforeLP)
        {
          #Delete the rows that have "Lick" before the final "LeverPress"
          LickRows = which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "Lick")
          FinalLeverPressRow = tail(which(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "LeverPress"), n=1)
          LickRowsBeforeLeverPressFinish = LickRows[LickRows<FinalLeverPressRow]
          #test if there are no Licks before the last lever press
          if(!identical(LickRowsBeforeLeverPressFinish, integer(0))) 
          {
            HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]][-LickRowsBeforeLeverPressFinish, ]
            HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
            HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1] <- "LickBoutStart"
            HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
          }
        }
        #test if there are no Licks before the last lever press, create warning message if licks are present before final LP
        observe(
          if(!identical(LickRowsBeforeLeverPressFinish, integer(0))) 
          {
            warning2 <- "Licks detected before final lever press for"
            message2 <- paste(warning2, UniqueID)
            shinyalert(html = T, text = message2, type = "warning")
          }
        ) 
        
        #Locate the First row labeled "Lick" and then change it to a "LickBoutStart", then recount the Counter column
        HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1] <- "LickBoutStart"
        HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
        
        if(input$NonContingentPresentation){
          if(((input$SipperPresentationTime*60)-(tail(HMSDataFrames.list[[UniqueID]]["CumulativeTime_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"],n=1)))<20){
            HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1&HMSDataFrames.list[[UniqueID]]["TimeSinceLastEvent_Seconds"]<20] <- "LickBoutStart"
            HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
            
            HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1] <- "LickBoutStart"
            HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
          }        
          
          HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LickBoutStart"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1] <- "SipperExtended"
          HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LickBoutStart"&HMSDataFrames.list[[UniqueID]]["Counter"] == 2] <- "SipperExtended"
          HMSDataFrames.list[[UniqueID]]["TimeSinceLastEvent_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "SipperExtended"&HMSDataFrames.list[[UniqueID]]["Counter"] == 2] <- 0
          HMSDataFrames.list[[UniqueID]]$CumulativeTime_Seconds = cumsum(ifelse(is.na(HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds), 0, HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds))
          HMSDataFrames.list[[UniqueID]]$CumulativeTime_Minutes = round(HMSDataFrames.list[[UniqueID]]$CumulativeTime_Seconds/60, digits = 3)
          
          HMSDataFrames.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]] %>% group_by(ActionAtTimestamp) %>% mutate(Counter = row_number(ActionAtTimestamp))
        }
        
        #Locate the times of interest
        #StartTime is set to the first timestamp recorded
        StartTimes.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]]$Timestamps[1]
        
        #Accurate Lick Bout Information
        LickBoutNumber.list = list()
        LickBoutTime.list = list()
        LickBoutLicks.list = list()
        LickBoutNumber=0
        LickBoutTime=0
        LickBoutLicks=0
        for (row in 1:nrow(HMSDataFrames.list[[UniqueID]])) 
        {
          if(!is.na(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]))
          {
            if(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]=="LickBoutStart")
            {
              LickBoutNumber.list = c(LickBoutNumber.list, LickBoutNumber)
              LickBoutTime.list = c(LickBoutTime.list, LickBoutTime)
              LickBoutLicks.list = c(LickBoutLicks.list, LickBoutLicks)
              LickBoutNumber=LickBoutNumber+1
              LickBoutTime=0
              LickBoutLicks=0
            }
            if(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]=="Lick")
            {
              LickBoutTime = LickBoutTime + HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds[row] 
              LickBoutLicks = LickBoutLicks + 1 #Counts each lick within bout
            }
          } 
        }
        #add to the lists one final time because the last action recorded is a Lick
        LickBoutNumber.list = c(LickBoutNumber.list, LickBoutNumber)
        LickBoutTime.list = c(LickBoutTime.list, round(LickBoutTime, digits = 2))
        LickBoutLicks.list = c(LickBoutLicks.list, LickBoutLicks)
        
        AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]] = data.frame(LickBoutNumber = unlist(LickBoutNumber.list),
                                                                           LickBoutTime = unlist(LickBoutTime.list),
                                                                           LickBoutLicks = unlist(LickBoutLicks.list))
        
        #Calculating Total Lick Time (Accurate)
        TotalLickTime.list[[UniqueID]] = list()
        TotalLickTime.list[[UniqueID]] = round(sum(AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutTime), digits = 2)
        
        FirstBoutDuration_Percentage.list[[UniqueID]] = list()
        FirstBoutDuration_Percentage.list[[UniqueID]] = round((100*(AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutTime[2] / TotalLickTime.list[[UniqueID]])), digits = 2)
        FirstBoutLick_Percentage.list[[UniqueID]] = round((100*(AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutLicks[2] / sum(AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutLicks))), digits = 2)
        FirstBoutRate.list[[UniqueID]] = round(((AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutLicks[2] / AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutTime[2])*60), digits = 2)
        
        LickBout_Nontrivial.list[[UniqueID]] = list()
        LickBout_Nontrivial.list[[UniqueID]] = sum(AccurateLickBoutTimes.list[[paste0(UniqueID,"_LBT")]]$LickBoutLicks > input$MinimumNumberofLicksForaBout)
        
        
        #check if there are no LeverPresses recorded, if so put NA, otherwise find the Timestamp of LeverPress and Counter=1
        if(length(HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"])==0)
        {
          FirstLeverPressTimes.list[[UniqueID]] = NA
          LastLeverPressTimes.list[[UniqueID]] = NA
          LatencyToFirstLP.list[[UniqueID]] = NA
          SessionStartToLastLP.list[[UniqueID]] = NA
          TimeBetweenFirstandLastLP.list[[UniqueID]] = NA
          LPRate_SessionStarttoLastLP.list[[UniqueID]] = NA
          LPRate_FirsttoLastLP.list[[UniqueID]] = NA
        }
        else
        {
          FirstLeverPressTimes.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1]
          LastLeverPressTimes.list[[UniqueID]] = tail(HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"],n=1)
          LatencyToFirstLP.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]]["CumulativeTime_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1]
          SessionStartToLastLP.list[[UniqueID]] = tail(HMSDataFrames.list[[UniqueID]]["CumulativeTime_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "LeverPress"],n=1)
          TimeBetweenFirstandLastLP.list[[UniqueID]] = round((SessionStartToLastLP.list[[UniqueID]]-LatencyToFirstLP.list[[UniqueID]]), digits = 2)
          LPRate_SessionStarttoLastLP.list[[UniqueID]] = round(((RRLeverPresses.list[[UniqueID]]/SessionStartToLastLP.list[[UniqueID]])*60), digits = 2)
          LPRate_FirsttoLastLP.list[[UniqueID]] = round(((RRLeverPresses.list[[UniqueID]]/TimeBetweenFirstandLastLP.list[[UniqueID]])*60), digits = 2)
        }
        
        #First and Last LickTime: check if there are no Lick recorded, if so put NA, otherwise find the Timestamp of Lick and Counter=1
        if(length(HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"])==0)
        {
          FirstLickTimes.list[[UniqueID]] = NA
          LastLickTimes.list[[UniqueID]] = NA
          LatencyToFirstLick.list[[UniqueID]] = NA
          TimeBetweenFirstandLastLick.list[[UniqueID]] = NA
          LickRate_FirsttoLastLick.list[[UniqueID]] = NA
          LickRate_TotalLickTime.list[[UniqueID]] = NA
        }
        else
        {
          FirstLickTimes.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1]
          LastLickTimes.list[[UniqueID]] = tail(HMSDataFrames.list[[UniqueID]]["Timestamps"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"],n=1)
          LatencyToFirstLick.list[[UniqueID]] = HMSDataFrames.list[[UniqueID]]["TimeSinceLastEvent_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1]
          TimeBetweenFirstandLastLick.list[[UniqueID]] = round((tail(HMSDataFrames.list[[UniqueID]]["CumulativeTime_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"],n=1)-HMSDataFrames.list[[UniqueID]]["CumulativeTime_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1]), digits = 2)
          LickRate_FirsttoLastLick.list[[UniqueID]] = round(((TotalLicks.list[[UniqueID]]/TimeBetweenFirstandLastLick.list[[UniqueID]])*60), digits = 2)
          LickRate_TotalLickTime.list[[UniqueID]] = round(((TotalLicks.list[[UniqueID]]/TotalLickTime.list[[UniqueID]])*60), digits = 2)
        }
        
        #STAGE 4: GENERATE GRAPHS AND PUT THE GGPLOT OBJECTS IN THE NAMED LISTS 
        #Create a new column called "TogetherPlotValue" which sets LeverPress to 0 and Licks to Counter value
        HMSDataFrames.list[[UniqueID]]$TogetherPlotValue = ifelse(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp=="LeverPress", 0, ifelse(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp=="Lick", HMSDataFrames.list[[UniqueID]]$Counter, NA))
        
        #Create a subdataframe only containing the data rows with LeverPress - to be used to make hashes on the graph via geom_errorbar
        HashData = HMSDataFrames.list[[UniqueID]][HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "LeverPress", ]
        
        TogetherPlots.list[[UniqueID]] = ggplot(HMSDataFrames.list[[UniqueID]], aes(x=CumulativeTime_Minutes, y=TogetherPlotValue,color=ActionAtTimestamp)) +
          geom_errorbar(data=HashData, aes(ymin = -20, ymax=20), color = "black", width = 0, size = 1)+
          geom_step(size=1, color = "black") +
          scale_x_continuous(limits=c(0,40)) +
          #scale_y_continuous(limits=c(-300,1000)) +
          xlab("Time (min)") +
          ylab("Number of Licks") +
          ggtitle(paste0('CR Plot: ',UniqueID)) +
          theme_minimal()
        
        #Create subdataframes only containing the data rows with LeverPress or Lick - to be used for SeparatedPlot (this command basically removes the LickBout rows)
        OnlyLeverPresses = HMSDataFrames.list[[UniqueID]][HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "LeverPress", ]
        OnlyLicks = HMSDataFrames.list[[UniqueID]][HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp == "Lick", ]
        
        # Find the time when 50% of licks occurred
        total_licks <- max(OnlyLicks$TogetherPlotValue, na.rm = TRUE)
        if ((total_licks %% 2) == 0) {
          half_licks <- total_licks / 2
        } else {
          half_licks <- ceiling(total_licks / 2)
        }
        
        time_of_50_percent_licks <- OnlyLicks$CumulativeTime_Seconds[half_licks]
        time_of_first_lick <- HMSDataFrames.list[[UniqueID]]["CumulativeTime_Seconds"][HMSDataFrames.list[[UniqueID]]["ActionAtTimestamp"] == "Lick"&HMSDataFrames.list[[UniqueID]]["Counter"] == 1]
        
        # Store in a new list
        Lick50PercentTime.list[[UniqueID]] <- round((time_of_50_percent_licks - time_of_first_lick), digits = 2)
        
        LeverPressPlot = ggplot(OnlyLeverPresses, aes(x=CumulativeTime_Minutes, y=Counter)) +
          geom_step(size=1, show.legend = FALSE, color="coral") +
          scale_x_continuous(limits=c(0,20), breaks = seq(0,20,1)) +
          xlab("Time (min)") +
          ylab("Number of Lever Presses") + 
          theme_minimal() 
        
        LickPlot = ggplot(OnlyLicks, aes(x=CumulativeTime_Minutes, y=Counter,color="blue")) +
          geom_step(size=1, show.legend = FALSE, color="deepskyblue") +
          scale_x_continuous(limits=c(0,40)) +
          xlab("Time (min)") +
          ylab("Number of Licks") +
          theme_minimal() 
        
        #Create final plot with LP and Lick CRs side by side: 
          SeparatedPlots.list[[UniqueID]] = grid.arrange(LeverPressPlot,LickPlot,nrow=1,top=paste0("Lever Press and Lick Profiles: ",UniqueID))
          
          observeEvent(input$SaveAllPlotsButton, {
            SelectedFilePath <- reactive({
              ExportFolder <- paste0(Thesheets_dir(),"/",format(Sys.time(), "_%Y-%m-%d"))
              print(ExportFolder)
            })
            
            
            for (i in 1:length(SeparatedPlots.list))
            {
              ImageFileName = paste(UniqueIDs.list[[i]],".", "jpg", sep = "")
              ggsave(ImageFileName,
                     plot = SeparatedPlots.list[[i]],
                     device = "jpeg",
                     path = SelectedFilePath(),
                     width = 20,
                     height = 5)
            }
          })
          
          #LP Microstructures
            bin_list_LP  <- seq(0, input$LPAccessLength-input$LPMicrostructure_BinLength, by = input$LPMicrostructure_BinLength)
            BinnedLPdata = list()
            
            
            for (u in 1:length(bin_list_LP))
            {
              LP_in_this_bin = length(which(
                !is.na(OnlyLeverPresses$CumulativeTime_Seconds) & 
                  OnlyLeverPresses$CumulativeTime_Seconds>=bin_list_LP[u] & 
                  OnlyLeverPresses$CumulativeTime_Seconds<(bin_list_LP[u]+ input$LPMicrostructure_BinLength)))
              
              LP_in_this_bin_percent <- eventReactive(input$microstructure_type == "Percentage", {
                round(100*(LP_in_this_bin/RRLeverPresses), digits = 2)
              })
              
              LP_in_this_bin_cumpercent <- eventReactive(input$microstructure_type == "Cumulative Percentage", {
                round(100*(length(which(
                           !is.na(OnlyLeverPresses$CumulativeTime_Seconds) & 
                             OnlyLeverPresses$CumulativeTime_Seconds<(bin_list_LP[u]+ input$LPMicrostructure_BinLength)))/RRLeverPresses), digits = 2)
                         
              })
              
              if (input$microstructure_type == "Raw") {
                BinnedLPdata = c(BinnedLPdata, LP_in_this_bin)
              }
              if (input$microstructure_type == "Percentage") {
                BinnedLPdata = c(BinnedLPdata, LP_in_this_bin_percent())
              }
              if (input$microstructure_type == "Cumulative Percentage") {
                BinnedLPdata = c(BinnedLPdata, LP_in_this_bin_cumpercent())
              }
            }
            BinnedLP.list[[UniqueID]] = data.frame(NumLevPressTempName = unlist(BinnedLPdata))
            colnames(BinnedLP.list[[UniqueID]])[1] = UniqueID
            
            #Lick Microstructures
            bin_list_lick <- seq(0, input$LickAccessLength-input$LickMicrostructure_BinLength, by = input$LickMicrostructure_BinLength)
            BinnedLickdata = list()
            
            SipperExtends <- max(OnlyLeverPresses$CumulativeTime_Seconds, na.rm = T)
            
            if(input$NonContingentPresentation) {
              SipperExtends <- (input$SipperPresentationTime)*60
            }
            
            OnlyLicks$LickingTime_Seconds <- (OnlyLicks$CumulativeTime_Seconds - SipperExtends)
            
            
            
            for (v in 1:length(bin_list_lick))
            {
              Lick_in_this_bin = as.numeric(length(which(
                !is.na(OnlyLicks$LickingTime_Seconds) &
                  (OnlyLicks$LickingTime_Seconds) >=bin_list_lick[v] &
                  (OnlyLicks$LickingTime_Seconds) <(bin_list_lick[v]+ input$LickMicrostructure_BinLength))))
              
              Lick_in_this_bin_percent <- eventReactive(input$microstructure_type == "Percentage", {
                round(100*(Lick_in_this_bin/TotalLicks), digits = 2)
              })
              
              Lick_in_this_bin_cumpercent <- eventReactive(input$microstructure_type == "Cumulative Percentage", {
                round(100*(length(which(
                  !is.na(OnlyLicks$LickingTime_Seconds) &
                    (OnlyLicks$LickingTime_Seconds) <(bin_list_lick[v]+ input$LickMicrostructure_BinLength)))/TotalLicks),digits = 2)
                              })
              
              if (input$microstructure_type == "Raw") {
                BinnedLickdata = c(BinnedLickdata, Lick_in_this_bin)
              }
              if (input$microstructure_type == "Percentage") {
                BinnedLickdata = c(BinnedLickdata, Lick_in_this_bin_percent())
              }
              if (input$microstructure_type == "Cumulative Percentage") {
                BinnedLickdata = c(BinnedLickdata, Lick_in_this_bin_cumpercent())
              }
            }
            BinnedLicks.list[[UniqueID]] = data.frame(NumLickTempName = unlist(BinnedLickdata))
            colnames(BinnedLicks.list[[UniqueID]])[1] = UniqueID
            
            #Bouts
            LickBoutNumber=0
            for (row in 1:nrow(HMSDataFrames.list[[UniqueID]])) 
            {
              if(!is.na(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]))
              {
                if(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]=="LickBoutStart")
                {
                  LickBoutNumber=LickBoutNumber+1
                }
                
              } 
            }
            #add to the lists one final time because the last action recorded is a Lick
            LickBouts.list[[UniqueID]] = LickBoutNumber
            
            LPBoutNumber = 1
            for (row in 2:nrow(HMSDataFrames.list[[UniqueID]]))
            {
              if(!is.na(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]))
              {
                if(HMSDataFrames.list[[UniqueID]]$ActionAtTimestamp[row]=="LeverPress")
                {
                  if(HMSDataFrames.list[[UniqueID]]$TimeSinceLastEvent_Seconds[row]>=input$LPBoutThreshold)
                  {
                    LPBoutNumber=LPBoutNumber+1
                  }
                  
                }
              }
            }
            #Adjust the LP Bout to 0 if it says 0 but there was more than 0 LeverPresses
            ifelse((LPBoutNumber==1)&(TotalActualLeverPresses==0), 
                   LeverPressBouts.list[[UniqueID]] <- 0, 
                   LeverPressBouts.list[[UniqueID]] <- LPBoutNumber
                ) 
            LeverPressBouts.list[[UniqueID]] = LPBoutNumber
    
  }
    
    #Create data frame of Summary tab table  
      SummaryData = data.frame(
        #Animal Information
        UniqueID = unlist(UniqueIDs.list),
        ExperimentDate = unlist(ExperimentDates.list),
        SubjectID = unlist(SubjectIDs.list),
        GroupNumber = unlist(GroupNumbers.list),
        BoxNumber = unlist(BoxNumbers.list),
        StartTime = unlist(StartTimes.list),
        #LP Information
        FirstLeverPressTime = unlist(FirstLeverPressTimes.list),
        LastLeverPressTime = unlist(LastLeverPressTimes.list),
        LatencyToFirstLP = unlist(LatencyToFirstLP.list),
        SessionStartToLastLP = unlist(SessionStartToLastLP.list),
        TimeBetweenFirstandLastLP= unlist(TimeBetweenFirstandLastLP.list),
        LeverPressBouts = unlist(LeverPressBouts.list),
        TotalActualLeverPresses = unlist(TotalActualLeverPresses.list),
        RRLeverPresses = unlist(RRLeverPresses.list),
        LPRate_SessionStarttoLastLP = unlist(LPRate_SessionStarttoLastLP.list),
        LPRate_FirsttoLastLP = unlist(LPRate_FirsttoLastLP.list),
        #Lick Information       
        FirstLickTime = unlist(FirstLickTimes.list),
        LastLickTime = unlist(LastLickTimes.list),
        LatencyToFirstLick = unlist(LatencyToFirstLick.list),
        TimeBetweenFirstandLastLick = unlist(TimeBetweenFirstandLastLick.list),
        TotalLickTime = unlist(TotalLickTime.list),
        LickBouts = unlist(LickBouts.list),
        LickBout_Nontrivial = unlist(LickBout_Nontrivial.list),
        TotalLicks = unlist(TotalLicks.list),
        LickRate_FirsttoLastLick = unlist(LickRate_FirsttoLastLick.list), 
        LickRate_TotalLickTime = unlist(LickRate_TotalLickTime.list),   
        FirstBoutDuration_Percentage = unlist(FirstBoutDuration_Percentage.list),
        FirstBoutLick_Percentage = unlist(FirstBoutLick_Percentage.list),
        FirstBoutRate = unlist(FirstBoutRate.list),
        LicksHalfMax = unlist(Lick50PercentTime.list)
      )
      
      output$table = DT::renderDataTable(
        SummaryData,
        server = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'tB',
          buttons = 
            list(
                 list(extend = 'excel',
                      title = "temporary_file_summary.xlsx")),
          "pageLength" = 30
          ),
        class = "display"
      )
   
  #Updating Drop Down Menu to Select what CRs to View   
    updateSelectizeInput(
       inputId = "selected_uniqueID",
       label = "Select Unique ID:",
       choices = unique(SummaryData$UniqueID)
      )
    
  #Creating CRs, depends on the selected input: Unique ID
    plot_output <- reactive({
      n <- input$selected_uniqueID
      #Repeating this line is important because the only lps/licks line above gets overridden for each id
      plotlps <- HMSDataFrames.list[[n]][HMSDataFrames.list[[n]]$ActionAtTimestamp == "LeverPress", ]
      plotlicks <- HMSDataFrames.list[[n]][HMSDataFrames.list[[n]]$ActionAtTimestamp == "Lick", ]

      a = ggplot(plotlps, aes(x=CumulativeTime_Minutes, y=Counter)) +
        geom_step(size=1, show.legend = FALSE, color="coral") +
        scale_x_continuous(limits=c(0,20), breaks = seq(0,20,1)) +
        xlab("Time (min)") +
        ylab("Number of Lever Presses") +
        theme_minimal()

      b = ggplot(plotlicks, aes(x=CumulativeTime_Minutes, y=Counter,color="blue")) +
        geom_step(size=1, show.legend = FALSE, color="deepskyblue") +
        scale_x_continuous(limits=c(0,40)) +
        xlab("Time (min)") +
        ylab("Number of Licks") +
        theme_minimal()

      #Create final plot with LP and Lick CRs side by side:
      g <- grid.arrange(a, b, nrow=1, top=paste0("Lever Press and Lick Profiles: ",n))

    })
    
    #Display the Selected CR
    output$plot <- renderPlot({
      plot_output()
    })
    
    #Download This CR Button
    output$downloadThisCR <- downloadHandler(filename = function(){
      paste(input$selected_uniqueID, ".", input$ftype, sep = "")
      
    },
    
    content = function(file){
      ggsave(plot_output(), 
             filename = file
            )
    # dev.off() #seems like I don't need this line
    }
    )
    
    #Creating table for LP Bins Table
    MegaLeverPressDataFrame = data.frame(cbind(bin_list_LP, bind_cols(BinnedLP.list)))
    
    output$lpbinstable = DT::renderDataTable(
      MegaLeverPressDataFrame,
      server = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'tB',
        buttons = 
          list(
            list(extend = 'excel',
                 title = "temporary_file_LPBins.xlsx")),
        "pageLength" = 50
      ),
      class = "display",
      rownames = FALSE 
    )
    
    #Creating table for Lick Bins Table
    MegaLickDataFrame = data.frame(cbind(bin_list_lick, bind_cols(BinnedLicks.list)))
    
    output$lickbinstable = DT::renderDataTable(
      MegaLickDataFrame,
      server = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'tB',
        buttons = 
          list(
            list(extend = 'excel',
                 title = "temporary_file_LickBins.xlsx")),
        "pageLength" = 50
      ),
      class = "display",
      rownames = FALSE 
    )
    
    #BOUTS
    #Updating Drop Down Menu to Select what CRs to View   
    updateSelectizeInput(
      inputId = "selected_uniqueID_forbouts",
      label = "Select Unique ID:",
      choices = unique(SummaryData$UniqueID)
    )
    
    licksperbout_table <- reactive({
      nn <- input$selected_uniqueID_forbouts
      #Repeating this line is important because the only lps/licks line above gets overridden for each id
      LickBoutNumber.list = list()
      LickBoutTime.list = list()
      LickBoutLicks.list = list()
      LickBoutNumber=0
      LickBoutTime=0
      LickBoutLicks=0
      for (row in 1:nrow(HMSDataFrames.list[[nn]])) 
      {
        if(!is.na(HMSDataFrames.list[[nn]]$ActionAtTimestamp[row]))
        {
          if(HMSDataFrames.list[[nn]]$ActionAtTimestamp[row]=="LickBoutStart")
          {
            LickBoutNumber.list = c(LickBoutNumber.list, LickBoutNumber)
            LickBoutTime.list = c(LickBoutTime.list, LickBoutTime)
            LickBoutLicks.list = c(LickBoutLicks.list, LickBoutLicks)
            LickBoutNumber=LickBoutNumber+1
            LickBoutTime=0
            LickBoutLicks=0
          }
          if(HMSDataFrames.list[[nn]]$ActionAtTimestamp[row]=="Lick")
          {
            LickBoutTime = round((LickBoutTime + HMSDataFrames.list[[nn]]$TimeSinceLastEvent_Seconds[row]), digits = 2) 
            LickBoutLicks = LickBoutLicks + 1 #Counts each lick within bout
          }
        } 
      }
      #add to the lists one final time because the last action recorded is a Lick
      LickBoutNumber.list = c(LickBoutNumber.list, LickBoutNumber)
      LickBoutTime.list = c(LickBoutTime.list, round(LickBoutTime, digits = 2))
      LickBoutLicks.list = c(LickBoutLicks.list, LickBoutLicks)
      
      print(data.frame(LickBoutNumber = unlist(LickBoutNumber.list),
                 LickBoutTime = unlist(LickBoutTime.list),
                 LickBoutLicks = unlist(LickBoutLicks.list)))
      
    })
    
    
    
    output$licksperbouttable = DT::renderDataTable(
      licksperbout_table(),
      server = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'tB',
        buttons = 
          list(
            list(extend = 'excel',
                 title = "temporary_file_LicksPerBout.xlsx")),
        "pageLength" = 50
      ),
      class = "display",
      rownames = FALSE 
    )
    
    lpsperbout_table <- reactive({
      nn <- input$selected_uniqueID_forbouts
      #Repeating this line is important because the only lps/licks line above gets overridden for each id
      LPBoutNumber.list = list()
      LPBoutTime.list = list()
      LPBoutLPs.list = list()
      LPBoutNumber=1
      LPBoutTime=0
      LPBoutLPs=0

      for (row in 2:nrow(HMSDataFrames.list[[nn]]))
      {
        if(!is.na(HMSDataFrames.list[[nn]]$ActionAtTimestamp[row]))
        {
          if(HMSDataFrames.list[[nn]]$ActionAtTimestamp[row]=="LeverPress")
          {
            if(HMSDataFrames.list[[nn]]$TimeSinceLastEvent_Seconds[row]>=input$BoutLength)
            {
            LPBoutNumber.list = c(LPBoutNumber.list, LPBoutNumber)
            LPBoutTime.list = c(LPBoutTime.list, LPBoutTime)
            LPBoutLPs.list = c(LPBoutLPs.list, LPBoutLPs)
            LPBoutNumber=LPBoutNumber+1
            LPBoutTime=0
            LPBoutLPs= 1
            }
            
            if(HMSDataFrames.list[[nn]]$TimeSinceLastEvent_Seconds[row]<input$BoutLength)
            {
              LPBoutTime = LPBoutTime + HMSDataFrames.list[[nn]]$TimeSinceLastEvent_Seconds[row] 
              LPBoutLPs = LPBoutLPs + 1 #Counts each LP within bout
          }
        }
        }
      }
      LPBoutNumber.list = c(LPBoutNumber.list, LPBoutNumber)
      LPBoutTime.list = c(LPBoutTime.list, LPBoutTime)
      LPBoutLPs.list = c(LPBoutLPs.list, LPBoutLPs) 
      
      print(data.frame(LPBoutNumber = unlist(LPBoutNumber.list),
                       LPBoutTime = unlist(LPBoutTime.list),
                       LPBoutLPs = unlist(LPBoutLPs.list)))
      
      
    })
    
    
    
    output$lpsperbouttable = DT::renderDataTable(
      lpsperbout_table(),
      server = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'tB',
        buttons = 
          list(
            list(extend = 'excel',
                 title = "temporary_file_LicksPerBout.xlsx")),
        "pageLength" = 50
      ),
      class = "display",
      rownames = FALSE 
    )
    
    
    
    }
    #Saving File Path for CR

     Thesheets_dir <- reactive({
       volumes = getVolumes()
       shinyDirChoose(input,
                      "folder",
                      roots = volumes()) #needs this to be a function
       filepaths <- parseDirPath(roots = volumes, input$folder)
       return(filepaths)
     })
 
     output$filepaths <- renderPrint({
       Thesheets_dir()
       })
 })
  
  #Create data frame of Summary tab table
  ListofVariables <- c(
    "UniqueID",
    "ExperimentDate",
    "SubjectID",
    "GroupNumber",
    "BoxNumber",
    "StartTime",
    "FirstLeverPressTime",
    "LastLeverPressTime",
    "LatencyToFirstLP",
    "SessionStartToLastLP",
    "TimeBetweenFirstandLastLP",
    "LeverPressBouts",
    "TotalActualLeverPresses",
    "RRLeverPresses",
    "LPRate_SessionStarttoLastLP",
    "LPRate_FirsttoLastLP",
    "FirstLickTime",
    "LastLickTime",
    "LatencyToFirstLick",
    "TimeBetweenFirstandLastLick",
    "TotalLickTime",
    "LickBouts",
    "LickBout_Nontrivial",
    "TotalLicks",
    "LickRate_FirsttoLastLick",
    "LickRate_TotalLickTime",
    "FirstBoutDuration_Percentage",
    "FirstBoutLick_Percentage",
    "LicksHalfMax"
  )
  
  DefsofVariables <- c(
    " SubjectID_ExperimentDate",
    "Date MedPC file was generated, per computer's date information",
    "SubjectID; user-defined in Med-PC",
    "Group Number; user-defined in Med-PC",
    "Box Number; assigned in Med-PC Smart Control card",
    "Time (H:M:S) start signal was sent to box, per computer's time information ",
    "Time (H:M:S) of first lever press, per computer's time information",
    "Time (H:M:S) of last lever press, per computer's time information",
    "Cumulative time (seconds) elapsed from session start until first lever press",
    "Cumulative time (seconds) elapsed from session start until last lever press",
    "Time (seconds) between the first and last lever press",
    "Number of runs animal was pressing the lever without a 20 second pause between lever pressing",
    "Total number of lever presses within the session",
    "Number of lever presses animal completed before sipper was presented;
                       Value could differ from TotalActualLeverPresses if rat played with retracted lever during drinking session AND 'RemoveLPsAfterLicks' is selected ",
    "Lever pressing rate: # of lever presses per minute, time based on difference between start of session and time of last LP",
    "Lever pressing rate: # of lever presses per minute, time based on difference between time of first LP and time of last LP",
    "Time (H:M:S) of first lick, per your computer's time information",
    "Time (H:M:S) of last lick, per your computer's time information",
    " Cumulative time (seconds) elapsed from session start until first lick",
    "Time (seconds) between the first and last lick",
    "Cumulative time (seconds) rat spends making contact with sipper, excluding time elapsed between drinking bouts",
    "Number of runs animal was making contact with sipper without a 20 second pause between contacts",
    " Number of runs animal was making contact with sipper without a 20 second pause between contacts and number of events within this bout was greater than 50 (or user-defined value)",
    "Total number of licks as reported from MedPC;
                      Value can be different than Med-PC display if rat made contact with sipper tube before sipper was extended AND 'RemoveLicksAfterLPs' is selected",
    "Lick rate; number of licks per minute, time based on difference between first and last lick",
    "Lick rate; number of licks per minute, time based on total lick time (excluding time elapsed between drinking bouts)",
    "Percentage of duration (seconds) of first drinking bout compared to total time spent drinking",
    "Percentage of licks in first drinking bout compared to total number of licks",
    "Time (in sec.) elapsed until 50% of licks were completed"
  )
  
  FormulaForVar <- c(
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "SessionStartToLastLP-LatencyToFirstLP",
    "",
    "",
    "",
    "(RRLeverPresses / SessionStartToLastLP) * 60",
    "(RRLeverPresses /TimeBetweenFirstandLastLP)* 60",
    "",
    "",
    "",
    "",
    "",
    "",
    "sum(AccurateLickBoutTimes$LickBoutLicks > input$MinimumNumberofLicksForaBout)",
    "",
    "(TotalLicks/TimeBetweenFirstandLastLick)*60",
    "(TotalLicks/TotalLickTime)*60",
    "100*(AccurateLickBoutTimes$LickBoutTime / TotalLickTime)",
    "100*(AccurateLickBoutTimes$LickBoutLicks / sum(AccurateLickBoutTimes$LickBoutLicks)",
    ""
  )
  
  
  VariableTable = data.frame(
    ListofVariables,
    DefsofVariables,
    FormulaForVar
  )
  
  colnames(VariableTable) = c("Variable", "Description", "Formula")
  
  output$vartable = DT::renderDataTable(
    VariableTable,
    server = FALSE,
    options = list(
      dom = 'Bfrtip',
      buttons = 
        list(
          list(extend = 'excel',
               title = "temporary_file_summary.xlsx")),
      "pageLength" = 30
    ),
    class = "display",
    rownames = FALSE                ## don't show row numbers/name
  )
  
   output$OtherOptionsToNote <- renderUI(HTML(markdown::renderMarkdown(text = "Additional Options\n
  - Remove LPs After Licks: Select this box if subject presses lever after sipper extends into chamber (behavior irrelevant to appetitive behavior)\n
  - Remove Licks Before LP: Select this box if subject makes contact with sipper tube before sipper extends into chamber/response requirement is met.\n
  - Minimum Number of Licks for a 'NonTrivial' Bout: Used to create 'LickBout_NonTrivial' variable, described below\n"
           )))
 
}


# Run the app -------------------------------------------------------------
runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
