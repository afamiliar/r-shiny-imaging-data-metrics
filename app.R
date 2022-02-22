#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#   https://rstudio.github.io/shinydashboard/structure.html
#   https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/


# TO DO:
#   -- add filter for sessions ONLY containing file classifications (vs. ANY)
#   -- include vox dim? (if so, handle where vox dim = NA)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(DT)

# code for making horizontal scroll bar @ top of DT data table
css <- HTML(
  "#table > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #table > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }"
)

#setwd('~/Documents/R/shiny/imaging_parameters/cbtn_imaging_parameters')

# ==================== Load & prep data ==============================================================================

#histdata <- read.csv("~/Documents/R/shiny/imaging_parameters/cbtn_imaging_parameters/fw_table_4_shiny.csv", header = TRUE)
histdata <- read.csv("fw_table_4_shiny.csv", header = TRUE, stringsAsFactors=FALSE)

filtered_df <- histdata

# convert Magnetic field strengths to numbers
# harmonize and round values
#filtered_df = subset(filtered_df, magnetic_field_strength!="NULL")
filtered_df$magnetic_field_strength <- as.numeric(filtered_df$magnetic_field_strength)
filtered_df$magnetic_field_strength[filtered_df$magnetic_field_strength==15000]<-1.5
filtered_df$magnetic_field_strength<-round(filtered_df$magnetic_field_strength, digits = 1)
filtered_df$magnetic_field_strength[filtered_df$magnetic_field_strength==2.9]<-3

filtered_df$magnetic_field_strength[is.na(filtered_df$magnetic_field_strength)]<-"N/A"

# adjust modality labels
filtered_df$file_modality[filtered_df$file_modality=="PT"]<-"PET"

# adjust scanner labels
filtered_df$manufacturer[is.na(filtered_df$manufacturer)]<-"N/A"
filtered_df$model[is.na(filtered_df$model)]<-"N/A"
filtered_df$software_ver[is.na(filtered_df$software_ver)]<-"N/A"
filtered_df$manufacturer[filtered_df$manufacturer==""]<-"N/A"
filtered_df$model[filtered_df$model==""]<-"N/A"
filtered_df$software_ver[filtered_df$software_ver==""]<-"N/A"

# adjust sdg-id labels
filtered_df$sdg_id[filtered_df$sdg_id==""]<-"N/A"

# adjust body part labels
filtered_df$body_part_examined[filtered_df$body_part_examined=="sinus"]<-"sinuses"

# round ages
filtered_df$age_at_imaging_in_years<-round(filtered_df$age_at_imaging_in_years, digits = 1)

# convert voxel dimension fields to numbers
#filtered_df$dim1 <- as.numeric(filtered_df$dim1)
#filtered_df$dim2 <- as.numeric(filtered_df$dim2)

# construct 'coded' molec-status columns
filtered_df$WGS_coded = filtered_df$WGS
filtered_df$RNAseq_coded = filtered_df$RNAseq
filtered_df$clin_event_coded = filtered_df$clin_event
filtered_df$clin_other_coded = filtered_df$clin_other

filtered_df$WGS_coded[filtered_df$WGS_coded=="Yes"] <- "WGS"
filtered_df$RNAseq_coded[filtered_df$RNAseq_coded=="Yes"] <- "RNAseq"
filtered_df$clin_event_coded[filtered_df$clin_event_coded=="Yes"] <- "Clinical Seq for Event"
filtered_df$clin_other_coded[filtered_df$clin_other_coded=="Yes"] <- "Clinical Seq Other"

# combine FW classifications into 1 label
filtered_df$fw_class <- paste(filtered_df$file_classification_intent,
                              filtered_df$file_classification_measurement,
                              filtered_df$file_classification_features)
filtered_df$fw_class[filtered_df$fw_class=="  "]<-"None"

# date <- as.Date(as.character(date), format = "%Y-%m-%d")

# separate projects
df_corsica = subset(filtered_df, project_label=="Corsica")
df_cbtn = subset(filtered_df, project_label!="Corsica")

# adjust body-part labels
df_cbtn$body_part_examined[df_cbtn$body_part_examined=="iac"]<-"brain (iac)"
df_cbtn$body_part_examined[df_cbtn$body_part_examined=="pituitary"]<-"brain (pituitary)"
df_cbtn$body_part_examined[df_cbtn$body_part_examined=="face"]<-"brain (face)"

# filter text
cbtn_diagnoses <- as.character(unique(sort(df_cbtn$project_label)))
cbtn_file_types <- unique(sort(df_cbtn$fw_class,decreasing = TRUE,))
cbtn_body_parts <- unique(sort(df_cbtn$body_part_examined))
cbtn_manufacturer <- unique(sort(df_cbtn$manufacturer))
cbtn_model <- unique(sort(df_cbtn$model))
cbtn_software <- unique(sort(df_cbtn$software_ver))

corsica_diagnoses <- as.character(unique(sort(df_corsica$project_label)))
corsica_file_types <- unique(sort(df_corsica$fw_class,decreasing = TRUE,))
corsica_body_parts <- unique(sort(df_corsica$body_part_examined))
corsica_manufacturer <- unique(sort(df_corsica$manufacturer))
corsica_model <- unique(sort(df_corsica$model))
corsica_software <- unique(sort(df_corsica$software_ver))

# ==================================================================================================
ui <- dashboardPage(

  # ========= Dashboard Header ==========
  dashboardHeader(title = "Flywheel Data Metrics"),
  
  # ========= Dashboard Sidebar ==========
  dashboardSidebar(
    sidebarMenu( id = "sidebarmenu",
      menuItem("Overview", tabName = "overview_tab", icon = icon("home")),
      menuItem("CBTN", tabName = "cbtn_imaging", icon = icon("table")),
      menuItem("CORSICA", tabName = "corsica_imaging", icon = icon("table"))
    )
  ),
  
  # ========= Dashboard Body ==========
  dashboardBody(
    tabItems(
      # ******** first tab page ******** ******** ******** ******** ******** ******** 
      tabItem(tabName = "overview_tab",
              # titlePanel("Overview"),
              fluidRow(
                box( width = 6,
                     uiOutput(outputId = "d3b_logo") )
              ),
              
              # mainPanel(
                fluidRow(column(11,
                  div("This application is designed to enable users to explore and curate data hosted on the D3b-Flywheel site.",
                    style = "font-size:20px"),
                  br(),
                  div(HTML("For each available dataset there is a table of relevant subject, session, and image information."),
                      style = "font-size:18px"),
                  br(),
                  div("The results in the table can be filtered by the user according to the properties listed below.",
                      style = "font-size:18px"),
                  br(),
                  div(HTML("The results can be exported to an Excel file by clicking the <em><q>Download Current Page</q></em> button. In order to export the entire table, make sure to set the <em><q>Show</q></em> dropdown menu to <em><q>All</q></em> before downloading. Note that this may take a few moments to process depending on the size of the table."),
                      style = "font-size:18px"),
                  br()
                  ) # col
                  ), # fluidRow
                fluidRow(column(11,
                                p(strong("Description of Session Filters:",
                                    style = "font-size:18px")),
                  ) # col
                ), # fluidRow
                fluidRow(column(11, offset = 1,
                                div(HTML("<u><em>Diagnosis:</em></u> Histological diagnosis (based on WHO classifiation for pediatric tumors)."),
                                  style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Molecular sequencing availability:</em></u> Whether there is DNA (whole genome seq), RNA, and/or clinical sequencing available for surgically collected biospecimens around the time of imaging (+/- 100 days). <br> <b>NOTE:</b> if OR is selected, will include cases with any of the selected methods available, if AND is selected, will include cases with all of the selected methods available."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Scan modalities:</em></u> Modality of scanner on which images were acquired."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Magnetic field strength:</em></u> Strength of magnetic field (for MR scans) in teslas (T)."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Imaging event:</em></u> Whether a given imaging event was collected prior to any treatment (Pre-treatment), during or after some form of treatment (Post or during treatment). <br> <b>NOTE:</b> If there is no available treatment info, the event is declared undetermined. These time points can include subjects who have not undergone treatment as well as those without treatment dates available in our database but could have potentially undergone treatment (Undetermined)."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Age range:</em></u> Age (in years) at time of imaging (range specifies dates to include)."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Body part examined:</em></u> Body part scanned during the imaging exam."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Scanner Manufacturer:</em></u> Manufacturer of the scanning equipment for a given imaging exam."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Scanner Model:</em></u> Model name of the scanning equipment for a given imaging exam."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>Scanner Software Version:</em></u> Software version designated by the manufacturer for a given imaging exam."),
                                    style = "font-size:16px"),
                                br(),
                ) # col
                ), # fluidRow
                fluidRow(column(11,
                                p(strong("Description of Acquisition Filters:",
                                         style = "font-size:18px")),
                  ) # col
                ), # fluidRow
                fluidRow(column(11, offset=1,
                                div(HTML("<u><em>Acquisition type:</em></u> Classification of the type of an acquisition based on text in the file name (file names are derived from the SeriesDescription field in original DICOMs). <br> <b>NOTE:</b> Sessions with any of the selected file types will be included."),
                                    style = "font-size:16px"),
                                br(),
                                div(HTML("<u><em>File added to Flywheel:</em></u> Date that a file was uploaded to Flywheel (range specifies dates to include)."),
                                    style = "font-size:16px")
                                # br(),
                                # div(HTML("<u><em>File modified on Flywheel:</em></u> Date that a file was modified on Flywheel (range specifies dates to include). Modification can include updates to file information and classifications."),
                                #     style = "font-size:16px"),
                                #                   
                                ) # col
                ), # fluidRow
              # ), # mainPanel
      ), # tabItem
      
      # ******** second tab page ******** ******** ******** ******** ******** ******** 
      tabItem(tabName = "cbtn_imaging",
              # titlePanel("Children's Brain Tumor Network (Radiology)"),
              fluidRow(
                box( width = 8,
                     uiOutput(outputId = "cbtn_logo") )
              ),
              
              # summary # boxes
              fluidRow(
                valueBoxOutput("subjects_w_img_cbtn"),
                valueBoxOutput("sessions_w_img_cbtn")
              ),
              
              # left-hand filters
              sidebarPanel(width = 3,
                pickerInput("fw_proj","Diagnosis", 
                            choices=cbtn_diagnoses,
                            selected=cbtn_diagnoses,
                            options = list(`actions-box` = TRUE),
                            multiple = T),

                pickerInput("molec_status","Molecular sequencing availability", 
                            choices=c("WGS","RNAseq","Clinical Seq for Event","Clinical Seq Other"),
                            selected=c(),
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                selectInput("filter_join1", label = "", choices = c("OR","AND")),
                
                # fluidRow(
                  checkboxGroupInput(inputId = "ModalityFinder",
                                     label = "Scan Modalities",
                                     choices = unique(df_cbtn$file_modality),
                                     selected = unique(df_cbtn$file_modality)
                  ),
                  
                  checkboxGroupInput(inputId = "EventFinder",
                                     label = "Event",
                                     choices = unique(df_cbtn$event_label),
                                     selected = unique(df_cbtn$event_label)
                  ),

                  checkboxGroupInput(inputId = "FieldStrengthFinder",
                                     label = "Magnetic Field Strength",
                                     choices = unique(sort(df_cbtn$magnetic_field_strength,decreasing = TRUE,)),
                                     selected = unique(sort(df_cbtn$magnetic_field_strength,decreasing = TRUE,))
                  ),
                
                  pickerInput(inputId = "ManufacturerFinder",
                                     label = "Scanner Manufacturer",
                                     choices = cbtn_manufacturer,
                                     selected = cbtn_manufacturer,
                                     options = list(`actions-box` = TRUE),
                                     multiple = T
                  ),
                  pickerInput(inputId = "ModelFinder",
                              label = "Scanner Model",
                              choices = cbtn_model,
                              selected = cbtn_model,
                              options = list(`actions-box` = TRUE),
                              multiple = T
                  ),
                  pickerInput(inputId = "SoftwareFinder",
                              label = "Scanner Software Version",
                              choices = cbtn_software,
                              selected = cbtn_software,
                              options = list(`actions-box` = TRUE),
                              multiple = T
                  ),

                  sliderInput("age_range",
                              "Age at imaging (years)",
                              min = min(df_cbtn$age_at_imaging_in_years),
                              max = max(df_cbtn$age_at_imaging_in_years),
                              value = c(min(df_cbtn$age_at_imaging_in_years),
                                        max(df_cbtn$age_at_imaging_in_years)),
                              sep = "",),
                  
                  dateRangeInput('file_created_range',
                                 label = 'File added to Flywheel (yyyy-mm-dd)',
                                 start = min(as.Date(df_cbtn$file_created)),
                                 end = Sys.Date(), # today
                                 min = min(as.Date(df_cbtn$file_created)),
                                 max = Sys.Date()
                  ),
                  actionButton("resetFileCreate", "Reset"),
                  br(),
                  br(),
                  # dateRangeInput('file_mod_range',
                  #                label = 'File modified on Flywheel (yyyy-mm-dd)',
                  #                start = min(as.Date(df_cbtn$file_modified)),
                  #                end = Sys.Date(), # today
                  #                min = min(as.Date(df_cbtn$file_modified)),
                  #                max = Sys.Date()
                  # ),
                  # actionButton("resetFileMod", "Reset"),
                  # br(),
                  # br(),
                
                  # sliderInput("dim1_range",
                  #             "Voxel size: dim1",
                  #             min = min(df_cbtn$dim1, na.rm = TRUE),  
                  #             max = max(df_cbtn$dim1, na.rm = TRUE), 
                  #             value = c(min(df_cbtn$dim1, na.rm = TRUE), 
                  #                       max(df_cbtn$dim1, na.rm = TRUE)),
                  #             sep = "",),
                  # sliderInput("dim2_range",
                  #             "Voxel size: dim2",
                  #             min = min(df_cbtn$dim2, na.rm = TRUE),  
                  #             max = max(df_cbtn$dim2, na.rm = TRUE), 
                  #             value = c(min(df_cbtn$dim2, na.rm = TRUE), 
                  #                       max(df_cbtn$dim2, na.rm = TRUE)),
                  #             sep = "",),
                  
                  actionLink("selectallbody","Select/deselect All Body Parts"),
                  checkboxGroupInput(inputId = "BodyPartFinder",
                                     label = "Body part examined",
                                     choices = cbtn_body_parts,
                                     selected = cbtn_body_parts
                  ),
                  
                  actionLink("selectall","Select/deselect All File Types"),
                  selectInput("filter_join2", label = "", choices = c("ANY","ALL")),
                  checkboxGroupInput(inputId = "ClassificationFinder",
                                     label = "Acquisition type",
                                     choices = cbtn_file_types,
                                     selected = cbtn_file_types)
                # )
              ),
              
              # Data Table
              tags$head(tags$style(css)), # horiz scroll bar
              fluidRow(
                column(8, # width
                       DT::dataTableOutput('cbtn_table')
                )
              )
      ), # second tab page
      
      # ******** third tab page ******** ******** ******** ******** ******** ******** 
      tabItem(tabName = "corsica_imaging",
              # titlePanel("CORSICA (Radiology)"),
              fluidRow(
                box( width = 8,
                     uiOutput(outputId = "corsica_logo") )
              ),
              
              # summary # boxes
              fluidRow(
                valueBoxOutput("subjects_w_img_corsica"),
                valueBoxOutput("sessions_w_img_corsica")
              ),
              
              # left-hand filters
              sidebarPanel(width = 3,
                           pickerInput("fw_proj2","Diagnosis", 
                                       choices=corsica_diagnoses,
                                       selected=corsica_diagnoses,
                                       options = list(`actions-box` = TRUE),
                                       multiple = T),
                           
                           # pickerInput("molec_status","Molecular sequencing availability", 
                           #             choices=c("WGS","RNAseq","Clinical Seq for Event","Clinical Seq Other"),
                           #             selected=c(),
                           #             options = list(`actions-box` = TRUE),
                           #             multiple = T),
                           # selectInput("filter_join1", label = "", choices = c("OR","AND")),
                           
                           # fluidRow(
                           checkboxGroupInput(inputId = "ModalityFinder2",
                                              label = "Scan Modalities",
                                              choices = unique(df_corsica$file_modality),
                                              selected = unique(df_corsica$file_modality)
                           ),
                           
                           # checkboxGroupInput(inputId = "EventFinder",
                           #                    label = "Event",
                           #                    choices = unique(df_corsica$event_label),
                           #                    selected = unique(df_corsica$event_label)
                           # ),
                           
                           checkboxGroupInput(inputId = "FieldStrengthFinder2",
                                              label = "Magnetic Field Strength",
                                              choices = unique(sort(df_corsica$magnetic_field_strength,decreasing = TRUE,)),
                                              selected = unique(sort(df_corsica$magnetic_field_strength,decreasing = TRUE,))
                           ),
                           
                           pickerInput(inputId = "ManufacturerFinder2",
                                       label = "Scanner Manufacturer",
                                       choices = corsica_manufacturer,
                                       selected = corsica_manufacturer,
                                       options = list(`actions-box` = TRUE),
                                       multiple = T
                           ),
                           pickerInput(inputId = "ModelFinder2",
                                       label = "Scanner Model",
                                       choices = corsica_model,
                                       selected = corsica_model,
                                       options = list(`actions-box` = TRUE),
                                       multiple = T
                           ),
                           pickerInput(inputId = "SoftwareFinder2",
                                       label = "Scanner Software Version",
                                       choices = corsica_software,
                                       selected = corsica_software,
                                       options = list(`actions-box` = TRUE),
                                       multiple = T
                           ),
                           
                           sliderInput("age_range2",
                                       "Age at imaging (years)",
                                       min = min(df_corsica$age_at_imaging_in_years),
                                       max = max(df_corsica$age_at_imaging_in_years),
                                       value = c(min(df_corsica$age_at_imaging_in_years),
                                                 max(df_corsica$age_at_imaging_in_years)),
                                       sep = "",),
                           
                           dateRangeInput('file_created_range2',
                                          label = 'File added to Flywheel (yyyy-mm-dd)',
                                          start = min(as.Date(df_corsica$file_created)),
                                          end = Sys.Date(), # today
                                          min = min(as.Date(df_corsica$file_created)),
                                          max = Sys.Date()
                           ),
                           actionButton("resetFileCreate2", "Reset"),
                           br(),
                           br(),

                           actionLink("selectallbody2","Select/deselect All Body Parts"),
                           checkboxGroupInput(inputId = "BodyPartFinder2",
                                              label = "Body part examined",
                                              choices = corsica_body_parts,
                                              selected = corsica_body_parts
                           # ),
                           
                           # actionLink("selectall2","Select/deselect All File Types"),
                           # checkboxGroupInput(inputId = "ClassificationFinder2",
                           #                    label = "Acquisition type",
                           #                    choices = corsica_file_types,
                           #                    selected = corsica_file_types
                           )
              ),
              
              # Data Table
              tags$head(tags$style(css)), # horiz scroll bar
              fluidRow(
                column(8, # width
                       DT::dataTableOutput('corsica_table')
                )
              )
      ) # third tab page
    ) # tabItems
  ) # dashboardBody

)

# ==================================================================================================

# Define server logic
server <- function(input, output, session) {

  # configure the "select all" for file classification filter
  observe({
    if(input$selectall == 0) return(NULL)
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"ClassificationFinder","Acquisition type",choices=cbtn_file_types)
    }
    else
    {
      updateCheckboxGroupInput(session,"ClassificationFinder","Acquisition type",choices=cbtn_file_types,selected=cbtn_file_types)
    }
  })

  # configure the "select all" for body-part filter
  observe({
    if(input$selectallbody == 0) return(NULL)
    else if (input$selectallbody%%2 == 0)
    {
      updateCheckboxGroupInput(session,"BodyPartFinder","Body part examined",choices=cbtn_body_parts)
    }
    else
    {
      updateCheckboxGroupInput(session,"BodyPartFinder","Body part examined",choices=cbtn_body_parts, selected=cbtn_body_parts)
    }
  })

  # dynamic date range
  observeEvent(input$resetFileCreate, {
    updateDateRangeInput(session,
                    "file_created_range",
                    start = min(as.Date(df_cbtn$file_created)),
                    end = Sys.Date(),
                    min = min(as.Date(df_cbtn$file_created)),
                    max = Sys.Date()
                    )
  })

  # dynamic scanner manufacturer filter (based on Diagnosis)
  observeEvent(D0(),{
    updatePickerInput(session,"ManufacturerFinder",choices = unique(D0()$manufacturer),selected = unique(D0()$manufacturer))
  })
  D0  <- reactive({
    df_cbtn[df_cbtn$project_label %in% input$fw_proj,]
  })
  
  # dynamic scanner model filter (based on Diagnosis & Manufacturer)
  observeEvent(D1(),{
    updatePickerInput(session,"ModelFinder",choices = unique(D1()$model),selected = unique(D1()$model))
  })
  D1  <- reactive({
    filter(df_cbtn, project_label %in% input$fw_proj ) %>%
      filter(manufacturer %in% input$ManufacturerFinder )
  })

  # dynamic scanner software filter (based on Diagnosis, Manufacturer, & Model)
  observeEvent(D2(),{
    updatePickerInput(session,"SoftwareFinder",choices = unique(D2()$software),selected = unique(D2()$software))
  })
  D2  <- reactive({
    filter(df_cbtn, project_label %in% input$fw_proj ) %>%
      filter(manufacturer %in% input$ManufacturerFinder ) %>%
      filter(model %in% input$ModelFinder )
  })
  
  # filter the input dataframe
  final_df_cbtn <- reactive({
    r_df <- filter(df_cbtn, project_label %in% input$fw_proj ) %>%
            filter(file_modality %in% input$ModalityFinder ) %>%
            filter(event_label %in% input$EventFinder ) %>%
            filter(body_part_examined %in% input$BodyPartFinder ) %>%
            filter(magnetic_field_strength %in% input$FieldStrengthFinder ) %>%
            filter(manufacturer %in% input$ManufacturerFinder ) %>%
            filter(model %in% input$ModelFinder ) %>%
            filter(software_ver %in% input$SoftwareFinder )
      
    # filter files based on classifications
    if (input$filter_join2=="ANY"){
      # includes any sessions w/selected file types
      r_df <- filter(r_df, fw_class %in% input$ClassificationFinder )
    } else {
      # includes all sessions w/selected file types
      r_df <- r_df %>% group_by(session_label) %>%
                  filter(fw_class %in% input$ClassificationFinder )
    }

    wgs <- gsub("'", "", r_df$WGS_coded)
    rna <- gsub("'", "", r_df$RNAseq_coded)
    clin1 <- gsub("'", "", r_df$clin_event_coded)
    clin2 <- gsub("'", "", r_df$clin_other_coded)
    
    # molec seq filter
    if (!is.null(input$molec_status)){
      # print(input$molec_status)
      join1 <- ifelse(test = input$filter_join1 == "OR", yes = "| ", no = "& ")
      if (join1=="| "){
        cond_str <- paste0(
                "with(input, ",
                paste0("wgs %in% ", "c(", paste0("'", input$molec_status, collapse = "',"), "')", colapse = " "),
                join1,
                paste0("rna %in% ", "c(", paste0("'", input$molec_status, collapse = "',"), "')", colapse = " "),
                join1,
                paste0("clin1 %in% ", "c(", paste0("'", input$molec_status, collapse = "',"), "')", colapse = " "),
                join1,
                paste0("clin2 %in% ", "c(", paste0("'", input$molec_status, collapse = "',"), "')", colapse = " "),
                ")")
        cond <- parse(text = cond_str)
        r_df <- as.data.frame(r_df)[eval(cond), ]
        
        } else {
            if('WGS' %in% input$molec_status){
              r_df <- r_df[r_df$WGS == "Yes",]
            }
            if('RNAseq' %in% input$molec_status){
              r_df <- r_df[r_df$RNAseq == "Yes",]
            }
            if('Clinical Seq for Event' %in% input$molec_status){
              r_df <- r_df[r_df$clin_event == "Yes",]
            }
            if('Clinical Seq Other' %in% input$molec_status){
              r_df <- r_df[r_df$clin_other == "Yes",]
            }
        }
      }

    # age-at-imaging filter
    r_df <- r_df[r_df$age_at_imaging_in_years >= input$age_range[1] & r_df$age_at_imaging_in_years <= input$age_range[2],]

    # file dates filter
    r_df <- r_df[r_df$file_created >= input$file_created_range[1] & r_df$file_created <= input$file_created_range[2],]
    # r_df <- r_df[r_df$file_modified >= input$file_mod_range[1] & r_df$file_modified <= input$file_mod_range[2],]
    
    # voxel sizes filter
    # r_df <- r_df[r_df$dim1 >= input$dim1_range[1] & r_df$dim1 <= input$dim1_range[2],]
    # r_df <- r_df[r_df$dim2 >= input$dim2_range[1] & r_df$dim2 <= input$dim2_range[2],]

    # clean up & output table
#    r_df = subset(r_df, select = c("project_label","subject_label","session_label",
                                    # "acquisition_label","file_modality","magnetic_field_strength",
                                    # "fw_class","dim1","dim2","event_label",'body_part_examined') )
    r_df = subset(r_df, select = c("project_label","subject_label","session_label",
                                   "acquisition_label","file_modality","magnetic_field_strength",
                                   "fw_class","event_label",'body_part_examined') )
    r_df <- r_df[!duplicated(r_df), ]
    r_df <- r_df[order(r_df$subject_label,r_df$session_label,r_df$acquisition_label), ]
    r_df
  }) # reactive

  ## ****************** CORSICA DF ******************  ******************  ****************** 
  # configure the "select all" for file classification filter
  # observe({
  #   if(input$selectall2 == 0) return(NULL)
  #   else if (input$selectall2%%2 == 0)
  #   {
  #     updateCheckboxGroupInput(session,"ClassificationFinder2","Acquisition type",choices=corsica_file_types)
  #   }
  #   else
  #   {
  #     updateCheckboxGroupInput(session,"ClassificationFinder2","Acquisition type",choices=corsica_file_types,selected=corsica_file_types)
  #   }
  # })
  
  # configure the "select all" for body-part filter
  observe({
    if(input$selectallbody2 == 0) return(NULL)
    else if (input$selectallbody2%%2 == 0)
    {
      updateCheckboxGroupInput(session,"BodyPartFinder2","Body part examined",choices=corsica_body_parts)
    }
    else
    {
      updateCheckboxGroupInput(session,"BodyPartFinder2","Body part examined",choices=corsica_body_parts, selected=corsica_body_parts)
    }
  })
  
  # dynamic date range filter
  observeEvent(input$resetFileCreate2, {
    updateDateRangeInput(session,
                         "file_created_range2",
                         start = min(as.Date(df_corsica$file_created)),
                         end = Sys.Date(),
                         min = min(as.Date(df_corsica$file_created)),
                         max = Sys.Date()
    )
  })
  
  # dynamic scanner manufacturer filter (based on Diagnosis)
  observeEvent(D3(),{
    updatePickerInput(session,"ManufacturerFinder2",choices = unique(D3()$manufacturer),selected = unique(D3()$manufacturer))
  })
  D3  <- reactive({
    df_corsica[df_corsica$project_label %in% input$fw_proj2,]
  })
  
  # dynamic scanner model filter (based on Diagnosis & Manufacturer)
  observeEvent(D4(),{
    updatePickerInput(session,"ModelFinder2",choices = unique(D4()$model),selected = unique(D4()$model))
  })
  D4  <- reactive({
    filter(df_corsica, project_label %in% input$fw_proj2 ) %>%
      filter(manufacturer %in% input$ManufacturerFinder2 )
  })
  
  # dynamic scanner software filter (based on Diagnosis, Manufacturer, & Model)
  observeEvent(D5(),{
    updatePickerInput(session,"SoftwareFinder2",choices = unique(D5()$software),selected = unique(D5()$software))
  })
  D5  <- reactive({
    filter(df_corsica, project_label %in% input$fw_proj2 ) %>%
      filter(manufacturer %in% input$ManufacturerFinder2 ) %>%
      filter(model %in% input$ModelFinder2 )
  })
  
  # filter the dataframe
  final_df_corsica <- reactive({
    r_df <- filter(df_corsica, project_label %in% input$fw_proj2 ) %>%
      filter(file_modality %in% input$ModalityFinder2 ) %>%
      # filter(event_label %in% input$EventFinder2 ) %>%
      filter(body_part_examined %in% input$BodyPartFinder2 ) %>%
      filter(magnetic_field_strength %in% input$FieldStrengthFinder2 ) %>%
      filter(manufacturer %in% input$ManufacturerFinder2 ) %>%
      filter(model %in% input$ModelFinder2 ) %>%
      filter(software_ver %in% input$SoftwareFinder2 )
    
    # age-at-imaging filter
    r_df <- r_df[r_df$age_at_imaging_in_years >= input$age_range2[1] & r_df$age_at_imaging_in_years <= input$age_range2[2],]
    
    # file dates filter
    r_df <- r_df[r_df$file_created >= input$file_created_range2[1] & r_df$file_created <= input$file_created_range2[2],]

    # clean up & output table
    #    r_df = subset(r_df, select = c("project_label","subject_label","session_label",
    # "acquisition_label","file_modality","magnetic_field_strength",
    # "fw_class","dim1","dim2","event_label",'body_part_examined') )
    r_df = subset(r_df, select = c("project_label","subject_label","session_label","sdg_id",
                                   "acquisition_label","file_modality","magnetic_field_strength",
                                   "fw_class",'body_part_examined') )
                            # "event_label"
    r_df <- r_df[!duplicated(r_df), ]
    r_df <- r_df[order(r_df$subject_label,r_df$session_label,r_df$acquisition_label), ]
    r_df
  }) # reactive
  
  # ********** render stuff ****************************************************************************************
  # render images
  output$cbtn_logo<-renderUI({
    img(src='cbtn_logo.png', height = '150px')
  })
  output$d3b_logo<-renderUI({
    img(src='d3b_logo_cropped.jpg', height = '150px')
  })
  output$corsica_logo<-renderUI({
    img(src='corsica_logo.png', height = '150px')
  })
  
  output$cbtn_table <- DT::renderDT( { #server = FALSE, { # if server = TRUE, then Download-all button doesn't work (but could remove it and keep Download-current-page w/"All" menu length)
    df <- final_df_cbtn()
    dtable <- DT::datatable(df, 
                        filter = "top",
                        rownames = FALSE, 
                        extensions = "Buttons", 
                        class = "nowrap display", 
                        options = list(pageLength = 10,
                                       lengthMenu = list(c(10, 50, 100, -1), c('10', '50','100','All')),
                                       #rowsGroup = list(0,1,2,3,4,5,6,7,8),
                                       dom = "Blfrtip",
                                       scrollX = TRUE,
                                       buttons = list(
                                         list(extend = "excel", text = "Download Current Page", filename = "flywheel_file_metadata",
                                              exportOptions = list(
                                                modifier = list(page = "current"),
                                                worksheet = 'new',
                                                title = NULL
                                              )
                                              ) # list
                                       )
                        ) # datatable
    ) # renderDT
    path <- "./www" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  })
  
  output$corsica_table <- DT::renderDT( { #server = FALSE, { # if server = TRUE, then Download-all button doesn't work (but could remove it and keep Download-current-page w/"All" menu length)
    df <- final_df_corsica()
    dtable <- DT::datatable(df, 
                            filter = "top",
                            rownames = FALSE, 
                            extensions = "Buttons", 
                            class = "nowrap display", 
                            options = list(pageLength = 10,
                                           lengthMenu = list(c(10, 50, 100, -1), c('10', '50','100','All')),
                                           #rowsGroup = list(0,1,2,3,4,5,6,7,8),
                                           dom = "Blfrtip",
                                           scrollX = TRUE,
                                           buttons = list(
                                             list(extend = "excel", text = "Download Current Page", filename = "flywheel_file_metadata",
                                                  exportOptions = list(
                                                    modifier = list(page = "current"),
                                                    worksheet = 'new',
                                                    title = NULL
                                                  )
                                             ) # list
                                           )
                            ) # datatable
    ) # renderDT
    path <- "./www" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  })
  
  # construct the # subjects box
    output$subjects_w_img_cbtn <- renderValueBox({
        df <- final_df_cbtn() 
        distinct_sub_w_img <- df %>% summarise(n = n_distinct(`subject_label`))
        distinct_sub_w_img %>%
          as.integer() %>%
          prettyNum(big.mark = ",") %>%
          valueBox(subtitle = "Number of subjects",
                   color = "green")
      })
    # construct the # sessions box
    output$sessions_w_img_cbtn <- renderValueBox({
      df <- final_df_cbtn()
      distinct_ses_w_img <- df %>% group_by(subject_label) %>%
                                   dplyr::summarise(n = n_distinct(session_label))
      sum(distinct_ses_w_img$n) %>%
        # as.integer() %>%
        prettyNum(big.mark = ",") %>%
        valueBox(subtitle = "Number of sessions",
                 color = "green")
    })
    
    # construct the # subjects box
    output$subjects_w_img_corsica <- renderValueBox({
      df <- final_df_corsica() 
      distinct_sub_w_img <- df %>% summarise(n = n_distinct(`subject_label`))
      distinct_sub_w_img %>%
        as.integer() %>%
        prettyNum(big.mark = ",") %>%
        valueBox(subtitle = "Number of subjects",
                 color = "green")
    })
    # construct the # sessions box
    output$sessions_w_img_corsica <- renderValueBox({
      df <- final_df_corsica()
      distinct_ses_w_img <- df %>% group_by(subject_label) %>%
        dplyr::summarise(n = n_distinct(session_label))
      sum(distinct_ses_w_img$n) %>%
        # as.integer() %>%
        prettyNum(big.mark = ",") %>%
        valueBox(subtitle = "Number of sessions",
                 color = "green")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
