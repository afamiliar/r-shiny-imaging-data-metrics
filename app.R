#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#   https://rstudio.github.io/shinydashboard/structure.html


# TO DO:
#   -- add filter for sessions ONLY containing file classifications (vs. ANY)
#   -- handle where vox dim = NA
#   -- add DNA/RNA and/or
#   -- add description of fields at top

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
histdata <- read.csv("fw_table_4_shiny.csv", header = TRUE)

filtered_df <- histdata

# convert Magnetic field strengths to numbers
# harmonize and round values
#filtered_df = subset(filtered_df, magnetic_field_strength!="NULL")
filtered_df$magnetic_field_strength <- as.numeric(filtered_df$magnetic_field_strength)
filtered_df$magnetic_field_strength[filtered_df$magnetic_field_strength==15000]<-1.5
filtered_df$magnetic_field_strength<-round(filtered_df$magnetic_field_strength, digits = 1)

filtered_df$magnetic_field_strength[is.na(filtered_df$magnetic_field_strength)]<-"N/A"

# adjust body-part labels
filtered_df$body_part_examined[filtered_df$body_part_examined=="iac"]<-"brain (iac)"
filtered_df$body_part_examined[filtered_df$body_part_examined=="pituitary"]<-"brain (pituitary)"
filtered_df$body_part_examined[filtered_df$body_part_examined=="face"]<-"brain (face)"

# round ages
filtered_df$age_at_imaging_in_years<-round(filtered_df$age_at_imaging_in_years, digits = 1)

# convert voxel dimension fields to numbers
#filtered_df$dim1 <- as.numeric(filtered_df$dim1)
#filtered_df$dim2 <- as.numeric(filtered_df$dim2)
#filtered_df$dim3 <- as.numeric(filtered_df$dim3)

# combine FW classifications into 1 label
filtered_df$fw_class <- paste(filtered_df$file_classification_intent,
                              filtered_df$file_classification_measurement,
                              filtered_df$file_classification_features)
filtered_df$fw_class[filtered_df$fw_class=="  "]<-"None"

# filter text
diagnoses <- as.character(unique(sort(filtered_df$project_label)))
file_types <- unique(sort(filtered_df$fw_class,decreasing = TRUE,))

# ==================================================================================================
ui <- dashboardPage(
  # ========= Dashboard Header ==========
  dashboardHeader(title = "Flywheel Data Metrics"),
  
  # ========= Dashboard Sidebar ==========
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview_tab", icon = icon("home")),
      menuItem("CBTN Imaging Data", tabName = "cbtn_imaging", icon = icon("table"))
    )
  ),
  # ========= Dashboard Body ==========
  dashboardBody(
    tabItems(
      # first tab page
      tabItem(tabName = "overview_tab",
              titlePanel("Overview"),
              mainPanel(
                fluidRow(
                  align = "left",
                  div(style = "font-size:20px;",
                      'This is where information will go')
                ),
                width = 12
              ),
      ),
      
      # second tab page
      tabItem(tabName = "cbtn_imaging",
              titlePanel("Children's Brain Tumor Network - Radiology Data"),
              
              # summary # boxes
              fluidRow(
                valueBoxOutput("subjects_w_img_report"),
                valueBoxOutput("sessions_w_img_report")
              ),
              
              # left-hand filters
              sidebarPanel(
                pickerInput("fw_proj","Diagnosis", 
                            choices=diagnoses,
                            selected=diagnoses,
                            options = list(`actions-box` = TRUE),
                            multiple = T),

                pickerInput("molec_status","Availability of molecular sequencing", 
                            choices=c("WGS","RNAseq","Clinical Seq for Event","Clinical Seq Other"),
                            selected=c(),
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                
                fluidRow(  # Define filters
                  checkboxGroupInput(inputId = "ModalityFinder",
                                     label = "Scan Modalities",
                                     choices = unique(filtered_df$file_modality),
                                     selected = unique(filtered_df$file_modality)
                  ),
                  
                  checkboxGroupInput(inputId = "FieldStrengthFinder",
                                     label = "Magnetic Field Strength",
                                     choices = unique(sort(filtered_df$magnetic_field_strength,decreasing = TRUE,)),
                                     selected = unique(sort(filtered_df$magnetic_field_strength,decreasing = TRUE,))
                  ),
                  
                  checkboxGroupInput(inputId = "EventFinder",
                                     label = "Event",
                                     choices = unique(filtered_df$event_label),
                                     selected = unique(filtered_df$event_label)
                  ),
                  
                  sliderInput("age_range",
                              "Age at imaging (years)",
                              min = min(filtered_df$age_at_imaging_in_years),
                              max = max(filtered_df$age_at_imaging_in_years),
                              value = c(min(filtered_df$age_at_imaging_in_years),
                                        max(filtered_df$age_at_imaging_in_years)),
                              sep = "",),
                  
                  # sliderInput("dim1_range",
                  #             "Voxel size: dim1",
                  #             min = min(filtered_df$dim1, na.rm = TRUE),  
                  #             max = max(filtered_df$dim1, na.rm = TRUE), 
                  #             value = c(min(filtered_df$dim1, na.rm = TRUE), 
                  #                       max(filtered_df$dim1, na.rm = TRUE)),
                  #             sep = "",),
                  # sliderInput("dim2_range",
                  #             "Voxel size: dim2",
                  #             min = min(filtered_df$dim2, na.rm = TRUE),  
                  #             max = max(filtered_df$dim2, na.rm = TRUE), 
                  #             value = c(min(filtered_df$dim2, na.rm = TRUE), 
                  #                       max(filtered_df$dim2, na.rm = TRUE)),
                  #             sep = "",),
                  #                    sliderInput("dim3_range",
                  #                                "Voxel size: dim3",
                  #                                min = min(filtered_df$dim3, na.rm = TRUE),  
                  #                                max = max(filtered_df$dim3, na.rm = TRUE), 
                  #                                value = c(min(filtered_df$dim3, na.rm = TRUE), 
                  #                                          max(filtered_df$dim3, na.rm = TRUE)),
                  #                                sep = "",),
                  
                  actionLink("selectallbody","Select/deselect All Body Parts"),
                  checkboxGroupInput(inputId = "BodyPartFinder",
                                     label = "Body part examined",
                                     choices = unique(sort(filtered_df$body_part_examined)),
                                     selected = unique(sort(filtered_df$body_part_examined))
                  ),
                  
                  actionLink("selectall","Select/deselect All File Types"),
                  checkboxGroupInput(inputId = "ClassificationFinder",
                                     label = "File classification",
                                     choices = file_types,
                                     selected = file_types)
                )
              ),
              
              # Data Table
              tags$head(tags$style(css)), # horiz scroll bar
              fluidRow(
                column(7, # width
                       DT::dataTableOutput('table')
                )
              )
      ) # second tab page
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
      updateCheckboxGroupInput(session,"ClassificationFinder","File classification",choices=file_types)
    }
    else
    {
      updateCheckboxGroupInput(session,"ClassificationFinder","File classification",choices=file_types,selected=file_types)
    }
  })

  # configure the "select all" for body-part filter
  observe({
    if(input$selectallbody == 0) return(NULL) 
    else if (input$selectallbody%%2 == 0)
    {
      updateCheckboxGroupInput(session,"BodyPartFinder","Body part examined",choices=unique(filtered_df$body_part_examined))
    }
    else
    {
      updateCheckboxGroupInput(session,"BodyPartFinder","Body part examined",choices=unique(filtered_df$body_part_examined),selected=unique(filtered_df$body_part_examined))
    }
  })
  
  # filter the input dataframe
  final_df <- reactive({
    r_df <- filter(filtered_df, project_label %in% input$fw_proj ) %>%
            filter(file_modality %in% input$ModalityFinder ) %>%
            filter(fw_class %in% input$ClassificationFinder ) %>%
            filter(event_label %in% input$EventFinder ) %>%
            filter(body_part_examined %in% input$BodyPartFinder ) %>%
            filter(magnetic_field_strength %in% input$FieldStrengthFinder )

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

    r_df <- r_df[r_df$age_at_imaging_in_years >= input$age_range[1] & r_df$age_at_imaging_in_years <= input$age_range[2],]
    
    # r_df <- r_df[r_df$dim1 >= input$dim1_range[1] & r_df$dim1 <= input$dim1_range[2],]
    # r_df <- r_df[r_df$dim2 >= input$dim2_range[1] & r_df$dim2 <= input$dim2_range[2],]
#    r_df <- r_df[r_df$dim3 >= input$dim3_range[1] & r_df$dim3 <= input$dim3_range[2],]
    
    r_df = subset(r_df, select = c("project_label","subject_label","session_label","acquisition_label","file_modality","magnetic_field_strength","fw_class","dim1","dim2","event_label",'body_part_examined') )
    r_df <- r_df[!duplicated(r_df), ]
    r_df <- r_df[order(r_df$subject_label,r_df$session_label,r_df$acquisition_label), ]
    r_df
  })

  #CBTN clinical table
  output$table <- DT::renderDT( { #server = FALSE, { # if server = TRUE, then Download-all button doesn't work (but could remove it and keep Download-current-page w/"All" menu length)
    df <- final_df()
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
                                         list(extend = "excel", text = "Download Current Page", filename = "flywheel_cbtn_file_metadata",
                                              exportOptions = list(
                                                modifier = list(page = "current"),
                                                worksheet = 'new',
                                                title = NULL
                                              )
#                                         ),
#                                         list(extend = "excel", text = "Download Full Results", filename = "flywheel_cbtn_file_metadata",
#                                              exportOptions = list(
#                                                modifier = list(page = "all"),
#                                                worksheet = 'new',
#                                                title = NULL
                                              ) # list
#                                         ) # list
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
    output$subjects_w_img_report <- renderValueBox({
        df <- final_df() 
        distinct_sub_w_img <- df %>% summarise(n = n_distinct(`subject_label`))
        distinct_sub_w_img %>%
          as.integer() %>%
          prettyNum(big.mark = ",") %>%
          valueBox(subtitle = "Number of subjects",
                   color = "green")
      })
    # construct the # sessions box
    output$sessions_w_img_report <- renderValueBox({
      df <- final_df() 
      distinct_ses_w_img <- df %>% summarise(n = n_distinct(`session_label`))
      distinct_ses_w_img %>%
        as.integer() %>%
        prettyNum(big.mark = ",") %>%
        valueBox(subtitle = "Number of sessions",
                 color = "green")
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
