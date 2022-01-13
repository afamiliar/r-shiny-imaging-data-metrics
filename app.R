#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(DT)

histdata <- read.csv('~/Documents/R/shiny/imaging_parameters/cbtn_imaging_parameters/fw_table_4_shiny.csv', header = TRUE)
#filtered_df = subset(histdata, magnetic_field_strength!="NULL")

# remove specific projects
filtered_df = subset(histdata, project_label!="CBTN_D0143")
filtered_df = subset(filtered_df, project_label!="CBTN_D0146")

# convert Magnetic field strengths to numbers
# harmonize and round values
filtered_df$magnetic_field_strength <- as.numeric(filtered_df$magnetic_field_strength)
filtered_df$magnetic_field_strength[filtered_df$magnetic_field_strength==15000]<-1.5
filtered_df$magnetic_field_strength<-round(filtered_df$magnetic_field_strength, digits = 1)

# convert voxel dimension fields to numbers
filtered_df$dim1 <- as.numeric(filtered_df$dim1)
filtered_df$dim2 <- as.numeric(filtered_df$dim2)
filtered_df$dim3 <- as.numeric(filtered_df$dim3)

# combine FW classifications into 1 label
filtered_df$fw_class <- paste(filtered_df$file_classification_intent,
                              filtered_df$file_classification_measurement,
                              filtered_df$file_classification_features)

#   REMOVE NO-MODALITY FILES (CT)

#   -- add filter for sessions ONLY containing file classifications (vs. ANY)
#   -- handle where vox dim = NA

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinydashboard(), # added this
  
  titlePanel("Flywheel Data Metrics - CBTN"),
  fluidRow(
    valueBoxOutput("subjects_w_img_report"),
    valueBoxOutput("sessions_w_img_report")
  ),
  sidebarPanel(
    pickerInput("fw_proj","Cancer type", choices=unique(sort(filtered_df$project_label)),
                                         selected=unique(sort(filtered_df$project_label)),
                                         options = list(`actions-box` = TRUE),
                                         multiple = T),

    fluidRow(  # Define filters
                    checkboxGroupInput(inputId = "ModalityFinder",
                                       label = "Scan Modalities:",
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
                    
                    sliderInput("dim1_range",
                                "Voxel size: dim1",
                                min = min(filtered_df$dim1, na.rm = TRUE),  
                                max = max(filtered_df$dim1, na.rm = TRUE), 
                                value = c(min(filtered_df$dim1, na.rm = TRUE), 
                                          max(filtered_df$dim1, na.rm = TRUE)),
                                sep = "",),
                    sliderInput("dim2_range",
                                "Voxel size: dim2",
                                min = min(filtered_df$dim2, na.rm = TRUE),  
                                max = max(filtered_df$dim2, na.rm = TRUE), 
                                value = c(min(filtered_df$dim2, na.rm = TRUE), 
                                          max(filtered_df$dim2, na.rm = TRUE)),
                                sep = "",),
                    sliderInput("dim3_range",
                                "Voxel size: dim3",
                                min = min(filtered_df$dim3, na.rm = TRUE),  
                                max = max(filtered_df$dim3, na.rm = TRUE), 
                                value = c(min(filtered_df$dim3, na.rm = TRUE), 
                                          max(filtered_df$dim3, na.rm = TRUE)),
                                sep = "",),
                    
                    actionLink("selectallbody","Select/deselect All Body Parts"),
                    checkboxGroupInput(inputId = "BodyPartFinder",
                                       label = "Body part examined",
                                       choices = unique(filtered_df$body_part_examined),
                                       selected = unique(filtered_df$body_part_examined)
                    ),
                    
                    actionLink("selectall","Select/deselect All File Types"),
                    checkboxGroupInput(inputId = "ClassificationFinder",
                                       label = "File classification",
                                       choices = unique(sort(filtered_df$fw_class)),
                                       selected = unique(sort(filtered_df$fw_class)))
            )
  ),
  fluidRow(
    column(7, # width
           DT::dataTableOutput('table')
    )
    )
)

# Define server logic
server <- function(input, output, session) {
  # configure the "select all" for file classification filter
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"ClassificationFinder","File classification",choices=unique(sort(filtered_df$fw_class)))
    }
    else
    {
      updateCheckboxGroupInput(session,"ClassificationFinder","File classification",choices=unique(sort(filtered_df$fw_class)),selected=unique(sort(filtered_df$fw_class)))
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
    
    r_df <- r_df[r_df$dim1 >= input$dim1_range[1] & r_df$dim1 <= input$dim1_range[2],]
    r_df <- r_df[r_df$dim2 >= input$dim2_range[1] & r_df$dim2 <= input$dim2_range[2],]
    r_df <- r_df[r_df$dim3 >= input$dim3_range[1] & r_df$dim3 <= input$dim3_range[2],]
    
    r_df = subset(r_df, select = c("subject_label","session_label","acquisition_label","file_modality","magnetic_field_strength","fw_class","dim1","dim2","event_label",'body_part_examined') )
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
  
  # construct the output
    output$subjects_w_img_report <- renderValueBox({
        df <- final_df() 
        distinct_sub_w_img <- df %>% summarise(n = n_distinct(`subject_label`))
        distinct_sub_w_img %>%
          as.integer() %>%
          prettyNum(big.mark = ",") %>%
          valueBox(subtitle = "Number of subjects",
                   color = "green")
      })
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
