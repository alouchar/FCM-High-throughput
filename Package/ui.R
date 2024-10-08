ui <- dashboardPage(
  
  # Dashboard Header
  dashboardHeader(title = "PhytoCytoTraits"),
  
  # Dashboard sidebar  
  dashboardSidebar(
    shinyjs::useShinyjs(),
    
    sidebarMenu(id = "steps",
                style = "position: fixed; overflow: visible;",
                menuItem("Home", tabName = "Home", icon = icon("house", lib = "font-awesome")),
                menuItem("Quality Control", tabName = "QA-QC", icon = icon("filter")),
                menuItem("Import", tabName = "Import", icon = icon("file-import")),
                menuItem("Pre-processing", tabName = "Pre-processing", icon = icon("gears")),
                menuItem("Parametrisation", tabName = "Parametrisation", icon = icon("gears")),
                menuItem("Clustering", tabName = "Clustering", icon = icon("network-wired")),
                uiOutput('visu'),
                menuItem("Multivariate Trait Analysis", tabName = "MultiTraitAnalysis", icon = icon("diagram-project")),
                menuItem("Batch mode", tabName = "Batch mode", icon = icon("circle-nodes")),
                menuItem("LICENSE", tabName = "LICENSE", icon = icon("book-open")),
                menuItem("Contact", tabName = "Contact", icon = icon("envelope"))
    )
  ),
  
  # Dashboard Body  
  dashboardBody(
    tabItems(
      
      #_____________________________________________________________ Item 1
      tabItem("Home",
              h2("PhytoCytoTraits app"),
              fluidPage(
                fluidRow(
                  includeMarkdown("Introduction.md"),
                  column(6,imageOutput("NIOO")),
                  column(6,imageOutput("ERC"))
                )
              )
      ),
      
      #_____________________________________________________________ Item 2
      tabItem("QA-QC",
              h2("Quality Assurance - Quality Control"),
              
              fluidRow(
                box(includeMarkdown("QAQC description.md"), height = '350px', width = 12)
              ),
              
              fluidRow(
                box(width = 6,
                    column(width = 4, fileInput('QC_beads_file', ' Please select a Beads file', accept = ".fcs"),
                           verbatimTextOutput("coefficientvariation")),
                    column(width = 8, plotOutput('QC_beads_plot',dblclick = "QC_beads_plot_dblclick",
                                                 brush = brushOpts(
                                                   id = "QC_beads_plot_brush",
                                                   resetOnNew = TRUE
                                                 ))
                    )
                ),
                box(width = 6,
                    column(width = 4, fileInput('QC_blank_file', ' Please select a Blank file', accept = ".fcs"),
                           verbatimTextOutput("nbevent")),
                    column(width = 8, plotOutput('QC_blank_plot')))
              ),
              
              actionButton(inputId="nextbutton_1", label="Next")
      ),
      
      
      #_____________________________________________________________ Item 3
      tabItem("Import",
              h2("Data Importation"),
              fluidRow(
                box(width = 9, 
                    column(width = 4, fileInput('file', 'Please select a FCS file',accept = c(".fcs", ".csv"))),
                    column(width = 4, uiOutput("filesizebox")),
                    column(width = 4, uiOutput("files_length_box"))
                )
              ),
              fluidRow(
                box(includeMarkdown("FCS description.md"), height = '550px', width = 6),
                box(style = 'overflow-y: scroll;height:550px',withSpinner(DT::dataTableOutput("files")), width = 6)
              ),
              
              fluidRow(
                box(column(width = 3,uiOutput('var_name')),
                    column(width = 9, plotOutput('histo_preview'))
                ),
                box(includeMarkdown("Explhisto.md"))
              ),
              actionButton(inputId="nextbutton_2", label="Next")
      ),

      #_____________________________________________________________ Item 4
      tabItem("Pre-processing",
              h2("Data transformation and normalization"),
              fluidRow(
                box(includeMarkdown("Preprocess description.md"), height = '500px', width = 4),
                box(height = '500px', width = 8,
                    column(width = 3,uiOutput("var_name2"),
                           radioButtons("transform", "Type of transformation:",
                                        c("Original" = "Normal",
                                          "Log10" = "logarithm_10",
                                          "Biexponential" = "biexp",
                                          "Square Root" = "square_root",
                                          "Logicle" = "lgcl",
                                          "Arcsinh" ="ASinh"
                                        )
                           ),
                           downloadButton("downloadData", "Download")
                    ),
                    column(width = 8,div(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot",plotOutput('histo_transform')),
                                  tabPanel("Table",style = 'overflow-y: scroll; overflow-x: scroll; height:400px',withSpinner(DT::dataTableOutput("transformed_table")))
                      ),
                      class = "span7")
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                  numericInput('numColumns', 'Number of New Columns', value = 1, min = 1, width = '30%'),
                  actionButton(inputId = "addColumn", "Add Columns"),
                  uiOutput("columnBoxes")
                )
              ),
              actionButton(inputId="nextbutton_3", label="Next")
      ),
      
      #_____________________________________________________________ Item 5
      tabItem("Parametrisation",
              h2("Parametrisation"),
              fluidRow(
                box(width = 3,
                    uiOutput("selected_columns"),
                    downloadButton("download", "Donwload"),
                    actionButton(inputId="nextbutton_4", label="Next"),
                    uiOutput("filtered_var1"),
                    uiOutput("filtered_var2"),
                    # Button to create filters for numeric columns
                    actionButton("create_filter", "Create Filter"),
                    # Numeric columns will be generated dynamically in the server
                    actionButton("submit_changes", "Apply Thresholds"),
                    ),
                box(width = 9,
                  tabsetPanel(type = "tabs",
                              tabPanel("Table", style = 'overflow-y: scroll;height:550px',withSpinner(DT::dataTableOutput("filtered_table"))),
                              tabPanel("Plot",plotlyOutput("filtered_plot"))
                              )
                )
              ),
              fluidRow(
                uiOutput("filter_inputs")  # Display filter input boxes
              )
      ),
      
      #_____________________________________________________________ Item 6
      tabItem("Clustering",
              h2("Unsupervised classification"),
              fluidRow(
                box(includeMarkdown("Unsupervised clustering.md"), height = '400px', width = 8),
                box(width = 4, 
                    radioButtons("algorithm_choice", "Choose your algorithm:",
                                 c("None" = "none", 
                                   "K-means" = "kmeans",
                                   "PAM" = "pam",
                                   "HDBSCAN" = "hdbscan"
                                 ), 
                    ),
                    uiOutput("algo"),
                    actionButton("run_clust", "Run clustering"),
                    uiOutput("cluster_rename_ui"),  # Add this line
                    textInput("new_cluster_name", "New name:"), # Add this line
                    actionButton("update_cluster_names", "Update names")  # Add this line
                )
              ),
              fluidRow(
                box(
                  actionButton("Visualise", "Visualise outputs"), downloadButton("download_results", "Donwload"),
                  width = 12,style = 'overflow-y: scroll;height:550px',withSpinner(DT::DTOutput("clustered_data"))
                )
              )
      ),
      #_____________________________________________________________ Item 7
      tabItem("Visualisation",
              h2("Outputs from clustering"),
              fluidRow(
                box(width = 12, height = '500px',
                    column(width = 2, uiOutput('clust_var_name1'), uiOutput('clust_var_name2'), uiOutput("clust_var_name3"),
                           radioGroupButtons(inputId = "plot_type",
                                             label = "Type of plot:",
                                             choices = c("Histogram" = "hist",
                                                         "Density" = "dens",
                                                         "Boxplot" = "box",
                                                         "Densigram" = "densi"), 
                                             selected = "hist"
                           ),
                           downloadButton("download_2D_dot_Plot", "Download")
                    ),
                    column(width = 4, plotOutput('Plot_2D')),
                    column(width = 4, plotOutput("Density_2D_plot")),
                    column(width = 2, 
                           radioGroupButtons(inputId = "Density_plot_type",
                                             label = "Type of plot:",
                                             choices = c("Contour Plot" = "Cont_plot",
                                                         "Hexbin Plot" = "hex_plot",
                                                         "Image" = "img"),
                                             selected = "hex_plot"
                           ),
                           downloadButton("download_2D_density_Plot", "Download")
                    )
                ), 
                box(width = 12, height = '500px',
                    column(width = 2, uiOutput('clust_3D_name1'), uiOutput('clust_3D_name2'), uiOutput("clust_3D_name3"), uiOutput("clust_3D_factor"), uiOutput("size_select")),
                    column(width = 8, plotlyOutput("Plot_3D")),
                    column(width = 2, downloadButton("download_3D_Plot", "Download"))
                ),
                box(downloadButton("summary_results", "Donwload"),
                    width = 12,style = 'overflow-y: scroll;height:550px',withSpinner(DTOutput("summary_table"))
                    )
              )
      ),
      #_____________________________________________________________ Item 8
      tabItem("MultiTraitAnalysis",
              h2("Analysis of flow cytometry traits (scatters and fluorescence)")
      ),
      
      #_____________________________________________________________ Item 9
      tabItem("Batch mode",
              h2("")
      ),
      
      #_____________________________________________________________ Item 10
      tabItem("LICENSE",
              fluidPage(
                fluidRow(
                  includeMarkdown("LICENSE.md")
                )
              )
      ),
      
      #_____________________________________________________________ Item 11
      tabItem("Contact",
              h2("Contact information"),
              fluidPage(
                fluidRow(
                  includeMarkdown("Contact.md")
                )
              )
      )
    )
  )
)