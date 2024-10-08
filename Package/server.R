server <- function(input, output,session) {
  
  #_____________________________________________________________ Item 1 Home
  
  output$NIOO <- renderImage({
    return(list(src = "C:/Users/ArnaudL/Desktop/High throughput bioinfos/PhytoCytoTraits/img/NIOO.gif",
                width = "75%",style="display: block; margin-left: auto; margin-right: auto;"))
  }, deleteFile = FALSE)
  
  output$ERC <- renderImage({
    return(list(src = "C:/Users/ArnaudL/Desktop/High throughput bioinfos/PhytoCytoTraits/img/LOGO_ERC.png",
                width = "75%",style="display: block; margin-left: auto; margin-right: auto;"))
  }, deleteFile = FALSE)
  
  #_____________________________________________________________ Item 2 QA-QC
  QCbeadsfile <- reactive({
    dat <- as.data.frame(exprs(read.FCS(input$QC_beads_file$datapath, transformation = FALSE,alter.names = FALSE))) #Works also as the path is already loaded
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$QC_beads_plot <- renderPlot({
    QCbeadsfile() %>%
      select(`580/30[488]`) %>%
      group_by(`580/30[488]`) %>%
      mutate(Count = n()) %>%
      distinct(`580/30[488]`, .keep_all = TRUE) %>%
      ggplot(mapping = aes(x = log10(`580/30[488]`+1), y = Count)) + ## Plot the beads eight peaks; can also be `670/30[640]`
      geom_line() +
      xlab(expression("log"["10"]~"580/30 (488)"+1)) +
      theme_classic() +
      theme(axis.text = element_text(size=14),
            axis.title = element_text(size=16)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  
  observeEvent(input$QC_beads_plot_dblclick, {
    brush <- input$QC_beads_plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observe({
    brush <- input$QC_beads_plot_brush
    if (!is.null(brush)) {
      DATA <- QCbeadsfile()[log10(QCbeadsfile()$`580/30[488]`+1) >= brush$xmin & log10(QCbeadsfile()$`580/30[488]`+1) <= brush$xmax,]
      
      coeff_var <- CV(DATA$`580/30[488]`)
      
      output$coefficientvariation <- renderPrint({
        cat("Coefficient \nof variation: ", coeff_var)
      })
    }
  })
  
  
  QCblankfile <- reactive({
    dat <- as.data.frame(exprs(read.FCS(input$QC_blank_file$datapath, transformation = FALSE,alter.names = FALSE))) #Works also as the path is already loaded
  })
  
  output$QC_blank_plot <- renderPlot({
    ggplot(QCblankfile(), aes(x = log10(`FSC [Par]`), y = log10(`480/40[457]`))) + ## Plot the blank file
      geom_point(size = 2, pch = 19) +
      xlab(expression("log"["10"]~"FSC Par"+1 )) +
      ylab(expression("log"["10"]~"480/40 (457)"+1)) +
      theme_classic() +
      theme(axis.text = element_text(size=14),
            axis.title = element_text(size=16))
  })
  
  output$nbevent <- renderPrint({
    if (!is.null(QCblankfile())) {
      cat("Number of \nobservations: ", nrow(QCblankfile()))
    }
  })
  
  observeEvent(input$nextbutton_1, {
    updateTabItems(session, "steps", "Import") 
  })    
  
  #_____________________________________________________________ Item 3 Import
  
  data <- reactive({
    req(input$file)
    filepath <- input$file$datapath
    filename <- input$file$name
    extension <- tools::file_ext(filepath)
    
    if (extension == "fcs") {
      flow_data <- as.data.frame(exprs(read.FCS(filepath, transformation = FALSE,alter.names = FALSE)))
    } else if (extension == "csv") {
      flow_data <- read.table(filepath, header = TRUE, sep = ",")
    } else {
      stop("Unsupported file extension.")
    }
    
    return(flow_data)
  })

  vals <- reactiveValues()
  
  output$files <- 
    DT::renderDT(server = FALSE,{
      DT::datatable(
        data(), extensions = 'Buttons', 
        options = list(dom = 'Bfrtip', "autoWidth"=FALSE, "pagelength"=5,"scrollY"=TRUE,"scrollX"=TRUE,"searching"=FALSE, buttons = list( 
          list(extend = "csv", text = "Download Full Results", filename = paste0("Raw_FCS_",tools::file_path_sans_ext(basename(input$file$name))),
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
        )
      )
    })
  
  output$var_name <- renderUI({
    selectInput('xcol', 'Choose a variable:', colchoice(data()))
  })
  
  output$histo_preview <- renderPlot({
    ggplot(data(), aes_string(x = as.name(input$xcol)))+ # aes_string works here but not aes. | as.name is really important as it avoids conflict of special characters with aes_string
      geom_histogram(aes(y = ..density..), fill = "grey40") +
      geom_density(colour = 4, fill= 4, alpha= 0.25) +
      theme_classic() +
      theme(axis.text = element_text(size=14),
            axis.title = element_text(size=16))
    
  })
  
  
  output$filesizebox <- renderUI({
    infoBox(
      "File size", paste0(humanReadable(file.info(input$file$datapath)$size)), icon = icon("circle-exclamation", lib = "font-awesome"),
      color = "navy"
    )
  })
  
  output$files_length_box <- renderUI({
    infoBox(
      "File length", paste0(nrow(data()), " rows"), icon = icon("circle-exclamation", lib = "font-awesome"),
      color = "navy"
    )
  })
  
  observeEvent(input$file$datapath, {
    req(input$file$datapath)
    
    if(file.info(input$file$datapath)$size < 100000) {
      shinyalert("Warning!","Size or dataset is small.", type = "warning")
    } 
    else {
      shinyalert("Excellent!","Size or dataset looks good.", type = "success")
    }
  })
  
  observeEvent(input$nextbutton_2, {
    req(input$file$datapath)
    vals$dat <- NULL
    disable("nextbutton_2")
    vals$dat <- read.FCS(input$file$datapath)
  })
  
  observe({
    req(vals$dat)
    enable("nextbutton_2")
    updateTabItems(session, "steps", "Pre-processing")
  })
  
  #_____________________________________________________________ Item 4 Pre-processing
  column_boxes <- reactive({
    numColumns <- input$numColumns
    lapply(1:numColumns, function(i) {
      column(
        width = 3,
        wellPanel(
          selectInput(paste0("xcol_", i), "X Variable", choices = names(data())),
          selectInput(paste0("ycol_", i), "Y Variable", choices = names(data()), selected = names(data())[[2]]),
          selectInput(paste0("operator_", i), "Operator", choices = c("+", "-", "*", "/")),
          textInput(paste0("column_name_", i), "New Column Name", value = paste("Result", i))
        )
      )
    })
  })
  
  observe({
    output$columnBoxes <- renderUI({
      column_boxes()
    })
  })
  
  calculate_column <- function(i) {
    xcol <- input[[paste0("xcol_", i)]]
    ycol <- input[[paste0("ycol_", i)]]
    operator <- input[[paste0("operator_", i)]]
    
    if (operator == "+") {
      result <- data()[[xcol]] + data()[[ycol]]
    } else if (operator == "-") {
      result <- data()[[xcol]] - data()[[ycol]]
    } else if (operator == "*") {
      result <- data()[[xcol]] * data()[[ycol]]
    } else if (operator == "/" && all(data()[[ycol]] != 0)) {
      result <- data()[[xcol]] / data()[[ycol]]
    } else {
      result <- rep(NA, nrow(data()))
    }
    
    return(result)
  }
  
  newdf <- eventReactive(input$addColumn, {
    numColumns <- input$numColumns
    table_with_new_columns <- data()
    
    for (i in 1:numColumns) {
      column_name <- input[[paste0("column_name_", i)]]
      result_col <- calculate_column(i)
      table_with_new_columns[column_name] <- result_col
    }
    
    return(table_with_new_columns)
  })
  
  output$var_name2 <- renderUI({
    selectInput('Column', 'Choose a variable:', colchoice(newdf()))
  })
  
  datatransformed <- reactive({
    
    if (input$transform == "Normal") {
      identity(newdf())
    }
    else if
    (input$transform == "logarithm_10"){
      log10(newdf()+1)
    }
    else if
    (input$transform == "square_root"){
      sqrt(newdf())
    }
    else if
    (input$transform == "biexp"){
      flowCore::biexponentialTransform(newdf())
    }
    else if
    (input$transform == "lgcl"){
      flowCore::logicleTransform(newdf())
    }
    else if
    (input$transform == "ASinh"){
      flowCore::arcsinhTransform(newdf())
    }
    
  })
  
  output$histo_transform <- renderPlot({
    datatransformed() %>%
      ggplot(aes_string(x = as.name(input$Column))) +
      geom_histogram(aes(y = ..density..), fill = "grey40") +
      geom_density(colour = 4, fill= 4, alpha= 0.25) +
      theme_classic() +
      theme(axis.text = element_text(size=14),
            axis.title = element_text(size=16))
  })
  
  output$transformed_table <- DT::renderDataTable({
    
    DT::datatable(datatransformed(), 
                  options = list(dom = 'rtp', "pagelength"=1)
    )
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(paste0("Transformed_FCS_",tools::file_path_sans_ext(basename(input$file$name))), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datatransformed(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$nextbutton_3, {
    if(is.null(data())){
      shinyjs::disable("nextbutton_3")
    }
    else {
      shinyjs::enable("nexbutton_3")
    }
    updateTabItems(session, "steps", "Parametrisation")
  })
  
  #_____________________________________________________________ Item 5 Parametrisation
  
  output$selected_columns <- renderUI({
    selectizeInput("selected_columns", "Select variable to print",
                   choices = names(datatransformed()),
                   multiple = TRUE)
  })
  
  filtered_data <- reactive({
    selected_columns <- input$selected_columns
    if (length(selected_columns) > 0) {
      datatransformed()[, selected_columns, drop = FALSE]
    } else {
      NULL
    }
  })
  
  observe({
    choices <- names(datatransformed())
    updatePickerInput(session, "selected_columns", choices = choices)
  })
  
  # Affichage initial de la table
  output$filtered_table <- renderDT({
    datatable(filtered_data(), options = list(searching = FALSE))
  })
  
  ##############################################################################
  output$filtered_var1 <- renderUI({
    selectInput('filtered_var1', 'Choose X variable', choices = input$selected_columns)
  })
  
  output$filtered_var2 <- renderUI({
    selectInput('filtered_var2', 'Choose Y variable', choices = input$selected_columns)
  })
  
  observe({
    updateSelectizeInput(session, "filtered_var1", choices = input$selected_columns, server = TRUE)
    updateSelectizeInput(session, "filtered_var2", choices = input$selected_columns, server = TRUE)
  })

  output$filtered_plot <- renderPlotly({
    config((
      plot_ly(data = filtered_data(), x = ~get(input$filtered_var1), y = ~get(input$filtered_var2), 
              type = "scatter", mode = "markers") %>%
        layout(xaxis = list(title = input$filtered_var1), 
               yaxis = list(title = input$filtered_var2))
    ), scrollZoom = TRUE)
  })
  ##############################################################################
  
  # Reactive values to store filter inputs
  filters <- reactiveValues()
  DATA <- reactiveVal(NULL)
  
  # Create filter inputs dynamically for numeric columns
  observeEvent(input$create_filter, {
    numeric_columns <- names(filtered_data())[sapply(filtered_data(), is.numeric)]
    filter_inputs <- lapply(numeric_columns, function(column) {
      box(
        width = 2,
        title = column,
        selectInput(paste0("operator_", column), "Operator",
                    choices = c(">", "<", "==", "<=", ">=")),
        numericInput(paste0("num_input_", column), "Value:", value = 0),
        status = "info"
      )
    })
    filters$inputs <- filter_inputs
  })
  
  # Display filter inputs
  output$filter_inputs <- renderUI({
    req(filters$inputs)
    filters$inputs
  })
  
  observeEvent(input$submit_changes, {
    DATA <- filtered_data()
    numeric_columns <- names(DATA)[sapply(DATA, is.numeric)]
    for (column in numeric_columns) {
      DATA <- filter_numeric_columns(
        DATA,
        column,
        input[[paste0("operator_", column)]],
        input[[paste0("num_input_", column)]]
      )
      DATA(DATA)
    }
    output$filtered_table <- renderDT({
      datatable(DATA)
    })
    return(DATA)
  })
  

  output$download <- downloadHandler(
    filename = function() {
      paste(paste0("FCM_selected_columns_",tools::file_path_sans_ext(basename(input$file$name))), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(DATA(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$nextbutton_4, {
    if(is.null(data())){
      shinyjs::disable("nextbutton_4")
    }
    else {
      shinyjs::enable("nexbutton_4")
    }
    updateTabItems(session, "steps", "Clustering")
  })
  
  
  #_____________________________________________________________ Item 6 Clustering
  cluster_names <- reactiveVal(NULL)  # Store cluster names
  clust_dat <- reactiveVal(NULL)
  
  # Render the user interface for selecting the number of clusters (for K-means)
  output$algo <- renderUI({
    
    if (input$algorithm_choice == "kmeans") {
      numericInput("num_clusters", "Number of Clusters:", value = 3, min = 1)
    } else if (input$algorithm_choice == "pam") {
      numericInput("num_clusters", "Number of Clusters:", value = 3, min = 1)
    }
  })
  
  # Perform clustering using the selected algorithm
  observeEvent(input$run_clust, {
    cat("Run clustering button clicked\n") # Debug message
    
    num_clusters <- input$num_clusters
    
    if(is.null(input$algorithm_choice)) {
      shinyalert("No Algorithm Selected", "Please choose a clustering algorithm.", type = "warning")
      return(DATA())
      
    } else if (input$algorithm_choice == "kmeans") {
      
      
      if (!is.null(num_clusters)) {
        # Call your custom K-means function
        result <- kmeans(DATA(), centers = num_clusters)
        
        clust_dat <- DATA() %>%
          mutate(Cluster = as.factor(result$cluster))
        cluster_names(unique(clust_dat$Cluster))  # Store unique cluster names
        
        # Update the data with cluster assignments
        clust_dat(clust_dat)
        
      }
    } else if (input$algorithm_choice == "pam") {
      result <- pam(DATA(), k = num_clusters)
      # Update filtered_data with PAM results
      
      clust_dat <- DATA() %>%
        mutate(Cluster = as.factor(result$cluster))
      cluster_names(unique(clust_dat$Cluster))  # Store unique cluster names
      clust_dat(clust_dat)
      
    } else if (input$algorithm_choice == "hdbscan") {
      result <- isolate(dbscan_FCM(DATA()))
      # Update filtered_data with HDBSCAN results
      
      clust_dat <- DATA() %>%
        mutate(Cluster = as.factor(result$cluster))
      cluster_names(unique(clust_dat$Cluster))  # Store unique cluster names
      clust_dat(clust_dat)
      
    } 
    else {
      # Provide feedback if no algorithm is selected
      return(DATA())
    }
    cat("Cluster data created\n")
    
  })
  
  output$clustered_data <- renderDT({
    if (!is.null(clust_dat())) {
      datatable(clust_dat())
    }
  })
  
  output$cluster_rename_ui <- renderUI({
    num_clusters <- input$num_clusters
    selectInput("rename_cluster", "Select a cluster to rename:", 
                choices = cluster_names(), selected = cluster_names()[1])
  })
  
  observeEvent(input$update_cluster_names, {
    selected_cluster <- input$rename_cluster
    new_cluster_name <- input$new_cluster_name
    
    clust_dat_updated <- clust_dat() %>%
      mutate(Cluster = ifelse(Cluster == selected_cluster, new_cluster_name, Cluster))
    clust_dat(clust_dat_updated)
  })

  output$visu <- renderUI({
    if(input$Visualise == 0) return()
    print(input$Visualise)
    sidebarMenu(id = 'steps',
                menuItem("Visualisation", tabName = "Visualisation", icon = icon("chart-area"))
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste(paste0("Results_FCS_",tools::file_path_sans_ext(basename(input$file$name))), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(clust_dat(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$Visualise, {
    # Switch to the "Visualisation" menu item
    updateTabItems(session, "steps", "Visualisation")
  })
  
  #_____________________________________________________________ Item 7 Visualisation
  
  output$clust_var_name1 <- renderUI({
    selectInput('xcol_clust', 'Choose X variable:', colchoice(clust_dat()))
  })
  
  output$clust_var_name2 <- renderUI({
    selectInput('ycol_clust', 'Choose Y variable:', colchoice(clust_dat()))
  })
  
  output$clust_var_name3 <- renderUI({
    selectInput('clust_var', 'Choose a cluster:', colchoice(clust_dat()))
  })
  
  
  plotInput <- reactive({
    if (input$plot_type %in% "hist") {
      ggMarginal(
        clust_dat() %>%
          ggplot() +
          geom_point(aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust), color = as.name(input$clust_var))) +
          theme_bw(),
        type="histogram")
    } else if (input$plot_type %in% "dens") {
      ggMarginal(
        clust_dat() %>%
          ggplot() +
          geom_point(aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust), color = as.name(input$clust_var))) +
          theme_bw(),
        type="density")
    } else if (input$plot_type %in% "box") {
      ggMarginal(
        clust_dat() %>%
          ggplot() +
          geom_point(aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust), color = as.name(input$clust_var))) +
          theme_bw(),
        type="boxplot")
    } else if (input$plot_type %in% "densi") {
      ggMarginal(
        clust_dat() %>%
          ggplot() +
          geom_point(aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust), color = as.name(input$clust_var))) +
          theme_bw(),
        type="densigram")
    }
  })
  
  output$Plot_2D <- renderPlot({
    print(plotInput())
  })
  
  output$download_2D_dot_Plot <- downloadHandler(
    filename = function() { paste(paste0("2D_plot_",tools::file_path_sans_ext(basename(input$file$name))), '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 400, units = "cm")
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  
  plot_2D_Input <- reactive ({
    if (input$Density_plot_type %in% "hex_plot") {
      ggplot(data = clust_dat(), aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust))) +
        geom_hex() +
        scale_fill_distiller(palette = "Spectral", direction = -1) +
        theme_bw()
    } else if (input$Density_plot_type %in% "Cont_plot") {
      ggplot(data = clust_dat(), aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust))) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "grey80") +
        scale_fill_distiller(palette = "Spectral", direction = 0) +
        theme_bw()
    } else if (input$Density_plot_type %in% "img") {
      ggplot(data = clust_dat(), aes_string(x = as.name(input$xcol_clust), y = as.name(input$ycol_clust))) +
        stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
        scale_fill_distiller(palette = "Spectral", direction = 0) +
        theme_bw()
    }
  })
  
  output$Density_2D_plot <- renderPlot({
    print(plot_2D_Input())
  })
  
  output$download_2D_density_Plot <- downloadHandler(
    filename = function() { paste(paste0("2D_plot_",tools::file_path_sans_ext(basename(input$file$name))), '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 400, units = "cm")
      ggsave(file, plot = plot_2D_Input(), device = "png")
    }
  )
  
  output$clust_3D_name1 <- renderUI({
    selectInput("x_axis", "Choose X variable:", choices = colnames(clust_dat()))
  })
  
  output$clust_3D_name2 <- renderUI({
    selectInput("y_axis", "Choose Y variable:", choices = colnames(clust_dat()))
  })
  
  output$clust_3D_name3 <- renderUI({
    selectInput("z_axis", "Choose Z variable:", choices = colnames(clust_dat()))
  })
  
  output$clust_3D_factor <- renderUI({
    selectInput("group_by", "Choose your factor:", choices = colnames(clust_dat()))
  })
  
  output$size_select <- renderUI({
    sliderInput("size_variable", label = h4("Dot size"), min = 1, max = 15, value = 1)
  })
  
  output$Plot_3D <- renderPlotly({
    p <- plot_ly(clust_dat(), x = ~clust_dat()[, input$x_axis], y = ~clust_dat()[, input$y_axis], z = ~clust_dat()[, input$z_axis],
                 type = 'scatter3d', color = ~get(input$group_by), marker = list(size = input$size_variable), colors = "Accent") %>%
      layout(scene = list(
        xaxis = list(title = input$x_axis),
        yaxis = list(title = input$y_axis),
        zaxis = list(title = input$z_axis)
      ))
  })
  
  # output$download_3D_Plot <- downloadHandler(
  #   filename = function() { 
  #     paste(paste0("3D_plot_",tools::file_path_sans_ext(basename(input$file$name))), '.png', sep='') },
  #   content = function(file) {
  #     device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 400, units = "cm")
  #     reticulate::py_run_string("import sys")
  #     save_image(p = p(),file, format = "png")
  #   }
  #   )
  
  output$summary_table <- renderDT({
    datatable(
      clust_dat() %>%
        group_by(Cluster) %>%
        summarise(across(.cols = where(is.numeric), 
                         .fns = list(Min = ~ min(.), 
                                     Max = ~ max(.), 
                                     Median = ~ median(.), 
                                     Mean = ~ mean(.), 
                                     StdDev = ~ sd(.)), 
                         .names = "{.col}_{.fn}")) %>%
        left_join(clust_dat() %>% count(Cluster, name = "nb_event"), by = "Cluster") %>%
        left_join(data.frame(Filename = paste0(tools::file_path_sans_ext(basename(input$file$name))), Cluster = unique(clust_dat()$Cluster)), by = "Cluster")
    )
  })
  
  # output$summary_results <- downloadHandler(
  #   filename = function() {
  #     paste(paste0("summary_",tools::file_path_sans_ext(basename(input$file$name))), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(summary_table(), file, row.names = FALSE)
  #   }
  # )
  
  #_____________________________________________________________ Item 8 BATCH MODE
  
}
