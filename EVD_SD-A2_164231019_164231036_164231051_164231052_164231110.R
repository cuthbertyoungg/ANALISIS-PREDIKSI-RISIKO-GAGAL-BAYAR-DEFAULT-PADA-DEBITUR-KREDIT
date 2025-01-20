library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(MASS)
library(VIM)
library(mice)
library(tidyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(readr)
library(ggcorrplot)
library(leaps) # untuk best subset selection


# -----------------------------------------------
# Load Data Mentah
# -----------------------------------------------
data_raw <- read.csv("LoanData_Raw_v1.0.csv", stringsAsFactors = FALSE)

# Preprocessing function
handle_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound] <- lower_bound
  column[column > upper_bound] <- upper_bound
  return(column)
}

# Data setelah preprocessing
cleaned_data <- read.csv("completed_data1.csv", stringsAsFactors = FALSE)

# Fungsi statistik deskriptif
statistika_deskriptif <- function(data) {
  kolom_numerik <- data[, sapply(data, is.numeric)]
  
  variables <- colnames(kolom_numerik)
  means <- medians <- q1s <- q3s <- variances <- sds <- ranges <- sums <- mins <- maxs <- numeric(length(variables))
  
  for (i in seq_along(variables)) {
    values <- kolom_numerik[[variables[i]]]
    
    means[i] <- mean(values, na.rm = TRUE)
    medians[i] <- median(values, na.rm = TRUE)
    q1s[i] <- quantile(values, 0.25, na.rm = TRUE)
    q3s[i] <- quantile(values, 0.75, na.rm = TRUE)
    variances[i] <- var(values, na.rm = TRUE)
    sds[i] <- sd(values, na.rm = TRUE)
    ranges[i] <- max(values, na.rm = TRUE) - min(values, na.rm = TRUE)
    sums[i] <- sum(values, na.rm = TRUE)
    mins[i] <- min(values, na.rm = TRUE)
    maxs[i] <- max(values, na.rm = TRUE)
  }
  
  hasil <- data.frame(
    Variabel = variables,
    Mean = means,
    Median = medians,
    Q1 = q1s,
    Q3 = q3s,
    Variance = variances,
    SD = sds,
    Range = ranges,
    Sum = sums,
    Min = mins,
    Max = maxs,
    stringsAsFactors = FALSE
  )
  
  return(hasil)
}

hasil_statistika <- statistika_deskriptif(cleaned_data)

# -----------------------------------------------
# UI
# -----------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "UAS EVD SD-A2 KELOMPOK 8"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Deskripsi Data", tabName = "deskripsi", icon = icon("info-circle")),
      menuItem("Dataframe Mentah", tabName = "data_raw", icon = icon("table")),
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Dataframe (Cleaned)", tabName = "data_cleaned", icon = icon("table")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Hasil Analisis", tabName = "hasil_analisis", icon = icon("clipboard-check"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab Deskripsi
      tabItem(tabName = "deskripsi",
              fluidRow(
                box(
                  title = "Kelompok 8", status = "primary", solidHeader = TRUE, width = 12,
                  tags$ul(
                    tags$li("Nabella Yunita Sari (164231019)"),
                    tags$li("Aqila Malfa Zahira (164231036)"),
                    tags$li("Chelsea Dheirranaya Sitinjak (164231051)"),
                    tags$li("Cuthbert Young (164231052)"),
                    tags$li("Athalia Andria Loly Aruan (164231110)")
                  )
                ),
                box(
                  title = "Latar Belakang", status = "primary", solidHeader = TRUE, width = 12,
                  p("Pemberian kredit adalah aktivitas utama..."),
                  p("Analisis ini bertujuan memprediksi risiko gagal bayar...")
                ),
                box(
                  title = "Deskripsi Data", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("desc_table")
                )
              )
      ),
      
      # Tab Dataframe Mentah (filter default saja)
      tabItem(tabName = "data_raw",
              fluidRow(
                box(
                  title = "Dataframe Mentah", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("filter_default_raw", "Filter Berdasarkan Default:", choices = c("Semua", "0", "1")),
                  DTOutput("interactive_table")
                )
              )
      ),
      
      # Tab Preprocessing
      tabItem(tabName = "preprocessing",
              fluidRow(
                box(
                  title = "Cek Missing Value Awal", width = 6, status = "warning", solidHeader = TRUE,
                  plotOutput("missing_plot")
                ),
                box(
                  title = "Ringkasan Data Sebelum Imputasi & Outlier Handling", width = 6, status = "warning", solidHeader = TRUE,
                  verbatimTextOutput("summary_before")
                )
              ),
              fluidRow(
                box(
                  title = "Outlier Analysis (Sebelum Handling)", width = 6, status = "info", solidHeader = TRUE,
                  p("Outlier sebelum handling:"),
                  plotOutput("boxplot_before_outlier")
                ),
                box(
                  title = "Outlier Analysis (Sesudah Handling)", width = 6, status = "info", solidHeader = TRUE,
                  p("Outlier setelah handling:"),
                  plotOutput("boxplot_after_outlier")
                )
              ),
              fluidRow(
                box(
                  title = "Data Setelah Imputasi dan Outlier Handling", width = 6, status = "primary", solidHeader = TRUE,
                  verbatimTextOutput("summary_after")
                ),
                box(
                  title = "Scaling Check (Beberapa Variabel)", width = 6, status = "primary", solidHeader = TRUE,
                  selectizeInput("scaling_vars", "Variabel untuk dicek scaling:", 
                                 choices = names(cleaned_data)[sapply(cleaned_data, is.numeric)], multiple = TRUE,
                                 selected = "income"),
                  plotOutput("dist_scaling_plot")
                )
              ),
              fluidRow(
                box(
                  title = "PCA Visualisasi", width = 6, status = "success", solidHeader = TRUE,
                  plotOutput("pca_plot_ind"), 
                  plotOutput("pca_plot_var")
                ),
                box(
                  title = "Ringkasan PCA", width = 6, status = "success", solidHeader = TRUE,
                  verbatimTextOutput("pca_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Feature Selection (Forward Selection)", status = "primary", solidHeader = TRUE, width = 12,
                  p("Hasil Forward Selection:"),
                  verbatimTextOutput("forward_selection_summary"),
                  p("Model final (logistic_model3) dipilih berdasarkan forward selection ini.")
                )
              )
      ),
      
      # Tab Dataframe (Cleaned) dengan filter default
      tabItem(tabName = "data_cleaned",
              fluidRow(
                box(
                  title = "Dataframe Setelah Outlier & Missing Value Handling", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("filter_default_clean", "Filter Berdasarkan Default:", choices = c("Semua", "0", "1")),
                  DTOutput("cleaned_table")
                )
              )
      ),
      
      # Tab EDA
      tabItem(tabName = "eda",
              fluidRow(
                box(title = "Analisis Deskriptif", width = 12, status = "primary", solidHeader = TRUE,
                    selectizeInput("desc_var_select", "Pilih Variabel:", 
                                   choices = hasil_statistika$Variabel, selected = hasil_statistika$Variabel, multiple=TRUE),
                    DTOutput("desc_stat_table")
                )
              ),
              fluidRow(
                box(title = "Correlation Analysis", width = 6, status = "warning", solidHeader = TRUE,
                    selectizeInput("corr_vars", "Variabel:", 
                                   choices = names(cleaned_data)[sapply(cleaned_data, is.numeric)], 
                                   selected = names(cleaned_data)[sapply(cleaned_data, is.numeric)], multiple=TRUE),
                    plotOutput("corr_plot")
                ),
                box(title = "Distribusi Variabel (Histogram/Boxplot)", width = 6, status = "info", solidHeader = TRUE,
                    selectizeInput("dist_vars", "Variabel:", 
                                   choices = names(cleaned_data)[sapply(cleaned_data, is.numeric)], 
                                   selected = c("income", "age"), multiple=TRUE),
                    radioButtons("plot_type", "Tipe Plot:", choices = c("Histogram", "Boxplot"), inline=TRUE),
                    plotOutput("dist_plot")
                )
              ),
              fluidRow(
                box(title = "Scatter Plot Antar Variabel Numerik", width = 6, status = "primary", solidHeader = TRUE,
                    selectInput("scatter_x", "X Variable:", 
                                choices = names(cleaned_data)[sapply(cleaned_data, is.numeric)], selected = "age"),
                    selectInput("scatter_y", "Y Variable:", 
                                choices = names(cleaned_data)[sapply(cleaned_data, is.numeric)], selected = "income"),
                    plotOutput("scatter_plot")
                ),
                box(title = "Bar Plot (Berdasarkan Variabel Kategori)", width = 6, status = "primary", solidHeader = TRUE,
                    selectInput("bar_var", "Variabel Kategori:", 
                                choices = c("default"), selected = "default"),
                    plotOutput("bar_plot")
                )
              ),
              fluidRow(
                box(title = "Kesimpulan EDA", width = 12, status = "success", solidHeader = TRUE,
                    p("Data siap untuk tahap modeling.")
                )
              )
      ),
      
      # Tab Hasil Analisis
      tabItem(tabName = "hasil_analisis",
              fluidRow(
                box(
                  title = "Ringkasan Model Regresi Logistik", status = "primary", solidHeader = TRUE, width = 12,
                  p("Hasil analisis regresi logistik (Variabel Asli, PCA, dan Model Final hasil Forward Selection):"),
                  verbatimTextOutput("model_summary_original"),
                  verbatimTextOutput("conf_matrix_original"),
                  textOutput("accuracy_original"),
                  verbatimTextOutput("model_summary_pca"),
                  verbatimTextOutput("conf_matrix_pca"),
                  textOutput("accuracy_pca"),
                  p("Model final dipilih berdasarkan Forward Selection (logistic_model3):"),
                  verbatimTextOutput("model_summary_logistic_model3"),
                  textOutput("accuracy_model3")
                )
              ),
              fluidRow(
                box(
                  title = "Visualisasi Hasil (Distribusi Prediksi)", status = "info", solidHeader = TRUE, width = 12,
                  p("Model final dipilih melalui forward selection (logistic_model3)."),
                  plotOutput("prediction_plot_original"),
                  p("Distribusi Prediksi (Variabel Asli)"),
                  plotOutput("prediction_plot_pca"),
                  p("Distribusi Prediksi (PCA)")
                )
              ),
              fluidRow(
                box(
                  title = "Prediksi Data Baru (Model Variabel Asli)", status = "success", solidHeader = TRUE, width = 12,
                  fileInput("newdata_file_original", "Upload CSV:", accept = c(".csv")),
                  actionButton("predict_newdata_original", "Prediksi (Model Variabel Asli)"),
                  DTOutput("newdata_predictions_original")
                )
              )
      )
    )
  )
)

# -----------------------------------------------
# SERVER
# -----------------------------------------------
server <- function(input, output, session) {
  
  # Deskripsi Data
  output$desc_table <- renderDT({
    desc <- data.frame(
      Variabel = c("age", "ed", "employ", "address", "income", "debtinc", "creddebt", "othdebt", "default"),
      Keterangan = c(
        "Usia peminjam (tahun)", 
        "Tingkat pendidikan (1=SD,2=SMA,3=Sarjana,4=Pascasarjana)",
        "Lama pengalaman kerja (tahun)",
        "Lama tinggal di alamat (tahun)",
        "Pendapatan tahunan (ribu dolar)",
        "Rasio total utang-pendapatan (%)",
        "Utang kartu kredit (ribu dolar)",
        "Utang lain (ribu dolar)",
        "Status gagal bayar (1=gagal,0=tidak)"
      ),
      `Tipe Variabel` = c(rep("Independen", 8), "Dependen")
    )
    datatable(desc, options = list(pageLength = 10))
  })
  
  # Filter Dataframe Mentah
  filtered_data_raw <- reactive({
    data <- data_raw
    if (input$filter_default_raw != "Semua") {
      data <- data %>% filter(default == as.numeric(input$filter_default_raw))
    }
    return(data)
  })
  
  output$interactive_table <- renderDT({
    datatable(
      filtered_data_raw(),
      options = list(pageLength = 10, scrollX = TRUE),
      callback = JS(
        "table.rows().every(function() {",
        "  var data = this.data();",
        "  if (data[data.length - 1] == 1) { $(this.node()).css('background-color', '#ffcccc'); }",
        "  else if (data[data.length - 1] == 0) { $(this.node()).css('background-color', '#ccffcc'); }",
        "});"
      )
    )
  })
  
  # Preprocessing Steps
  data_loan <- data_raw
  data_loan$default <- as.character(data_loan$default)
  data_loan$default <- ifelse(data_loan$default %in% c("'0'", ":0", "0"), 0, 1)
  data_loan$default <- as.factor(data_loan$default)
  
  summary_before <- summary(data_loan)
  
  imputed_data <- mice(data_loan, m=1, maxit=5, method='pmm', seed=123)
  completed_data1_imp <- complete(imputed_data)
  
  numeric_columns <- names(completed_data1_imp)[sapply(completed_data1_imp, is.numeric)]
  data_after_outlier <- completed_data1_imp
  for (col in numeric_columns) {
    data_after_outlier[[col]] <- handle_outliers(data_after_outlier[[col]])
  }
  
  output$missing_plot <- renderPlot({
    aggr(data_loan, col=c('lightblue','pink'), numbers=TRUE, sortVars=TRUE, 
         labels=names(data_loan), cex.axis=.8,
         gap=1, ylab=c("Histogram of missing data","Pattern"))
  })
  
  output$summary_before <- renderPrint({
    cat("Summary Data Sebelum Imputasi & Outlier Handling:\n")
    print(summary_before)
    cat("\nJumlah Missing Value per Kolom:\n")
    print(sapply(data_loan, function(x) sum(is.na(x))))
    duplicates <- data_loan %>% filter(duplicated(.))
    cat("\nJumlah Duplikasi Data:", nrow(duplicates), "\n")
  })
  
  output$boxplot_before_outlier <- renderPlot({
    df_before <- data_loan[, numeric_columns, drop=FALSE]
    df_melt_before <- tidyr::gather(df_before, key="Variable", value="Value")
    ggplot(df_melt_before, aes(y=Value)) +
      geom_boxplot(fill="orange") +
      facet_wrap(~Variable, scales="free") +
      labs(title="Outlier Sebelum Handling")
  })
  
  output$boxplot_after_outlier <- renderPlot({
    df_after <- data_after_outlier[, numeric_columns, drop=FALSE]
    df_melt_after <- tidyr::gather(df_after, key="Variable", value="Value")
    ggplot(df_melt_after, aes(y=Value)) +
      geom_boxplot(fill="lightgreen") +
      facet_wrap(~Variable, scales="free") +
      labs(title="Outlier Setelah Handling")
  })
  
  output$summary_after <- renderPrint({
    cat("Summary Data Setelah Imputasi & Outlier Handling:\n")
    print(summary(data_after_outlier))
    cat("\nJumlah Missing Value Setelah Imputasi:\n")
    print(sapply(data_after_outlier, function(x) sum(is.na(x))))
  })
  
  set.seed(42)
  train_indices <- sample(1:nrow(data_after_outlier), size = 0.8 * nrow(data_after_outlier))
  train_data <- data_after_outlier[train_indices, ]
  test_data <- data_after_outlier[-train_indices, ]
  
  # Scaling & PCA
  columns_to_scale_minmax <- c("age", "debtinc", "creddebt")
  scaler_minmax <- preProcess(train_data[, columns_to_scale_minmax], method = "range")
  train_data[, columns_to_scale_minmax] <- predict(scaler_minmax, train_data[, columns_to_scale_minmax])
  test_data[, columns_to_scale_minmax]  <- predict(scaler_minmax, test_data[, columns_to_scale_minmax])
  
  columns_to_scale_robust <- c("income", "othdebt")
  for (col in columns_to_scale_robust) {
    med_val <- median(train_data[[col]], na.rm=TRUE)
    iqr_val <- IQR(train_data[[col]], na.rm=TRUE)
    if (iqr_val == 0) iqr_val <- 1
    train_data[[col]] <- (train_data[[col]] - med_val) / iqr_val
    test_data[[col]] <- (test_data[[col]] - med_val) / iqr_val
  }
  
  numeric_for_pca <- c("age","income","debtinc","creddebt","othdebt")
  train_data[, numeric_for_pca] <- lapply(train_data[, numeric_for_pca], as.numeric)
  test_data[, numeric_for_pca] <- lapply(test_data[, numeric_for_pca], as.numeric)
  
  pca_train <- PCA(train_data[, numeric_for_pca], graph = FALSE)
  
  output$dist_scaling_plot <- renderPlot({
    req(input$scaling_vars)
    data_before_scaling <- data_after_outlier
    df_before <- data_before_scaling[, input$scaling_vars, drop=FALSE]
    df_before_melt <- tidyr::gather(df_before, key="Variable", value="Value")
    df_before_melt$Stage <- "Before Scaling"
    
    df_after <- train_data[, input$scaling_vars, drop=FALSE]
    df_after_melt <- tidyr::gather(df_after, key="Variable", value="Value")
    df_after_melt$Stage <- "After Scaling"
    
    df_all <- rbind(df_before_melt, df_after_melt)
    
    ggplot(df_all, aes(x=Value, fill=Stage)) +
      geom_histogram(alpha=0.8, position="identity", bins=30, color="black") +
      facet_wrap(~Variable, scales="free") +
      scale_fill_manual(values=c("lightblue", "pink")) +
      labs(title="Distribusi Sebelum dan Sesudah Scaling")
  })
  
  output$pca_plot_ind <- renderPlot({
    fviz_pca_ind(
      pca_train,
      label = "none",
      habillage = train_data$default,
      addEllipses = TRUE,
      ellipse.level = 0.95
    )
  })
  
  output$pca_plot_var <- renderPlot({
    fviz_pca_var(pca_train, col.var = "contrib")
  })
  
  output$pca_summary <- renderPrint({
    cat("Hasil PCA (Eigenvalues dan Variance):\n")
    print(pca_train$eig)
  })
  
  # Dataframe Cleaned dengan filter default
  filtered_data_clean <- reactive({
    data <- cleaned_data
    if (input$filter_default_clean != "Semua") {
      data <- data %>% filter(default == as.numeric(input$filter_default_clean))
    }
    return(data)
  })
  
  output$cleaned_table <- renderDT({
    datatable(
      filtered_data_clean(),
      options = list(pageLength = 10, scrollX = TRUE),
      callback = JS(
        "table.rows().every(function() {",
        "  var data = this.data();",
        "  if (data[data.length - 1] == 1) { $(this.node()).css('background-color', '#ffcccc'); }",
        "  else if (data[data.length - 1] == 0) { $(this.node()).css('background-color', '#ccffcc'); }",
        "});"
      )
    )
  })
  
  # EDA
  output$desc_stat_table <- renderDT({
    req(input$desc_var_select)
    datatable(hasil_statistika %>% filter(Variabel %in% input$desc_var_select), 
              options = list(pageLength = 10))
  })
  
  output$corr_plot <- renderPlot({
    req(input$corr_vars)
    sub_data <- cleaned_data[, input$corr_vars, drop=FALSE]
    cor_mat <- cor(sub_data, use="pairwise.complete.obs")
    ggcorrplot(cor_mat, type="lower", hc.order=TRUE, outline.col="white", lab=TRUE)
  })
  
  output$dist_plot <- renderPlot({
    req(input$dist_vars)
    sub_data <- cleaned_data[, input$dist_vars, drop=FALSE]
    df_melt <- tidyr::gather(sub_data, key="Variable", value="Value")
    
    if (input$plot_type == "Histogram") {
      ggplot(df_melt, aes(x=Value)) +
        geom_histogram(fill="aquamarine2", color="black", bins=30) +
        facet_wrap(~Variable, scales="free") +
        labs(title="Histogram Variabel Terpilih")
    } else {
      ggplot(df_melt, aes(y=Value)) +
        geom_boxplot(fill="orange") +
        facet_wrap(~Variable, scales="free") +
        labs(title="Boxplot Variabel Terpilih")
    }
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(cleaned_data, aes_string(x=input$scatter_x, y=input$scatter_y)) +
      geom_point(color="black", fill="skyblue", shape=21, alpha=0.7, size=3, stroke=0.5) +
      labs(title=paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y)) +
      theme_minimal()
  })
  
  output$bar_plot <- renderPlot({
    ggplot(cleaned_data, aes_string(x=input$bar_var)) +
      geom_bar(fill="burlywood") +
      labs(title=paste("Bar Plot:", input$bar_var))
  })
  
  # Forward Selection
  # Gunakan train_data yang sudah di transformasi
  # Lakukan forward selection
  chi_sq <- sapply(train_data[, -which(names(train_data) == "default")], 
                   function(x) chisq.test(table(x, train_data$default))$p.value)
  
  full_model_fs <- glm(default ~ ., data = train_data, family = binomial)
  null_model <- glm(default ~ 1, data = train_data, family = binomial)
  forward_model <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model_fs), direction = "forward")
  
  output$forward_selection_summary <- renderPrint({
    cat("Summary Forward Model:\n")
    print(summary(forward_model))
  })
  
  # Model final (logistic_model3) dari forward selection
  logistic_model3 <- glm(default ~ age + ed + employ + address + debtinc + creddebt, data = train_data, family = binomial)
  test_predictions3 <- predict(logistic_model3, newdata = test_data, type = "response")
  test_class3 <- ifelse(test_predictions3 > 0.5, 1, 0)
  accuracy3 <- mean(test_class3 == test_data$default)
  
  # Model Original & PCA (menggunakan train_pca_result & test_pca_result)
  train_pca <- read.csv("train_pca_result.csv")
  test_pca <- read.csv("test_pca_result.csv")
  
  train_pca$default <- as.factor(train_pca$default)
  test_pca$default <- as.factor(test_pca$default)
  
  if ("coord.Dim.1" %in% names(test_pca)) colnames(test_pca)[which(names(test_pca)=="coord.Dim.1")] <- "Dim.1"
  if ("coord.Dim.2" %in% names(test_pca)) colnames(test_pca)[which(names(test_pca)=="coord.Dim.2")] <- "Dim.2"
  if ("coord.Dim.3" %in% names(test_pca)) colnames(test_pca)[which(names(test_pca)=="coord.Dim.3")] <- "Dim.3"
  if ("coord.Dim.4" %in% names(test_pca)) colnames(test_pca)[which(names(test_pca)=="coord.Dim.4")] <- "Dim.4"
  if ("coord.Dim.5" %in% names(test_pca)) colnames(test_pca)[which(names(test_pca)=="coord.Dim.5")] <- "Dim.5"
  
  # Model Original (dari train_pca)
  logistic_model_original <- glm(default ~ age + ed + employ + address + income + debtinc + creddebt + othdebt,
                                 data = train_pca, family = binomial)
  test_predictions_original_pca <- predict(logistic_model_original, newdata = test_pca, type = "response")
  test_class_original_pca <- ifelse(test_predictions_original_pca > 0.5, 1, 0)
  accuracy_original_pca <- mean(test_class_original_pca == test_pca$default)
  
  # Model PCA
  logistic_model_pca <- glm(default ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, data = train_pca, family = binomial)
  test_predictions_pca <- predict(logistic_model_pca, newdata = test_pca, type = "response")
  test_class_pca <- ifelse(test_predictions_pca > 0.5, 1, 0)
  accuracy_pca <- mean(test_class_pca == test_pca$default)
  
  output$model_summary_original <- renderPrint({
    cat("Summary Model (Variabel Asli):\n")
    summary(logistic_model_original)
  })
  
  output$conf_matrix_original <- renderPrint({
    cat("Confusion Matrix (Variabel Asli):\n")
    print(table(Predicted = test_class_original_pca, Actual = test_pca$default))
  })
  
  output$accuracy_original <- renderText({
    paste("Akurasi Model (Variabel Asli):", accuracy_original_pca)
  })
  
  output$model_summary_pca <- renderPrint({
    cat("Summary Model (PCA):\n")
    summary(logistic_model_pca)
  })
  
  output$conf_matrix_pca <- renderPrint({
    cat("Confusion Matrix (PCA):\n")
    print(table(Predicted = test_class_pca, Actual = test_pca$default))
  })
  
  output$accuracy_pca <- renderText({
    paste("Akurasi Model (PCA):", accuracy_pca)
  })
  
  output$model_summary_logistic_model3 <- renderPrint({
    cat("Summary logistic_model3 (Forward Selection):\n")
    summary(logistic_model3)
  })
  
  output$accuracy_model3 <- renderText({
    paste("Akurasi Model (logistic_model3 - Forward Selection):", accuracy3)
  })
  
  output$train_data_table <- renderDT({
    datatable(head(train_pca), options = list(pageLength = 5))
  })
  
  output$test_data_table <- renderDT({
    datatable(head(test_pca), options = list(pageLength = 5))
  })
  
  output$prediction_plot_original <- renderPlot({
    df_pred_original <- data.frame(Predicted_Prob = test_predictions_original_pca)
    ggplot(df_pred_original, aes(x=Predicted_Prob)) +
      geom_histogram(fill="deeppink3", color="black", bins=30) +
      labs(title="Distribusi Prediksi (Variabel Asli)")
  })
  
  output$prediction_plot_pca <- renderPlot({
    df_pred_pca <- data.frame(Predicted_Prob = test_predictions_pca)
    ggplot(df_pred_pca, aes(x=Predicted_Prob)) +
      geom_histogram(fill="darkolivegreen4", color="black", bins=30) +
      labs(title="Distribusi Prediksi (PCA)")
  })
  
  observeEvent(input$predict_newdata_original, {
    req(input$newdata_file_original)
    newdata_orig <- read.csv(input$newdata_file_original$datapath)
    needed_cols <- c("age","ed","employ","address","income","debtinc","creddebt","othdebt")
    req(all(needed_cols %in% colnames(newdata_orig)))
    
    # Gunakan model original (variabel asli) untuk prediksi data baru
    new_predictions_orig <- predict(logistic_model_original, newdata_orig, type = "response")
    threshold_now <- input$threshold_input
    if (is.null(threshold_now)) threshold_now <- 0.5
    new_class_orig <- ifelse(new_predictions_orig > threshold_now, 1, 0)
    new_res_orig <- cbind(newdata_orig, Predicted_Prob = new_predictions_orig, Predicted_Class = new_class_orig)
    output$newdata_predictions_original <- renderDT({
      datatable(new_res_orig, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

shinyApp(ui = ui, server = server)
