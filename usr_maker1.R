library(shiny)
library(bslib)
library(bsicons)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
# library(waiter) <--- REMOVED this dependency

# Increase max file upload size to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "PP URS Maker",
  
  # REMOVED: use_waiter()
  
  # CSS for Compact Table
  tags$head(
    tags$style(HTML("
      table.dataTable tbody td {
        padding: 2px 4px !important;
        font-size: 12px !important;
        line-height: 1.2 !important;
        vertical-align: middle !important;
      }
      table.dataTable thead th {
        padding: 4px 4px !important;
        font-size: 12px !important;
        white-space: nowrap;
      }
      .dataTables_wrapper { width: 100% !important; }
    "))
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr", 
    primary = "#2C3E50"
  ),
  
  sidebar = sidebar(
    title = "Configuration",
    fileInput("file_upload", "Upload Specification",
              buttonLabel = "Browse...",
              placeholder = "No file selected",
              accept = c(".xlsx")),
    hr(),
    card(
      class = "bg-light",
      card_header("Input Requirements"),
      card_body(
        p("For best results, your Excel file should contain ", strong("ONLY"), " these sheets:"),
        tags$ul(tags$li("Forms"), tags$li("Fields"), tags$li("...MASTERDASHBOARD...")),
        p(class = "text-muted small", "Remove extra backup sheets to speed up processing.")
      )
    )
  ),
  
  layout_columns(
    col_widths = 12,
    card(
      full_screen = TRUE, 
      card_header(
        class = "bg-primary text-white", "Processed Data Preview"
      ),
      card_body(class = "p-0", DTOutput("table_output"))
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  cleaned_data <- reactive({
    req(input$file_upload)
    
    # --- STANDARD SHINY PROGRESS BAR ---
    # This works without any external packages
    withProgress(message = 'Processing file...', value = 0, {
      
      input_path <- input$file_upload$datapath
      derive_domain <- function(oid) {
        case_when(
          oid == "PRIMARY002" ~ NA_character_,
          str_detect(oid, "PRIMARY") ~ NA_character_,
          TRUE ~ substr(oid, 1, 2)
        )
      }
      
      # Step 1: Validation
      incProgress(0.1, detail = "Checking sheets...")
      all_sheets <- excel_sheets(input_path)
      matrix_sheet_name <- grep("MASTERDASHBOARD", all_sheets, value = TRUE)
      
      validate(
        need("Forms" %in% all_sheets, "Error: 'Forms' sheet is missing."),
        need("Fields" %in% all_sheets, "Error: 'Fields' sheet is missing."),
        need(length(matrix_sheet_name) > 0, "Error: No sheet found containing 'MASTERDASHBOARD'.")
      )
      
      # Step 2: Reading
      incProgress(0.3, detail = "Reading Excel data...")
      tryCatch({
        raw_forms  <- read_excel(input_path, sheet = "Forms")
        raw_fields <- read_excel(input_path, sheet = "Fields")
        raw_matrix <- read_excel(input_path, sheet = matrix_sheet_name[1])
      }, error = function(e) {
        stop("Error reading Excel file. Is it corrupted? Details: ", e$message)
      })
      
      # Step 3: Cleaning
      incProgress(0.5, detail = "Cleaning HTML & Metadata...")
      fields_clean <- raw_fields %>%
        mutate(Ordinal = as.numeric(Ordinal)) %>%
        select(FormOID, FieldOID, Ordinal, PreText, IsLog)
      
      form_metadata <- raw_forms %>%
        distinct(OID, DraftFormName) %>%
        filter(!is.na(OID)) %>%
        mutate(Domain = derive_domain(OID))
      
      base_table <- form_metadata %>%
        left_join(fields_clean, by = c("OID" = "FormOID")) %>%
        mutate(PreText = str_remove_all(PreText, "<[^>]+>")) %>% 
        select(OID, DraftFormName, FieldOID, Ordinal, Domain, PreText, IsLog)
      
      # Step 4: Logic
      incProgress(0.7, detail = "Calculating visits & logs...")
      forms_with_visits <- raw_matrix %>%
        rename(FormOID = 1) %>% 
        mutate(X_count = rowSums(across(-(1:2), ~ .x == "X"), na.rm = TRUE)) %>%
        filter(X_count > 1) %>%
        select(FormOID, X_count) %>%
        left_join(select(form_metadata, OID, DraftFormName), by = c("FormOID" = "OID"))
      
      final_table_extended <- base_table 
      
      extra_rows_record <- final_table_extended %>%
        filter(IsLog == "TRUE") %>%
        distinct(OID, DraftFormName, .keep_all = TRUE) %>% 
        mutate(FieldOID = "RECORDNUMBER", Ordinal = 0, Domain = derive_domain(OID), PreText = "#Record") %>%
        select(OID, DraftFormName, FieldOID, Ordinal, Domain, PreText)
      
      # Step 5: Finishing
      incProgress(0.9, detail = "Finalizing table...")
      final_table <- final_table_extended %>%
        bind_rows(extra_rows_record) %>%
        select(OID, DraftFormName, FieldOID, Ordinal, Domain, PreText) %>%
        distinct(OID, FieldOID, Ordinal, PreText, .keep_all = TRUE) %>%
        arrange(OID, Ordinal)
      
      return(final_table)
    }) # End withProgress
  })
  
  output$table_output <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), 
              class = "display compact cell-border stripe", 
              filter = 'top',
              extensions = c('Buttons', 'Scroller'),
              options = list(
                dom = 'Bfrtip', 
                buttons = list(
                  list(extend = 'copy', title = NULL), 
                  list(extend = 'csv', title = paste0("URS_Data_", Sys.Date())),
                  list(extend = 'excel', title = paste0("URS_Data_", Sys.Date()))
                ),
                deferRender = TRUE,
                scrollY = 600,
                scroller = TRUE,
                scrollX = TRUE,
                autoWidth = TRUE,
                pageLength = 50
              ),
              rownames = FALSE)
  })
}

shinyApp(ui, server)