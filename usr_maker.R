library(shiny)
library(bslib)
library(bsicons)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(waiter)

# Increase max file upload size to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# ------------------------------------------------------------------------------
# UI: Modern bslib Layout
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "PP URS Maker",
  
  # Initialize the waiter loading screen engine
  use_waiter(), 
  
  # --- CUSTOM CSS FOR COMPACT TABLE ---
  tags$head(
    tags$style(HTML("
      /* Force the table to be very compact */
      table.dataTable tbody td {
        padding: 2px 4px !important;  /* Minimal padding: Top/Bottom 2px, Left/Right 4px */
        font-size: 12px !important;   /* Slightly smaller font to fit more data */
        line-height: 1.2 !important;  /* Tighter line height */
        vertical-align: middle !important;
      }
      /* Header styling to match */
      table.dataTable thead th {
        padding: 4px 4px !important;
        font-size: 12px !important;
        white-space: nowrap; /* Keep headers on one line */
      }
      /* Ensure the data container uses full width */
      .dataTables_wrapper {
        width: 100% !important;
      }
    "))
  ),
  
  # Theme: Modern, clean, professional
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr", 
    primary = "#2C3E50"
  ),
  
  # Sidebar: Inputs and Actions
  sidebar = sidebar(
    title = "Configuration",
    
    fileInput("file_upload", "Upload Specification",
              buttonLabel = "Browse...",
              placeholder = "No file selected",
              accept = c(".xlsx")),
    
    hr(),
    
    # Explicit instructions for file structure
    card(
      class = "bg-light",
      card_header("Input Requirements"),
      card_body(
        p("For best results, your Excel file should contain ", strong("ONLY"), " these sheets:"),
        tags$ul(
          tags$li("Forms"),
          tags$li("Fields"),
          tags$li("...MASTERDASHBOARD...")
        ),
        p(class = "text-muted small", "Remove extra backup sheets to speed up processing.")
      )
    )
  ),
  
  # Main Content: Cards
  layout_columns(
    col_widths = 12,
    
    # Card 1: Data Preview
    card(
      full_screen = TRUE, 
      card_header(
        class = "bg-primary text-white",
        "Processed Data Preview",
        tooltip(
          bs_icon("info-circle"),
          "This table shows the merged Forms, Fields, and Logical records."
        )
      ),
      card_body(
        class = "p-0", # Remove padding from the card body itself so table hits edges
        DTOutput("table_output")
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER: Backend Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive Data Processing
  cleaned_data <- reactive({
    req(input$file_upload)
    
    # --- ANIMATION START ---
    w <- Waiter$new(html = spin_dots(), color = transparent(.5))
    w$show()
    on.exit(w$hide()) 
    
    w$update(html = tagList(spin_dots(), h4("Reading Excel Sheets...")))
    
    input_path <- input$file_upload$datapath
    
    # --- HELPER FUNCTION: Domain Derivation ---
    derive_domain <- function(oid) {
      case_when(
        oid == "PRIMARY002" ~ NA_character_,
        str_detect(oid, "PRIMARY") ~ NA_character_,
        TRUE ~ substr(oid, 1, 2)
      )
    }
    
    # --- STEP 1: DYNAMIC SHEET DETECTION ---
    all_sheets <- excel_sheets(input_path)
    matrix_sheet_name <- grep("MASTERDASHBOARD", all_sheets, value = TRUE)
    
    validate(
      need("Forms" %in% all_sheets, "Error: 'Forms' sheet is missing."),
      need("Fields" %in% all_sheets, "Error: 'Fields' sheet is missing."),
      need(length(matrix_sheet_name) > 0, "Error: No sheet found containing 'MASTERDASHBOARD'.")
    )
    
    tryCatch({
      raw_forms  <- read_excel(input_path, sheet = "Forms")
      raw_fields <- read_excel(input_path, sheet = "Fields")
      raw_matrix <- read_excel(input_path, sheet = matrix_sheet_name[1])
    }, error = function(e) {
      stop("Error reading Excel file. Is it corrupted? Details: ", e$message)
    })
    
    # --- STEP 2: PRE-PROCESSING METADATA ---
    w$update(html = tagList(spin_dots(), h4("Cleaning HTML Tags & Metadata...")))
    
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
    
    # --- STEP 3: MATRIX VISIT LOGIC ---
    w$update(html = tagList(spin_dots(), h4("Calculating Visit Counts...")))
    
    forms_with_visits <- raw_matrix %>%
      rename(FormOID = 1) %>% 
      mutate(
        X_count = rowSums(across(-(1:2), ~ .x == "X"), na.rm = TRUE)
      ) %>%
      filter(X_count > 1) %>%
      select(FormOID, X_count) %>%
      left_join(select(form_metadata, OID, DraftFormName), by = c("FormOID" = "OID"))
    
    # Placeholder for Visit Logic binding
    final_table_extended <- base_table 
    
    # --- STEP 4: LOGICAL RECORD ROWS ---
    w$update(html = tagList(spin_dots(), h4("Generating Record Rows...")))
    
    extra_rows_record <- final_table_extended %>%
      filter(IsLog == "TRUE") %>%
      distinct(OID, DraftFormName, .keep_all = TRUE) %>% 
      mutate(
        FieldOID = "RECORDNUMBER",
        Ordinal  = 0, 
        Domain   = derive_domain(OID),
        PreText  = "#Record"
      ) %>%
      select(OID, DraftFormName, FieldOID, Ordinal, Domain, PreText)
    
    # --- STEP 5: FINAL ASSEMBLY ---
    final_table <- final_table_extended %>%
      bind_rows(extra_rows_record) %>%
      select(OID, DraftFormName, FieldOID, Ordinal, Domain, PreText) %>%
      distinct(OID, FieldOID, Ordinal, PreText, .keep_all = TRUE) %>%
      arrange(OID, Ordinal)
    
    return(final_table)
  })
  
  # --- OUTPUTS ---
  
  output$table_output <- renderDT({
    req(cleaned_data())
    
    datatable(cleaned_data(), 
              # CHANGED: Added 'cell-border' and 'stripe' for grid look
              # 'compact' is kept, but the CSS above does the heavy lifting
              class = "display compact cell-border stripe", 
              filter = 'top',
              extensions = c('Buttons', 'Scroller'), # Added Scroller for speed with large data
              options = list(
                dom = 'Bfrtip', 
                buttons = list(
                  list(extend = 'copy', title = NULL), 
                  list(extend = 'csv', title = paste0("URS_Data_", Sys.Date())),
                  list(extend = 'excel', title = paste0("URS_Data_", Sys.Date()))
                ),
                deferRender = TRUE, # Improves performance for large tables
                scrollY = 600,      # Vertical scroll height
                scroller = TRUE,    # Smoother scrolling
                scrollX = TRUE,     # Horizontal scrolling
                autoWidth = TRUE,   # Tries to fit width to text
                pageLength = 50     # Show more rows by default since they are small
              ),
              rownames = FALSE)
  })
}

shinyApp(ui, server)