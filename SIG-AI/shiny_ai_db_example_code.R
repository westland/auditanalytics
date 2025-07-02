# --- START OF R SHINY SCRIPT ---

library(shiny)
library(httr)
library(jsonlite)
library(DT) # For better table display

# --- Configuration ---
# IMPORTANT: Replace with your actual OpenAI API Key. Consider using environment variables.
OPENAI_API_KEY <- "YOUR_OPENAI_API_KEY_HERE"

API_BASE_URL <-  "http://24.199.119.110:8000/query"

OPENAI_API_URL <-  "https://api.openai.com/v1/chat/completions"
OPENAI_MODEL <- "gpt-4o" # Fixed model 
REQUEST_TIMEOUT <- 30 # Seconds for API requests

# --- Helper Functions ---

# Function to query the remote database API
query_api <- function(sql_query) {
  if (sql_query == "" || is.null(sql_query)) {
    return(list(error = "Empty SQL query provided."))
  }
  encoded_query <- URLencode(sql_query)
  full_url <- paste0(API_BASE_URL, "?sql=", encoded_query)
  
  tryCatch({
    response <- GET(full_url, timeout(REQUEST_TIMEOUT))
    
    if (status_code(response) == 200) {
      result_text <- content(response, as = "text", encoding = "UTF-8")
      # Handle cases where API might return non-JSON for errors or specific messages
      result_json <- tryCatch(fromJSON(result_text, flatten = TRUE), 
                              error = function(e) list(error = paste("API returned non-JSON or invalid JSON:", result_text)))
      return(result_json)
    } else {
      error_msg <- tryCatch(content(response, as = "text", encoding = "UTF-8"),
                            error = function(e) "Unknown error content")
      return(list(error = paste("HTTP Error:", status_code(response), ":", error_msg)))
    }
  }, error = function(e) {
    return(list(error = paste("Failed to connect to the database API:", e$message)))
  })
}

# Function to call OpenAI API
call_openai <- function(prompt, system_role = "You are a helpful assistant.", temperature = 0.7) {
  if (is.null(OPENAI_API_KEY) || OPENAI_API_KEY == "" || OPENAI_API_KEY == "YOUR_OPENAI_API_KEY_HERE") {
    return("OpenAI API Key not configured.")
  }
  
  payload <- toJSON(list(
    model = OPENAI_MODEL,
    messages = list(
      list(role = "system", content = system_role),
      list(role = "user", content = prompt)
    ),
    temperature = temperature,
    max_tokens = 400 # Limit response length slightly for summaries
  ), auto_unbox = TRUE)
  
  tryCatch({
    res <- POST(
      url = OPENAI_API_URL,
      add_headers(
        `Authorization` = paste("Bearer", OPENAI_API_KEY),
        `Content-Type` = "application/json"
      ),
      body = payload,
      encode = "raw",
      timeout(REQUEST_TIMEOUT) # Use the defined timeout
    )
    
    if (status_code(res) == 200) {
      json_res <- content(res, as = "parsed")
      if (!is.null(json_res$choices[[1]]$message$content)) {
        return(trimws(json_res$choices[[1]]$message$content))
      } else {
        error_detail <- content(res, as="text")
        return(paste("Error: OpenAI response structure unexpected.", error_detail))
      }
    } else {
      error_detail <- content(res, as="text")
      return(paste("Error: OpenAI API request failed with status", status_code(res), "-", error_detail))
    }
  }, error = function(e) {
    return(paste("Error: Failed to connect to OpenAI API -", e$message))
  })
}


# --- UI Definition ---
ui <- fluidPage(
  titlePanel("Database Explorer with AI Summary"),
  
  fluidRow(
    column(12,
           h3("Database Overview"),
           tags$p("Fetching metadata and generating initial AI summary..."),
           wellPanel(
             h4("AI Summary of Database Structure:"),
             textOutput("db_structure_summary")
           ),
           verbatimTextOutput("metadata_fetch_error"), # To show errors during metadata fetch
           tags$hr()
    )
  ),
  
  fluidRow(
    column(6,
           h3("Execute SQL Query"),
           tags$div(
             tags$p("Enter your SQL query below to run against the remote database."),
             tags$strong("Warning:"), " Ensure your queries are safe and intended. Direct execution can be risky."
           ),
           textAreaInput("sql_query_input", "SQL Query:", rows = 5, placeholder = "e.g., SELECT * FROM your_table LIMIT 10"),
           actionButton("run_query_button", "Run Query"),
           tags$br(), tags$br(),
           h4("Query Results:"),
           tags$p("Results from your SQL query will appear below."),
           dataTableOutput("query_results_table"), # Use DT for interactive tables
           verbatimTextOutput("query_error_output") # Show SQL execution errors here
    ),
    column(6,
           h3("AI Analysis of Query Results"),
           tags$p("Once query results are loaded, an AI summary will be generated here."),
           wellPanel(
             h4("AI Summary of Returned Data (approx 200 words):"),
             textOutput("ai_results_summary_output")
           )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive values to store state
  db_metadata <- reactiveVal(NULL)
  initial_summary <- reactiveVal("Initializing...")
  query_results <- reactiveVal(NULL)
  results_summary <- reactiveVal("Enter and run a SQL query to get an AI summary of the results.")
  
  # --- Initial Database Metadata Fetch and Summary (Runs Once on Start) ---
  observe({
    # Show progress to the user
    progress <- shiny::Progress$new()
    progress$set(message = "Fetching database metadata...", value = 0)
    on.exit(progress$close())
    
    all_meta_info <- list()
    fetch_errors <- c()
    
    # 1. Get list of tables
    progress$inc(1/4, detail = "Listing tables")
    tables_result <- query_api("SHOW TABLES")
    
    if (!is.null(tables_result$error)) {
      fetch_errors <- c(fetch_errors, paste("Error listing tables:", tables_result$error))
      table_names <- character(0) # Empty list if error
    } else if (is.null(tables_result) || nrow(tables_result) == 0 || !"name" %in% names(tables_result)) {
      fetch_errors <- c(fetch_errors, "Could not retrieve table names or API response format is unexpected ('name' column missing).")
      table_names <- character(0)
    } else {
      table_names <- tables_result$name
    }
    
    if (length(table_names) > 0) {
      # 2. Get metadata for each table
      for (i in seq_along(table_names)) {
        table <- table_names[i]
        progress$inc( (1/ (length(table_names)+1) )* (2/4) , detail = paste("Describing table:", table)) # Adjust progress increment
        
        # Describe columns
        desc_query <- sprintf("DESCRIBE %s", table)
        cols_result <- query_api(desc_query)
        
        # Get row count
        count_query <- sprintf("SELECT COUNT(*) as row_count FROM %s", table)
        count_result <- query_api(count_query)
        
        table_meta <- list(columns = "Error fetching columns.", row_count = "Error fetching count.")
        
        if (!is.null(cols_result$error)) {
          fetch_errors <- c(fetch_errors, paste("Error describing", table, ":", cols_result$error))
        } else {
          table_meta$columns <- cols_result
        }
        
        if (!is.null(count_result$error)) {
          fetch_errors <- c(fetch_errors, paste("Error counting", table, ":", count_result$error))
        } else if (!is.null(count_result$row_count)) {
          table_meta$row_count <- count_result$row_count
        } else {
          fetch_errors <- c(fetch_errors, paste("Could not get row_count for", table))
          table_meta$row_count <- "Unknown"
        }
        
        all_meta_info[[table]] <- table_meta
      }
    } else if (length(fetch_errors) == 0) { # Only add this message if no prior errors
      fetch_errors <- c(fetch_errors, "No tables found in the database.")
    }
    
    db_metadata(all_meta_info) # Store whatever metadata was fetched
    
    # Display fetch errors if any
    output$metadata_fetch_error <- renderText({
      if(length(fetch_errors) > 0) paste("Metadata Fetch Issues:", paste(fetch_errors, collapse="\n"), sep="\n") else ""
    })
    
    # 3. Generate initial AI summary if metadata is available
    if (length(all_meta_info) > 0) {
      progress$inc(1/4, detail = "Generating AI summary")
      metadata_text <- paste(sapply(names(all_meta_info), function(tbl_name) {
        meta <- all_meta_info[[tbl_name]]
        cols_str <- tryCatch(paste(capture.output(print(meta$columns)), collapse = "\n"), error=function(e) "Column info unavailable")
        paste("Table:", tbl_name, "\nRows:", meta$row_count, "\nColumns:\n", cols_str)
      }), collapse = "\n\n---\n\n")
      
      prompt <- paste("Based on the following database schema information, provide a brief synopsis of what kind of data this database likely contains and its potential purpose. Focus on the table names, column names, and row counts.\n\nSchema:\n", metadata_text)
      
      summary_result <- call_openai(prompt, system_role = "You are an expert data analyst summarizing database structures.")
      initial_summary(summary_result)
    } else {
      initial_summary("Could not fetch sufficient metadata to generate an initial summary.")
    }
    
  })
  
  # Render the initial AI summary
  output$db_structure_summary <- renderText({
    initial_summary()
  })
  
  # --- Handle User SQL Query Execution ---
  observeEvent(input$run_query_button, {
    sql <- input$sql_query_input
    
    # Clear previous outputs
    output$query_results_table <- renderDataTable(NULL) # Clear table
    output$query_error_output <- renderText("")
    results_summary("Running query and generating AI summary...") # Update status
    query_results(NULL) # Clear stored results
    
    if (is.null(sql) || trimws(sql) == "") {
      output$query_error_output <- renderText("SQL query cannot be empty.")
      results_summary("Please enter a valid SQL query.")
      return()
    }
    
    # Show progress
    progress <- shiny::Progress$new()
    progress$set(message = "Executing SQL Query...", value = 0)
    on.exit(progress$close())
    
    # Execute query via API
    progress$inc(1/2, detail = "Contacting database API")
    api_result <- query_api(sql)
    
    # Check for API/Query errors
    if (!is.null(api_result$error)) {
      output$query_error_output <- renderText(paste("Query Error:", api_result$error))
      results_summary("Failed to execute query.")
      query_results(NULL) # Ensure results are cleared on error
    } else if (is.null(api_result) || length(api_result) == 0) {
      output$query_results_table <- renderDataTable({
        # Show an empty data frame with a message if the result is empty but not an error
        data.frame(Message = "Query executed successfully, but returned no data.")
      }, options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE))
      results_summary("Query returned no data.")
      query_results(data.frame()) # Store empty data frame
    }
    else {
      # Display results
      # Convert lists within data frame columns to characters for DT display
      result_df <- as.data.frame(lapply(api_result, function(col) {
        if (is.list(col)) sapply(col, paste, collapse=", ") else col
      }))
      
      query_results(result_df) # Store successful results
      output$query_results_table <- renderDataTable({
        result_df
      }, options = list(pageLength = 10, scrollX = TRUE)) # Options for better table view
      
      # Generate AI summary of results
      progress$inc(1/2, detail = "Generating AI summary of results")
      
      # Prepare data for AI prompt (limit size to avoid exceeding token limits)
      max_rows_for_ai <- 50 
      max_cols_for_ai <- 20
      limited_df <- result_df[1:min(nrow(result_df), max_rows_for_ai), 
                              1:min(ncol(result_df), max_cols_for_ai), drop = FALSE]
      
      # Convert the limited data frame to a string format (e.g., CSV-like or markdown)
      results_string <- capture.output(print(limited_df)) # Simple print output
      results_string <- paste(results_string, collapse = "\n")
      
      # Add context if data was truncated
      truncation_note = ""
      if(nrow(result_df) > max_rows_for_ai || ncol(result_df) > max_cols_for_ai) {
        truncation_note = paste("\n(Note: The data provided below is a truncated sample - ", 
                                min(nrow(result_df), max_rows_for_ai), " rows, ", 
                                min(ncol(result_df), max_cols_for_ai), " columns - of the full result set of ",
                                nrow(result_df), " rows and ", ncol(result_df), " columns.)", sep="")
      }
      
      
      ai_prompt <- paste("The following data was returned from the SQL query:",
                         sql,
                         "\n\nPlease summarize this data in approximately 200 words. Describe the kind of information present, any obvious patterns or key values, and what the data represents.",
                         truncation_note,
                         "\n\nData Sample:\n", results_string)
      
      summary_text <- call_openai(ai_prompt, system_role = "You are a data analyst summarizing query results.")
      results_summary(summary_text)
    }
  })
  
  # Render the AI summary of query results
  output$ai_results_summary_output <- renderText({
    results_summary()
  })
  
}

# --- Run the Shiny App ---
shinyApp(ui = ui, server = server)

# --- END OF R SHINY SCRIPT ---