# 1. Loading libraries ####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, pdftools, tidyverse, writexl, DT)

# 2. User Interface ####
ui <- fluidPage(
    shinythemes::shinytheme("cyborg"),
    titlePanel("Financial Report Data Extractor"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Upload Section"),
            fileInput("upload", "Choose Portfolio PDF", 
                      accept = ".pdf"),
            hr(),
            downloadButton("downloadData", 
                           "Download Cleaned Excel", 
                           class = "btn-success")),
        
        mainPanel(
            h4("Data Preview"),
            DTOutput("preview_table"))))

# 3. Server Logic ####
server <- function(input, output) {
    
    # Processing the PDF reactively
    processed_data <- reactive({
        req(input$upload)
        
        # Reading the uploaded file
        raw_text <- pdf_text(input$upload$datapath)
        all_lines <- raw_text[2] %>% read_lines() %>% str_trim()
        
        # The Extraction Logic
        start_row <- which(str_detect(all_lines, 
                                      "Security Type")) + 1
        end_row <- which(str_detect(all_lines, "TOTAL"))
        
        portfolio_final <- all_lines[start_row:end_row] %>%
            as_tibble() %>%
            mutate(value = str_replace_all(value, "\\s{2,}", "|")) %>%
            separate(value, 
                     into = c("Security_Type", "Par_Value", 
                              "Book_Value", "Market_Value", 
                              "Market_Book_Price", 
                              "Current_Percent", "Max_Policy", 
                              "Compliant"), 
                     sep = "\\|", extra = "merge", 
                     fill = "right") %>%
            mutate(across(everything(), ~str_trim(.))) %>% 
            mutate(across(everything(), 
                          ~str_replace(., "^-$", "0"))) %>%
            mutate(across(c(Par_Value, Book_Value, Market_Value), 
                          ~str_remove_all(., "[\\$,]")))
        
        return(portfolio_final)
    })
    
    # Render the preview table
    output$preview_table <- renderDT({
        datatable(processed_data(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Handle the Excel download
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("SF_Treasurer_Cleaned_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write_xlsx(processed_data(), file)
        }
    )
}

# 4. Run the App ####
shinyApp(ui, server)


