source("./brokerslink.R")

library(shiny)
library(shinyjs)
library(DT)
library(dplyr)

make_url <- function(href, text, open_in_new_tab = TRUE) {
  if (open_in_new_tab)
    paste0("<a href='", href, "' target='_blank'>", text, "</a>")
  else
    paste0("<a href='", href, "'>", text, "</a>")
}

ui <- fluidPage(
  markdown("### Brokerslink Brokers"),
  markdown("This app would scrape data from [Brokerslink](https://www.brokerslink.com/) partners and affiliates list and show it as a dataframe.
  Currently following data are being scraped-
  
  1. [Insurance brokerage](https://www.brokerslink.com/partners-retail-brokers)
  2. [Reinsurance and wholesale](https://www.brokerslink.com/partners-specialist-brokers)
  3. [Risk & consulting firms](https://www.brokerslink.com/partners-specialist-companies)
  4. [Specialized brokers](https://www.brokerslink.com/partners-tech-firms)
  "),
  useShinyjs(),
  shiny::div(id = "wout_loading_md", markdown("#### Loading...")),
  DT::DTOutput(outputId = "wout_num_of_companies_by_category"),
  DT::DTOutput(outputId = "wout_brokerslink_brokers")
)

server <- function(input, output, session) {
  df <- brokerslink_partners_par()
  shinyjs::hide("wout_loading_md")
  output$wout_num_of_companies_by_category <- DT::renderDT(
    df |>
      group_by(partners_and_affiliates_category) |>
      count(name = "#of companies") |>
      DT::datatable(
        rownames= FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE
        )
      )
  )
  output$wout_brokerslink_brokers <- DT::renderDT(
    df |>
      mutate(company_name = make_url(href = company_link, text = company_name)) |>
      select(partners_and_affiliates_category, company_name, company_link) |>
      DT::datatable(
        escape = FALSE,
        rownames= FALSE,
        filter = "top",
        extensions = "Buttons", 
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      )
  )
}

shinyApp(ui = ui, server = server)
