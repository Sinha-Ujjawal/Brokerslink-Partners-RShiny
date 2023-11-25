library(rvest)
library(purrr)
library(furrr)
library(tibble)
library(dplyr)

BROKERSLINK_BASE_LINK <- "https://www.brokerslink.com"

PARTNERS_BASE_LINKS <- c(
  "Insurance brokerage" = paste0(BROKERSLINK_BASE_LINK, "/partners-retail-brokers"),
  "Reinsurance and wholesale" = paste0(BROKERSLINK_BASE_LINK, "/partners-specialist-brokers"),
  "Risk & consulting firms" = paste0(BROKERSLINK_BASE_LINK, "/partners-specialist-companies"),
  "Specialized brokers" = paste0(BROKERSLINK_BASE_LINK, "/partners-tech-firms")
)

process_company_info <- function(company_info) {
  company_href <- company_info %>%
    html_node("a") %>%
    html_attr("href")
  
  company_link <- paste0(BROKERSLINK_BASE_LINK, company_href)
  
  company_name <- company_info %>%
    html_nodes("span") %>%
    (function(x) x[length(x)]) %>%
    html_text(trim = TRUE)
  
  c(company_name = company_name, company_link = company_link)
}

process_companies_from_link <- function(link) {
  read_html(link) %>%
    html_elements(css=".view-content") %>%
    html_children() %>%
    map_dfr(process_company_info)
}

process_companies_from_paged_link <- function(base_link) {
  page_idx <- 0
  df_ret <- list()
  while(TRUE) {
    link <- paste0(base_link, "?page=", page_idx)
    df_companies <- process_companies_from_link(link)
    if (length(df_companies) == 0) {
      break
    }
    df_ret <- rbind(df_ret, df_companies)
    page_idx <- page_idx + 1
  }
  df_ret
}

process_base_link <- function(base_link, partners_and_affiliates_category) {
  process_companies_from_paged_link(base_link) %>%
    mutate(base_link = base_link, partners_and_affiliates_category = partners_and_affiliates_category) %>%
    select(partners_and_affiliates_category, base_link, company_name, company_link)
}

brokerslink_partners <- function() {
   imap_dfr(PARTNERS_BASE_LINKS, process_base_link)
}

brokerslink_partners_par <- function() {
  plan(multisession, workers = length(PARTNERS_BASE_LINKS))
  future_imap_dfr(PARTNERS_BASE_LINKS, process_base_link)
}
