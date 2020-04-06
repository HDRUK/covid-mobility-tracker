#download pdf
library(lubridate)
library(tidyverse)
library(pdftools)
source('pdf_scrape_functions.R')

#create ability to automate date downloads
#this is the mechanism for saving
date_val = today() - days(7)
# url_google = paste0('https://www.gstatic.com/covid19/mobility/', date_val, '_GB_Mobility_Report_en.pdf')
pdf_destination = paste0('pdf_download/', date_val, '_GB_report')

# download.file(url = url_google, destfile = pdf_destination)

#scrape PDFs
uk_overall_data = get_national_data(pdf_destination)

uk_location_data = get_subnational_data(pdf_destination)


#clear environment
rm(list=setdiff(ls(), c('uk_overall_data', 'uk_location_data', 'date_val')))
