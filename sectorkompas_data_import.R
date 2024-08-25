
#devtools::install_github("ropensci/tabulizer")
#library(devtools)


#functies om tabellen vanuit pdf's te kunnen lezen

library(tabulapdf)
library(purrr)
library(httr)
library(stringr)
library(tidyverse)
# download link pdf: 
# https://www.ambulancezorg.nl/static/upload/raw/dd0f3beb-7bed-45d3-a7b6-b5e51493726c/AZN+tabellenboek+2018+-+tabellen%2C+grafieken+en+kaarten+-+071019.pdf


save_git_files <- function(repos, filenames, token = NULL, save_dir = tempdir()) {
  
  # Ensure the directory exists or create it
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Function to handle a single file download and read
  process_file <- function(filename) {
    # Encode the file name to handle spaces and special characters
    encoded_filename <- URLencode(filename)
    
    # Define the URL
    url <- str_c(repos, encoded_filename)
    
    # Perform the GET request
    if (!is.null(token)) {
      request <- GET(url, add_headers(Authorization = paste("token", token)))
    } else {
      request <- GET(url)
    }
    
    # Check if the request was successful
    if (status_code(request) == 200) {
      # Save the content to a file with the original name in the specified directory
      save_path <- file.path(save_dir, filename)
      writeBin(content(request, "raw"), save_path)
      message("File saved to: ", save_path)
    } else {
      stop("Failed to download file. HTTP status code: ", status_code(request))
    }
  }
  
  # Apply the function to each filename
  walk(filenames, process_file)

}



# Git HUB connectie naar private repository configureren

repos <- "https://raw.githubusercontent.com/AndySibov/devise/main/"

# tijdelijke token om de repos te kunnen betreden
token <- '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'


pdf_naam <- 'Sectorkompas_Ambulancezorg.pdf'


locatie_pdf <- file.path(tempdir(), pdf_naam)



save_git_files(repos, pdf_naam, token)


#pdf tabellen inlezen

#tabel voor het aantal ambulances per regio

n_ambu <- extract_tables(locatie_pdf, pages = 7,method = 'stream')


n_ambu <- n_ambu%>%pluck(1)%>%
  select(where(~ !all(is.na(.))))

n_ambu <- n_ambu%>%select(-1)%>%
  rename_with(~ "VR_naam", .cols = 1)



#tabel voor het aantal standplaatsen per regio

n_standplaatsen <- extract_tables(locatie_pdf, pages = 10,method = 'stream')

n_standplaatsen <- n_standplaatsen%>%pluck(1)%>%
  select(where(~ !all(is.na(.))))

n_standplaatsen <- n_standplaatsen%>%select(-1)%>%
  rename_with(~ "VR_naam", .cols = 1)


#tabel voor het aantal inzetten per regio

n_inzetten <- extract_tables(locatie_pdf, pages = 14,method = 'stream')[[1]]

names(n_inzetten) <- c('VR_naam', '2019', '2018', '2017', '2016', '2015')

# formateren en joinen

# transponeren

am <- n_ambu%>%pivot_longer(cols = -1, names_to = 'jaar', values_to = 'ambulances')
# transponeren

st <- n_standplaatsen%>%pivot_longer(cols = -1, names_to = 'jaar', values_to = 'standplaatsen')

# transponeren

inz <- n_inzetten%>%
  pivot_longer(cols = -1, names_to = 'jaar', values_to = 'inzetten')%>%
  #opschonen
  mutate(inzetten = as.numeric(gsub('\\.', '', inzetten)),
         VR_naam = sub("^\\d+\\s+", "", VR_naam))


#joinen

data_sector <- am%>%left_join(st)%>%left_join(inz, by = join_by('VR_naam', 'jaar'))%>%filter(VR_naam != 'totaal')

# dataset opslaan als csv

#write_csv(data_sector, file = 'data_sectorkompas_RAV.csv')



library(readxl)

excel_referentie_tbl <- 'Pc4gem20230101ROAZ_v120230213.xlsx'

save_git_files(repos, excel_referentie_tbl, token)

ref_table_regio <- file.path(tempdir(), excel_referentie_tbl)
ref_reg <- read_excel(ref_table_regio, sheet = 'pc4_geo_postbus_NUTS')


#verschillen in de benamingen in data_sector
setdiff(
  data_sector$VR_naam%>%unique,
  ref_reg$VR_naam%>%unique)

library(forcats)

ref_reg <- ref_reg%>%
  mutate(VR_naam = 
           as.character(fct_recode(fct(VR_naam%>%gsub('-', ' ',.)),
                                   "Friesland" = "Fryslân" ,
                                   "Gooi  en Vechtstreek" ="Gooi en Vechtstreek",
                                   "Midden Gelderland" = "Gelderland Midden",
                                   "Midden West Brabant" = "Midden  en West Brabant",
                                   "Noord  en Midden Limburg" = "Limburg Noord",
                                   "Noordoost Gelderland"  = "Noord  en Oost Gelderland",
                                   "Zuid Limburg" = "Limburg Zuid")))
VR_naam_ref <- ref_reg%>%distinct(VR_naam, Provincie)

# check of ze nu hetzelde zijn

setdiff(
  data_sector$VR_naam%>%unique%>%gsub('-', ' ',.)%>%sort(),
  ref_reg$VR_naam%>%unique%>%sort()
)


# nu de provincie ophalen uit de ref tabel en koppelen met de data_sektor dataset
data_sector <- data_sector%>%
  mutate(VR_naam = gsub('-', ' ',VR_naam))%>%
  left_join(VR_naam_ref)%>%
  # namen aanpassen zpdat zij de devise dataset matcht
  mutate(Provincie = 
           as.character(fct_recode(fct(Provincie%>%gsub('-', ' ',.)),
                                   "Friesland" = "Fryslân",
                                   "Brabant" = "Noord Brabant")))




