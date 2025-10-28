library(tidyverse)

###### 
#  
# Looking at identifying current status of NSF terminated grants, 
# and merging in information from USASpending to see if funds have been deobligated

###### Identify Terminated NSF grants

# download list of terminated NSF grants downloaded from grant-witness.us
# pass award_ids back to NSF API to get current grant status (exclude reinstated grants)
termed_grants <- read_csv("nsf_terminations.csv")

all_awd_ids <- termed_grants$grant_id

nsf_api_result <- get_nsf_award_api(termed_grants$grant_id) # this function from the nsf_awd_api.R script

nsf_data_termed_grants <- nsf_api_result |>
  filter(awdSpAttnCode == '37')

###### USASpending data to get actual obligations and deobligations

# this data source can lag several weeks behind actuals

# i used the website download but it only has deobligations data for the EDU appropriations
usa_spending <- read_csv("All_Assistance_PrimeTransactions_2025-10-27_H23M30S20_1.csv")

usa_spending <- usa_spending |>
  mutate(awd_id = as.character(award_id_fain))

amendments <- usa_spending |>
  filter(awd_id %in% nsf_data_termed_grants$id,
          federal_action_obligation < 0)

# take 2 on USAspending 
# construct the unique grant ID and search by that using the transaction endpoint
# this API lists each amendment for the award, with the amendment number ("Modification number"),
# date, and amendment amount. A negative number indicates a deob

url_base <- "https://api.usaspending.gov/api/v2/transactions/"

termed_list <- tibble(nsf_id = nsf_data_termed_grants$id, usaspending_id = paste0('ASST_NON_',nsf_data_termed_grants$id,'_049'))

usa_spending2 <- NULL

# I haven't used a POST API before so this is a learning experience.

for (i in 1:nrow(termed_list)) {
  payload <- list(award_id = termed_list[[i,2]])

  response <- httr::POST(url_base, body = payload)$content |>
    rawToChar(as.raw(strtoi(16L))) |> # hex blob to text
    paste0(collapse = "") |>
    jsonlite::fromJSON()

  if (length(response$results) == 0) next # skip over empty results

  response_df <- response$results |>
    mutate(nsf_id = termed_list[[i,1]])

  usa_spending2 <- bind_rows(usa_spending2, response_df)

  Sys.sleep(2)
}

deobs <- usa_spending2 |>
  filter(federal_action_obligation < 0, 
          action_date > ymd('2025-04-01'))
# same result as the download file - and the file was a lot quicker
# but at least I learned about POST APIs

usa_spending2 |>
  filter(federal_action_obligation < 0, 
          action_date > ymd('2025-04-01')) |>
  left_join(nsf_data_termed_grants, by = c("nsf_id" = "id")) |>
  group_by(orgLongName) |>
  count()
# 87 deobs all from the EDU appropriation
# R&RA and non-appropriated deobs not represented

usa_spending2 |>
  group_by(nsf_id) |>
  slice_max(modification_number) |>
  filter(action_date > ymd('2024-04-01')) |>
  left_join(nsf_data_termed_grants, by = c("nsf_id" = "id")) |>
  group_by(orgLongName) |>
  count()
# definitly missing the deob amendments - lots of $0 amendments listed. 
# the pattern appears to be a $0 amendment at the time of grant termination
# followed by another amendment with the deob when that is processed after financial close
# the deobs for the R&RA terminated awards haven't made it to USA Spending for some reason?
# but many were processed in September 2025 alongside the EDU ones listed


  