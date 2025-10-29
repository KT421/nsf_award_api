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

# i used the website download 
# https://www.usaspending.gov/download_center/custom_award_data
# Prime Awards, Awarding Agency NSF, Last Modified Date, FY 2025
usa_spending <- read_csv("All_Assistance_PrimeTransactions_2025-10-29_H16M40S47_1.csv")

# data notes USASpending to NSF terminology
# award_id_fain = nsf award_id
# modification number = amendment id
# action date = amendment date (last_updated_date appears to be when usaspending got the data?)

usa_spending <- usa_spending |>
  mutate(awd_id = as.character(award_id_fain))

all_deob_amendments <- usa_spending |>
  filter(federal_action_obligation < 0)

termed_deob_amendments <- all_deob_amendments |>
  filter(awd_id %in% termed_grants$grant_id)

FY25_totals <- usa_spending |> 
  summarise(count = n(),
          total = sum(federal_action_obligation)) |>
  mutate(total = scales::dollar(total))

# function to set transactions to just MM/YY for ease of grouping
# can't JUST use month because FY starts in October
yearmonth <- function(date_string) {
  paste0(year(date_string),"-",month(date_string),"-01") |> ymd()
}

FY25_monthly_totals <- usa_spending |> 
  group_by(month = yearmonth(action_date)) |> 
  summarize(count = n(), total = sum(federal_action_obligation)) |> 
  mutate(total = scales::dollar(total)) |>
  pivot_wider(names_from = month, values_from = c(count, total))

FY25_monthly_totals_dir <- usa_spending |> 
  group_by(cfda_title, month = yearmonth(action_date)) |> 
  summarize(count = n(), total = sum(federal_action_obligation)) |> 
  mutate(total = scales::dollar(total)) |>
  pivot_wider(names_from = month, values_from = c(count, total)) 

# visualize monthly obligations
usa_spending |>
  group_by(month = yearmonth(action_date)) |> 
  summarize(count = n(), total = sum(federal_action_obligation)) |> 
  ggplot() +
  geom_line(aes(x = month, y = total, group = 1)) +
  scale_y_continuous(labels = scales::label_currency(), breaks = seq(from = 0, to = 3000000000, by = 500000000)) +
  scale_x_date(date_breaks = "month", date_labels = "%b-%Y") +
  geom_label(aes(x = month, y = total, label = total))

# Analysis: only the EDU deobligations are visible in the USASpending data. A termination is shown as a $0 Amendment on the 
# termination date, followed by a deobligation amendment of any remaining funds a month or two later, when the award is Financially Closed

# the non-EDU terminations have the $0 amendment but not the deob amendment in USASpending. Until that data catches up or the ETL is fixed,
# we cannot see the deob amount, although we can estimate it as total obligation minus total outlays. This will be an overestimate
# since the awards get those 30 days to drawdown any outstanding expenses and certain closeout costs. But it'll be close enough for a rough estimate.
# Not all of these terminations have been deobligated yet. How to tell? NSF's public API doesn't give us granular amendment data
# but it does tell us the last amendment date. If there is an amendment after the one listed on USASpending, we know we have missing data
# and if it's a terminated award, the only reasonable assumption is that the unlisted amendment is the deob amendment.

# find amendments associated with terminated awards

nsf_data_trimmed <- nsf_data_termed_grants |>
  select(id, latestAmendmentDate, dirAbbr, divAbbr, estimatedTotalAmt, fundsObligatedAmt) |>
  mutate(latestAmendmentDate = mdy(latestAmendmentDate),
        estimatedTotalAmt = as.numeric(estimatedTotalAmt),
        fundsObligatedAmt = as.numeric(fundsObligatedAmt))

termed_amd <- usa_spending |>
  filter(awd_id %in% nsf_data_trimmed$id) |>
  mutate(amd_type = case_when(
    federal_action_obligation == 0 ~ "administrative_amd", # this is a zero dollar "paperwork only" amendment that in this case ought to be termination notices but can be other things
    federal_action_obligation < 0 ~ "deobligation_amd",
    federal_action_obligation > 0 ~ "obligation_amd"
  )) |>
  select(awd_id, modification_number, action_date,amd_type, federal_action_obligation, total_obligated_amount, total_outlayed_amount_for_overall_award) |>
  filter(amd_type != "obligation_amd",
          action_date > ymd("2025-04-15"))

termed_amd |> 
  count(amd_type)

termed_amd <- termed_amd |>
  left_join(nsf_data_trimmed, by = c("awd_id" = "id"))

termed_amd2 <- termed_amd |>
  group_by(awd_id) |>
  mutate(most_recent = ifelse(any(latestAmendmentDate == action_date),T,F))

termations_with_missing_amd <- termed_amd2 |>
  filter(!most_recent) |>
  mutate(estimated_deob = total_obligated_amount - fundsObligatedAmt) 
# estimate the Deob as what USASpending thinks the total obligation is minus what NSF thinks the total obligation is

sum(termed_deob_amendments$federal_action_obligation)
# 47 million in deobligations documented on USASpending

sum(termations_with_missing_amd$estimated_deob, na.rm = T)
# 16 million in estimated undocumented deobligations
# have no way to validate this but proportionally it seems reasonable

#############################

# another take on USAspending data -- this ended up being a dead end, albeit an educational one
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
