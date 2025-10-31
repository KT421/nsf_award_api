# Getting NSF award termination data from the public facing Award API

library(tidyverse)

#' Search NSF award api
#' 
#' This function is a wrapper for the public facing award API
#' 
#' NSF public facing award database API is described here: https://resources.research.gov/common/webapi/awardapisearch-v1.htm
#' 
#' Returns a maximum of 10000 results at once, which is the API max. If you need more, you need to chunk your query somehow
#'
#' @param ... Desired search parameters as described in the API spec
#'
#' @returns dataframe of results from API
#'
#' @export
#' @examples {
#' # search for awards about water made to institutions in the state of Texas
#' search_nsf_award_api(keyword = "water",
#'                      awardeeStateCode = "TX")
#' }
search_nsf_award_api <- function(...) {
  
  params <- paste(names(list(...)), list(...), collapse = "&", sep = "=")

  offset <- seq(from = 0, to = 7500, by = 2500)

  urls <- paste0("https://www.research.gov/awardapi-service/v1/awards.json?",params,"&offset=",offset,"&rpp=2500")

  results <- NULL

  for (i in urls) {
    this_results <- jsonlite::fromJSON(URLencode(i))

    this_results <- as.data.frame(this_results$response$award)

    results <- dplyr::bind_rows(results,this_results)

    if (nrow(this_results) < 2500) break
    }

  if (nrow(results) == 10000) warning("API Max of 10,000 results reached.")

  results 
}


# if you already know your award_ids
# does 25 awd_ids at a time
get_nsf_award_api <- function(award_ids) {

  results <- NULL

  awd_ids <- split(award_ids, ceiling(seq_along(award_ids)/25)) 

  awd_ids <- map(awd_ids, ~ paste0(.x, collapse = ","))

  urls <- paste0("https://www.research.gov/awardapi-service/v1/awards/",awd_ids,".json")

  for (i in urls) {

    this_results <- jsonlite::fromJSON(URLencode(i))

    this_results <- as.data.frame(this_results$response$award)

    results <- dplyr::bind_rows(results,this_results)

    Sys.sleep(2)  
  }

  results 
}

###########################

# Sample ~~use~~ abuse of the API: Identifying terminated grants, the hard way
# (the easy way is to use grant-witness.us)

# Terminated Grants are identified by awdSpAttnCode == 37
# Since awdSpAttnCode is not a searchable parameter, we have to get everything it could possibly be applied to,
# which means every award with an end date after terminations started occuring, the earlist of which is Apr 2025
# Brute force; get every award with an end date in 2025 or later
# chunked by end date because the API only returns 10000 results maximum 
# MOST of the results will be in 2025 because award that are terminated have their end date changed to the termination date
# but that change is not automatic and lags some weeks behind the awdSpAttnCode update
results2025_1 <- search_nsf_award_api(expDateStart="01/01/2025",expDateEnd="12/30/2025")
results2025_2 <- search_nsf_award_api(expDateStart="07/01/2025",expDateEnd="12/31/2025")
results2026_1 <- search_nsf_award_api(expDateStart="01/01/2026",expDateEnd="06/30/2026")
results2026_2 <- search_nsf_award_api(expDateStart="07/01/2026",expDateEnd="08/31/2026")
results2026_3 <- search_nsf_award_api(expDateStart="09/01/2026",expDateEnd="12/31/2026")
results2027_1 <- search_nsf_award_api(expDateStart="01/01/2027",expDateEnd="06/30/2027")
results2027_2 <- search_nsf_award_api(expDateStart="07/01/2027",expDateEnd="12/31/2027")
results2028 <- search_nsf_award_api(expDateStart="01/01/2028",expDateEnd="12/31/2028")
results2029 <- search_nsf_award_api(expDateStart="01/01/2029",expDateEnd="12/31/2029")
results2030 <- search_nsf_award_api(expDateStart="01/01/2030")

all_results <- dplyr::bind_rows(results2025_1, results2025_2, results2026_1, results2026_2, results2026_3,
                          results2027_1, results2027_2, results2028, results2029, results2030) |>
  distinct() # not sure why there are some duplicates but there are


all_results |> count(awdSpAttnCode)


terminated_awds <- all_results |>
  filter(awdSpAttnCode == '37') |>
  mutate(expDate = mdy(expDate),
        startDate = mdy(startDate))
