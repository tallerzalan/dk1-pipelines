
library(httr2)

get_eds <- function(dataset, start, end, area = "DK2") {
  resp <- request("https://api.energidataservice.dk/dataset") |>
    req_url_path_append(dataset) |>
    req_url_query(
      start  = start,
      end    = end,
      filter = jsonlite::toJSON(list(PriceArea = area)),
      limit  = 0                                  # 0 = all records
    ) |>
    req_retry(max_tries = 6, retry_on_failure = TRUE, backoff = \(i) min(15 * 2^i, 240)) |>  # 30s, 1, 2, 4, 4 min
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  resp$records
}

