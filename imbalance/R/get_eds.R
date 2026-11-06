library(httr2)

# area = NULL without price areas, columns = NULL for all
get_eds <- function(dataset, start, end, area = "DK1", columns = NULL) {
  req <- request("https://api.energidataservice.dk/dataset") |>
    req_url_path_append(dataset) |>
    req_url_query(start = start, end = end, limit = 0)                # 0 = all records
  if (!is.null(area))    req <- req_url_query(req, filter = jsonlite::toJSON(list(PriceArea = area)))
  if (!is.null(columns)) req <- req_url_query(req, columns = paste(columns, collapse = ","))
  resp <- req |>
    req_retry(max_tries = 6, retry_on_failure = TRUE, backoff = \(i) min(15 * 2^i, 240)) |>  # 30s, 1, 2, 4, 4 min
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  resp$records
}
