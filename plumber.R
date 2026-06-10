library(plumber)
library(jsonlite)
library(httr)
library(stringr)

#* @apiTitle Q1 Literature Review Metadata Verification API
#* @apiDescription Verifies academic metadata inputs from LLM engines using Crossref and OpenAlex.

#' Helper function to clean and normalise titles for fuzzy matching
clean_title <- function(title_string) {
  if (is.null(title_string) || is.na(title_string)) return("")
  tolower(str_replace_all(title_string, "[[:punct:]\\s]+", ""))
}

#* Verify academic metadata incoming from the web front-end
#* @post /api/v1/verify-metadata
#* @param req The incoming HTTP request containing the JSON data payload
#* @serializer json
function(req) {
  # 1. Parse and validate incoming payload safety
  raw_body <- req$postBody
  if (is.null(raw_body) || nchar(raw_body) == 0) {
    res <- plumber::forward()
    res$status <- 400
    return(list(status = "error", message = "Missing or empty JSON payload."))
  }
  
  # Protect against malformed JSON structures from client side
  incoming_data <- tryCatch({
    jsonlite::fromJSON(raw_body)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(incoming_data) || !is.data.frame(incoming_data)) {
    return(list(status = "error", message = "Invalid JSON format. Must be an array of objects."))
  }
  
  # Ensure mandatory keys exist before processing to avoid runtime exceptions
  required_keys <- c("doi", "author", "title", "year")
  missing_keys <- setdiff(required_keys, names(incoming_data))
  if (length(missing_keys) > 0) {
    return(list(
      status = "error", 
      message = paste("Missing required fields:", paste(missing_keys, collapse = ", "))
    ))
  }
  
  # 2. Initialize verification metrics columns
  incoming_data$verified <- FALSE
  incoming_data$verification_source <- "None"
  incoming_data$error_log <- ""
  incoming_data$title_match_score <- 0.0
  
  # 3. Stateless verification execution loop (Per-Item Check)
  for (i in 1:nrow(incoming_data)) {
    current_doi <- str_trim(incoming_data$doi[i])
    
    # Fail quickly if DOI format is completely missing or corrupted
    if (is.na(current_doi) || !str_detect(current_doi, "^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$")) {
      incoming_data$error_log[i] <- "Malformatted or invalid DOI syntax structure."
      next
    }
    
    # Construct standard Crossref REST API URL endpoint
    crossref_url <- paste0("https://crossref.org", current_doi)
    
    # Execute API call with a strict timeout footprint to keep web server snappy
    api_response <- tryCatch({
      httr::GET(
        crossref_url, 
        httr::add_headers(`User-Agent` = "Q1LitReviewEngine/1.0 (mailto:your-email@domain.com)"),
        httr::timeout(5)
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(api_response)) {
      incoming_data$error_log[i] <- "Crossref connection timeout or network routing drop."
      next
    }
    
    # Evaluate HTTP standard protocol responses
    if (api_response$status_code == 404) {
      # Fallback mechanism: Check OpenAlex if Crossref lacks the record
      openalex_url <- paste0("https://openalex.org", current_doi)
      openalex_resp <- tryCatch({ httr::GET(openalex_url, httr::timeout(5)) }, error = function(e) NULL)
      
      if (!is.null(openalex_resp) && openalex_resp$status_code == 200) {
        # OpenAlex recovery successful
        oa_content <- jsonlite::fromJSON(httr::content(openalex_resp, "text", encoding = "UTF-8"))
        fetched_title <- oa_content$title
        incoming_data$verification_source[i] <- "OpenAlex"
      } else {
        incoming_data$error_log[i] <- "DOI not found across Crossref or OpenAlex index registers."
        next
      }
    } else if (api_response$status_code == 200) {
      # Crossref primary hit logic
      cr_content <- jsonlite::fromJSON(httr::content(api_response, "text", encoding = "UTF-8"))
      fetched_title <- cr_content$message$title[1]
      incoming_data$verification_source[i] <- "Crossref"
    } else {
      incoming_data$error_log[i] <- paste("Upstream registry returned status code:", api_response$status_code)
      next
    }
    
    # 4. Strict Title String Comparators (The Anti-Hallucination Gate)
    cleaned_input_title <- clean_title(incoming_data$title[i])
    cleaned_fetched_title <- clean_title(fetched_title)
    
    # Calculate deterministic matching score based on Levenshtein Distance
    distance_score <- utils::adist(cleaned_input_title, cleaned_fetched_title)[1,1]
    max_len <- max(nchar(cleaned_input_title), nchar(cleaned_fetched_title))
    match_pct <- 1 - (distance_score / max_len)
    incoming_data$title_match_score[i] <- round(match_pct, 2)
    
    # Gate threshold matching logic setup (Tolerance barrier set to 90% for edge punctuation diffs)
    if (match_pct >= 0.90) {
      incoming_data$verified[i] <- TRUE
      incoming_data$error_log[i] <- "Metadata successfully cleared validation checks."
    } else {
      incoming_data$error_log[i] <- paste0("Title mismatch deviation warning. Expected: '", fetched_title, "'")
    }
  }
  
  # 5. Output unified payload matrix straight back to web client framework
  return(list(
    status = "success",
    total_processed = nrow(incoming_data),
    all_passed = all(incoming_data$verified),
    data = incoming_data
  ))
}
