library(plumber)
library(jsonlite)
library(crul)
library(stringr)

#* @apiTitle Q1 Literature Review Metadata Verification API
#* @apiDescription Verifies academic metadata inputs using parallel asynchronous HTTP pooling via crul.

# HELPER: Normalise string text elements for fuzzy title matching gates
clean_title <- function(title_string) {
  if (is.null(title_string) || is.na(title_string)) return("")
  tolower(str_replace_all(title_string, "[[:punct:]\\s]+", ""))
}

#* Verify academic metadata incoming from the web front-end
#* @post /api/v1/verify-metadata
#* @serializer json
function(req, res) {
  # Wrap the entire execution block in a top-level tryCatch to capture precise crashes
  tryCatch({
    
    # 1. Structural Payload Extraction & Parsing Normalisation
    raw_body <- req$postBody
    if (is.null(raw_body) || length(raw_body) == 0 || nchar(paste(raw_body, collapse = "")) == 0) {
      res$status <- 400
      return(list(status = "error", message = "Missing or empty JSON payload matrix."))
    }
    
    # Flatten array lines if read loosely by the web stack gateway
    parsed_body_string <- paste(raw_body, collapse = "\n")
    
    incoming_data <- tryCatch({
      jsonlite::fromJSON(parsed_body_string)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(incoming_data) || !is.data.frame(incoming_data)) {
      res$status <- 400
      return(list(status = "error", message = "Invalid JSON schema layout. Must be a clear array of objects."))
    }
    
    required_keys <- c("doi", "author", "title", "year")
    missing_keys <- setdiff(required_keys, names(incoming_data))
    if (length(missing_keys) > 0) {
      res$status <- 400
      return(list(status = "error", message = paste("Missing mandatory array columns:", paste(missing_keys, collapse = ", "))))
    }
    
    # Clean string spacing structures before dispatching requests
    incoming_data$doi <- str_trim(incoming_data$doi)
    
    # 2. Setup Asynchronous HTTP Client with Crossref Polite Pool Headers
    polite_headers <- list(
      `User-Agent` = "Q1LitReviewEngine/2.0 (mailto:futathesis@gmail.com)"
    )
    
    cc <- crul::HttpClient$new(headers = polite_headers, opts = list(timeout = 5))
    
    # 3. Construct Concurrent URL Query Requests Mapping Vector
    crossref_urls <- paste0("https://crossref.org", incoming_data$doi)
    
    # 4. Fire Async Request Pool (All network queries execute concurrently)
    async_responses <- tryCatch({
      cc$async_get(urls = crossref_urls)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(async_responses)) {
      res$status <- 502
      return(list(status = "error", message = "Asynchronous upstream network mapping initialization loop failure."))
    }
    
    # 5. Initialize Matrix Target Response Storage Layout Fields
    incoming_data$verified <- FALSE
    incoming_data$verification_source <- "None"
    incoming_data$error_log <- ""
    incoming_data$title_match_score <- 0.0
    
    # 6. Parse Results Dataset Map
    for (i in 1:nrow(incoming_data)) {
      resp <- async_responses[[i]]
      
      if (is.null(resp) || inherits(resp, "error")) {
        incoming_data$error_log[i] <- "Network connection dropped or timed out during flight."
        next
      }
      
      # Handle DOI Not Found (HTTP 404) via OpenAlex Fallback Routine
      if (resp$status_code == 404) {
        fallback_url <- paste0("https://openalex.org", incoming_data$doi[i])
        oa_client <- crul::HttpClient$new(opts = list(timeout = 4))
        oa_resp <- tryCatch({ oa_client$get(url = fallback_url) }, error = function(e) NULL)
        
        if (!is.null(oa_resp) && oa_resp$status_code == 200) {
          oa_content <- jsonlite::fromJSON(oa_resp$parse("UTF-8"))
          fetched_title <- oa_content$title
          incoming_data$verification_source[i] <- "OpenAlex"
        } else {
          incoming_data$error_log[i] <- "DOI record not registered in Crossref or OpenAlex systems."
          next
        }
      } else if (resp$status_code == 200) {
        # Primary Crossref Parsing Logic
        cr_content <- jsonlite::fromJSON(resp$parse("UTF-8"))
        fetched_title <- cr_content$message$title
        incoming_data$verification_source[i] = "Crossref"
      } else {
        incoming_data$error_log[i] <- paste("Upstream registry tracking dropped with status code:", resp$status_code)
        next
      }
      
      # 7. Fuzzy String Comparators Logic (Validation Guard)
      cleaned_input_title <- clean_title(incoming_data$title[i])
      cleaned_fetched_title <- clean_title(fetched_title)
      
      distance_score <- utils::adist(cleaned_input_title, cleaned_fetched_title)
      max_len <- max(nchar(cleaned_input_title), nchar(cleaned_fetched_title))
      
      if (max_len > 0) {
        match_pct <- 1 - (distance_score / max_len)
        incoming_data$title_match_score[i] <- round(as.numeric(match_pct), 2)
      } else {
        match_pct <- 0
      }
      
      if (match_pct >= 0.90) {
        incoming_data$verified[i] <- TRUE
        incoming_data$error_log[i] <- "Passed verification."
      } else {
        incoming_data$error_log[i] <- paste0("Title mismatch deviation warning. Expected: '", fetched_title, "'")
      }
    }
    
    # Return valid processing metrics frame
    return(list(
      status = "success",
      total_processed = nrow(incoming_data),
      all_passed = all(incoming_data$verified),
      data = incoming_data
    ))
    
  }, error = function(err) {
    # If ANY unexpected code crash occurs, capture it and return it clearly as a 500 payload
    res$status <- 500
    return(list(
      status = "runtime_crash",
      message = err$message,
      hint = "Check R package versions, array boundary conditions, or data mutations inside the script blocks."
    ))
  })
}
