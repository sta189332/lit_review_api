library(plumber)
library(jsonlite)
library(crul)
library(stringr)

#* @apiTitle Q1 Literature Review Metadata Verification API
#* @apiDescription Verifies academic metadata inputs using Crossref and OpenAlex metadata checks.

clean_title <- function(title_string) {
  if (is.null(title_string) || length(title_string) == 0 || is.na(title_string[1])) return("")
  title_string <- as.character(title_string[1])
  tolower(str_replace_all(title_string, "[[:punct:]\\s]+", ""))
}

first_text_value <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  if (length(x) == 0 || is.na(x[1])) return("")
  as.character(x[1])
}

normalise_doi <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace(x, regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE), "")
  x
}

coerce_payload_to_data_frame <- function(req) {
  
  # Case 1: Swagger/OpenAPI sends proper application/json.
  # Plumber parses it into req$body.
  if (!is.null(req$body)) {
    body_obj <- req$body
    
    if (is.data.frame(body_obj)) {
      return(body_obj)
    }
    
    if (is.list(body_obj)) {
      # JSON array of objects often arrives as a list.
      df <- tryCatch({
        jsonlite::fromJSON(jsonlite::toJSON(body_obj, auto_unbox = TRUE))
      }, error = function(e) NULL)
      
      if (is.data.frame(df)) {
        return(df)
      }
      
      # If Swagger wraps it as {"body": "...json..."}
      if ("body" %in% names(body_obj)) {
        body_text <- as.character(body_obj$body)
        df <- tryCatch({
          jsonlite::fromJSON(body_text)
        }, error = function(e) NULL)
        
        if (is.data.frame(df)) {
          return(df)
        }
      }
    }
    
    if (is.character(body_obj)) {
      df <- tryCatch({
        jsonlite::fromJSON(body_obj)
      }, error = function(e) NULL)
      
      if (is.data.frame(df)) {
        return(df)
      }
    }
  }
  
  # Case 2: Plumber places body fields in argsBody.
  if (!is.null(req$argsBody)) {
    
    if (is.data.frame(req$argsBody)) {
      return(req$argsBody)
    }
    
    if (is.list(req$argsBody) && "body" %in% names(req$argsBody)) {
      body_text <- as.character(req$argsBody$body)
      
      if (length(body_text) > 0 && nchar(str_trim(body_text[1])) > 0) {
        df <- tryCatch({
          jsonlite::fromJSON(body_text[1])
        }, error = function(e) NULL)
        
        if (is.data.frame(df)) {
          return(df)
        }
      }
    }
  }
  
  # Case 3: Legacy/raw postBody route.
  if (!is.null(req$postBody) && length(req$postBody) > 0) {
    body_text <- paste(req$postBody, collapse = "\n")
    
    if (nchar(str_trim(body_text)) > 0) {
      df <- tryCatch({
        jsonlite::fromJSON(body_text)
      }, error = function(e) NULL)
      
      if (is.data.frame(df)) {
        return(df)
      }
    }
  }
  
  # Case 4: Raw body fallback.
  if (!is.null(req$bodyRaw) && length(req$bodyRaw) > 0) {
    body_text <- tryCatch({
      rawToChar(req$bodyRaw)
    }, error = function(e) "")
    
    if (nchar(str_trim(body_text)) > 0) {
      df <- tryCatch({
        jsonlite::fromJSON(body_text)
      }, error = function(e) NULL)
      
      if (is.data.frame(df)) {
        return(df)
      }
    }
  }
  
  NULL
}

#* Verify academic metadata incoming from the web front-end
#* @post /api/v1/verify-metadata
#* @parser json
#* @serializer json
function(req, res) {
  tryCatch({
    
    incoming_data <- coerce_payload_to_data_frame(req)
    
    if (is.null(incoming_data) || !is.data.frame(incoming_data)) {
      res$status <- 400
      return(list(
        status = "error",
        message = "No usable JSON body was received. In Swagger, paste the JSON into the Request body box, not into a query parameter."
      ))
    }
    
    if (nrow(incoming_data) == 0) {
      res$status <- 400
      return(list(
        status = "error",
        message = "JSON array is valid but contains no records."
      ))
    }
    
    required_keys <- c("doi", "author", "title", "year")
    missing_keys <- setdiff(required_keys, names(incoming_data))
    
    if (length(missing_keys) > 0) {
      res$status <- 400
      return(list(
        status = "error",
        message = paste(
          "Missing mandatory array columns:",
          paste(missing_keys, collapse = ", ")
        )
      ))
    }
    
    incoming_data$doi <- normalise_doi(unlist(incoming_data$doi))
    incoming_data$title <- as.character(unlist(incoming_data$title))
    incoming_data$author <- as.character(unlist(incoming_data$author))
    incoming_data$year <- suppressWarnings(as.integer(unlist(incoming_data$year)))
    
    polite_headers <- list(
      `User-Agent` = "Q1LitReviewEngine/2.0 (mailto:futathesis@gmail.com)"
    )
    
    crossref_urls <- paste0(
      "https://api.crossref.org/works/",
      utils::URLencode(incoming_data$doi, reserved = TRUE),
      "?mailto=futathesis@gmail.com"
    )
    
    crossref_urls <- as.character(unlist(crossref_urls))
    
    get_crossref_with_retry <- function(url, headers, max_attempts = 3) {
      
      client <- crul::HttpClient$new(
        url = url,
        headers = headers,
        opts = list(timeout = 10)
      )
      
      for (attempt in seq_len(max_attempts)) {
        
        resp <- tryCatch({
          client$get()
        }, error = function(e) e)
        
        if (inherits(resp, "error")) {
          Sys.sleep(1)
          next
        }
        
        if (!is.null(resp$status_code) && resp$status_code == 429) {
          retry_after <- resp$response_headers[["retry-after"]]
          
          wait_seconds <- suppressWarnings(as.numeric(retry_after))
          if (is.na(wait_seconds) || length(wait_seconds) == 0) {
            wait_seconds <- attempt * 2
          }
          
          Sys.sleep(wait_seconds)
          next
        }
        
        return(resp)
      }
      
      resp
    }
    
    async_responses <- vector("list", length(crossref_urls))
    
    for (j in seq_along(crossref_urls)) {
      async_responses[[j]] <- get_crossref_with_retry(
        url = crossref_urls[j],
        headers = polite_headers,
        max_attempts = 3
      )
      
      Sys.sleep(0.35)
    }
    
    incoming_data$verified <- FALSE
    incoming_data$verification_source <- "None"
    incoming_data$error_log <- ""
    incoming_data$title_match_score <- 0.0
    
    for (i in seq_len(nrow(incoming_data))) {
      
      resp <- async_responses[[i]]
      fetched_title <- ""
      
      if (is.null(resp) || inherits(resp, "error")) {
        incoming_data$error_log[i] <- "Network connection dropped or timed out during Crossref request."
        next
      }
      
      if (resp$status_code == 404) {
        
        fallback_url <- paste0(
          "https://api.openalex.org/works/doi:",
          utils::URLencode(incoming_data$doi[i], reserved = TRUE)
        )
        
        oa_client <- crul::HttpClient$new(
          url = fallback_url,
          headers = polite_headers,
          opts = list(timeout = 8)
        )
        
        oa_resp <- tryCatch({
          oa_client$get()
        }, error = function(e) NULL)
        
        if (!is.null(oa_resp) && oa_resp$status_code == 200) {
          
          oa_content <- tryCatch({
            jsonlite::fromJSON(oa_resp$parse("UTF-8"))
          }, error = function(e) NULL)
          
          if (!is.null(oa_content)) {
            fetched_title <- first_text_value(
              if (!is.null(oa_content$title)) oa_content$title else oa_content$display_name
            )
            incoming_data$verification_source[i] <- "OpenAlex"
          } else {
            incoming_data$error_log[i] <- "OpenAlex returned a response, but JSON parsing failed."
            next
          }
          
        } else {
          incoming_data$error_log[i] <- "DOI record not registered in Crossref or OpenAlex systems."
          next
        }
        
      } else if (resp$status_code == 200) {
        
        cr_content <- tryCatch({
          jsonlite::fromJSON(resp$parse("UTF-8"))
        }, error = function(e) NULL)
        
        if (!is.null(cr_content) && !is.null(cr_content$message$title)) {
          fetched_title <- first_text_value(cr_content$message$title)
          incoming_data$verification_source[i] <- "Crossref"
        } else {
          incoming_data$error_log[i] <- "Crossref returned a response, but no title was found."
          next
        }
        
      } else {
        incoming_data$error_log[i] <- paste(
          "Upstream registry returned status code:",
          resp$status_code
        )
        next
      }
      
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
        incoming_data$error_log[i] <- paste0(
          "Title mismatch deviation warning. Expected registry title: '",
          fetched_title,
          "'"
        )
      }
    }
    
    return(list(
      status = "success",
      total_processed = nrow(incoming_data),
      all_passed = all(incoming_data$verified),
      data = incoming_data
    ))
    
  }, error = function(err) {
    res$status <- 500
    return(list(
      status = "runtime_crash",
      message = err$message,
      hint = "Check request parsing, DOI URL construction, Crossref/OpenAlex response parsing, or package versions."
    ))
  })
}