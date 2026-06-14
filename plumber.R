library(plumber)
library(jsonlite)
library(crul)
library(stringr)

#* @apiTitle Q1 Literature Review Metadata Verification API
#* @apiDescription Verifies academic metadata inputs using DOI.org, Crossref, and OpenAlex metadata checks.

# -----------------------------
# Helper functions
# -----------------------------

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
  x <- str_replace(x, regex("^doi:", ignore_case = TRUE), "")
  x <- str_trim(x)
  x
}

coerce_payload_to_data_frame <- function(req) {
  
  # Case 1: Swagger/OpenAPI sends proper application/json.
  if (!is.null(req$body)) {
    body_obj <- req$body
    
    if (is.data.frame(body_obj)) {
      return(body_obj)
    }
    
    if (is.list(body_obj)) {
      df <- tryCatch({
        jsonlite::fromJSON(jsonlite::toJSON(body_obj, auto_unbox = TRUE))
      }, error = function(e) NULL)
      
      if (is.data.frame(df)) {
        return(df)
      }
      
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

doi_resolves_via_proxy <- function(doi, headers) {
  doi <- normalise_doi(doi)
  
  if (is.null(doi) || length(doi) == 0 || is.na(doi[1]) || nchar(str_trim(doi[1])) == 0) {
    return(list(resolves = FALSE, status_code = NA_integer_))
  }
  
  doi_url <- paste0(
    "https://doi.org/",
    utils::URLencode(doi[1], reserved = TRUE)
  )
  
  client <- crul::HttpClient$new(
    url = doi_url,
    headers = headers,
    opts = list(
      timeout = 10,
      followlocation = FALSE
    )
  )
  
  head_resp <- tryCatch({
    client$head()
  }, error = function(e) NULL)
  
  if (!is.null(head_resp) && !is.null(head_resp$status_code)) {
    if (head_resp$status_code %in% 200:399) {
      return(list(resolves = TRUE, status_code = head_resp$status_code))
    }
  }
  
  get_resp <- tryCatch({
    client$get()
  }, error = function(e) NULL)
  
  if (!is.null(get_resp) && !is.null(get_resp$status_code)) {
    if (get_resp$status_code %in% 200:399) {
      return(list(resolves = TRUE, status_code = get_resp$status_code))
    } else {
      return(list(resolves = FALSE, status_code = get_resp$status_code))
    }
  }
  
  list(resolves = FALSE, status_code = NA_integer_)
}

get_crossref_with_retry <- function(url, headers, max_attempts = 3) {
  
  client <- crul::HttpClient$new(
    url = url,
    headers = headers,
    opts = list(timeout = 10)
  )
  
  last_resp <- NULL
  
  for (attempt in seq_len(max_attempts)) {
    
    resp <- tryCatch({
      client$get()
    }, error = function(e) e)
    
    if (inherits(resp, "error")) {
      last_resp <- resp
      Sys.sleep(1)
      next
    }
    
    last_resp <- resp
    
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
  
  last_resp
}

get_openalex_by_doi <- function(doi, headers) {
  
  fallback_url <- paste0(
    "https://api.openalex.org/works/doi:",
    utils::URLencode(doi, reserved = TRUE)
  )
  
  oa_client <- crul::HttpClient$new(
    url = fallback_url,
    headers = headers,
    opts = list(timeout = 10)
  )
  
  tryCatch({
    oa_client$get()
  }, error = function(e) NULL)
}

title_match_decision <- function(input_title, registry_title, doi_ok, match_pct) {
  
  cleaned_input <- clean_title(input_title)
  cleaned_registry <- clean_title(registry_title)
  
  title_containment <- (
    nchar(cleaned_input) > 0 &&
      nchar(cleaned_registry) > 0 &&
      (
        startsWith(cleaned_input, cleaned_registry) ||
          startsWith(cleaned_registry, cleaned_input)
      )
  )
  
  if (isTRUE(match_pct >= 0.90)) {
    return(list(
      verified = TRUE,
      status = "verified_exact_title_match",
      message = "Passed verification: exact or near-exact DOI-title match."
    ))
  }
  
  if (isTRUE(doi_ok) && isTRUE(match_pct >= 0.70) && isTRUE(title_containment)) {
    return(list(
      verified = TRUE,
      status = "verified_title_subtitle_variant",
      message = "Passed verification as title/subtitle variant. DOI resolves and registry title is contained in submitted title."
    ))
  }
  
  if (isTRUE(doi_ok) && !isTRUE(title_containment)) {
    return(list(
      verified = FALSE,
      status = "rejected_doi_valid_but_title_mismatch",
      message = paste0(
        "Rejected: DOI resolves, but submitted title does not match registry title: '",
        registry_title,
        "'."
      )
    ))
  }
  
  return(list(
    verified = FALSE,
    status = "rejected_title_mismatch_or_unresolved_doi",
    message = paste0(
      "Rejected: insufficient DOI-title evidence. Registry title: '",
      registry_title,
      "'."
    )
  ))
}

# -----------------------------
# Main route
# -----------------------------

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
    
    # Output evidence fields
    incoming_data$doi_resolves <- FALSE
    incoming_data$doi_proxy_status_code <- NA_integer_
    incoming_data$metadata_found <- FALSE
    incoming_data$verified <- FALSE
    incoming_data$verification_source <- "None"
    incoming_data$verification_status <- "unchecked"
    incoming_data$registry_title <- ""
    incoming_data$registry_status_code <- NA_integer_
    incoming_data$error_log <- ""
    incoming_data$title_match_score <- 0.0
    
    # 1. DOI.org resolution layer
    for (j in seq_len(nrow(incoming_data))) {
      doi_check <- doi_resolves_via_proxy(
        doi = incoming_data$doi[j],
        headers = polite_headers
      )
      
      incoming_data$doi_resolves[j] <- isTRUE(doi_check$resolves)
      incoming_data$doi_proxy_status_code[j] <- suppressWarnings(as.integer(doi_check$status_code))
      
      Sys.sleep(0.20)
    }
    
    # 2. Crossref metadata layer
    crossref_urls <- paste0(
      "https://api.crossref.org/works/",
      utils::URLencode(incoming_data$doi, reserved = TRUE),
      "?mailto=futathesis@gmail.com"
    )
    
    crossref_urls <- as.character(unlist(crossref_urls))
    crossref_responses <- vector("list", length(crossref_urls))
    
    for (j in seq_along(crossref_urls)) {
      crossref_responses[[j]] <- get_crossref_with_retry(
        url = crossref_urls[j],
        headers = polite_headers,
        max_attempts = 3
      )
      
      Sys.sleep(0.50)
    }
    
    # 3. Parse metadata and make final verification decisions
    for (i in seq_len(nrow(incoming_data))) {
      
      resp <- crossref_responses[[i]]
      fetched_title <- ""
      
      if (is.null(resp) || inherits(resp, "error")) {
        incoming_data$verification_status[i] <- "crossref_request_failed"
        incoming_data$error_log[i] <- "Network connection dropped or timed out during Crossref request."
        next
      }
      
      incoming_data$registry_status_code[i] <- suppressWarnings(as.integer(resp$status_code))
      
      if (resp$status_code == 200) {
        
        cr_content <- tryCatch({
          jsonlite::fromJSON(resp$parse("UTF-8"))
        }, error = function(e) NULL)
        
        if (!is.null(cr_content) && !is.null(cr_content$message$title)) {
          fetched_title <- first_text_value(cr_content$message$title)
          incoming_data$metadata_found[i] <- TRUE
          incoming_data$verification_source[i] <- "Crossref"
          incoming_data$registry_title[i] <- fetched_title
        } else {
          incoming_data$verification_status[i] <- "crossref_no_title_found"
          incoming_data$error_log[i] <- "Crossref returned a response, but no title was found."
          next
        }
        
      } else if (resp$status_code == 404) {
        
        # Crossref did not find the DOI. Try OpenAlex.
        oa_resp <- get_openalex_by_doi(
          doi = incoming_data$doi[i],
          headers = polite_headers
        )
        
        if (!is.null(oa_resp) && !is.null(oa_resp$status_code)) {
          incoming_data$registry_status_code[i] <- suppressWarnings(as.integer(oa_resp$status_code))
        }
        
        if (!is.null(oa_resp) && oa_resp$status_code == 200) {
          
          oa_content <- tryCatch({
            jsonlite::fromJSON(oa_resp$parse("UTF-8"))
          }, error = function(e) NULL)
          
          if (!is.null(oa_content)) {
            fetched_title <- first_text_value(
              if (!is.null(oa_content$title)) oa_content$title else oa_content$display_name
            )
            incoming_data$metadata_found[i] <- TRUE
            incoming_data$verification_source[i] <- "OpenAlex"
            incoming_data$registry_title[i] <- fetched_title
          } else {
            incoming_data$verification_status[i] <- "openalex_json_parse_failed"
            incoming_data$error_log[i] <- "OpenAlex returned a response, but JSON parsing failed."
            next
          }
          
        } else {
          
          if (isTRUE(incoming_data$doi_resolves[i])) {
            incoming_data$verification_status[i] <- "doi_resolves_but_no_crossref_openalex_metadata"
            incoming_data$error_log[i] <- "DOI resolves through DOI.org, but no usable Crossref/OpenAlex metadata was found."
          } else {
            incoming_data$verification_status[i] <- "doi_unresolved_and_no_registry_metadata"
            incoming_data$error_log[i] <- "DOI does not resolve through DOI.org and no usable Crossref/OpenAlex metadata was found."
          }
          
          next
        }
        
      } else if (resp$status_code == 429) {
        
        incoming_data$verification_status[i] <- "rate_limited_retry_needed"
        incoming_data$error_log[i] <- "Crossref returned 429 rate limit. Retry later with slower sequential requests."
        next
        
      } else {
        
        incoming_data$verification_status[i] <- paste0("upstream_status_", resp$status_code)
        incoming_data$error_log[i] <- paste(
          "Upstream registry returned status code:",
          resp$status_code
        )
        next
      }
      
      # 4. DOI-title evidence scoring
      cleaned_input_title <- clean_title(incoming_data$title[i])
      cleaned_fetched_title <- clean_title(fetched_title)
      
      distance_score <- utils::adist(cleaned_input_title, cleaned_fetched_title)
      max_len <- max(nchar(cleaned_input_title), nchar(cleaned_fetched_title))
      
      if (max_len > 0) {
        match_pct <- 1 - (distance_score / max_len)
        incoming_data$title_match_score[i] <- round(as.numeric(match_pct), 2)
      } else {
        match_pct <- 0
        incoming_data$title_match_score[i] <- 0.0
      }
      
      decision <- title_match_decision(
        input_title = incoming_data$title[i],
        registry_title = fetched_title,
        doi_ok = incoming_data$doi_resolves[i],
        match_pct = match_pct
      )
      
      incoming_data$verified[i] <- decision$verified
      incoming_data$verification_status[i] <- decision$status
      incoming_data$error_log[i] <- decision$message
    }
    
    return(list(
      status = "success",
      total_processed = nrow(incoming_data),
      verified_count = sum(incoming_data$verified, na.rm = TRUE),
      all_passed = all(incoming_data$verified),
      data = incoming_data
    ))
    
  }, error = function(err) {
    res$status <- 500
    return(list(
      status = "runtime_crash",
      message = err$message,
      hint = "Check request parsing, DOI URL construction, DOI.org resolution, Crossref/OpenAlex response parsing, or package versions."
    ))
  })
}