library(plumber)
library(jsonlite)
library(crul)
library(stringr)

#* @apiTitle Q1 Literature Review Metadata Verification API
#* @apiDescription Verifies academic metadata inputs using DOI.org, Crossref, DataCite, and OpenAlex metadata checks.

# -----------------------------
# Configuration
# -----------------------------

CONTACT_EMAIL <- Sys.getenv("METADATA_VERIFIER_EMAIL", unset = "futathesis@gmail.com")

POLITE_HEADERS <- list(
  `User-Agent` = paste0("Q1LitReviewEngine/2.1 (mailto:", CONTACT_EMAIL, ")")
)

# -----------------------------
# Helper functions
# -----------------------------

is_blank_string <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  x <- as.character(x[1])
  if (is.na(x)) return(TRUE)
  nchar(stringr::str_trim(x)) == 0
}

clean_title <- function(title_string) {
  if (is.null(title_string) || length(title_string) == 0 || is.na(title_string[1])) return("")
  title_string <- as.character(title_string[1])
  tolower(str_replace_all(title_string, "[[:punct:]\\s]+", ""))
}

first_text_value <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (is.data.frame(x) && nrow(x) > 0 && ncol(x) > 0) {
    x <- x[[1]]
  }
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

source_detail <- function(source_name) {
  switch(
    source_name,
    "Crossref" = "Crossref DOI metadata",
    "DataCite" = "DataCite DOI metadata",
    "OpenAlex" = "OpenAlex DOI metadata",
    "Semantic Scholar" = "Semantic Scholar enrichment",
    "Europe PMC" = "Europe PMC biomedical metadata",
    "None"
  )
}

verified_evidence_level <- function(source_name) {
  switch(
    source_name,
    "Crossref" = "primary_doi_title_match",
    "DataCite" = "datacite_doi_title_match",
    "OpenAlex" = "secondary_doi_title_match",
    "Semantic Scholar" = "secondary_doi_title_match",
    "Europe PMC" = "secondary_doi_title_match",
    "secondary_doi_title_match"
  )
}

looks_like_title_repair_candidate <- function(title, author, year) {
  title_ok <- !is_blank_string(title) && nchar(clean_title(title)) >= 20
  author_ok <- !is_blank_string(author)
  year_ok <- !is.na(suppressWarnings(as.integer(year)))
  isTRUE(title_ok && author_ok && year_ok)
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

doi_resolves_via_proxy <- function(doi, headers = POLITE_HEADERS) {
  doi <- normalise_doi(doi)
  
  if (is_blank_string(doi)) {
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

get_url_with_retry <- function(url, headers = POLITE_HEADERS, max_attempts = 3) {
  
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

get_crossref_with_retry <- function(url, headers = POLITE_HEADERS, max_attempts = 3) {
  get_url_with_retry(url = url, headers = headers, max_attempts = max_attempts)
}

get_crossref_by_doi <- function(doi, headers = POLITE_HEADERS) {
  
  crossref_url <- paste0(
    "https://api.crossref.org/works/",
    utils::URLencode(doi, reserved = TRUE),
    "?mailto=",
    utils::URLencode(CONTACT_EMAIL, reserved = TRUE)
  )
  
  get_url_with_retry(crossref_url, headers = headers, max_attempts = 3)
}

get_datacite_by_doi <- function(doi, headers = POLITE_HEADERS) {
  
  datacite_url <- paste0(
    "https://api.datacite.org/dois/",
    utils::URLencode(doi, reserved = TRUE)
  )
  
  get_url_with_retry(datacite_url, headers = headers, max_attempts = 3)
}

get_openalex_by_doi <- function(doi, headers = POLITE_HEADERS) {
  
  fallback_url <- paste0(
    "https://api.openalex.org/works/doi:",
    utils::URLencode(doi, reserved = TRUE)
  )
  
  get_url_with_retry(fallback_url, headers = headers, max_attempts = 3)
}

parse_json_response <- function(resp) {
  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) return(NULL)
  
  tryCatch({
    jsonlite::fromJSON(resp$parse("UTF-8"))
  }, error = function(e) NULL)
}

extract_crossref_title <- function(cr_content) {
  if (is.null(cr_content) || is.null(cr_content$message) || is.null(cr_content$message$title)) {
    return("")
  }
  first_text_value(cr_content$message$title)
}

extract_datacite_title <- function(dc_content) {
  if (is.null(dc_content) || is.null(dc_content$data) || is.null(dc_content$data$attributes)) {
    return("")
  }
  
  attrs <- dc_content$data$attributes
  
  if (!is.null(attrs$titles)) {
    titles <- attrs$titles
    
    if (is.data.frame(titles) && "title" %in% names(titles)) {
      return(first_text_value(titles$title))
    }
    
    if (is.list(titles)) {
      if (!is.null(titles$title)) {
        return(first_text_value(titles$title))
      }
      
      first_title <- tryCatch({
        titles[[1]]$title
      }, error = function(e) NULL)
      
      if (!is.null(first_title)) {
        return(first_text_value(first_title))
      }
    }
  }
  
  if (!is.null(attrs$title)) {
    return(first_text_value(attrs$title))
  }
  
  ""
}

extract_openalex_title <- function(oa_content) {
  if (is.null(oa_content)) return("")
  first_text_value(
    if (!is.null(oa_content$title)) oa_content$title else oa_content$display_name
  )
}

empty_metadata_result <- function(status_code = NA_integer_, error_log = "") {
  list(
    found = FALSE,
    source = "None",
    source_detail = "None",
    title = "",
    status_code = suppressWarnings(as.integer(status_code)),
    error_log = error_log
  )
}

metadata_result <- function(source, title, status_code) {
  list(
    found = TRUE,
    source = source,
    source_detail = source_detail(source),
    title = title,
    status_code = suppressWarnings(as.integer(status_code)),
    error_log = ""
  )
}

query_crossref_metadata <- function(doi, headers = POLITE_HEADERS) {
  resp <- get_crossref_by_doi(doi = doi, headers = headers)
  
  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) {
    return(empty_metadata_result(
      status_code = NA_integer_,
      error_log = "Network connection dropped or timed out during Crossref request."
    ))
  }
  
  if (resp$status_code == 200) {
    content <- parse_json_response(resp)
    fetched_title <- extract_crossref_title(content)
    
    if (!is_blank_string(fetched_title)) {
      return(metadata_result("Crossref", fetched_title, resp$status_code))
    }
    
    return(empty_metadata_result(
      status_code = resp$status_code,
      error_log = "Crossref returned a response, but no title was found."
    ))
  }
  
  empty_metadata_result(
    status_code = resp$status_code,
    error_log = paste("Crossref returned status code:", resp$status_code)
  )
}

query_datacite_metadata <- function(doi, headers = POLITE_HEADERS) {
  resp <- get_datacite_by_doi(doi = doi, headers = headers)
  
  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) {
    return(empty_metadata_result(
      status_code = NA_integer_,
      error_log = "Network connection dropped or timed out during DataCite request."
    ))
  }
  
  if (resp$status_code == 200) {
    content <- parse_json_response(resp)
    fetched_title <- extract_datacite_title(content)
    
    if (!is_blank_string(fetched_title)) {
      return(metadata_result("DataCite", fetched_title, resp$status_code))
    }
    
    return(empty_metadata_result(
      status_code = resp$status_code,
      error_log = "DataCite returned a response, but no title was found."
    ))
  }
  
  empty_metadata_result(
    status_code = resp$status_code,
    error_log = paste("DataCite returned status code:", resp$status_code)
  )
}

query_openalex_metadata <- function(doi, headers = POLITE_HEADERS) {
  resp <- get_openalex_by_doi(doi = doi, headers = headers)
  
  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) {
    return(empty_metadata_result(
      status_code = NA_integer_,
      error_log = "Network connection dropped or timed out during OpenAlex request."
    ))
  }
  
  if (resp$status_code == 200) {
    content <- parse_json_response(resp)
    fetched_title <- extract_openalex_title(content)
    
    if (!is_blank_string(fetched_title)) {
      return(metadata_result("OpenAlex", fetched_title, resp$status_code))
    }
    
    return(empty_metadata_result(
      status_code = resp$status_code,
      error_log = "OpenAlex returned a response, but no title was found."
    ))
  }
  
  empty_metadata_result(
    status_code = resp$status_code,
    error_log = paste("OpenAlex returned status code:", resp$status_code)
  )
}

query_metadata_waterfall <- function(doi, headers = POLITE_HEADERS) {
  
  crossref_result <- query_crossref_metadata(doi = doi, headers = headers)
  if (isTRUE(crossref_result$found)) {
    return(crossref_result)
  }
  
  # DataCite is added as a DOI metadata fallback after Crossref.
  datacite_result <- query_datacite_metadata(doi = doi, headers = headers)
  if (isTRUE(datacite_result$found)) {
    return(datacite_result)
  }
  
  # OpenAlex remains a broad secondary fallback.
  openalex_result <- query_openalex_metadata(doi = doi, headers = headers)
  if (isTRUE(openalex_result$found)) {
    return(openalex_result)
  }
  
  combined_error <- paste(
    crossref_result$error_log,
    datacite_result$error_log,
    openalex_result$error_log,
    sep = " | "
  )
  
  final_status_code <- openalex_result$status_code
  if (is.na(final_status_code)) final_status_code <- datacite_result$status_code
  if (is.na(final_status_code)) final_status_code <- crossref_result$status_code
  
  empty_metadata_result(
    status_code = final_status_code,
    error_log = combined_error
  )
}

title_match_decision <- function(input_title, registry_title, doi_ok, match_pct, source_name) {
  
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
  
  if (isTRUE(match_pct >= 0.90) && isTRUE(doi_ok)) {
    return(list(
      verified = TRUE,
      status = "verified_exact_title_match",
      evidence_level = verified_evidence_level(source_name),
      message = "Passed verification: exact or near-exact DOI-title match."
    ))
  }
  
  if (isTRUE(match_pct >= 0.90) && !isTRUE(doi_ok)) {
    return(list(
      verified = FALSE,
      status = "doi_unresolved_title_match_requires_recheck",
      evidence_level = "title_repair_candidate",
      message = "Metadata title matched the submitted title, but DOI.org resolution failed. This is not verified until the DOI is rerun and resolves."
    ))
  }
  
  if (isTRUE(doi_ok) && isTRUE(match_pct >= 0.70) && isTRUE(title_containment)) {
    return(list(
      verified = TRUE,
      status = "verified_title_subtitle_variant",
      evidence_level = verified_evidence_level(source_name),
      message = "Passed verification as title/subtitle variant. DOI resolves and registry title is contained in submitted title."
    ))
  }
  
  if (!isTRUE(doi_ok) && isTRUE(match_pct >= 0.70) && isTRUE(title_containment)) {
    return(list(
      verified = FALSE,
      status = "doi_unresolved_title_subtitle_variant_requires_recheck",
      evidence_level = "title_repair_candidate",
      message = "Registry title appears to be a title/subtitle variant, but DOI.org resolution failed. This is not verified until the DOI is rerun and resolves."
    ))
  }
  
  if (isTRUE(doi_ok) && !isTRUE(title_containment)) {
    return(list(
      verified = FALSE,
      status = "rejected_doi_valid_but_title_mismatch",
      evidence_level = "rejected",
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
    evidence_level = "rejected",
    message = paste0(
      "Rejected: insufficient DOI-title evidence. Registry title: '",
      registry_title,
      "'."
    )
  ))
}

# -----------------------------
# Future repair route note
# -----------------------------
# A future /api/v1/repair-metadata endpoint can be added after this
# DOI-first verifier is stable. That route should accept title, author,
# year, and journal, perform title-first searches in Crossref/OpenAlex
# and optional enrichment sources, return suggested DOI candidates only,
# and require rerunning every suggested DOI through /api/v1/verify-metadata.
# It must not silently replace a submitted DOI.

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
    
    # Output evidence fields
    incoming_data$doi_resolves <- FALSE
    incoming_data$doi_proxy_status_code <- NA_integer_
    incoming_data$metadata_found <- FALSE
    incoming_data$verified <- FALSE
    incoming_data$verification_source <- "None"
    incoming_data$verification_source_detail <- "None"
    incoming_data$verification_status <- "unchecked"
    incoming_data$evidence_level <- "rejected"
    incoming_data$registry_title <- ""
    incoming_data$registry_status_code <- NA_integer_
    incoming_data$error_log <- ""
    incoming_data$title_match_score <- 0.0
    
    # 1. DOI.org resolution layer
    for (j in seq_len(nrow(incoming_data))) {
      
      if (is_blank_string(incoming_data$doi[j])) {
        incoming_data$verification_status[j] <- "missing_or_empty_doi"
        incoming_data$evidence_level[j] <- "manual_review_only"
        incoming_data$error_log[j] <- "No DOI was supplied. This DOI-first endpoint cannot verify the record."
        next
      }
      
      doi_check <- doi_resolves_via_proxy(
        doi = incoming_data$doi[j],
        headers = POLITE_HEADERS
      )
      
      incoming_data$doi_resolves[j] <- isTRUE(doi_check$resolves)
      incoming_data$doi_proxy_status_code[j] <- suppressWarnings(as.integer(doi_check$status_code))
      
      Sys.sleep(0.20)
    }
    
    # 2. DOI metadata waterfall and DOI-title evidence scoring
    for (i in seq_len(nrow(incoming_data))) {
      
      if (incoming_data$verification_status[i] == "missing_or_empty_doi") {
        next
      }
      
      fetched_title <- ""
      
      metadata <- query_metadata_waterfall(
        doi = incoming_data$doi[i],
        headers = POLITE_HEADERS
      )
      
      incoming_data$registry_status_code[i] <- suppressWarnings(as.integer(metadata$status_code))
      
      if (isTRUE(metadata$found)) {
        fetched_title <- metadata$title
        incoming_data$metadata_found[i] <- TRUE
        incoming_data$verification_source[i] <- metadata$source
        incoming_data$verification_source_detail[i] <- metadata$source_detail
        incoming_data$registry_title[i] <- fetched_title
      } else {
        
        incoming_data$verification_source[i] <- "None"
        incoming_data$verification_source_detail[i] <- "None"
        
        if (isTRUE(incoming_data$doi_resolves[i])) {
          incoming_data$verification_status[i] <- "doi_resolves_metadata_missing"
          incoming_data$evidence_level[i] <- "doi_resolves_metadata_missing"
          incoming_data$error_log[i] <- paste(
            "DOI resolves through DOI.org, but no usable Crossref/DataCite/OpenAlex metadata was found.",
            metadata$error_log
          )
        } else if (looks_like_title_repair_candidate(
          title = incoming_data$title[i],
          author = incoming_data$author[i],
          year = incoming_data$year[i]
        )) {
          incoming_data$verification_status[i] <- "doi_unresolved_title_repair_candidate"
          incoming_data$evidence_level[i] <- "title_repair_candidate"
          incoming_data$error_log[i] <- paste(
            "DOI does not resolve and no registry metadata was found. The title/author/year fields are complete enough for a future title-first repair search, but this record is not verified.",
            metadata$error_log
          )
        } else {
          incoming_data$verification_status[i] <- "doi_unresolved_and_no_registry_metadata"
          incoming_data$evidence_level[i] <- "rejected"
          incoming_data$error_log[i] <- paste(
            "DOI does not resolve through DOI.org and no usable Crossref/DataCite/OpenAlex metadata was found.",
            metadata$error_log
          )
        }
        
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
        incoming_data$title_match_score[i] <- 0.0
      }
      
      decision <- title_match_decision(
        input_title = incoming_data$title[i],
        registry_title = fetched_title,
        doi_ok = incoming_data$doi_resolves[i],
        match_pct = match_pct,
        source_name = incoming_data$verification_source[i]
      )
      
      incoming_data$verified[i] <- decision$verified
      incoming_data$verification_status[i] <- decision$status
      incoming_data$evidence_level[i] <- decision$evidence_level
      incoming_data$error_log[i] <- decision$message
      
      Sys.sleep(0.30)
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
      hint = "Check request parsing, DOI URL construction, DOI.org resolution, Crossref/DataCite/OpenAlex response parsing, or package versions."
    ))
  })
}
