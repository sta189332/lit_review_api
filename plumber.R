library(plumber)
library(jsonlite)
library(crul)
library(stringr)

#* @apiTitle Q1 Literature Review Metadata Verification API
#* @apiDescription Verifies academic metadata inputs using DOI.org, Crossref, DataCite, and OpenAlex metadata checks. Returns DOI-title and full metadata verification levels.

# -----------------------------
# Configuration
# -----------------------------

CONTACT_EMAIL <- Sys.getenv("METADATA_VERIFIER_EMAIL", unset = "futathesis@gmail.com")

POLITE_HEADERS <- list(
  `User-Agent` = paste0("Q1LitReviewEngine/2.2 (mailto:", CONTACT_EMAIL, ")")
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

safe_integer <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  x <- suppressWarnings(as.integer(x[1]))
  if (length(x) == 0 || is.na(x)) return(NA_integer_)
  x
}

# Build top-level API responses with scalar JSON values.
# Plumber's default JSON serializer can emit length-one R vectors as arrays.
# jsonlite::unbox() keeps summary fields such as status, counts, and flags as
# JSON scalars while preserving data as an array of record objects.
api_response <- function(...) {
  response <- list(...)

  scalar_fields <- c(
    "status",
    "message",
    "hint",
    "total_processed",
    "verified_count",
    "doi_title_verified_count",
    "fully_metadata_verified_count",
    "metadata_partially_verified_count",
    "doi_exists_only_count",
    "rejected_or_unverified_count",
    "manual_confirmation_required_count",
    "step_b_eligible_count",
    "all_passed",
    "all_fully_metadata_verified"
  )

  for (field in intersect(names(response), scalar_fields)) {
    value <- response[[field]]
    if (is.atomic(value) && length(value) == 1 && !inherits(value, "AsIs")) {
      response[[field]] <- jsonlite::unbox(value)
    }
  }

  response
}

strip_registry_markup <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  x <- as.character(x[1])
  x <- stringr::str_replace_all(x, "<[^>]+>", " ")
  x <- stringr::str_replace_all(x, "&amp;", " and ")
  x <- stringr::str_replace_all(x, "&nbsp;", " ")
  x <- stringr::str_replace_all(x, "&quot;", '"')
  x <- stringr::str_replace_all(x, "&#39;", "'")
  x <- stringr::str_replace_all(x, "&apos;", "'")
  x
}

normalise_general_text <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  x <- strip_registry_markup(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "[\u2018\u2019\u201A\u201B`´]", "'")
  x <- stringr::str_replace_all(x, "[\u201C\u201D\u201E\u201F]", '"')
  x <- stringr::str_replace_all(x, "[\u2010\u2011\u2012\u2013\u2014\u2212]", "-")
  x <- stringr::str_replace_all(x, "&", " and ")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_trim(x)
}

clean_title <- function(title_string) {
  title_string <- normalise_general_text(title_string)
  if (is_blank_string(title_string)) return("")
  stringr::str_replace_all(title_string, "[[:punct:]\\s]+", "")
}

clean_name <- function(x) {
  x <- normalise_general_text(x)
  if (is_blank_string(x)) return("")
  stringr::str_replace_all(x, "[^a-z0-9]+", "")
}

clean_journal <- function(x) {
  x <- normalise_general_text(x)
  if (is_blank_string(x)) return("")
  x <- stringr::str_replace_all(x, "\\band\\b", "and")
  stringr::str_replace_all(x, "[^a-z0-9]+", "")
}

string_similarity <- function(a, b, cleaner = clean_title) {
  a <- cleaner(a)
  b <- cleaner(b)
  if (is_blank_string(a) || is_blank_string(b)) return(0)
  distance_score <- utils::adist(a, b)
  max_len <- max(nchar(a), nchar(b))
  if (max_len <= 0) return(0)
  score <- 1 - (as.numeric(distance_score) / max_len)
  max(0, min(1, score))
}

normalise_doi <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_replace(x, regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE), "")
  x <- stringr::str_replace(x, regex("^doi:", ignore_case = TRUE), "")
  stringr::str_trim(x)
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
  if (!is.null(req$body)) {
    body_obj <- req$body

    if (is.data.frame(body_obj)) return(body_obj)

    if (is.list(body_obj)) {
      df <- tryCatch({
        jsonlite::fromJSON(jsonlite::toJSON(body_obj, auto_unbox = TRUE))
      }, error = function(e) NULL)
      if (is.data.frame(df)) return(df)

      if ("body" %in% names(body_obj)) {
        body_text <- as.character(body_obj$body)
        df <- tryCatch(jsonlite::fromJSON(body_text), error = function(e) NULL)
        if (is.data.frame(df)) return(df)
      }
    }

    if (is.character(body_obj)) {
      df <- tryCatch(jsonlite::fromJSON(body_obj), error = function(e) NULL)
      if (is.data.frame(df)) return(df)
    }
  }

  if (!is.null(req$argsBody)) {
    if (is.data.frame(req$argsBody)) return(req$argsBody)

    if (is.list(req$argsBody) && "body" %in% names(req$argsBody)) {
      body_text <- as.character(req$argsBody$body)
      if (length(body_text) > 0 && nchar(stringr::str_trim(body_text[1])) > 0) {
        df <- tryCatch(jsonlite::fromJSON(body_text[1]), error = function(e) NULL)
        if (is.data.frame(df)) return(df)
      }
    }
  }

  if (!is.null(req$postBody) && length(req$postBody) > 0) {
    body_text <- paste(req$postBody, collapse = "\n")
    if (nchar(stringr::str_trim(body_text)) > 0) {
      df <- tryCatch(jsonlite::fromJSON(body_text), error = function(e) NULL)
      if (is.data.frame(df)) return(df)
    }
  }

  if (!is.null(req$bodyRaw) && length(req$bodyRaw) > 0) {
    body_text <- tryCatch(rawToChar(req$bodyRaw), error = function(e) "")
    if (nchar(stringr::str_trim(body_text)) > 0) {
      df <- tryCatch(jsonlite::fromJSON(body_text), error = function(e) NULL)
      if (is.data.frame(df)) return(df)
    }
  }

  NULL
}

doi_resolves_via_proxy <- function(doi, headers = POLITE_HEADERS) {
  doi <- normalise_doi(doi)

  if (is_blank_string(doi)) {
    return(list(resolves = FALSE, status_code = NA_integer_))
  }

  doi_url <- paste0("https://doi.org/", utils::URLencode(doi[1], reserved = TRUE))

  client <- crul::HttpClient$new(
    url = doi_url,
    headers = headers,
    opts = list(timeout = 10, followlocation = FALSE)
  )

  head_resp <- tryCatch(client$head(), error = function(e) NULL)

  if (!is.null(head_resp) && !is.null(head_resp$status_code)) {
    if (head_resp$status_code %in% 200:399) {
      return(list(resolves = TRUE, status_code = head_resp$status_code))
    }
  }

  get_resp <- tryCatch(client$get(), error = function(e) NULL)

  if (!is.null(get_resp) && !is.null(get_resp$status_code)) {
    if (get_resp$status_code %in% 200:399) {
      return(list(resolves = TRUE, status_code = get_resp$status_code))
    }
    return(list(resolves = FALSE, status_code = get_resp$status_code))
  }

  list(resolves = FALSE, status_code = NA_integer_)
}

get_url_with_retry <- function(url, headers = POLITE_HEADERS, max_attempts = 3) {
  client <- crul::HttpClient$new(url = url, headers = headers, opts = list(timeout = 10))
  last_resp <- NULL

  for (attempt in seq_len(max_attempts)) {
    resp <- tryCatch(client$get(), error = function(e) e)

    if (inherits(resp, "error")) {
      last_resp <- resp
      Sys.sleep(1)
      next
    }

    last_resp <- resp

    if (!is.null(resp$status_code) && resp$status_code == 429) {
      retry_after <- resp$response_headers[["retry-after"]]
      wait_seconds <- suppressWarnings(as.numeric(retry_after))
      if (is.na(wait_seconds) || length(wait_seconds) == 0) wait_seconds <- attempt * 2
      Sys.sleep(wait_seconds)
      next
    }

    return(resp)
  }

  last_resp
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
  datacite_url <- paste0("https://api.datacite.org/dois/", utils::URLencode(doi, reserved = TRUE))
  get_url_with_retry(datacite_url, headers = headers, max_attempts = 3)
}

get_openalex_by_doi <- function(doi, headers = POLITE_HEADERS) {
  fallback_url <- paste0("https://api.openalex.org/works/doi:", utils::URLencode(doi, reserved = TRUE))
  get_url_with_retry(fallback_url, headers = headers, max_attempts = 3)
}

parse_json_response <- function(resp) {
  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) return(NULL)
  tryCatch(jsonlite::fromJSON(resp$parse("UTF-8")), error = function(e) NULL)
}

date_parts_year <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  values <- suppressWarnings(as.integer(unlist(x, recursive = TRUE, use.names = FALSE)))
  values <- values[!is.na(values)]
  years <- values[values >= 1000 & values <= 3000]
  if (length(years) == 0) return(NA_integer_)
  as.integer(years[1])
}

extract_crossref_title <- function(cr_content) {
  if (is.null(cr_content) || is.null(cr_content$message) || is.null(cr_content$message$title)) return("")
  first_text_value(cr_content$message$title)
}

extract_crossref_author <- function(cr_content) {
  if (is.null(cr_content) || is.null(cr_content$message) || is.null(cr_content$message$author)) return("")
  authors <- cr_content$message$author

  if (is.data.frame(authors) && nrow(authors) > 0) {
    if ("family" %in% names(authors) && !is_blank_string(authors$family[1])) return(first_text_value(authors$family[1]))
    if ("name" %in% names(authors) && !is_blank_string(authors$name[1])) return(first_text_value(authors$name[1]))
  }

  if (is.list(authors)) {
    if (!is.null(authors$family)) return(first_text_value(authors$family))
    if (!is.null(authors$name)) return(first_text_value(authors$name))
    first_family <- tryCatch(authors[[1]]$family, error = function(e) NULL)
    if (!is.null(first_family)) return(first_text_value(first_family))
    first_name <- tryCatch(authors[[1]]$name, error = function(e) NULL)
    if (!is.null(first_name)) return(first_text_value(first_name))
  }

  ""
}

extract_crossref_journal <- function(cr_content) {
  if (is.null(cr_content) || is.null(cr_content$message)) return("")
  msg <- cr_content$message
  if (!is.null(msg[["container-title"]])) {
    value <- first_text_value(msg[["container-title"]])
    if (!is_blank_string(value)) return(value)
  }
  if (!is.null(msg[["short-container-title"]])) {
    value <- first_text_value(msg[["short-container-title"]])
    if (!is_blank_string(value)) return(value)
  }
  if (!is.null(msg$publisher)) return(first_text_value(msg$publisher))
  ""
}

extract_crossref_year <- function(cr_content) {
  if (is.null(cr_content) || is.null(cr_content$message)) return(NA_integer_)
  msg <- cr_content$message
  date_fields <- c("published-print", "published-online", "published", "issued", "created", "deposited")

  for (field in date_fields) {
    if (!is.null(msg[[field]])) {
      year <- date_parts_year(msg[[field]][["date-parts"]])
      if (!is.na(year)) return(year)
      year <- date_parts_year(msg[[field]])
      if (!is.na(year)) return(year)
    }
  }

  NA_integer_
}

extract_datacite_title <- function(dc_content) {
  if (is.null(dc_content) || is.null(dc_content$data) || is.null(dc_content$data$attributes)) return("")
  attrs <- dc_content$data$attributes

  if (!is.null(attrs$titles)) {
    titles <- attrs$titles
    if (is.data.frame(titles) && "title" %in% names(titles)) return(first_text_value(titles$title))
    if (is.list(titles)) {
      if (!is.null(titles$title)) return(first_text_value(titles$title))
      first_title <- tryCatch(titles[[1]]$title, error = function(e) NULL)
      if (!is.null(first_title)) return(first_text_value(first_title))
    }
  }

  if (!is.null(attrs$title)) return(first_text_value(attrs$title))
  ""
}

extract_datacite_author <- function(dc_content) {
  if (is.null(dc_content) || is.null(dc_content$data) || is.null(dc_content$data$attributes)) return("")
  creators <- dc_content$data$attributes$creators
  if (is.null(creators)) return("")

  if (is.data.frame(creators) && nrow(creators) > 0) {
    if ("familyName" %in% names(creators) && !is_blank_string(creators$familyName[1])) return(first_text_value(creators$familyName[1]))
    if ("name" %in% names(creators) && !is_blank_string(creators$name[1])) return(first_text_value(creators$name[1]))
  }

  if (is.list(creators)) {
    if (!is.null(creators$familyName)) return(first_text_value(creators$familyName))
    if (!is.null(creators$name)) return(first_text_value(creators$name))
    first_family <- tryCatch(creators[[1]]$familyName, error = function(e) NULL)
    if (!is.null(first_family)) return(first_text_value(first_family))
    first_name <- tryCatch(creators[[1]]$name, error = function(e) NULL)
    if (!is.null(first_name)) return(first_text_value(first_name))
  }

  ""
}

extract_datacite_journal <- function(dc_content) {
  if (is.null(dc_content) || is.null(dc_content$data) || is.null(dc_content$data$attributes)) return("")
  attrs <- dc_content$data$attributes

  candidate_fields <- list(
    attrs$container$title,
    attrs$container,
    attrs$publisher,
    attrs$publicationYear
  )

  for (value in candidate_fields) {
    out <- first_text_value(value)
    if (!is_blank_string(out) && !grepl("^[0-9]{4}$", out)) return(out)
  }

  ""
}

extract_datacite_year <- function(dc_content) {
  if (is.null(dc_content) || is.null(dc_content$data) || is.null(dc_content$data$attributes)) return(NA_integer_)
  attrs <- dc_content$data$attributes

  if (!is.null(attrs$publicationYear)) {
    year <- safe_integer(attrs$publicationYear)
    if (!is.na(year)) return(year)
  }

  if (!is.null(attrs$dates)) {
    year <- date_parts_year(attrs$dates)
    if (!is.na(year)) return(year)
  }

  NA_integer_
}

extract_openalex_title <- function(oa_content) {
  if (is.null(oa_content)) return("")
  first_text_value(if (!is.null(oa_content$title)) oa_content$title else oa_content$display_name)
}

extract_openalex_author <- function(oa_content) {
  if (is.null(oa_content) || is.null(oa_content$authorships)) return("")
  authorships <- oa_content$authorships

  if (is.data.frame(authorships) && nrow(authorships) > 0) {
    possible <- c("author.display_name", "author_display_name", "display_name")
    for (nm in possible) {
      if (nm %in% names(authorships) && !is_blank_string(authorships[[nm]][1])) {
        full_name <- first_text_value(authorships[[nm]][1])
        parts <- unlist(strsplit(full_name, "\\s+"))
        return(tail(parts[parts != ""], 1))
      }
    }
  }

  first_name <- tryCatch(authorships[[1]]$author$display_name, error = function(e) NULL)
  if (!is.null(first_name)) {
    full_name <- first_text_value(first_name)
    parts <- unlist(strsplit(full_name, "\\s+"))
    return(tail(parts[parts != ""], 1))
  }

  ""
}

extract_openalex_journal <- function(oa_content) {
  if (is.null(oa_content)) return("")

  candidates <- list(
    tryCatch(oa_content$primary_location$source$display_name, error = function(e) NULL),
    tryCatch(oa_content$host_venue$display_name, error = function(e) NULL),
    tryCatch(oa_content$source$display_name, error = function(e) NULL)
  )

  for (value in candidates) {
    out <- first_text_value(value)
    if (!is_blank_string(out)) return(out)
  }

  ""
}

extract_openalex_year <- function(oa_content) {
  if (is.null(oa_content)) return(NA_integer_)
  year <- safe_integer(oa_content$publication_year)
  if (!is.na(year)) return(year)
  NA_integer_
}

empty_metadata_result <- function(status_code = NA_integer_, error_log = "") {
  list(
    found = FALSE,
    source = "None",
    source_detail = "None",
    title = "",
    author = "",
    journal = "",
    year = NA_integer_,
    status_code = suppressWarnings(as.integer(status_code)),
    error_log = error_log
  )
}

metadata_result <- function(source, title, author, journal, year, status_code) {
  list(
    found = TRUE,
    source = source,
    source_detail = source_detail(source),
    title = first_text_value(title),
    author = first_text_value(author),
    journal = first_text_value(journal),
    year = safe_integer(year),
    status_code = suppressWarnings(as.integer(status_code)),
    error_log = ""
  )
}

query_crossref_metadata <- function(doi, headers = POLITE_HEADERS) {
  resp <- get_crossref_by_doi(doi = doi, headers = headers)

  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) {
    return(empty_metadata_result(NA_integer_, "Network connection dropped or timed out during Crossref request."))
  }

  if (resp$status_code == 200) {
    content <- parse_json_response(resp)
    fetched_title <- extract_crossref_title(content)

    if (!is_blank_string(fetched_title)) {
      return(metadata_result(
        source = "Crossref",
        title = fetched_title,
        author = extract_crossref_author(content),
        journal = extract_crossref_journal(content),
        year = extract_crossref_year(content),
        status_code = resp$status_code
      ))
    }

    return(empty_metadata_result(resp$status_code, "Crossref returned a response, but no title was found."))
  }

  empty_metadata_result(resp$status_code, paste("Crossref returned status code:", resp$status_code))
}

query_datacite_metadata <- function(doi, headers = POLITE_HEADERS) {
  resp <- get_datacite_by_doi(doi = doi, headers = headers)

  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) {
    return(empty_metadata_result(NA_integer_, "Network connection dropped or timed out during DataCite request."))
  }

  if (resp$status_code == 200) {
    content <- parse_json_response(resp)
    fetched_title <- extract_datacite_title(content)

    if (!is_blank_string(fetched_title)) {
      return(metadata_result(
        source = "DataCite",
        title = fetched_title,
        author = extract_datacite_author(content),
        journal = extract_datacite_journal(content),
        year = extract_datacite_year(content),
        status_code = resp$status_code
      ))
    }

    return(empty_metadata_result(resp$status_code, "DataCite returned a response, but no title was found."))
  }

  empty_metadata_result(resp$status_code, paste("DataCite returned status code:", resp$status_code))
}

query_openalex_metadata <- function(doi, headers = POLITE_HEADERS) {
  resp <- get_openalex_by_doi(doi = doi, headers = headers)

  if (is.null(resp) || inherits(resp, "error") || is.null(resp$status_code)) {
    return(empty_metadata_result(NA_integer_, "Network connection dropped or timed out during OpenAlex request."))
  }

  if (resp$status_code == 200) {
    content <- parse_json_response(resp)
    fetched_title <- extract_openalex_title(content)

    if (!is_blank_string(fetched_title)) {
      return(metadata_result(
        source = "OpenAlex",
        title = fetched_title,
        author = extract_openalex_author(content),
        journal = extract_openalex_journal(content),
        year = extract_openalex_year(content),
        status_code = resp$status_code
      ))
    }

    return(empty_metadata_result(resp$status_code, "OpenAlex returned a response, but no title was found."))
  }

  empty_metadata_result(resp$status_code, paste("OpenAlex returned status code:", resp$status_code))
}

query_metadata_waterfall <- function(doi, headers = POLITE_HEADERS) {
  crossref_result <- query_crossref_metadata(doi = doi, headers = headers)
  if (isTRUE(crossref_result$found)) return(crossref_result)

  datacite_result <- query_datacite_metadata(doi = doi, headers = headers)
  if (isTRUE(datacite_result$found)) return(datacite_result)

  openalex_result <- query_openalex_metadata(doi = doi, headers = headers)
  if (isTRUE(openalex_result$found)) return(openalex_result)

  combined_error <- paste(crossref_result$error_log, datacite_result$error_log, openalex_result$error_log, sep = " | ")

  final_status_code <- openalex_result$status_code
  if (is.na(final_status_code)) final_status_code <- datacite_result$status_code
  if (is.na(final_status_code)) final_status_code <- crossref_result$status_code

  empty_metadata_result(final_status_code, combined_error)
}

title_match_decision <- function(input_title, registry_title, doi_exists_verified, match_pct, source_name) {
  cleaned_input <- clean_title(input_title)
  cleaned_registry <- clean_title(registry_title)

  title_containment <- (
    nchar(cleaned_input) > 0 &&
      nchar(cleaned_registry) > 0 &&
      (startsWith(cleaned_input, cleaned_registry) || startsWith(cleaned_registry, cleaned_input))
  )

  # Conservative thresholds: >= .95 is accepted automatically; .90-.94 requires manual review.
  if (isTRUE(match_pct >= 0.95) && isTRUE(doi_exists_verified)) {
    return(list(
      verified = TRUE,
      status = "verified_exact_title_match",
      evidence_level = verified_evidence_level(source_name),
      message = "Passed verification: exact or near-exact DOI-title match."
    ))
  }

  if (isTRUE(match_pct >= 0.90) && isTRUE(title_containment) && isTRUE(doi_exists_verified)) {
    return(list(
      verified = TRUE,
      status = "verified_title_subtitle_variant",
      evidence_level = verified_evidence_level(source_name),
      message = "Passed verification as title/subtitle variant. DOI exists and registry title is contained in submitted title."
    ))
  }

  if (isTRUE(match_pct >= 0.90) && isTRUE(doi_exists_verified)) {
    return(list(
      verified = FALSE,
      status = "manual_review_only",
      evidence_level = "manual_review_only",
      message = "Title similarity is between 0.90 and 0.94. Manual review is required; this is not automatically verified."
    ))
  }

  if (!isTRUE(doi_exists_verified) && isTRUE(match_pct >= 0.90)) {
    return(list(
      verified = FALSE,
      status = "doi_unresolved_title_match_requires_recheck",
      evidence_level = "title_repair_candidate",
      message = "Metadata title matched the submitted title, but DOI existence was not confirmed. This is not verified until the DOI is rerun and resolves or appears in registry metadata."
    ))
  }

  if (isTRUE(doi_exists_verified) && !isTRUE(title_containment)) {
    return(list(
      verified = FALSE,
      status = "rejected_doi_valid_but_title_mismatch",
      evidence_level = "rejected",
      message = paste0("Rejected: DOI exists, but submitted title does not match registry title: '", registry_title, "'.")
    ))
  }

  list(
    verified = FALSE,
    status = "rejected_title_mismatch_or_unresolved_doi",
    evidence_level = "rejected",
    message = paste0("Rejected: insufficient DOI-title evidence. Registry title: '", registry_title, "'.")
  )
}

match_author <- function(input_author, registry_author) {
  score <- string_similarity(input_author, registry_author, cleaner = clean_name)
  verified <- !is_blank_string(input_author) && !is_blank_string(registry_author) && score >= 0.95
  list(verified = isTRUE(verified), score = round(score, 2))
}

match_journal <- function(input_journal, registry_journal) {
  if (is_blank_string(input_journal) || is_blank_string(registry_journal)) {
    return(list(verified = FALSE, score = 0))
  }

  score <- string_similarity(input_journal, registry_journal, cleaner = clean_journal)
  input_clean <- clean_journal(input_journal)
  registry_clean <- clean_journal(registry_journal)

  containment <- nchar(input_clean) > 0 && nchar(registry_clean) > 0 &&
    (startsWith(input_clean, registry_clean) || startsWith(registry_clean, input_clean))

  verified <- score >= 0.95 || (score >= 0.90 && containment)
  list(verified = isTRUE(verified), score = round(score, 2))
}

match_year <- function(input_year, registry_year) {
  input_year <- safe_integer(input_year)
  registry_year <- safe_integer(registry_year)

  if (is.na(input_year) || is.na(registry_year)) {
    return(list(verified = FALSE, status = "missing_year_metadata"))
  }

  if (input_year == registry_year) {
    return(list(verified = TRUE, status = "exact_year_match"))
  }

  list(verified = FALSE, status = paste0("year_mismatch_registry_year_", registry_year))
}

classify_verification_level <- function(doi_exists_verified, title_verified, author_verified, journal_verified, year_verified, hard_rejected = FALSE) {
  if (isTRUE(hard_rejected) || !isTRUE(doi_exists_verified)) return("rejected_or_unverified")
  if (!isTRUE(title_verified)) return("doi_exists_only")

  extra_count <- sum(c(isTRUE(author_verified), isTRUE(journal_verified), isTRUE(year_verified)))

  if (extra_count == 3) return("fully_metadata_verified")
  if (extra_count >= 1) return("metadata_partially_verified")
  "doi_title_verified"
}

metadata_summary <- function(level, title_status, author_ok, journal_ok, year_ok, year_status) {
  paste0(
    "verification_level=", level,
    "; title_status=", title_status,
    "; author_verified=", isTRUE(author_ok),
    "; journal_verified=", isTRUE(journal_ok),
    "; year_verified=", isTRUE(year_ok),
    "; year_match_status=", year_status
  )
}

# -----------------------------
# Future repair route note
# -----------------------------
# A future /api/v1/repair-metadata endpoint can be added after this DOI-first
# verifier is stable. It must not silently replace a submitted DOI; every
# suggested DOI must be rerun through /api/v1/verify-metadata.

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
      return(api_response(
        status = "error",
        message = "No usable JSON body was received. In Swagger, paste the JSON into the Request body box, not into a query parameter."
      ))
    }

    if (nrow(incoming_data) == 0) {
      res$status <- 400
      return(api_response(status = "error", message = "JSON array is valid but contains no records."))
    }

    required_keys <- c("doi", "author", "title", "year")
    missing_keys <- setdiff(required_keys, names(incoming_data))

    if (length(missing_keys) > 0) {
      res$status <- 400
      return(api_response(status = "error", message = paste("Missing mandatory array columns:", paste(missing_keys, collapse = ", "))))
    }

    if (!"journal" %in% names(incoming_data)) incoming_data$journal <- ""
    if (!"methodology" %in% names(incoming_data)) incoming_data$methodology <- ""
    if (!"relevance_note" %in% names(incoming_data)) incoming_data$relevance_note <- ""

    incoming_data$doi <- normalise_doi(unlist(incoming_data$doi))
    incoming_data$title <- as.character(unlist(incoming_data$title))
    incoming_data$author <- as.character(unlist(incoming_data$author))
    incoming_data$year <- suppressWarnings(as.integer(unlist(incoming_data$year)))
    incoming_data$journal <- as.character(unlist(incoming_data$journal))
    incoming_data$methodology <- as.character(unlist(incoming_data$methodology))
    incoming_data$relevance_note <- as.character(unlist(incoming_data$relevance_note))

    # Existing evidence fields, preserved for backward compatibility.
    # `verified = TRUE` continues to mean DOI-title verified, not fully metadata-verified.
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

    # New explicit metadata-level verification fields.
    incoming_data$doi_exists_verified <- FALSE
    incoming_data$title_verified <- FALSE
    incoming_data$author_verified <- FALSE
    incoming_data$journal_verified <- FALSE
    incoming_data$year_verified <- FALSE
    incoming_data$verification_level <- "rejected_or_unverified"
    incoming_data$registry_author <- ""
    incoming_data$registry_journal <- ""
    incoming_data$registry_year <- NA_integer_
    incoming_data$author_match_score <- 0.0
    incoming_data$journal_match_score <- 0.0
    incoming_data$year_match_status <- "unchecked"
    incoming_data$metadata_verification_summary <- ""
    incoming_data$manual_confirmation_required <- TRUE
    incoming_data$step_b_eligible <- FALSE

    # 1. DOI.org resolution layer.
    for (j in seq_len(nrow(incoming_data))) {
      if (is_blank_string(incoming_data$doi[j])) {
        incoming_data$verification_status[j] <- "missing_or_empty_doi"
        incoming_data$evidence_level[j] <- "manual_review_only"
        incoming_data$error_log[j] <- "No DOI was supplied. This DOI-first endpoint cannot verify the record."
        incoming_data$year_match_status[j] <- "not_checked_due_to_missing_doi"
        incoming_data$metadata_verification_summary[j] <- "rejected_or_unverified: missing DOI."
        next
      }

      doi_check <- doi_resolves_via_proxy(doi = incoming_data$doi[j], headers = POLITE_HEADERS)
      incoming_data$doi_resolves[j] <- isTRUE(doi_check$resolves)
      incoming_data$doi_proxy_status_code[j] <- suppressWarnings(as.integer(doi_check$status_code))
      incoming_data$doi_exists_verified[j] <- isTRUE(doi_check$resolves)
      Sys.sleep(0.20)
    }

    # 2. DOI metadata waterfall and metadata-level evidence scoring.
    for (i in seq_len(nrow(incoming_data))) {
      if (incoming_data$verification_status[i] == "missing_or_empty_doi") next

      metadata <- query_metadata_waterfall(doi = incoming_data$doi[i], headers = POLITE_HEADERS)
      incoming_data$registry_status_code[i] <- suppressWarnings(as.integer(metadata$status_code))

      if (isTRUE(metadata$found)) {
        incoming_data$metadata_found[i] <- TRUE
        incoming_data$verification_source[i] <- metadata$source
        incoming_data$verification_source_detail[i] <- metadata$source_detail
        incoming_data$registry_title[i] <- metadata$title
        incoming_data$registry_author[i] <- metadata$author
        incoming_data$registry_journal[i] <- metadata$journal
        incoming_data$registry_year[i] <- safe_integer(metadata$year)

        # A DOI can also be treated as existing if it appears in a credible registry, even when DOI.org has a transient failure.
        incoming_data$doi_exists_verified[i] <- isTRUE(incoming_data$doi_exists_verified[i]) || isTRUE(metadata$found)
      } else {
        incoming_data$verification_source[i] <- "None"
        incoming_data$verification_source_detail[i] <- "None"
        incoming_data$verification_level[i] <- if (isTRUE(incoming_data$doi_exists_verified[i])) "doi_exists_only" else "rejected_or_unverified"
        incoming_data$manual_confirmation_required[i] <- TRUE
        incoming_data$year_match_status[i] <- "metadata_missing"

        if (isTRUE(incoming_data$doi_resolves[i])) {
          incoming_data$verification_status[i] <- "doi_resolves_metadata_missing"
          incoming_data$evidence_level[i] <- "doi_resolves_metadata_missing"
          incoming_data$error_log[i] <- paste(
            "DOI resolves through DOI.org, but no usable Crossref/DataCite/OpenAlex metadata was found.",
            metadata$error_log
          )
        } else if (looks_like_title_repair_candidate(incoming_data$title[i], incoming_data$author[i], incoming_data$year[i])) {
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

        incoming_data$metadata_verification_summary[i] <- metadata_summary(
          incoming_data$verification_level[i],
          incoming_data$verification_status[i],
          FALSE,
          FALSE,
          FALSE,
          incoming_data$year_match_status[i]
        )
        next
      }

      title_score <- string_similarity(incoming_data$title[i], metadata$title, cleaner = clean_title)
      incoming_data$title_match_score[i] <- round(title_score, 2)

      decision <- title_match_decision(
        input_title = incoming_data$title[i],
        registry_title = metadata$title,
        doi_exists_verified = incoming_data$doi_exists_verified[i],
        match_pct = title_score,
        source_name = incoming_data$verification_source[i]
      )

      incoming_data$title_verified[i] <- isTRUE(decision$verified)
      incoming_data$verified[i] <- isTRUE(decision$verified)
      incoming_data$verification_status[i] <- decision$status
      incoming_data$evidence_level[i] <- decision$evidence_level
      incoming_data$error_log[i] <- decision$message

      author_match <- match_author(incoming_data$author[i], metadata$author)
      incoming_data$author_verified[i] <- isTRUE(author_match$verified)
      incoming_data$author_match_score[i] <- author_match$score

      journal_match <- match_journal(incoming_data$journal[i], metadata$journal)
      incoming_data$journal_verified[i] <- isTRUE(journal_match$verified)
      incoming_data$journal_match_score[i] <- journal_match$score

      year_match <- match_year(incoming_data$year[i], metadata$year)
      incoming_data$year_verified[i] <- isTRUE(year_match$verified)
      incoming_data$year_match_status[i] <- year_match$status

      hard_rejected <- incoming_data$verification_status[i] %in% c(
        "manual_review_only",
        "doi_unresolved_title_match_requires_recheck",
        "doi_unresolved_title_subtitle_variant_requires_recheck",
        "rejected_doi_valid_but_title_mismatch",
        "rejected_title_mismatch_or_unresolved_doi"
      )

      incoming_data$verification_level[i] <- classify_verification_level(
        doi_exists_verified = incoming_data$doi_exists_verified[i],
        title_verified = incoming_data$title_verified[i],
        author_verified = incoming_data$author_verified[i],
        journal_verified = incoming_data$journal_verified[i],
        year_verified = incoming_data$year_verified[i],
        hard_rejected = hard_rejected
      )

      incoming_data$manual_confirmation_required[i] <- incoming_data$verification_level[i] != "fully_metadata_verified"
      incoming_data$step_b_eligible[i] <- incoming_data$verification_level[i] == "fully_metadata_verified"
      incoming_data$metadata_verification_summary[i] <- metadata_summary(
        incoming_data$verification_level[i],
        incoming_data$verification_status[i],
        incoming_data$author_verified[i],
        incoming_data$journal_verified[i],
        incoming_data$year_verified[i],
        incoming_data$year_match_status[i]
      )

      Sys.sleep(0.30)
    }

    doi_title_verified_count <- sum(incoming_data$verification_level %in% c(
      "doi_title_verified", "metadata_partially_verified", "fully_metadata_verified"
    ), na.rm = TRUE)

    return(api_response(
      status = "success",
      total_processed = nrow(incoming_data),
      verified_count = sum(incoming_data$verified, na.rm = TRUE),
      doi_title_verified_count = doi_title_verified_count,
      fully_metadata_verified_count = sum(incoming_data$verification_level == "fully_metadata_verified", na.rm = TRUE),
      metadata_partially_verified_count = sum(incoming_data$verification_level == "metadata_partially_verified", na.rm = TRUE),
      doi_exists_only_count = sum(incoming_data$verification_level == "doi_exists_only", na.rm = TRUE),
      rejected_or_unverified_count = sum(incoming_data$verification_level == "rejected_or_unverified", na.rm = TRUE),
      manual_confirmation_required_count = sum(incoming_data$manual_confirmation_required, na.rm = TRUE),
      step_b_eligible_count = sum(incoming_data$step_b_eligible, na.rm = TRUE),
      all_passed = all(incoming_data$verified),
      all_fully_metadata_verified = all(incoming_data$verification_level == "fully_metadata_verified"),
      data = incoming_data
    ))
  }, error = function(err) {
    res$status <- 500
    return(api_response(
      status = "runtime_crash",
      message = err$message,
      hint = "Check request parsing, DOI URL construction, DOI.org resolution, Crossref/DataCite/OpenAlex response parsing, metadata extraction, or package versions."
    ))
  })
}
