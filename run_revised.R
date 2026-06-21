library(plumber)

pr <- plumber::plumb("plumber.R")

pr <- plumber::pr_set_api_spec(pr, function(spec) {
  
  if (!is.null(spec$paths[["/api/v1/verify-metadata"]][["post"]])) {
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$summary <- 
      "Verify academic metadata against DOI.org, Crossref, DataCite, and OpenAlex"
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$description <- 
      paste(
        "Accepts a raw JSON array of academic metadata records.",
        "The API checks DOI.org resolution, retrieves DOI metadata from Crossref, DataCite, and OpenAlex,",
        "compares submitted and registry titles, and returns strict verification evidence.",
        "Semantic Scholar and Europe PMC are reserved for future enrichment or domain-specific workflows and do not override DOI-title mismatches."
      )
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$requestBody <- list(
      required = TRUE,
      description = "Paste a raw JSON array of academic metadata records.",
      content = list(
        "application/json" = list(
          schema = list(
            type = "array",
            items = list(
              type = "object",
              required = list("doi", "author", "title", "year"),
              properties = list(
                doi = list(
                  type = "string",
                  example = "10.3389/feduc.2025.1667884"
                ),
                author = list(
                  type = "string",
                  example = "Gawe"
                ),
                title = list(
                  type = "string",
                  example = "A systematic review on AI-enhanced pedagogies in higher education in the Global South"
                ),
                year = list(
                  type = "integer",
                  example = 2025
                ),
                journal = list(
                  type = "string",
                  example = "Frontiers in Education"
                ),
                methodology = list(
                  type = "string",
                  example = "Review-based"
                ),
                relevance_note = list(
                  type = "string",
                  example = "Examines AI-enhanced pedagogies in higher education contexts relevant to Global South curriculum and equity debates."
                )
              )
            )
          ),
          example = list(
            list(
              doi = "10.3389/feduc.2025.1667884",
              author = "Gawe",
              title = "A systematic review on AI-enhanced pedagogies in higher education in the Global South",
              year = 2025,
              journal = "Frontiers in Education",
              methodology = "Review-based",
              relevance_note = "Examines AI-enhanced pedagogies in higher education contexts relevant to Global South curriculum and equity debates."
            ),
            list(
              doi = "10.14742/ajet.10596",
              author = "Medina-Gual",
              title = "A tridimensional model of AI literacy: An empirical analysis of student performance and demographic patterns in higher education",
              year = 2025,
              journal = "Australasian Journal of Educational Technology",
              methodology = "Empirical",
              relevance_note = "Provides evidence on AI literacy dimensions that can support higher education curriculum design."
            )
          )
        )
      )
    )
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$responses <- list(
      "200" = list(
        description = "Successful verification response.",
        content = list(
          "application/json" = list(
            example = list(
              status = "success",
              total_processed = 2,
              verified_count = 2,
              all_passed = TRUE,
              data = list(
                list(
                  doi = "10.3389/feduc.2025.1667884",
                  author = "Gawe",
                  title = "A systematic review on AI-enhanced pedagogies in higher education in the Global South",
                  year = 2025,
                  journal = "Frontiers in Education",
                  methodology = "Review-based",
                  relevance_note = "Examines AI-enhanced pedagogies in higher education contexts relevant to Global South curriculum and equity debates.",
                  doi_resolves = TRUE,
                  doi_proxy_status_code = 302,
                  metadata_found = TRUE,
                  verified = TRUE,
                  verification_source = "Crossref",
                  verification_source_detail = "Crossref DOI metadata",
                  verification_status = "verified_exact_title_match",
                  evidence_level = "primary_doi_title_match",
                  registry_title = "A systematic review on AI-enhanced pedagogies in higher education in the Global South",
                  registry_status_code = 200,
                  error_log = "Passed verification: exact or near-exact DOI-title match.",
                  title_match_score = 1
                )
              )
            )
          )
        )
      ),
      "400" = list(
        description = "Bad request. The JSON body is missing, empty, malformed, or missing required columns."
      ),
      "500" = list(
        description = "Runtime crash inside the verification service."
      )
    )
  }
  
  spec
})

pr$run(host = "127.0.0.1", port = 8000, swagger = TRUE)
