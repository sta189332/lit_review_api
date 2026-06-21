library(plumber)

pr <- plumber::plumb("plumber.R")

pr <- plumber::pr_set_api_spec(pr, function(spec) {
  if (!is.null(spec$paths[["/api/v1/verify-metadata"]][["post"]])) {
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$summary <-
      "Verify academic metadata against DOI.org, Crossref, DataCite, and OpenAlex"
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$description <- paste(
      "Accepts a raw JSON array of academic metadata records.",
      "The API checks DOI.org resolution, retrieves DOI metadata from Crossref, DataCite, and OpenAlex,",
      "compares submitted and registry titles, authors, journals or venues, and years,",
      "and returns explicit DOI proxy versus registry semantics including doi_proxy_resolves, doi_registry_verified, doi_verification_basis, doi_resolution_status, doi_resolution_summary,",
      "as well as metadata verification fields including doi_exists_verified, title_verified, author_verified, journal_verified, year_verified, verification_level, registry_author, registry_journal, and registry_year.",
      "The legacy verified field remains backward-compatible and indicates DOI-title verification, not full metadata verification.",
      "Top-level summary fields are returned as JSON scalars; data remains an array of record objects."
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
                doi = list(type = "string", example = "10.1016/j.caeai.2021.100041"),
                author = list(type = "string", example = "Ng"),
                title = list(type = "string", example = "Conceptualizing AI literacy: An exploratory review"),
                year = list(type = "integer", example = 2021),
                journal = list(type = "string", example = "Computers and Education: Artificial Intelligence"),
                methodology = list(type = "string", example = "Review-based"),
                relevance_note = list(type = "string", example = "Explains AI literacy as a curriculum-relevant construct for higher education.")
              )
            )
          ),
          example = list(
            list(
              doi = "10.1016/j.caeai.2021.100041",
              author = "Ng",
              title = "Conceptualizing AI literacy: An exploratory review",
              year = 2021,
              journal = "Computers and Education: Artificial Intelligence",
              methodology = "Review-based",
              relevance_note = "Explains AI literacy as a curriculum-relevant construct for higher education."
            ),
            list(
              doi = "10.1186/s41239-019-0171-0",
              author = "Zawacki-Richter",
              title = "Systematic review of research on artificial intelligence applications in higher education – where are the educators?",
              year = 2019,
              journal = "International Journal of Educational Technology in Higher Education",
              methodology = "Review-based",
              relevance_note = "Provides foundational evidence on AI in higher education and educator involvement."
            )
          )
        )
      )
    )
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$responses <- list(
      "200" = list(
        description = "Successful verification response with DOI-title and full metadata verification levels.",
        content = list(
          "application/json" = list(
            example = list(
              status = "success",
              total_processed = 1,
              verified_count = 1,
              doi_title_verified_count = 1,
              fully_metadata_verified_count = 1,
              metadata_partially_verified_count = 0,
              doi_exists_only_count = 0,
              rejected_or_unverified_count = 0,
              manual_confirmation_required_count = 0,
              step_b_eligible_count = 1,
              all_passed = TRUE,
              all_fully_metadata_verified = TRUE,
              data = list(
                list(
                  doi = "10.1016/j.caeai.2021.100041",
                  author = "Ng",
                  title = "Conceptualizing AI literacy: An exploratory review",
                  year = 2021,
                  journal = "Computers and Education: Artificial Intelligence",
                  methodology = "Review-based",
                  relevance_note = "Explains AI literacy as a curriculum-relevant construct for higher education.",
                  doi_resolves = TRUE,
                  doi_proxy_resolves = TRUE,
                  doi_proxy_status_code = 302,
                  doi_registry_verified = TRUE,
                  registry_metadata_found = TRUE,
                  doi_verification_basis = "doi_proxy",
                  doi_resolution_status = "proxy_resolved",
                  doi_resolution_summary = "DOI.org proxy resolved and Crossref registry metadata was found.",
                  metadata_found = TRUE,
                  verified = TRUE,
                  verification_source = "Crossref",
                  verification_source_detail = "Crossref DOI metadata",
                  verification_status = "verified_exact_title_match",
                  evidence_level = "primary_doi_title_match",
                  registry_title = "Conceptualizing AI literacy: An exploratory review",
                  registry_status_code = 200,
                  error_log = "Passed verification: exact or near-exact DOI-title match.",
                  title_match_score = 1,
                  doi_exists_verified = TRUE,
                  title_verified = TRUE,
                  author_verified = TRUE,
                  journal_verified = TRUE,
                  year_verified = TRUE,
                  verification_level = "fully_metadata_verified",
                  registry_author = "Ng",
                  registry_journal = "Computers and Education: Artificial Intelligence",
                  registry_year = 2021,
                  author_match_score = 1,
                  journal_match_score = 1,
                  year_match_status = "exact_year_match",
                  metadata_verification_summary = "verification_level=fully_metadata_verified; title_status=verified_exact_title_match; author_verified=TRUE; journal_verified=TRUE; year_verified=TRUE; year_match_status=exact_year_match",
                  manual_confirmation_required = FALSE,
                  step_b_eligible = TRUE
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
