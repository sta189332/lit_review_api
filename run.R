library(plumber)

pr <- plumber::plumb("plumber.R")

pr <- plumber::pr_set_api_spec(pr, function(spec) {
  
  if (!is.null(spec$paths[["/api/v1/verify-metadata"]][["post"]])) {
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$summary <- 
      "Verify academic metadata against DOI.org, Crossref, and OpenAlex"
    
    spec$paths[["/api/v1/verify-metadata"]][["post"]]$description <- 
      "Accepts a raw JSON array of academic metadata records. The API checks DOI resolution, retrieves registry metadata, compares submitted and registry titles, and returns verification evidence."
    
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
                  example = "10.1007/978-3-032-11449-5"
                ),
                author = list(
                  type = "string",
                  example = "Reimers"
                ),
                title = list(
                  type = "string",
                  example = "Artificial intelligence and education in the Global South: A systems perspective"
                ),
                year = list(
                  type = "integer",
                  example = 2026
                ),
                journal = list(
                  type = "string",
                  example = "Springer Nature"
                ),
                methodology = list(
                  type = "string",
                  example = "Mixed"
                ),
                relevance_note = list(
                  type = "string",
                  example = "Evaluates how AI ecosystems disrupt traditional university curricula and systemic educational governance in developing economies."
                )
              )
            )
          ),
          example = list(
            list(
              doi = "10.1007/978-3-032-11449-5",
              author = "Reimers",
              title = "Artificial intelligence and education in the Global South: A systems perspective",
              year = 2026,
              journal = "Springer Nature",
              methodology = "Mixed",
              relevance_note = "Evaluates how AI ecosystems disrupt traditional university curricula and systemic educational governance in developing economies."
            ),
            list(
              doi = "10.3389/feduc.2025.1667884",
              author = "Gawe",
              title = "A systematic review on AI-enhanced pedagogies in higher education in the Global South",
              year = 2025,
              journal = "Frontiers in Education",
              methodology = "Review-based",
              relevance_note = "Critiques the dominance of Western tech-centric frameworks over localized, student-centered learning in regional syllabi."
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
              data = list()
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