library(plumber)

pr <- plumber::plumb("plumber.R")

pr <- plumber::pr_set_api_spec(pr, function(spec) {
  
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
          )
        )
      )
    )
  )
  
  spec
})

pr$run(host = "127.0.0.1", port = 8000, swagger = TRUE)