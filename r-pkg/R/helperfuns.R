# [title] Extract the content of an HTTP response into a different format
# [name] .content
# [description] Mainly here to making mocking easier in testing.
# [references] https://testthat.r-lib.org/reference/local_mocked_bindings.html#namespaced-calls
#' @importFrom httr2 resp_body_json resp_body_json
.content <- function(response, as) {
    if (as == "text") {
        return(httr2::resp_body_string(response))
    }
    # otherwise, assume JSON
    # (equivalent to httr::content(as = "parsed"))
    return(httr2::resp_body_json(
        resp = response
        , simplifyVector = FALSE
    ))
}

# [title] Get a random length-n string
# [name] .random_string
# [description] Get a random length-n string of lowercase letters.
#               Note that this uses sample() and so might produce deterministic
#               results in programs where set.seed() is used to control randomness.
.random_string <- function(num_characters) {
    return(
        paste(
            sample(letters, replace = TRUE, size = num_characters)
            , collapse = ""
        )
    )
}

# [title] Execute an HTTP request and return the result
# [name] .request
# [description] Mainly here to making mocking easier in testing, but this
#               also centralizes the mechanism for HTTP request exexcution in one place.
# [references] https://testthat.r-lib.org/reference/local_mocked_bindings.html#namespaced-calls
#' @importFrom httr2 request req_body_raw req_headers req_method req_perform req_retry
#' @importFrom httr2 resp_status
.request <- function(verb, url, body) {

    # prepare "request" object
    req <- httr2::req_method(
        req = httr2::request(url)
        , method = verb
    )

    # add headers
    #
    # Why is this hard-coded instead of taking in a 'headers' argument?
    #
    # httr2::req_headers() does not accept a character vector or list,
    # only named arguments.
    #
    # But no problem! Every HTTP request this project executes uses JSON.
    req <- httr2::req_headers(
        req
        , Accept = "application/json"          # nolint[non_portable_path]
        , `Content-Type` = "application/json"  # nolint[non_portable_path]
    )

    # add body
    if (!is.null(body)) {
        req <- httr2::req_body_raw(
            req = req
            , body = body
        )
    }

    # execute the request
    response <- httr2::req_perform(
        httr2::req_retry(
            req = req
            , max_tries = 3L
            # By default, req_perform() will raise an R exception for
            # any 4xx and 5xx responses. This function indicates some 4xx and 5xx
            # that should be treated as retryable.
            , is_transient = function(res) {
                httr2::resp_status(res) %in% c(
                    # 429 - too many requests
                    429L
                    # 500 - internal server error
                    , 500L
                    # 502 - bad gateway
                    , 502L
                    # 504 - gateway timeout
                    , 504L
                    # 503 - unavailable
                    , 503L
                )
            }
        )
    )
    return(response)
}
