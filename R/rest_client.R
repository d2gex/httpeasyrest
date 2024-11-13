#' @title HttpRestClient class
#'
#' @description
#' Class providing simple atomic CRUD requests to a particular end_point
HttpRestClient <- R6::R6Class("HttpRestClient", public = list( # nolint
  # @formatter:off
  #' @field base_url string containing the base url of the querying API (without the end_point) part
  base_url = NULL,
  #' @field token string to authenticate against the API
  token = NULL,
  #' @field headers list of HTTP headers
  headers = NULL,

  #' @description
  #' Initialise HttpRestClient client
  #'
  #' @param base_url string containing the base url of the querying API (without the end_point) part
  #' @param headers list of HTTP headers
  #' @param token string to authenticate against the API
  #' @export
  # @formatter:on
  initialize = function(base_url, headers, token) {
    self$base_url <- base_url
    self$headers <- headers
    self$token <- token
  },
  # @formatter:off
  #' @description
  #' Gets data from the API and return it in JSON format
  #'
  #' @param end_point string containing the relative part of the API URL
  #' @param parameters list of parameters to pass on to the query string of the underlying GET method
  #' @returns a list with success flag, status code of the call and response from the API in JSON format. It will
  #' contain the error message in case of error.
  #' @export
  # @formatter:on
  get_object = function(end_point, parameters = NULL) {
    return(private$get_item(end_point, parameters))
  },
  # @formatter:off
  #' @description
  #' Gets data from the API and return it in dataframe format. It expects one record per row in the rendered dataframe.
  #'
  #' @param end_point string containing the relative part of the API URL
  #' @param parameters list of parameters to pass on to the query string of the underlying GET method
  #' @returns a list with success flag, status code of the call and response from the API in a dataframe format. In case
  #' of error it will contain the error in JSON format.
  #' @export
  # @formatter:on
  get_dataframe = function(end_point, parameters = NULL) {
    ret <- private$get_item(end_point, parameters)
    if (isFALSE(ret$success)) {
      return(ret)
    }
    ret$http_resp <- ret$http_resp %>%
      lapply(function(list_obj) {
        data.frame(private$null_to_na(list_obj), stringsAsFactors = FALSE)
      }) %>%
      dplyr::bind_rows()
    return(ret)
  },
  # @formatter:off
  #' @description
  #' Post every single row of teh passed dataframe in one-by-one fahsion to the API. It expects a row per record to be
  #' sent, therefore it makes nrow() calls to the API. It will not stop until all rows of the dataframe have been walked
  #' through.
  #'
  #' @param end_point string containing the relative part of the API URL
  #' @param df a dataframe of data to be sent to the API
  #' @returns a list of lists. Each element in the outer list will contain the the successful or failed response, which
  #' object structure is similar to other calls (success, status_code and response in JSON format).
  #' @export
  # @formatter:on
  post_dataframe = function(end_point, df) {
    ret <- list()
    for (offset in seq_len(nrow(df))) {
      ret[[offset]] <- private$post_item(end_point, as.vector(df[offset, , drop = FALSE]))
    }
    return(ret)
  },
  # @formatter:off
  #' @description
  #' Post data to the API. The returned JSON may contain a unique ID generated after the record was registered.
  #'
  #' @param end_point string containing the relative part of the API URL
  #' @param obj a flat or nested list convertable to a JSON object
  #' @returns a list with success flag, status code of the call and response from the API in JSON format. It will
  #' contain the error message in case of error.
  #' @export
  # @formatter:on
  post_object = function(end_point, obj) {
    return(private$post_item(end_point, obj))
  },
  # @formatter:off
  #' @description
  #' Delete a single record identified by the passed field which should be unique.
  #'
  #' @param end_point string containing the relative part of the API URL
  #' @param field a flat or nested list convertable to a JSON object
  #' @returns a list with success flag, status code of the call and response from the API in JSON format. It will
  #' contain the error message in case of error.
  #' @export
  # @formatter:on
  delete_object = function(end_point, field) {
    return(private$delete_item(end_point, field))
  }
), private = list(
  build_request_object = function(base_url, end_point, token, headers) {
    http_request <- httr2::request(base_url) %>%
      httr2::req_headers(!!!headers) %>%
      httr2::req_url_path_append(end_point)

    if (!is.null(token)) {
      http_request <- http_request %>%
        httr2::req_auth_bearer_token(token)
    }
    return(http_request)
  },
  error_handler = function(e, method = "post") {
    ret <- list(
      success = FALSE,
      status_code = e$status
    )
    # GET handling
    if (method == "get") {
      ret$errors <- e$message
      if (!is.null(e$parent$message)) {
        ret$errors <- paste(ret$errors, ":", e$parent$message)
      }
    } else { # POST handling
      errors <- tryCatch(
        {
          e$resp %>% httr2::resp_body_json()
        },
        error = function(ex) { # something happened at server level?
          logger::log_error("Something unexpected happened. Look at the returned error message and status code for
          more information")
          if (is.null(ex$resp)) {
            ex$message
          } else {
            ex$resp %>% httr2::resp_body_string()
          }
        }
      )
      ret$errors <- errors
    }
    return(ret)
  },
  get_item = function(end_point, parameters) {
    http_req <- private$build_request_object(self$base_url, end_point, self$token, self$headers)
    ret <- tryCatch(
      {
        if (is.null(parameters)) {
          http_resp <- http_req %>%
            httr2::req_perform()
        } else {
          http_resp <- http_req %>%
            httr2::req_url_query(!!!parameters) %>%
            httr2::req_perform()
        }
        list(
          success = TRUE,
          status_code = http_resp$status_code,
          http_resp = http_resp %>%
            httr2::resp_body_json()
        )
      },
      error = function(e) {
        private$error_handler(e, method = "get")
      }
    )
    return(ret)
  },
  delete_item = function(end_point, field) {
    http_req <- private$build_request_object(self$base_url, end_point, self$token, self$headers)
    ret <- tryCatch(
      {
        http_resp <- http_req %>%
          httr2::req_url_path_append(field) %>%
          httr2::req_method("DELETE") %>%
          httr2::req_perform()
        list(
          success = TRUE,
          status_code = http_resp$status_code,
          http_resp = http_resp %>% httr2::resp_body_json()
        )
      },
      error = function(e) {
        private$error_handler(e)
      }
    )
    return(ret)
  },
  post_item = function(end_point, df) {
    http_req <- private$build_request_object(self$base_url, end_point, self$token, self$headers)
    ret <- tryCatch(
      {
        http_resp <- http_req %>%
          httr2::req_method("POST") %>%
          httr2::req_body_json(df) %>%
          httr2::req_perform()
        list(
          success = TRUE,
          status_code = http_resp$status_code,
          http_resp = http_resp %>% httr2::resp_body_json()
        )
      },
      error = function(e) {
        private$error_handler(e)
      }
    )
    return(ret)
  },
  null_to_na = function(data) {
    for (field in names(data)) {
      if (is.null(data[[field]])) {
        data[[field]] <- NA
      }
    }
    return(data)
  }
))
