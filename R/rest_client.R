#' @title HttpRestClient class
#'
#' @description
#' it makes simple atomic CRUD requests to a particular end_point
HttpRestClient <- R6::R6Class("HttpRestClient", public = list( # nolint
  http_req = NULL,
  base_url = NULL,
  end_point = NULL,
  # @formatter:off
  #' @description
  #' Initialise HttpRestClient client
  #'
  #' @export
  # @formatter:on
  initialize = function(base_url, end_point) {
    self$base_url <- base_url
    self$end_point <- end_point
    self$http_req <- private$build_request_object(self$base_url, self$end_point)
  },
  get = function(parameters = NULL) {
    ret <- tryCatch(
      {
        if (is.null(parameters)) {
          http_resp <- self$http_req %>%
            httr2::req_perform()
        } else {
          http_resp <- self$http_req %>%
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
  get_dataframe = function(parameters = NULL) {
    ret <- tryCatch(
      {
        if (is.null(parameters)) {
          http_resp <- self$http_req %>%
            httr2::req_perform()
        } else {
          http_resp <- self$http_req %>%
            httr2::req_url_query(!!!parameters) %>%
            httr2::req_perform()
        }
        list(
          success = TRUE,
          status_code = http_resp$status_code,
          http_resp = http_resp %>% # to dataframe
            httr2::resp_body_json() %>%
            lapply(function(list_obj) {
              data.frame(private$null_to_na(list_obj), stringsAsFactors = FALSE)
            }) %>%
            dplyr::bind_rows()
        )
      },
      error = function(e) {
        private$error_handler(e, method = "get")
      }
    )
    return(ret)
  },
  post_dataframe = function(df) {
    ret <- list()
    for (offset in seq_len(nrow(df))) {
      ret[[offset]] <- private$post_item(as.vector(df[offset, , drop = FALSE]))
    }
    return(ret)
  },
  post_object = function(obj) {
    return(private$post_item(obj))
  },
  delete_object = function(id) {
    return(private$delete_item(id))
  }
), private = list(
  build_request_object = function(base_url, end_point) {
    return(
      httr2::request(base_url) %>%
        httr2::req_url_path_append(end_point) %>%
        httr2::req_headers("Accept" = "application/json") %>%
        httr2::req_headers("Content-type" = "application/json")
    )
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
          logger::log_error("Something unexpected happened. Review either the data your are sending or the endpoint")
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
  delete_item = function(id) {
    ret <- tryCatch(
      {
        http_resp <- self$http_req %>%
          httr2::req_url_path_append(id) %>%
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
  post_item = function(df_row) {
    ret <- tryCatch(
      {
        http_resp <- self$http_req %>%
          httr2::req_method("POST") %>%
          httr2::req_body_json(df_row) %>%
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
