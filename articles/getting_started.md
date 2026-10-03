# Getting started

``` r

project_folder <- dirname(getwd())
path_to_env <- file.path(project_folder, ".env")
readRenviron(path_to_env)
api_base_url <- "https://restful-booker.herokuapp.com/"
auth_token <- Sys.getenv("AUTH_TOKEN")
headers <- list("Accept" = "application/json", "Content-type" = "application/json")
```

**Note (2026):** This vignette uses Restful Booker, a third-party
sandbox API. All chunks that configure or call the sandbox are marked
`eval = FALSE`, so documentation builds do not make external requests or
create, retrieve, or delete sandbox records. Run the examples
interactively only if you accept responsibility for their effects on
that service.

Let’s instantiate the API client

``` r

booking_client <- httpeasyrest::HttpRestClient$new(
  base_url = api_base_url,
  headers = headers,
  token = auth_token
)
```

## Get all booking IDS as a DATAFRAME

``` r

api_data <- booking_client$get_dataframe(end_point = "booking")
head(api_data$http_resp)
```

## Get all bookings IDS as a JSON object

``` r

api_data <- booking_client$get_object(end_point = "booking")
head(api_data$http_resp)
```

## Get single specific booking record as JSON object

Notice that this specific API maps the `id` identifying the record in
the URL itself, rather than as a query string

``` r

booking_id <- api_data$http_resp[[1]]$bookingid
api_data <- booking_client$get_object(end_point = paste0("booking/", booking_id))
api_data$http_resp
```

## Create a single booking object afresh

Let’s create a unique name that we can confidently search for it
afterwards ensuring that is unique, and therefore its fetching is not
down to luck - recall this API is used by many people.

``` r

unique_name <- stringi::stri_rand_strings(1, 12)
data <- list(
  firstname = unique_name,
  lastname = "Jimenez",
  totalprice = 111,
  depositpaid = TRUE,
  bookingdates = list(
    checkin = "2018-01-01",
    checkout = "2019-01-01"
  ),
  additionalneeds = "Breakfast"
)
api_data <- booking_client$post_object(end_point = "booking", data)
testit::assert(isTRUE(api_data$success))
api_data$http_resp
```

Let’s ensure that such object was indeed sent to the API successfully.

``` r

end_point <- paste0("booking/", api_data$http_resp$bookingid)
inserted_booking <- booking_client$get_object(end_point = end_point)
testit::assert(inserted_booking$http_resp$firstname == unique_name)
inserted_booking$http_resp
```

## Delete a single booking object

Delete the booking created in the previous example by appending its
identifier to the `booking` endpoint.

``` r

end_point <- "booking/"
deleted_booking <- booking_client$delete_object(
  end_point = end_point,
  field = api_data$http_resp$bookingid
)
deleted_booking
```
