# HttpRestClient class

Class providing simple atomic CRUD requests to a particular end_point

## Value

a list with success flag, status code of the call and response from the
API in JSON format. It will contain the error message in case of error.

a list with success flag, status code of the call and response from the
API in a dataframe format. In case of error it will contain the error in
JSON format.

a list of lists. Each element in the outer list will contain the the
successful or failed response, which object structure is similar to
other calls (success, status_code and response in JSON format).

a list with success flag, status code of the call and response from the
API in JSON format. It will contain the error message in case of error.

a list with success flag, status code of the call and response from the
API in JSON format. It will contain the error message in case of error.

## Public fields

- `base_url`:

  string containing the base url of the querying API (without the
  end_point) part

- `token`:

  string to authenticate against the API

- `headers`:

  list of HTTP headers

## Methods

### Public methods

- [`HttpRestClient$new()`](#method-HttpRestClient-new)

- [`HttpRestClient$get_object()`](#method-HttpRestClient-get_object)

- [`HttpRestClient$get_dataframe()`](#method-HttpRestClient-get_dataframe)

- [`HttpRestClient$post_dataframe()`](#method-HttpRestClient-post_dataframe)

- [`HttpRestClient$post_object()`](#method-HttpRestClient-post_object)

- [`HttpRestClient$delete_object()`](#method-HttpRestClient-delete_object)

- [`HttpRestClient$clone()`](#method-HttpRestClient-clone)

------------------------------------------------------------------------

### Method `new()`

Initialise HttpRestClient client

#### Usage

    HttpRestClient$new(base_url, headers, token)

#### Arguments

- `base_url`:

  string containing the base url of the querying API (without the
  end_point) part

- `headers`:

  list of HTTP headers

- `token`:

  string to authenticate against the API

------------------------------------------------------------------------

### Method `get_object()`

Gets data from the API and return it in JSON format

#### Usage

    HttpRestClient$get_object(end_point, parameters = NULL)

#### Arguments

- `end_point`:

  string containing the relative part of the API URL

- `parameters`:

  list of parameters to pass on to the query string of the underlying
  GET method

------------------------------------------------------------------------

### Method `get_dataframe()`

Gets data from the API and return it in dataframe format. It expects one
record per row in the rendered dataframe.

#### Usage

    HttpRestClient$get_dataframe(end_point, parameters = NULL)

#### Arguments

- `end_point`:

  string containing the relative part of the API URL

- `parameters`:

  list of parameters to pass on to the query string of the underlying
  GET method

------------------------------------------------------------------------

### Method `post_dataframe()`

Post every single row of teh passed dataframe in one-by-one fahsion to
the API. It expects a row per record to be sent, therefore it makes
nrow() calls to the API. It will not stop until all rows of the
dataframe have been walked through.

#### Usage

    HttpRestClient$post_dataframe(end_point, df)

#### Arguments

- `end_point`:

  string containing the relative part of the API URL

- `df`:

  a dataframe of data to be sent to the API

------------------------------------------------------------------------

### Method `post_object()`

Post data to the API. The returned JSON may contain a unique ID
generated after the record was registered.

#### Usage

    HttpRestClient$post_object(end_point, obj)

#### Arguments

- `end_point`:

  string containing the relative part of the API URL

- `obj`:

  a flat or nested list convertable to a JSON object

------------------------------------------------------------------------

### Method `delete_object()`

Delete a single record identified by the passed field which should be
unique.

#### Usage

    HttpRestClient$delete_object(end_point, field)

#### Arguments

- `end_point`:

  string containing the relative part of the API URL

- `field`:

  a flat or nested list convertable to a JSON object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HttpRestClient$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
