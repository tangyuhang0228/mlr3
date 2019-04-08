#' @title DataBackend for Matrix
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackend].
#' @include DataBackend.R
#'
#' @description
#' [DataBackend] for \CRANpkg{Matrix}. Data is stored as (sparse) matrix.
#'
#' @section Construction:
#' ```
#' DataBackendMatrix$new(data, primary_key = NULL)
#' ```
#'
#' * `data` :: [Matrix::Matrix()].
#'
#' * `dense` :: [data.frame()]\cr
#'   Data frame of additional variables which are not sparse.
#'   Must have the same number of rows as `data`.
#'
#' * `primary_key` :: `character(1)`\cr
#'   Name of the primary key column.
#'   Must be a column name of the provided `dense` data part,
#'   or will be automatically constructed from the rownames of `data`.
#'
#' Alternatively, use [as_data_backend] on a [Matrix::Matrix()].
#'
#' @section Fields:
#' @inheritSection DataBackend Fields
#'
#' @section Methods:
#' @inheritSection DataBackend Methods
#'
#' @family DataBackend
#' @export
#' @examples
#' requireNamespace("Matrix")
#' data = Matrix::Matrix(sample(0:1, 20, replace = TRUE), ncol = 2)
#' colnames(data) = c("x1", "x2")
#' rownames(data) = paste0("row_", 1:10)
#'
#' b = as_data_backend(data)
#' expect_backend(b)
#' b$missings(b$rownames, b$colnames)
#' b$head()
#' b$data(b$rownames[1:3], b$colnames, data_format = "Matrix")
DataBackendMatrix = R6Class("DataBackendMatrix", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(data, dense = NULL, primary_key = NULL) {
      require_namespaces("Matrix")
      assert_class(data, "Matrix")
      if (any(dim(data) == 0L))
        stopf("No data in Matrix")
      assert_names(colnames(data), type = "unique")

      if (is.null(rownames(data))) {
        rn = seq_row(data)
      } else {
        rn = assert_names(rownames(data), type = "unique")
        rownames(data) = NULL
      }

      if (is.null(dense)) {
        dense = data.table()
      } else {
        assert_data_frame(dense, nrow = nrow(data), min.cols = 1L)
        dense = as.data.table(dense)
        assert_names(names(dense), disjunct.from = colnames(data))
      }

      if (is.null(primary_key)) {
        primary_key = "..row_id"
        dense = ref_cbind(dense, data.table(..row_id = rn))
      } else {
        assert_choice(primary_key, names(dense))
      }
      setkeyv(dense, primary_key, physical = FALSE)
      super$initialize(data = list(sparse = data, dense = dense), "..row_id", c("data.table", "Matrix"))
    },

    data = function(rows, cols, data_format = "data.table") {
      assert_choice(data_format, self$data_formats)
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      query_rows = private$.translate_rows(rows)
      query_cols = intersect(cols, c(colnames(private$.data$dense), colnames(private$.data$sparse)))
      # data = private$.data[query_rows, query_cols, drop = FALSE]

      dense = private$.data$dense[query_rows, intersect(query_cols, colnames(private$.data$dense)), with = FALSE]
      sparse = private$.data$sparse[query_rows, intersect(query_cols, colnames(private$.data$sparse)), drop = FALSE]

      switch(data_format,
        "data.table" = ref_cbind(dense, as.data.table(as.matrix(sparse))),
        "Matrix" = list(dense = dense, sparse = sparse)
      )
    },

    head = function(n = 6L) {
      self$data(head(self$rownames, n), self$colnames, data_format = "data.table")
    },

    distinct = function(rows, cols) {
      ii = if (is.null(rows)) self$rownames else private$.translate_rows(rows)

      cols_dense = setdiff(intersect(cols, colnames(private$.data$dense)), self$primary_key)
      cols_sparse = intersect(cols, colnames(private$.data$sparse))

      pk = if (self$primary_key %in% cols) set_names(list(self$rownames[ii]), self$primary_key) else NULL
      dense = set_names(lapply(cols_dense, function(col) distinct(private$.data$dense[ii, col, with = FALSE][[1L]])), cols_dense)
      sparse = set_names(lapply(cols_sparse, function(col) distinct(private$.data$sparse[ii, col])), cols_sparse)

      res = c(pk, dense, sparse)
      res[match(cols, names(res), nomatch = 0L)]
    },

    missings = function(rows, cols) {
      ii = private$.translate_rows(rows)

      cols_dense = setdiff(intersect(cols, colnames(private$.data$dense)), self$primary_key)
      cols_sparse = intersect(cols, colnames(private$.data$sparse))

      pk = if (self$primary_key %in% cols) set_names(0L, self$primary_key) else NULL
      dense = set_names(map_int(cols_dense, function(col) private$.data$dense[ii, sum(is.na(col)), with = FALSE][[1L]]), cols_dense)
      sparse = apply(private$.data$sparse[ii, cols_sparse, drop = FALSE], 2L, function(x) sum(is.na(x)))

      res = c(pk, dense, sparse)
      res[match(cols, names(res), nomatch = 0L)]
    }
  ),

  active = list(
    rownames = function() {
      private$.data$dense[[self$primary_key]]
    },

    colnames = function() {
      c(colnames(private$.data$dense), colnames(private$.data$sparse))
    },

    nrow = function() {
      nrow(private$.data$dense)
    },

    ncol = function() {
      ncol(private$.data$dense) + ncol(private$.data$sparse)
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(private$.data)
    },

    .translate_rows = function(rows) {
      ii = set_names(list(rows), self$primary_key)
      private$.data$dense[ii, nomatch = 0L, on = self$primary_key, which = TRUE]
    }
  )
)

#' @export
as_data_backend.Matrix = function(data, dense = NULL, primary_key = NULL, ...) {
  DataBackendMatrix$new(data, dense = dense, primary_key = primary_key)
}
