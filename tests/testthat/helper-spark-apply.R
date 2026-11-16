# Reproduce sparklyr's grouped spark_apply() worker in this process, as measured
# with sparklyr 1.8.6 on Spark 3.5.1: `f(df, context)` runs once per `group_by`
# group, the worker prepends that group's key column to each result, typed
# `columns` are then applied by position (extra trailing columns are dropped,
# too few is an error), and untyped results keep a duplicated key as
# "<key>_1". Returns the combined data frame; no Spark connection is used.
spark_apply_grouped_result <- function(x, f, columns = NULL, group_by, context = NULL) {
  x <- as.data.frame(x)
  pieces <- lapply(split(x, x[[group_by]]), function(df) {
    result <- cbind(df[1, group_by, drop = FALSE], f(df, context))
    if (is.null(columns)) {
      names(result) <- gsub("\\.", "_", make.unique(names(result)))
      return(result)
    }
    if (ncol(result) < length(columns)) stop("Spark result has fewer columns than its schema")
    stats::setNames(result[seq_along(columns)], names(columns))
  })
  result <- do.call(rbind, unname(pieces))
  rownames(result) <- NULL
  result
}
