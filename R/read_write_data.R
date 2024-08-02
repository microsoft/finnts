#' Get Final Forecast Data
#'
#' @param run_info run info using the [set_run_info()] function
#' @param return_type return type
#'
#' @return table of final forecast results
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     id == "M2",
#'     Date >= "2012-01-01",
#'     Date <= "2015-06-01"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3,
#'   recipes_to_run = "R1"
#' )
#'
#' prep_models(run_info,
#'   models_to_run = c("arima", "ets"),
#'   num_hyperparameters = 1
#' )
#'
#' train_models(run_info,
#'   run_local_models = TRUE
#' )
#'
#' final_models(run_info,
#'   average_models = FALSE
#' )
#'
#' fcst_tbl <- get_forecast_data(run_info)
#' }
#' @export
get_forecast_data <- function(run_info,
                              return_type = "df") {
  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("return_type", return_type, "character", c("df", "sdf"))

  # get input values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  combo_variables <- strsplit(log_df$combo_variables, split = "---")[[1]]
  forecast_approach <- log_df$forecast_approach

  # get train test split data
  model_train_test_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-train_test_split.", run_info$data_output
    ),
    return_type = return_type
  ) %>%
    dplyr::select(Run_Type, Train_Test_ID) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID))

  # check if data has been condensed
  cond_path <- paste0(
    run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
    hash_data(run_info$run_name), "*condensed", ".", run_info$data_output
  )

  condensed_files <- list_files(run_info$storage_object, fs::path(cond_path))

  if (length(condensed_files) > 0) {
    condensed <- TRUE
  } else {
    condensed <- FALSE
  }

  # get forecast data
  if (forecast_approach != "bottoms_up") {
    fcst_path <- paste0(
      "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*reconciled", ".", run_info$data_output
    )
  } else if (condensed) {
    fcst_path <- paste0(
      "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*condensed", ".", run_info$data_output
    )
  } else {
    fcst_path <- paste0(
      "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*models", ".", run_info$data_output
    )
  }

  forecast_tbl <- read_file(run_info,
    path = fcst_path,
    return_type = return_type
  ) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
    dplyr::left_join(model_train_test_tbl,
      by = "Train_Test_ID"
    ) %>%
    dplyr::relocate(Run_Type, .before = Train_Test_ID) %>%
    dplyr::select(-Combo_ID, -Hyperparameter_ID) %>%
    dplyr::relocate(Combo) %>%
    dplyr::arrange(Combo, dplyr::desc(Best_Model), Model_ID, Train_Test_ID, Date) %>%
    tidyr::separate(
      col = Combo,
      into = combo_variables,
      remove = FALSE,
      sep = "--"
    ) %>%
    base::suppressWarnings()

  return(forecast_tbl)
}

#' Get Final Trained Models
#'
#' @param run_info run info using the [set_run_info()] function
#'
#' @return table of final trained models
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     id == "M2",
#'     Date >= "2012-01-01",
#'     Date <= "2015-06-01"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3,
#'   recipes_to_run = "R1"
#' )
#'
#' prep_models(run_info,
#'   models_to_run = c("arima", "ets"),
#'   num_hyperparameters = 1
#' )
#'
#' train_models(run_info,
#'   run_global_models = FALSE,
#'   run_local_models = TRUE
#' )
#'
#' final_models(run_info,
#'   average_models = FALSE
#' )
#'
#' models_tbl <- get_trained_models(run_info)
#' }
#' @export
get_trained_models <- function(run_info) {
  # check input values
  check_input_type("run_info", run_info, "list")

  # get trained files
  model_path <- paste0(
    "/models/*", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
    "-*_models.", run_info$object_output
  )

  trained_model_tbl <- read_file(run_info,
    path = model_path,
    return_type = "df"
  )

  return(trained_model_tbl)
}

#' Get Prepped Data
#'
#' @param run_info run info using the [set_run_info()] function
#' @param recipe recipe to return. Either a value of "R1" or "R2"
#' @param return_type return type
#'
#' @return table of prepped data
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     id == "M2",
#'     Date >= "2012-01-01",
#'     Date <= "2015-06-01"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3,
#'   recipes_to_run = "R1"
#' )
#'
#' R1_prepped_data_tbl <- get_prepped_data(run_info,
#'   recipe = "R1"
#' )
#' }
#' @export
get_prepped_data <- function(run_info,
                             recipe,
                             return_type = "df") {
  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("recipe", recipe, "character", c("R1", "R2"))
  check_input_type("return_type", return_type, "character", c("df", "sdf"))

  # get input values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  combo_variables <- strsplit(log_df$combo_variables, split = "---")[[1]]

  # get prepped data
  data_path <- paste0(
    "/prep_data/*", hash_data(run_info$experiment_name), "-",
    hash_data(run_info$run_name), "*", recipe, ".", run_info$data_output
  )

  prep_data_tbl <- read_file(run_info,
    path = data_path,
    return_type = return_type
  ) %>%
    tidyr::separate(
      col = Combo,
      into = combo_variables,
      remove = FALSE
    ) %>%
    base::suppressWarnings()

  return(prep_data_tbl)
}

#' Get Prepped Model Info
#'
#' @param run_info run info using the [set_run_info()] function
#'
#' @return table with data related to model workflows, hyperparameters, and back testing
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     id == "M2",
#'     Date >= "2012-01-01",
#'     Date <= "2015-06-01"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3,
#'   recipes_to_run = "R1"
#' )
#'
#' prep_models(run_info,
#'   models_to_run = c("arima", "ets"),
#'   num_hyperparameters = 1
#' )
#'
#' prepped_models_tbl <- get_prepped_models(run_info = run_info)
#' }
#' @export
get_prepped_models <- function(run_info) {
  # check input values
  check_input_type("run_info", run_info, "list")

  # get prepped model info
  data_path <- paste0(
    "/prep_models/*", hash_data(run_info$experiment_name), "-",
    hash_data(run_info$run_name)
  )

  train_test_tbl <- read_file(run_info,
    path = paste0(data_path, "-train_test_split.", run_info$data_output)
  )

  model_hyperparameters_tbl <- read_file(run_info,
    path = paste0(data_path, "-model_hyperparameters.", run_info$object_output)
  )

  model_workflows_tbl <- read_file(run_info,
    path = paste0(data_path, "-model_workflows.", run_info$object_output)
  )

  final_tbl <- tibble::tibble(
    Type = c("Model_Workflows", "Model_Hyperparameters", "Train_Test_Splits"),
    Data = list(model_workflows_tbl, model_hyperparameters_tbl, train_test_tbl)
  )

  return(final_tbl)
}

#' Hash run info in file name
#'
#' @param x object to hash
#'
#' @return hashed value
#' @noRd
hash_data <- function(x) {
  digest::digest(object = x, algo = "xxhash64")
}

#' Write data outputs to disk
#'
#' @param x object to write to disk
#' @param combo name of time series combo
#' @param run_info run info using [set_run_info()]
#' @param output_type type of data output written to disk
#' @param folder file path where outputs are written
#' @param suffix suffix value to add on the end of the file name
#'
#' @return Object written to disk.
#' @noRd
write_data <- function(x,
                       combo,
                       run_info,
                       output_type,
                       folder = NULL,
                       suffix = NULL) {
  if (output_type == "data") {
    file_type <- run_info$data_output
  } else if (output_type == "log") {
    file_type <- "csv"
  } else {
    file_type <- run_info$object_output
  }

  if (is.null(combo)) {
    combo_hash <- NULL
  } else {
    combo_hash <- paste0("-", hash_data(combo))
  }

  # write to temp folder
  temp_path <- NULL

  if (inherits(run_info$storage_object, c("blob_container", "ms_drive")) ||
    (inherits(run_info$storage_object, "NULL") & is.null(run_info$path))) {
    fs::dir_create(tempdir(), folder)

    temp_path <- paste0(fs::path(tempdir(), folder), "\\", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), combo_hash, suffix, ".", file_type)

    write_data_type(x, temp_path, file_type)
  }

  # write to final folder output
  if (inherits(run_info$storage_object, c("blob_container", "ms_drive", "NULL")) & !is.null(run_info$path)) {
    if (is.null(run_info$storage_object)) {
      fs::dir_create(run_info$path, folder)
    }

    final_path <- paste0(fs::path(run_info$path, folder), "/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), combo_hash, suffix, ".", file_type)

    write_data_folder(
      x,
      run_info$storage_object,
      class(run_info$storage_object)[[1]],
      temp_path,
      final_path,
      file_type
    )
  }
}

#' Write object in specific file format
#'
#' @param x object to write to disk
#' @param folder file path where outputs are written
#' @param type type of data output written to disk
#'
#' @return Object written to disk.
#' @noRd
write_data_type <- function(x,
                            path,
                            type) {
  if (type == "csv") {
    if (nrow(x) == 1) {
      type <- "log"
    }
  }

  switch(type,
    rds = saveRDS(x, path),
    parquet = arrow::write_parquet(x, path),
    csv = vroom::vroom_write(x, path, delim = ",", progress = FALSE),
    log = utils::write.csv(x, path, row.names = FALSE),
    qs = qs::qsave(x, path)
  )
}

#' Write object final storage destination
#'
#' @param x object to write to disk
#' @param storage_object storage object used to write to azure
#' @param final_dest final file destination
#' @param temp_path temporary file destination
#' @param final_path final folder path
#' @param type type of data output written to disk
#'
#' @return Object written to disk.
#' @noRd
write_data_folder <- function(x,
                              storage_object,
                              final_dest,
                              temp_path,
                              final_path,
                              type) {
  options(azure_storage_progress_bar = FALSE)

  switch(final_dest,
    "NULL" = write_data_type(x, final_path, type),
    blob_container = AzureStor::storage_upload(storage_object, src = temp_path, dest = final_path),
    ms_drive = storage_object$upload_file(src = temp_path, dest = final_path)
  )
}

#' List files to either download up upload
#'
#' @param storage_object storage object used to write to azure
#' @param path file destination
#'
#' @return List of file names with path
#' @noRd
list_files <- function(storage_object,
                       path) {
  if (fs::path_dir(path) %in% c("/prep_data", "/prep_models", "/models", "/logs", "/forecasts")) {
    fs::dir_create(tempdir(), fs::path_dir(path))
    dir <- fs::path_dir(paste0(tempdir(), path))
    file <- fs::path_file(path)
  } else {
    dir <- fs::path_dir(path)
    file <- fs::path_file(path)
  }

  files <- switch(class(storage_object)[[1]],
    "NULL" = if (grepl("*", file, fixed = TRUE)) {
      fs::dir_ls(path = dir, glob = file)
    } else {
      path
    },
    ms_drive = storage_object$list_files(dir) %>% dplyr::filter(grepl(utils::glob2rx(file), name)) %>% dplyr::pull(name),
    blob_container = tryCatch(
      AzureStor::list_storage_files(storage_object, dir) %>% dplyr::filter(grepl(utils::glob2rx(path), name)) %>% dplyr::pull(name),
      error = function(e) {
        NULL
      }
    )
  )

  return(files)
}

#' Download file
#'
#' @param storage_object Object for writing data to Azure
#' @param path file path
#' @param folder specific folder finnts writes outputs to
#'
#' @return file is downloaded
#' @noRd
download_file <- function(storage_object,
                          path,
                          folder) {
  options(azure_storage_progress_bar = FALSE)

  fs::dir_create(tempdir(), folder)

  dest_path <- fs::path(tempdir(), folder)

  if (inherits(storage_object, c("ms_drive", "NULL"))) {
    files <- list_files(storage_object, path)
  }

  switch(class(storage_object)[[1]],
    blob_container = if (grepl("*", fs::path_file(path), fixed = TRUE)) {
      AzureStor::storage_multidownload(storage_object, src = path, dest = dest_path, overwrite = TRUE)
    } else {
      AzureStor::storage_download(storage_object, src = path, dest = paste0(dest_path, "/", fs::path_file(path)), overwrite = TRUE)
    },
    ms_drive = for (file in files) {
      storage_object$download_file(src = fs::path(fs::path_dir(path), file), dest = paste0(dest_path, "/", fs::path_file(file)), overwrite = TRUE)
    }
  )
}

#' Write object final storage destination
#'
#' @param run_info run info using the [set_run_info()] function
#' @param path file path
#' @param file_list files
#' @param return_type type of data output read
#' @param schema column schema for arrow::open_dataset()
#'
#' @return file read into memory
#' @noRd
read_file <- function(run_info,
                      path = NULL,
                      file_list = NULL,
                      return_type = "df",
                      schema = NULL) {
  storage_object <- run_info$storage_object

  if (!is.null(path)) {
    folder <- fs::path_dir(path)
    initial_path <- run_info$path
    file <- fs::path_file(path)
  }

  if (!is.null(file_list)) {
    files <- file_list
  } else if (inherits(storage_object, c("blob_container", "ms_drive"))) {
    download_file(storage_object, fs::path(initial_path, path), folder)
    final_path <- fs::path(tempdir(), folder)
    files <- list_files(NULL, fs::path(final_path, file))
  } else if (is.null(run_info$path)) {
    final_path <- fs::path(tempdir(), folder)
    files <- list_files(storage_object, fs::path(final_path, file))
  } else {
    files <- list_files(storage_object, fs::path(initial_path, path))
  }

  if (!is.null(file_list)) {
    file_temp <- files[[1]]
    file_ext <- fs::path_ext(file_temp)
  } else if (fs::path_ext(file) == "*") {
    file_temp <- files[[1]]
    file_ext <- fs::path_ext(file_temp)
  } else {
    file_ext <- fs::path_ext(file)
  }

  if (return_type == "df") {
    switch(file_ext,
      rds = files %>% purrr::map_dfr(readRDS),
      parquet = files %>% purrr::map_dfr(function(path) {
        arrow::read_parquet(path)
      }),
      csv = tryCatch(
        vroom::vroom(files, show_col_types = FALSE, altrep = FALSE, delim = ","),
        error = function(e) {
          files %>%
            purrr::map(function(path) {
              read.csv(path, stringsAsFactors = FALSE)
            }) %>%
            plyr::rbind.fill() %>%
            tibble::tibble()
        }
      ),
      qs = qs::qread(path)
    )
  } else if (return_type == "sdf") {
    switch(file_ext,
      parquet = tryCatch(
        sparklyr::spark_read_parquet(sc, path = fs::path(initial_path, path) %>% stringr::str_replace("/dbfs", "")),
        error = function(e) {
          sparklyr::spark_read_parquet(sc, path = fs::path(initial_path, path) %>% stringr::str_replace("/synfs", "synfs:"))
        }
      ),
      csv = tryCatch(
        sparklyr::spark_read_csv(sc, path = fs::path(initial_path, path) %>% stringr::str_replace("/dbfs", "")),
        error = function(e) {
          sparklyr::spark_read_csv(sc, path = fs::path(initial_path, path) %>% stringr::str_replace("/synfs", "synfs:"))
        }
      )
    )
  } else if (return_type == "arrow") {
    switch(file_ext,
      parquet = arrow::open_dataset(
        sources = files,
        format = "parquet",
        schema = schema,
        skip = ifelse(is.null(schema), 0, 1)
      ),
      csv = arrow::open_dataset(
        sources = files,
        format = "csv",
        schema = schema,
        skip = ifelse(is.null(schema), 0, 1)
      )
    )
  } else if (return_type == "object") {
    readRDS(files)
  }
}

#' Load recipe data into memory
#'
#' @param run_info run info using the [set_run_info()] function
#' @param combo how much recipe data to ready into memory
#'
#' @return recipe data as data frame
#' @noRd
get_recipe_data <- function(run_info,
                            combo = "single") {
  get_combo <- function(df,
                        combo) {
    if (combo == "single") {
      df %>%
        dplyr::filter(Combo == .$Combo[[1]])
    } else if (combo == "All-Data") {
      df
    } else {
      df %>%
        dplyr::filter(Combo == combo)
    }
  }

  recipe_tbl <- tibble::tibble()

  file_name_tbl <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/prep_data/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*R*.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .,
      File = fs::path_file(.)
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Recipe"), sep = "-", remove = TRUE) %>%
    get_combo(combo) %>%
    dplyr::mutate(Recipe = substr(Recipe, 1, 2))

  for (recipe in unique(file_name_tbl$Recipe)) {
    temp_path <- file_name_tbl %>%
      dplyr::filter(Recipe == recipe)

    if (nrow(temp_path) > 1) {
      temp_path <- paste0(
        "/prep_data/*", hash_data(run_info$experiment_name), "-",
        hash_data(run_info$run_name), "*", recipe, ".", run_info$data_output
      )
    } else {
      temp_path <- temp_path %>%
        dplyr::pull(Path)

      temp_path <- switch(class(run_info$storage_object)[[1]],
        "NULL" = gsub(fs::path(run_info$path), "", temp_path),
        blob_container = gsub(fs::path(run_info$path), "", temp_path),
        ms_drive = fs::path("/prep_data/", temp_path)
      )
    }

    temp_file_tbl <- read_file(run_info,
      path = temp_path,
      return_type = "df"
    )

    temp_final_tbl <- tibble::tibble(
      Recipe = recipe,
      Data = list(temp_file_tbl)
    )

    recipe_tbl <- rbind(recipe_tbl, temp_final_tbl)
  }

  return(recipe_tbl)
}

#' Condense forecast output files into less files
#'
#' @param run_info run info using the [set_run_info()] function
#' @param parallel_processing type of parallel processing to run
#' @param num_cores number of cores to use
#'
#' @return nothing
#' @noRd
condense_data <- function(run_info,
                          parallel_processing = NULL,
                          num_cores = NULL) {
  # get initial list of files to condense
  initial_file_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*_models.", run_info$data_output
    )
  )

  # Initialize an empty list to store the batches
  list_of_batches <- list()

  # Define the batch size
  batch_size <- 10000

  # Calculate the number of batches needed
  num_batches <- ceiling(length(initial_file_list) / batch_size)

  # Loop through the large list and create batches
  for (i in 1:num_batches) {
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, length(initial_file_list))
    batch_name <- paste0("batch_", i)
    list_of_batches[[batch_name]] <- initial_file_list[start_index:end_index]
  }

  # parallel run info
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = parallel_processing,
    num_cores = min(length(names(list_of_batches)), num_cores),
    task_length = length(names(list_of_batches))
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # submit tasks
  condense_data_tbl <- foreach::foreach(
    batch = names(list_of_batches),
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %op% {
    files <- list_of_batches[[batch]]

    data <- read_file(run_info,
      file_list = files,
      return_type = "df"
    )

    write_data(
      x = data,
      combo = batch,
      run_info = run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-condensed"
    )

    return(batch)
  }
}
