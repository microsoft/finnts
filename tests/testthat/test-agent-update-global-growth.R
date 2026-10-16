test_that("uniform global updates publish growing hierarchies and remain chainable", {
  cases <- list(
    list(approach = "grouped_hierarchy", legacy = TRUE,
      models = c("xgboost", "chronos2"), recipes = c("R1", "R1")),
    list(approach = "standard_hierarchy", legacy = FALSE,
      models = "xgboost", recipes = "R2"),
    list(approach = "grouped_hierarchy", legacy = FALSE,
      models = c("xgboost", "xgboost"), recipes = c("R1", "R2"))
  )
  for (settings in cases) {
    path <- withr::local_tempdir()
    previous <- do.call(make_update_chain_case,
      c(list(path = path, uniform = TRUE), settings))
    requested <- previous$hierarchy$original_combos
    added_product <- if (settings$approach == "standard_hierarchy") "C" else "A"
    members <- rbind(previous$members, data.frame(Region = "West", Product = added_product))
    current <- make_update_chain_case(path, "grown", settings$approach,
      members = members, models = settings$models, recipes = settings$recipes, legacy = TRUE)
    new_nodes <- setdiff(current$hierarchy$hts_combos, previous$hierarchy$hts_combos)
    expect_gt(length(new_nodes), 1L)
    expect_length(changed_update_hierarchy_sources(previous$hierarchy, current$hierarchy), 0L)
    if (settings$legacy) {
      expect_warning(step <- run_update_chain_step(previous, current), "Legacy global update")
    } else step <- expect_no_warning(run_update_chain_step(previous, current))
    expect_identical(step$result$status, "done")
    expect_length(step$result$quality_rejected_combos, 0L)
    if (!identical(step$result$status, "done")) next

    expected_id <- paste(sort(previous$ids), collapse = "_")
    expected <- stats::setNames(rep(expected_id, length(current$hierarchy$hts_combos)),
      current$hierarchy$hts_combos)
    mapping <- expect_no_warning(read_global_update_selection(current$info,
      read_selection_file(current$info, "logs"), requested))
    expect_identical(mapping$selected_ids, expected)
    expect_true(all(vapply(mapping$components, setequal, logical(1), previous$ids)))
    saved <- read_update_result(current$info, requested, TRUE, 6, settings$approach,
      current$splits, paste(unique(settings$recipes), collapse = "---"),
      expected, previous$hierarchy)
    expect_type(saved, "list")
    expect_setequal(saved$forecasts$Combo, requested)
    expect_true(all(is.finite(saved$forecasts$Forecast)))
    for (combo in new_nodes) {
      rows <- read_candidate_forecasts(current$info, combo,
        read_selection_file(current$info, "logs"), reconciled = FALSE)
      expect_setequal(rows$Model_ID, unique(c(previous$ids, expected_id)))
      selected <- rows[rows$Best_Model == "Yes", ]
      expect_true(all(selected$Model_ID == expected_id))
      expect_equal(selected$Forecast,
        rep(current$contexts[[combo]]$history$Target[1], nrow(selected)))
    }
    parent <- step$agent$project_info
    parent$run_name <- step$agent$run_id
    for (combo in requested) {
      metadata <- read_selection_file(parent, "logs", "-agent_best_run", combo)
      expect_identical(metadata$best_run_name, current$info$run_name)
      expect_identical(metadata$model_type, "global")
    }
    for (combo in setdiff(current$hierarchy$original_combos, requested)) {
      expect_equal(nrow(read_selection_file(parent, "logs", "-agent_best_run", combo, optional = TRUE)), 0L)
    }
    if (settings$legacy) {
      expect_warning(resumed <- run_update_chain_step(previous, current), "Legacy global update")
    } else resumed <- expect_no_warning(run_update_chain_step(previous, current))
    expect_identical(resumed$result$status, "done")
    expect_length(resumed$fits, 0L)

    following <- make_update_chain_case(path, "grown-again", settings$approach,
      members = rbind(members, data.frame(Region = "West",
        Product = if (settings$approach == "standard_hierarchy") "D" else "B")),
      models = settings$models, recipes = settings$recipes, legacy = TRUE)
    next_step <- expect_no_warning(run_update_chain_step(current, following, combos = requested))
    expect_identical(next_step$result$status, "done")
    expect_length(next_step$result$quality_rejected_combos, 0L)
    next_mapping <- read_global_update_selection(following$info,
      read_selection_file(following$info, "logs"), requested)
    expect_setequal(names(next_mapping$selected_ids), following$hierarchy$hts_combos)
    expect_true(all(next_mapping$selected_ids == expected_id))
  }
})

test_that("new hierarchy nodes recover from a missing component using a complete global alternative", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, approach = "grouped_hierarchy", legacy = TRUE)
  current <- make_update_chain_case(path, "missing-new-component", "grouped_hierarchy",
    members = rbind(previous$members, data.frame(Region = "West", Product = "A")), legacy = TRUE)
  new_node <- setdiff(current$hierarchy$hts_combos, previous$hierarchy$hts_combos)[1]
  rows <- current$fitted$Forecast_Tbl[[1]]
  current$fitted$Forecast_Tbl[[1]] <- rows[rows$Combo != new_node, ]
  expect_warning(step <- run_update_chain_step(previous, current), "Legacy global update")
  expect_identical(step$result$status, "done")
  expect_length(step$result$quality_rejected_combos, 0L)
  rows <- read_selection_file(current$info, "forecasts", "-global_models", new_node)
  expect_true(all(rows$Model_ID[rows$Best_Model == "Yes"] == current$ids[2]))
  expect_equal(nrow(read_selection_file(current$info, "forecasts", "-average_models",
    new_node, optional = TRUE)), 0L)
})
