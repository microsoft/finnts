test_that("updates compare complete current topology rather than counts or order", {
  base <- data.frame(Region = c("North", "North", "South", "South"), Product = c("A", "B", "A", "B"))
  changes <- list(
    first_group_member = rbind(base, data.frame(Region = "East", Product = "C")),
    new_overlapping_member = rbind(base, data.frame(Region = "North", Product = "C")),
    last_group_member = base[c(1, 3), ],
    remove_one = base[1:3, ],
    remove_first_group = base[3:4, ],
    same_count_replacement = rbind(base[1:3, ], data.frame(Region = "East", Product = "C")),
    add_and_remove = rbind(base[1:2, ], data.frame(Region = c("East", "East"), Product = c("A", "C"))),
    input_reordering = base[4:1, ])
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    for (change in names(changes)) {
      path <- withr::local_tempdir()
      legacy <- change %in% c("first_group_member", "last_group_member", "same_count_replacement")
      previous <- make_update_chain_case(path, approach = approach, members = base, legacy = legacy)
      current <- make_update_chain_case(path, paste0("update-", change), approach,
        members = changes[[change]], legacy = TRUE)
      expected_nodes <- current$hierarchy$hts_combos
      new_nodes <- setdiff(expected_nodes, previous$hierarchy$hts_combos)
      if (legacy) {
        expect_warning(step <- run_update_chain_step(previous, current), "Legacy global update")
      } else step <- expect_no_warning(run_update_chain_step(previous, current))
      surviving <- intersect(previous$hierarchy$original_combos, current$hierarchy$original_combos)
      parent <- step$agent$project_info
      parent$run_name <- step$agent$run_id
      uniform <- length(unique(previous$winners)) == 1L
      reassigned <- changed_update_hierarchy_sources(previous$hierarchy, current$hierarchy)
      if ((length(new_nodes) && !uniform) || length(reassigned)) {
        expect_setequal(step$result$quality_rejected_combos, vapply(surviving, hash_data, character(1)))
        for (combo in current$hierarchy$original_combos) {
          expect_equal(nrow(read_selection_file(parent, "logs", "-agent_best_run", combo, optional = TRUE)), 0)
        }
      } else {
        expect_identical(step$result$status, "done")
        mapping <- read_global_update_selection(current$info, read_selection_file(current$info, "logs"), surviving)
        expect_setequal(names(mapping$selected_ids), expected_nodes)
        expected_winners <- previous$winners[names(mapping$selected_ids)]
        if (uniform) expected_winners[is.na(expected_winners)] <- unique(previous$winners)
        expect_identical(unname(mapping$selected_ids), unname(expected_winners))
        delivered <- read_selection_file(current$info, "forecasts", "-reconciled", "Best-Model")
        expect_setequal(delivered$Combo, surviving)
        next_run <- make_update_chain_case(path, paste0("repeat-", change), approach,
          members = changes[[change]], legacy = TRUE)
        repeated <- expect_no_warning(run_update_chain_step(current, next_run, combos = surviving))
        expect_identical(repeated$result$status, "done")
        expect_length(repeated$result$quality_rejected_combos, 0)
        expect_setequal(read_selection_file(next_run$info, "forecasts", "-reconciled", "Best-Model")$Combo,
          surviving)
      }
      expect_length(step$fits, 1)
    }
  }
})

test_that("iteration selection and hierarchy persistence survive membership replacement", {
  base <- data.frame(Region = c("North", "North", "South", "South"), Product = c("A", "B", "A", "B"))
  changed <- rbind(base[1:3, ], data.frame(Region = "East", Product = "C"))
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    path <- withr::local_tempdir()
    initial <- make_update_chain_case(path, "iterate-1", approach, members = base, data_output = "csv")
    first <- finalize_update_chain_iteration(initial)
    expect_length(first$rejected_combos, 0)
    next_iteration <- make_update_chain_case(path, "iterate-2", approach, members = changed, data_output = "csv")
    second <- finalize_update_chain_iteration(next_iteration)
    expect_length(second$rejected_combos, 0)
    mapping <- read_global_update_selection(next_iteration$info,
      read_selection_file(next_iteration$info, "logs"), next_iteration$hierarchy$original_combos)
    expect_gt(length(unique(mapping$selected_ids)), 1)
    expect_setequal(names(mapping$selected_ids), next_iteration$hierarchy$hts_combos)
    current <- make_update_chain_case(path, "update-after-iterate", approach, members = changed,
      legacy = TRUE, data_output = "csv")
    step <- run_update_chain_step(next_iteration, current)
    expect_identical(step$result$status, "done")
    expect_identical(read_global_update_selection(current$info, read_selection_file(current$info, "logs"),
      current$hierarchy$original_combos)$selected_ids, mapping$selected_ids)
    reintroduced <- make_update_chain_case(path, "reintroduced", approach, members = base,
      legacy = TRUE, data_output = "csv")
    outcome <- run_update_chain_step(current, reintroduced)
    expect_setequal(outcome$result$quality_rejected_combos,
      vapply(intersect(current$hierarchy$original_combos, reintroduced$hierarchy$original_combos),
        hash_data, character(1)))
  }
})
