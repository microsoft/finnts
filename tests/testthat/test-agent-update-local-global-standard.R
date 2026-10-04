for (mode in c("local", "global", "mixed")) {
  test_that(paste("standard hierarchy chains preserve", mode, "routes across membership changes"), {
    check_update_node_chain("standard_hierarchy", mode)
  })
}
