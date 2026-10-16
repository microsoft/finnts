for (mode in c("local", "global", "mixed")) {
  test_that(paste("grouped hierarchy chains preserve", mode, "routes across membership changes"), {
    check_update_node_chain("grouped_hierarchy", mode)
  })
}
