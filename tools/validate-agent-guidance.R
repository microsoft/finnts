fail <- function(...) {
  stop(..., call. = FALSE)
}

read_lines <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

frontmatter <- function(path) {
  lines <- read_lines(path)
  if (length(lines) < 3 || lines[[1]] != "---") {
    fail(path, " must start with YAML frontmatter.")
  }

  closing <- which(lines[-1] == "---")
  if (length(closing) == 0) {
    fail(path, " has unclosed YAML frontmatter.")
  }

  lines[2:closing[[1]]]
}

frontmatter_value <- function(block, field, path) {
  matches <- grep(paste0("^", field, "\\s*:"), block, value = TRUE)
  if (length(matches) != 1) {
    fail(path, " must define exactly one '", field, "' field.")
  }

  value <- trimws(sub("^[^:]+:\\s*", "", matches[[1]]))
  sub("^(['\"])(.*)\\1$", "\\2", value)
}

frontmatter_paths <- function(block, path) {
  paths_index <- which(trimws(block) == "paths:")
  if (length(paths_index) != 1) {
    fail(path, " must define exactly one paths list.")
  }

  candidates <- block[seq.int(paths_index + 1, length(block))]
  path_lines <- candidates[grepl("^\\s*-\\s+", candidates)]
  if (length(path_lines) == 0) {
    fail(path, " must contain at least one path pattern.")
  }

  values <- sub("^\\s*-\\s+", "", path_lines)
  sub("^(['\"])(.*)\\1$", "\\2", trimws(values))
}

expand_braces <- function(pattern) {
  location <- regexpr("\\{[^{}]+\\}", pattern, perl = TRUE)
  if (location[[1]] == -1) {
    return(pattern)
  }

  token <- regmatches(pattern, location)
  prefix <- substr(pattern, 1, location[[1]] - 1)
  suffix_start <- location[[1]] + attr(location, "match.length")
  suffix <- substr(pattern, suffix_start, nchar(pattern))
  choices <- strsplit(substr(token, 2, nchar(token) - 1), ",", fixed = TRUE)[[1]]

  unlist(
    lapply(choices, function(choice) {
      expand_braces(paste0(prefix, choice, suffix))
    }),
    use.names = FALSE
  )
}

glob_to_regex <- function(pattern) {
  characters <- strsplit(pattern, "", fixed = TRUE)[[1]]
  result <- "^"
  index <- 1

  while (index <= length(characters)) {
    character <- characters[[index]]
    next_character <- if (index < length(characters)) characters[[index + 1]] else ""
    after_next <- if (index + 1 < length(characters)) characters[[index + 2]] else ""

    if (character == "*" && next_character == "*" && after_next == "/") {
      result <- paste0(result, "(?:.*/)?")
      index <- index + 3
    } else if (character == "*" && next_character == "*") {
      result <- paste0(result, ".*")
      index <- index + 2
    } else if (character == "*") {
      result <- paste0(result, "[^/]*")
      index <- index + 1
    } else if (character == "?") {
      result <- paste0(result, "[^/]")
      index <- index + 1
    } else {
      if (character %in% c("\\", ".", "^", "$", "+", "(", ")", "[", "]", "{", "}", "|")) {
        character <- paste0("\\", character)
      }
      result <- paste0(result, character)
      index <- index + 1
    }
  }

  paste0(result, "$")
}

collect_files <- function(path = ".") {
  entries <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  entries <- entries[!basename(entries) %in% c(".git", ".Rproj.user")]
  if (length(entries) == 0) {
    return(character())
  }

  directory <- file.info(entries)$isdir
  files <- entries[!directory]
  children <- unlist(lapply(entries[directory], collect_files), use.names = FALSE)
  c(files, children)
}

workspace_files <- gsub("\\\\", "/", collect_files())
workspace_files <- sub("^\\./", "", workspace_files)

matches_workspace_file <- function(pattern) {
  expanded <- expand_braces(pattern)
  any(vapply(expanded, function(candidate) {
    any(grepl(glob_to_regex(candidate), workspace_files, perl = TRUE))
  }, logical(1)))
}

required_files <- c(
  "AGENTS.md",
  "CLAUDE.md",
  "tools/generate-agent-adapters.R",
  "tools/run-r.ps1",
  ".github/agent-guides/architecture-map.md",
  ".github/agent-guides/evaluation-cases.md",
  ".github/agent-guides/validation-matrix.md",
  ".claude/skills/finnts-validation/SKILL.md"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  fail("Missing required agent-guidance files: ", paste(missing_files, collapse = ", "))
}

agents_lines <- read_lines("AGENTS.md")
if (length(agents_lines) > 120) {
  fail("AGENTS.md must stay at or below 120 lines; found ", length(agents_lines), ".")
}

claude_text <- paste(read_lines("CLAUDE.md"), collapse = "\n")
imports <- gregexpr("@AGENTS\\.md", claude_text, perl = TRUE)[[1]]
import_count <- if (imports[[1]] == -1) 0 else length(imports)
if (import_count != 1) {
  fail("CLAUDE.md must import AGENTS.md exactly once; found ", import_count, ".")
}
if (length(read_lines("CLAUDE.md")) > 20) {
  fail("CLAUDE.md must remain a concise compatibility layer of at most 20 lines.")
}

if (!any(grepl("^## Code Review Rules$", agents_lines))) {
  fail("AGENTS.md must define repository-wide Codex code review rules.")
}

rule_files <- list.files(".claude/rules", pattern = "\\.md$", full.names = TRUE)
if (length(rule_files) == 0) {
  fail("No scoped rules found under .claude/rules.")
}

agents_text <- paste(agents_lines, collapse = "\n")
for (rule_file in rule_files) {
  relative_rule <- gsub("\\\\", "/", rule_file)
  if (!grepl(relative_rule, agents_text, fixed = TRUE)) {
    fail("AGENTS.md does not map scoped rule ", relative_rule, ".")
  }

  block <- frontmatter(rule_file)
  patterns <- frontmatter_paths(block, rule_file)
  unmatched <- patterns[!vapply(patterns, matches_workspace_file, logical(1))]
  if (length(unmatched) > 0) {
    fail(rule_file, " contains path patterns that match no files: ", paste(unmatched, collapse = ", "))
  }
}

critical_terms <- list(
  "testing.md" = c("skip_on_cran", "FINNTS_TEST_TIME_LIMIT_SECONDS", "PSOCK"),
  "agent-runtime.md" = c("execute_node", "finalize_run", "hash_data", "new-configuration budget"),
  "multistep.md" = c("prune_method", "horizon-to-submodel routing", "daily multistep matrix"),
  "optional-dependencies.md" = c("getImpRfZ", "Suggests", "without any feature-selection package"),
  "r-package.md" = c("devtools::document", "Never hand-edit", "dependency-changing pull request")
)
for (rule_name in names(critical_terms)) {
  rule_path <- file.path(".claude/rules", rule_name)
  text <- paste(read_lines(rule_path), collapse = "\n")
  terms <- critical_terms[[rule_name]]
  missing_terms <- terms[!vapply(terms, grepl, logical(1), x = text, fixed = TRUE)]
  if (length(missing_terms) > 0) {
    fail(rule_path, " is missing critical invariants: ", paste(missing_terms, collapse = ", "))
  }
}

skill_path <- ".claude/skills/finnts-validation/SKILL.md"
skill_block <- frontmatter(skill_path)
skill_name <- frontmatter_value(skill_block, "name", skill_path)
skill_description <- frontmatter_value(skill_block, "description", skill_path)
if (skill_name != basename(dirname(skill_path))) {
  fail("Skill name must match its folder: ", basename(dirname(skill_path)), ".")
}
if (!grepl("Use when", skill_description, fixed = TRUE) || nchar(skill_description) > 1024) {
  fail("Skill description must contain 'Use when' and remain at or below 1024 characters.")
}

ignore_lines <- trimws(read_lines(".gitignore"))
required_ignores <- c("CLAUDE.local.md", ".claude/settings.local.json")
missing_ignores <- required_ignores[!required_ignores %in% ignore_lines]
if (length(missing_ignores) > 0) {
  fail(".gitignore is missing local Claude paths: ", paste(missing_ignores, collapse = ", "))
}

forbidden_ignores <- c(
  ".github/copilot-instructions.md",
  ".github/instructions/",
  ".cursor/",
  ".agents/"
)
present_forbidden_ignores <- forbidden_ignores[forbidden_ignores %in% ignore_lines]
if (length(present_forbidden_ignores) > 0) {
  fail(
    ".gitignore excludes required agent adapters: ",
    paste(present_forbidden_ignores, collapse = ", ")
  )
}

adapter_environment <- new.env(parent = baseenv())
sys.source("tools/generate-agent-adapters.R", envir = adapter_environment)
adapter_environment$check_agent_adapters()

message(
  "Agent guidance validated: ",
  length(rule_files), " scoped rules, ",
  length(workspace_files), " workspace files, and skill '", skill_name, "'."
)