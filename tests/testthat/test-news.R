# Count whitespace-delimited words in NEWS' plain Markdown bullets, returning
# their starting line numbers and counts. Wrapped text belongs to its containing
# bullet; nested bullets are counted independently. Headings and unindented
# paragraphs after blank lines end a list. Inline code and hyphenated terms use
# the same whitespace rule. This helper performs no file I/O.
news_bullet_word_counts <- function(lines) {
  counts <- data.frame(line = integer(), words = integer())
  indentation <- integer()
  parents <- integer()
  paragraph_break <- TRUE

  for (line_number in seq_along(lines)) {
    line <- gsub("\t", "    ", lines[[line_number]], fixed = TRUE)
    text <- trimws(line)
    if (!nzchar(text)) {
      paragraph_break <- TRUE
      next
    }
    if (grepl("^#{1,6}([[:space:]]|$)", text)) {
      parents <- integer()
      next
    }

    indent <- nchar(line) - nchar(sub("^ *", "", line))
    if (grepl("^[-+*]([[:space:]]|$)", text)) {
      bullet <- nrow(counts) + 1L
      counts[bullet, ] <- list(line_number, 0L)
      indentation[[bullet]] <- indent
      parents <- c(parents[indentation[parents] < indent], bullet)
      text <- sub("^[-+*][[:space:]]*", "", text)
    } else {
      containing <- parents[indentation[parents] < indent]
      if (length(containing)) {
        parents <- containing
      } else if (paragraph_break) {
        parents <- integer()
      }
      if (!length(parents)) next
    }

    bullet <- tail(parents, 1L)
    words <- strsplit(text, "[[:space:]]+")[[1]]
    counts$words[[bullet]] <- counts$words[[bullet]] + sum(nzchar(words))
    paragraph_break <- FALSE
  }
  counts
}

test_that("NEWS word counts distinguish twenty words from twenty-one", {
  twenty <- paste(rep("word", 20), collapse = " ")
  counts <- news_bullet_word_counts(c(
    "-", paste("-", twenty), paste("-", twenty, "extra"),
    "- hyphenated-term\t`function()`"
  ))

  expect_equal(counts$words, c(0, 20, 21, 2))
  expect_identical(counts$words <= 20, c(TRUE, TRUE, FALSE, TRUE))
})

test_that("NEWS word counts join wrapped text without counting child bullets twice", {
  counts <- news_bullet_word_counts(c(
    "# News", "- parent words", "  wrapped words", "  * child words",
    "    wrapped words", "  parent continuation", "- sibling", "",
    "Paragraph outside the list.", "## Next", "+ final bullet"
  ))

  expect_equal(counts$line, c(2, 4, 7, 11))
  expect_equal(counts$words, c(6, 4, 1, 2))
  expect_equal(news_bullet_word_counts(c("- wrapped", "without indentation"))$words, 3)
  expect_equal(nrow(news_bullet_word_counts(c("# News", "", "Only prose."))), 0)
  expect_equal(nrow(news_bullet_word_counts(character())), 0)
})

test_that("every NEWS bullet contains at most twenty words", {
  news_path <- testthat::test_path("..", "..", "NEWS.md")
  if (!file.exists(news_path)) {
    news_path <- system.file("NEWS.md", package = "finnts")
  }
  expect_true(file.exists(news_path), info = "Cannot locate the package NEWS.md file.")
  if (file.exists(news_path)) {
    counts <- news_bullet_word_counts(readLines(news_path, warn = FALSE, encoding = "UTF-8"))
    expect_gt(nrow(counts), 0)
    overlong <- counts[counts$words > 20, , drop = FALSE]
    expect_equal(nrow(overlong), 0, info = paste(
      c("NEWS.md bullets must contain at most 20 words:",
        sprintf("NEWS.md:%d: %d words", overlong$line, overlong$words)),
      collapse = "\n"
    ))
  }
})