# Convert Date values to consecutive integer months. Invalid dates are rejected
# by the caller; the key does not depend on month length or fiscal-year labels.
acr_scott_month <- function(dates) {
  as.integer(format(dates, "%Y")) * 12L + as.integer(format(dates, "%m"))
}

# Return signed median-centered quality scores for numeric growth observations.
# Zero dispersion gives zero at the median, signed infinity elsewhere, and NA
# for missing observations; it never fabricates a reliable missing observation.
acr_scott_quality <- function(growth, center, spread) {
  if (spread == 0) {
    return(ifelse(is.na(growth), NA_real_,
                  ifelse(growth == center, 0, sign(growth - center) * Inf)))
  }
  (growth - center) / spread
}

# Fit a twelve-month peer reference trend or retain a finite training fallback.
# The small lm formula uses baseenv so serialization cannot capture panel data.
# Returns model, fallback and reason; neither plots nor changes global state.
acr_scott_reference_fit <- function(values, fallback) {
  model <- NULL
  if (length(values) == 12L && all(is.finite(values))) {
    reference_data <- data.frame(reference = values, month_index = seq_len(12L))
    model <- stats::lm(
      stats::as.formula("reference ~ log(month_index)", env = baseenv()),
      data = reference_data
    )
  }
  list(model = model, fallback = fallback,
       reason = if (is.null(model)) "incomplete_reference_window" else "trend")
}

# Extrapolate a fitted reference to offsets after its 12-month training window.
# Missing/nonfinite predictions, and negative dispersion predictions when
# dispersion=TRUE, use the stored training fallback, not future observations.
acr_scott_reference_predict <- function(reference, offsets, dispersion = FALSE) {
  values <- rep(reference$fallback, length(offsets))
  if (!is.null(reference$model)) {
    values <- as.numeric(stats::predict(reference$model,
      newdata = data.frame(month_index = 12L + offsets)))
  }
  invalid <- !is.finite(values)
  if (dispersion) invalid <- invalid | values < 0
  values[invalid] <- reference$fallback
  values
}

# Apply the selected original calendar override to a scalar monthly growth.
# These overrides occur after branch caps and may move outside those caps.
acr_scott_calendar <- function(growth, month, policy) {
  if (policy == "none") return(growth)
  if (month == 7L && growth < 0) return(growth / 0.85)
  if (month == 12L) {
    if (policy == "scott_main" && growth > 0) return(growth * 0.85)
    if (policy == "scott_pricing" && growth < 0) return(growth / 0.85)
  }
  growth
}

# Return source coefficients for an active high-growth branch. These are not
# normalized: Scott's amplified and non-unit-sum branches retain their values.
acr_scott_rule_weights <- function(rule) {
  switch(rule,
    high_growth_1 = c(recent = 0.6, reference = 0.4),
    high_growth_2 = c(recent = 0.6, reference = 0.4),
    high_growth_3 = c(recent = 0.4, latest = 0.2, reference = 0.4),
    high_growth_4 = , high_growth_9 = c(oldest = 0.3, middle = 0.55, latest = 0.15),
    high_growth_5 = , high_growth_10 = c(middle = 0.8, latest = 0.2),
    high_growth_6 = , high_growth_11 = c(latest = 0.3, reference = 0.7),
    high_growth_7_1 = c(middle = 0.8, reference = 0.2),
    high_growth_7_2 = , high_growth_12 = c(middle = 0.8, latest = 0.1, reference = 0.1),
    high_growth_8 = , high_growth_13 = c(latest = 0.15, reference = 0.85),
    high_growth_14 = c(recent = 0.6, reference = 0.4),
    high_growth_15 = c(recent = 0.65, reference = 0.455, latest = 0.1, middle = 0.05),
    high_growth_15_1 = c(latest = 0.9, previous_reference = 0.1),
    high_growth_15_2 = c(latest = 0.75, previous_reference = 0.25),
    high_growth_16 = c(middle = 0.3, latest = 0.7),
    high_growth_17 = c(recent = 0.1, latest = 0.85, reference = 0.05),
    high_growth_18 = c(recent = 0.1, latest = 0.5, middle = 0.3, reference = 0.05),
    high_growth_19 = c(recent = 0.1, latest = 0.6, oldest = 0.2, reference = 0.05),
    high_growth_20 = c(recent = 0.2, middle = 0.65, reference = 0.15),
    high_growth_21 = c(recent = 0.25, oldest = 0.55, reference = 0.2),
    high_growth_22 = c(recent = 0.5, reference = 0.5),
    numeric())
}

# Evaluate one original active forecast branch using oldest-to-newest analogue
# vectors (three, two and one years ago). quality retains signed scores; ranking
# uses magnitudes. Returns pre/post-cap growth, rule, analogue and applied weights.
# Missing analogues are excluded. Unusable arithmetic remains an error for the
# prediction boundary to explain, rather than being silently replaced with zero.
acr_scott_growth <- function(profile, mom, quality, bases, last_rate,
                             median_month, sd_month, median_all, sd_all,
                             recent, previous_median) {
  available <- is.finite(mom) & !is.na(quality)
  magnitudes <- abs(quality)
  eligible <- which(available)
  analogue <- if (length(eligible)) eligible[which.min(magnitudes[eligible])] else NA_integer_
  best_quality <- if (is.na(analogue)) Inf else magnitudes[analogue]
  best_mom <- if (is.na(analogue)) NA_real_ else mom[analogue]
  latest <- mom[3]
  middle <- mom[2]
  oldest <- mom[1]
  # Scalar predicates return FALSE for unavailable comparisons, so missing
  # history cannot accidentally satisfy an early high-confidence branch.
  below <- function(index, threshold) {
    isTRUE(available[index] && magnitudes[index] < threshold)
  }
  above <- function(index, threshold) {
    isTRUE(available[index] && magnitudes[index] > threshold)
  }
  # Damping retains the source's positive factor and 0.1/0.9 bounds. Missing
  # historical bases are not imputed and cannot produce a valid weighted term.
  damp <- function(index, factor) {
    max(min(factor * bases[index] / last_rate, 0.9), 0.1)
  }
  # Package one branch with optional source dispersion cap and diagnostics.
  emit <- function(rule, value, spread = NULL, weights = numeric(),
                   damping = numeric()) {
    if (!is.finite(value) && profile == "standard" && is.finite(median_all)) {
      return(emit("standard_11", median_all, weights = c(historical_median = 1)))
    }
    if (!length(weights)) weights <- acr_scott_rule_weights(rule)
    if (!length(damping)) {
      factors <- rep(if (profile == "standard") 1.06 else 0.98, 3)
      if (rule %in% c("standard_5_1", "standard_5_4")) factors[1] <- 0.96
      damping <- vapply(seq_len(3), function(index) {
        if (is.finite(bases[index])) damp(index, factors[index]) else NA_real_
      }, numeric(1))
      names(damping) <- c("oldest", "middle", "latest")
    }
    bounded <- if (is.null(spread)) value else {
      max(min(value, median_month + 2 * abs(spread)),
          median_month - 2 * abs(spread))
    }
    list(rule = rule, growth = bounded, unclamped = value,
         analogue = analogue, weights = weights, damping = damping,
         cap_lower = if (is.null(spread)) NA_real_ else median_month - 2 * abs(spread),
         cap_upper = if (is.null(spread)) NA_real_ else median_month + 2 * abs(spread))
  }
  # Weighted seasonal terms ignore zero-weight missing years, retaining the
  # original per-year denominator factors and common reference contribution.
  blend <- function(weights, factors, median_weight = 0) {
    used <- which(weights != 0)
    sum(mom[used] * weights[used] * vapply(used, function(index) {
      damp(index, factors[index])
    }, numeric(1))) + median_weight * median_month
  }
  if (profile == "standard") {
    latest_damping <- if (is.finite(bases[3])) damp(3, 1.06) else NA_real_
    if (above(3, 2)) {
      if (best_quality > 2) {
        if (isTRUE(latest < 0)) {
          return(emit("standard_1_positive_quality", median_month * latest_damping * 0.65 +
                        0.35 * (median_month + abs(sd_month)), sd_month,
                      c(reference = 0.65, shifted_reference = 0.35), latest_damping))
        }
        return(emit("standard_1", median_month * latest_damping, sd_month,
                    c(reference = 1), latest_damping))
      }
      return(emit("standard_2", (best_mom * 0.6 + median_month * 0.4) *
                    latest_damping, sd_month, c(analogue = 0.6, reference = 0.4), latest_damping))
    }
    if (!is.na(analogue) && best_quality > 1.5) {
      return(emit("standard_3", (best_mom * 0.6 + median_month * 0.4) *
                    latest_damping, sd_month, c(analogue = 0.6, reference = 0.4), latest_damping))
    }
    if (!is.na(analogue) && best_quality > 1) {
      return(emit("standard_4", (best_mom * 0.7 + median_month * 0.3) *
                    latest_damping, sd_month, c(analogue = 0.7, reference = 0.3), latest_damping))
    }
    if (below(1, 2) && below(2, 2) && isTRUE(latest < 0 && middle > 0 && oldest > 0)) {
      return(emit("standard_5_1", blend(c(0.2, 0.45, 0.25), c(0.96, 1.06, 1.06), 0.1),
                  sd_all, c(oldest = 0.2, middle = 0.45, latest = 0.25, reference = 0.1)))
    }
    if (all(vapply(1:3, below, logical(1), threshold = 0.8))) {
      return(emit("standard_5_4", blend(c(0.15, 0.3, 0.55), c(0.96, 1.06, 1.06)),
                  sd_all, c(oldest = 0.15, middle = 0.3, latest = 0.55)))
    }
    if (below(3, 1.5) && below(1, 1.5) && isTRUE(latest > 0 && oldest < 0)) {
      return(emit("standard_5_5", blend(c(0.25, 0, 0.7), rep(1.06, 3), 0.05),
                  sd_all, c(oldest = 0.25, latest = 0.7, reference = 0.05)))
    }
    if (!is.na(analogue) && analogue < 3) {
      selected_damping <- damp(analogue, 1.06)
      return(emit(paste0("standard_5_year_", 4 - analogue),
                  (best_mom * 0.95 + median_month * 0.05) * selected_damping,
                  sd_month, c(analogue = 0.95, reference = 0.05), selected_damping))
    }
    if (is.finite(latest) && is.finite(latest_damping)) {
      return(emit("standard_10", latest * latest_damping, sd_all,
                  c(latest = 1), latest_damping))
    }
    return(emit("standard_11", median_all, weights = c(historical_median = 1)))
  }
  if (!is.finite(latest)) {
    return(emit("high_growth_1", (recent * 0.6 + median_month * 0.4) * 0.78,
                weights = c(recent = 0.6, reference = 0.4), damping = 0.78))
  }
  latest_damping <- damp(3, 0.98)
  no_older <- !is.finite(middle) && !is.finite(oldest)
  if (above(3, 3) && no_older) {
    return(emit("high_growth_2", (recent * 0.6 + median_month * 0.4) * latest_damping))
  }
  if (above(3, 2) && no_older) {
    return(emit("high_growth_3", (recent * 0.4 + latest * 0.2 + median_month * 0.4) * latest_damping))
  }
  negative_latest <- isTRUE(latest < 0 && middle > 0 && oldest > 0)
  positive_latest <- isTRUE(latest > 0 && middle < 0 && oldest < 0)
  if (negative_latest || positive_latest) {
    offset <- if (negative_latest) 0 else 5
    if (below(2, 1.5) && below(1, 1.5)) {
      return(emit(paste0("high_growth_", 4 + offset),
                  blend(c(0.3, 0.55, 0.15), rep(0.98, 3)), sd_all))
    }
    if (below(2, 1.5)) {
      return(emit(paste0("high_growth_", 5 + offset),
                  blend(c(0, 0.8, 0.2), rep(0.98, 3)), sd_all))
    }
    return(emit(paste0("high_growth_", 6 + offset),
                blend(c(0, 0, 0.3), rep(0.98, 3), 0.7), sd_all))
  }
  if (isTRUE(latest < 0 && middle > 0)) {
    if (below(2, 1.5) && above(3, 1.5)) {
      return(emit("high_growth_7_1", blend(c(0, 0.8, 0), rep(0.98, 3), 0.2), sd_all))
    }
    if (below(2, 2) && below(3, 2)) {
      return(emit("high_growth_7_2", blend(c(0, 0.8, 0.1), rep(0.98, 3), 0.1), sd_all))
    }
    return(emit("high_growth_8", blend(c(0, 0, 0.15), rep(0.98, 3), 0.85), sd_all))
  }
  if (isTRUE(latest > 0 && middle < 0) && !is.finite(oldest)) {
    if (below(2, 1.5)) {
      return(emit("high_growth_12", blend(c(0, 0.8, 0.1), rep(0.98, 3), 0.1), sd_all))
    }
    return(emit("high_growth_13", blend(c(0, 0, 0.15), rep(0.98, 3), 0.85), sd_all))
  }
  if (!is.finite(oldest) && is.finite(middle)) {
    if (isTRUE(quality[2] > 3 && quality[3] > 3)) {
      return(emit("high_growth_14", (recent * 0.6 + median_month * 0.4) * latest_damping))
    }
    if (isTRUE(quality[2] > 1.5 && quality[3] > 1.5)) {
      return(emit("high_growth_15", (recent * 0.5 * 1.3 + median_month * 0.35 * 1.3 +
                                      latest * 0.1 + middle * 0.05) * latest_damping))
    }
    if (above(2, 2) && below(3, 1)) {
      return(emit("high_growth_15_1", latest * latest_damping * 0.9 + previous_median * 0.1, sd_all))
    }
    if (above(2, 2) && below(3, 2)) {
      return(emit("high_growth_15_2", latest * latest_damping * 0.75 + previous_median * 0.25, sd_all))
    }
    return(emit("high_growth_16", blend(c(0, 0.3, 0.7), rep(0.98, 3)), sd_all))
  }
  if (below(3, 1)) {
    return(emit("high_growth_17", recent * 0.1 + (latest * 0.85 + median_month * 0.05) * latest_damping))
  }
  if (below(3, 1.5) && below(2, 1.5)) {
    return(emit("high_growth_18", recent * 0.1 + (latest * 0.5 + middle * 0.3 + median_month * 0.05) * latest_damping))
  }
  if (below(3, 1.5) && below(1, 1.5)) {
    return(emit("high_growth_19", recent * 0.1 + (latest * 0.6 + oldest * 0.2 + median_month * 0.05) * latest_damping))
  }
  if (below(2, 2)) {
    return(emit("high_growth_20", recent * 0.2 + (middle * 0.65 + median_month * 0.15) * latest_damping))
  }
  if (below(1, 2)) {
    return(emit("high_growth_21", recent * 0.25 + (oldest * 0.55 + median_month * 0.2) * latest_damping))
  }
  emit("high_growth_22", recent * 0.5 + median_month * 0.5 * latest_damping)
}