# Data requirements

Use this page when a user asks "what does my data need to look like?", before their first
project, or when `profile_data.R` or `validate_config.R` reports a problem. Offer the
template `templates/data_template.csv` as a working example they can open in Excel.

## The shape Finn needs

One row per series per period ("long" format):

| Date | Region | Product | Revenue | Price |
|---|---|---|---|---|
| 2024-11-01 | East | Laptops | 118250 | 1007.33 |
| 2024-12-01 | East | Laptops | 121400 | 1008.16 |
| 2025-01-01 | East | Laptops | | 1009.00 |

- **Date column**: one date per row. Use the first day of the period for month, quarter,
  and year data (`2024-12-01`, not `Dec-24`). Weekly data uses the same weekday each week.
- **Series columns** (`combo_variables`): one or more text columns that name a series,
  such as region, product, or account. Each unique combination is one series.
- **Target column**: the numeric value to forecast, such as revenue or units.
- **Driver columns** (optional): numbers that help explain the target, such as price or
  headcount. A driver is only useful if the user also has its values for the future
  periods being forecast.
- **Future rows** (only with drivers): rows after the last actual date, with the target
  left empty and driver values filled in.

Wide layouts (one column per month), title rows, and subtotal rows are fine to start with:
`profile_data.R` detects them and `prepare_data.R` reshapes them after the user agrees.
The original file is never changed.

## How much history

- At least **2 times the forecast horizon** per series; fewer points produces a warning.
- Ideally 2-3 years of monthly data, so Finn can see the yearly pattern and still keep
  periods aside for back testing.
- Short or new series still run, but their accuracy is less reliable. Say so plainly.

Typical and maximum horizons by date type (beyond the maximum, Finn warns but still runs):

| date type | typical | maximum |
|---|---|---|
| year | 3 | 5 |
| quarter | 8 | 12 |
| month | 12 | 24 |
| week | 13 | 104 |
| day | 90 | 365 |

## Problems that stop a run

`validate_config.R` returns these as errors; fix them with the user before running:

- Two rows for the same series and date (duplicates). Ask whether to sum them or which one
  to keep; `prepare_data.R` can aggregate.
- A configured column that is not in the file (often a renamed header).
- The date column listed as a series column.
- A date type other than `day`, `week`, `month`, `quarter`, or `year`.
- A forecast horizon of 0 or less, or a fiscal year start outside months 1-12.

## Warnings that never block a run

Explain these and let the user decide:

- Series with fewer than 2 times the horizon of history.
- Negative values while negative forecasts are turned off.
- Series that are all zeros.
- Horizon above the maximum in the table above.
- Fiscal year start not set for monthly, quarterly, or yearly data.
- 100 or more series stored on OneDrive (sync can slow runs; see storage.md).
- Driver columns with no future values.

## Excel and regional files

- Excel files work directly; pass `--sheet=<name>` when the data is not on the first sheet.
- Dates and decimals are detected automatically. When they are ambiguous (for example
  `03/04/2024`, or `1.234,56`), confirm with the user and set `date_format` or
  `decimal_mark` (data-and-settings.md "International data").
- Remove totals and subtotals unless the user wants Finn to forecast them as series;
  for totals that must add up, use a hierarchy (data-and-settings.md).

## Starting from the template

Copy `templates/data_template.csv` to the user's folder (never edit the skill copy), then
help them paste their own columns over it. The template has 4 series (2 regions x 2
products), monthly history from 2023-01 to 2024-12, and 3 future months with prices only.
