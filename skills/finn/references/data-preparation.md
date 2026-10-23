# Preparing messy input data

Use this page when `profile_data.R` returns `needs_reshape` with `needs_transform`
(and no `suggested_prepare_args`), when it returns `layout_problems`, or when the fixed
`prepare_data.R` flags (`--header_row`, `--pivot_longer`, `--drop_totals`) do not cover
the layout. Examples: pivots with blank row labels or `<label> Total` rows, two-row
Year/Month headers, filter areas, footnotes, several tables, numbers stored as text,
mixed date formats, fiscal periods, duplicate rows, or units that change partway down a
column.

You describe the fix as a **transform spec**: a JSON list of steps drawn from the fixed
set of operations below. The skill applies those steps, checks the result with finnts'
own input checks, and writes a prepared copy only after the user approves. Specs cannot
contain R code, file paths, or web addresses.

## The loop: diagnose, propose, approve, write, verify

1. **Diagnose.** Run `profile_data.R --project=<name>` (or `--file=<file>`) and read
   `data.diagnostics`. It reports the likely header row, title and footnote rows, pivot
   filters, total rows and columns, outline columns, date and number readability,
   duplicate keys, and unit mentions, plus a small masked sample. Column and filter field
   names are shown; data labels and values are masked. This masking covers
   `data.diagnostics` only: the rest of the result lists column names read from the first
   row (in a pivot, possibly a filter's selected item) and, for a readable table, up to
   three example values per column. Add `--reveal_labels=true` only if
   the user agrees to show real labels in this chat, and ask again for each file.
   When `data.needs_transform` is present, the automatic reshape flags would misread the
   file; use a transform spec and never the flags.
2. **Propose.** Write the spec and preview it. Do not save the spec to a file:

   PowerShell:
   ```powershell
   $OutputEncoding = [Text.UTF8Encoding]::new($false)
   @'
   { "version": 1, "description": "...", "steps": [ ... ] }
   '@ | & "<Rscript>" "<skill>/scripts/prepare_data.R" --file="<file>" --transform=- --preview=true --project=<name>
   ```
   macOS/Linux:
   ```sh
   "<Rscript>" "<skill>/scripts/prepare_data.R" --file="<file>" --transform=- --preview=true --project=<name> <<'EOF'
   { "version": 1, "description": "...", "steps": [ ... ] }
   EOF
   ```
3. **Show the preview.** Show `data.preview_markdown` exactly as returned; do not reorder,
   reformat, or paraphrase it. Its sections are always: Source, Result, Checked against
   settings, Target total, Latest period (only when it has not ended), Changes, Needs your
   extra confirmation, Dropped rows, Before, After, and Verification. The preview masks
   labels and values, including labels named in the Verification messages, and `data.checks`
   carries the same masked messages. Numbers and dates in a message stay as written.
   `data.preview_file` is a local CSV with real values for the user to open; never read it
   into the chat.
4. **Approve once.** Ask for one approval of the whole plan, naming every item under
   "Needs your extra confirmation". Nothing is written before approval. If the spec
   or effective worksheet selection changes, preview again and get approval again.
5. **Write.** Pipe the same spec again with `--approve=<data.token>` instead of
   `--preview=true`, keeping the same `--sheet` when it selected the previewed worksheet.
   Explicit `source.sheet` or `source.sheets` takes precedence over `--sheet`; without
   either, Excel uses its first worksheet. The skill applies the spec again, reruns the
   checks, and writes `<file>_finn_ready.csv` only if the token matches the spec, data
   file, effective worksheet selection, and settings.
   It never overwrites the original or an existing file.
6. **Use the copy.** Run `profile_data.R --file=<output>` against the prepared file.
   Copy the full saved configuration (or the template for a new project) to a draft
   outside the project, preserving all other settings. Set `data.file` to the absolute
   `data.output` path returned by preparation, `data.sheet` to null, `date_column` to
   `"Date"`, `date_format` to `"%Y-%m-%d"`, and `decimal_mark` to `"."`.
   Run `validate_config.R --project=<name> --config=<draft>` without saving. Show the
   user the prepared file path and the returned settings review. After approval, rerun
   with the same `--config=<draft>` and `--save=true`, then profile or forecast the
   project normally. Updating project metadata with `setup_project.R --replace_data=true`
   alone does **not** replace an existing configuration's `data.file`; do not use it
   as the configuration handoff. The settings review and the forecast run read series
   columns as text, so IDs such as `001` keep their leading zeros and blank identifiers
   fail the same input check during preparation and forecasting.

If a check fails, the response is `gate_failed`, with no token and no file written. Explain
each FAIL row in plain language, revise the spec, and preview again.

## CSV series identifiers and quick samples

Profiling suggests repeated zero-padded numeric columns, such as `001`, as series
identifiers rather than numeric drivers. When profiling a project, saved
`combo_variables` are included, even for numeric-looking IDs without leading
zeros. Confirm the suggested columns with the user; numeric value and driver columns
still retain their usual numeric types.

For a quick test subset, use `prepare_data.R --sample_series=<n>
--combo_variables=Region,Product`. The written CSV keeps exact identifiers, so `001`
and `1` remain separate series, and includes every row of each selected series.
When also reshaping, declare the source identifiers with
`--id_columns=Region,Product`; those values stay intact through header promotion and
pivoting as well as sampling. The source file is never changed.

## Spec format

```json
{
  "version": 1,
  "description": "Outline pivot: fill region labels, drop totals, one row per month",
  "source": { "sheet": "Data" },
  "settings": { "combo_variables": ["Region", "Product"], "target_variable": "Value",
                "date_type": "month", "forecast_horizon": 6 },
  "tolerance": { "absolute": 0.5, "relative": 1e-6 },
  "not_totals": ["All Products"],
  "steps": [
    { "op": "use_header_rows", "rows": [4] },
    { "op": "drop_rows", "rows": [11], "reason": "footnote" },
    { "op": "fill_down", "columns": ["Region"] },
    { "op": "drop_total_rows", "columns": ["Region", "Product"] },
    { "op": "drop_total_columns", "columns": ["Total"] },
    { "op": "pivot_longer", "id_columns": ["Region", "Product"] },
    { "op": "parse_numbers", "columns": ["Value"] },
    { "op": "parse_dates", "column": "Date" }
  ]
}
```

- `settings` are merged over the project's config. The checks need `combo_variables`,
  `target_variable`, `date_type`, and `forecast_horizon`, and also use `hist_end_date`,
  `fiscal_year_start`, `external_regressors`, `forecast_approach`, `clean_missing_values`,
  and `combo_cleanup_date` when set. The prepared date column is always `Date`.
- Row numbers are the source file's rows (1 = first row). Column letters (`A`, `B`, ...)
  work before `use_header_rows`; names work after it.
- Every step that removes rows or columns, or changes, fills, combines, or derives
  values needs a `reason`, which appears in the preview.
- `not_totals` lists real series labels that read like totals (for example "All Other").

### Operations

| Operation | Use for |
|---|---|
| `use_header_rows` (`rows`, `sep`) | Column names from one row, or several rows joined (two-row Year/Month headers) |
| `keep_rows`, `drop_rows` (`rows`, `reason`) | Choose one table; drop title, filter, or footnote rows |
| `drop_rows_where` (`column`, `reason`, one of `equals`/`in`/`matches`/`blank`) | Drop repeated headers or notes by content |
| `drop_blank_rows` (`columns`) | Drop empty separator rows |
| `drop_columns` (`columns`, `reason`), `rename_columns` (`map`) | Remove or rename columns |
| `fill_down` (`columns`) | Outline pivots with blank repeated labels |
| `split_compact_labels` (`column`, `into`, `parent_rows`) | Compact pivots (groups and items in one column) |
| `drop_total_rows` (`columns`, `value_columns`, `reconcile`) | Drop total and subtotal rows after checking that they match their rows |
| `drop_total_columns` (`columns`, `of`, `reconcile`) | Drop total columns after checking that they match |
| `pivot_longer` (`id_columns`, `names_to`, `values_to`) | One column per period to one row per period |
| `split_column`, `combine_columns` | Series names split across or packed into columns |
| `parse_numbers` (`columns`, `decimal_mark`, `missing`, `suffixes`) | `(1,234)`, `$1,234.50`, `1.2K`, `3M`; list no-value text such as `n/a` or a dash in `missing` |
| `parse_dates` (`column`, `format`, `formats`) | Mixed date formats; ambiguous values such as `01/02/2024` stop until `format` is given |
| `fiscal_to_date` (`column`, `pattern`, `fiscal_year_start`, `year_is`, `period_type`, `into`) | `FY24 P01` style labels |
| `scale` (`column`, `factor`, `reason`, `where`) | Unit changes, such as thousands in some rows |
| `aggregate_duplicates` (`keys`, `columns`, `reason`) | Add up rows for the same series and date |
| `drop_duplicates` (`keys`, `reason`, `keep`) | Remove exact copies; `first`/`last` for conflicting rows |
| `derive_column` (`name`, `expression`, `reason`) | A target computed with `+ - * /` from number columns |
| `fill_values` (`column`, `method`, `reason`, `value`, `by`, `extend_into_future`) | Fill blanks with `zero`, a `value`, `previous`, or `next` |

Common orders: outline pivot `use_header_rows` > `fill_down` > `drop_total_rows` >
`pivot_longer` > `parse_numbers` > `parse_dates`; compact pivot `use_header_rows` >
`split_compact_labels` > `drop_total_rows` (group and item columns) > `pivot_longer` >
`parse_numbers` > `parse_dates`.

## Pivot filters and the latest period

- **Pivot filters.** `diagnostics.pivot_filters` lists filter rows above an Excel pivot,
  such as `Adjustment Type | (Multiple Items)`. The sheet then holds only the slice those
  filters select. `use_header_rows` drops the filter rows as the area above the header;
  tell the user what the slice is and confirm it is the data they want to forecast. A
  single selected item is masked unless the user agreed to `--reveal_labels=true`.
- **Latest period not ended.** For month, quarter, and year data, the preview's
  `Latest period` line and `data.latest_period` flag a last period with actuals that
  has not ended yet, so its actuals are probably partial. Do not drop those rows in the
  spec. Offer `hist_end_date` set to `suggested_hist_end_date` in the settings review, and
  let the user decide. This note depends on today's date, so it never fails the checks,
  changes the approval token, or goes into the audit record.

## What needs extra confirmation

These change data rather than only its layout and are listed under "Needs your extra
confirmation": `scale` (rescaling or unit conversion), `aggregate_duplicates`,
`drop_duplicates` with `first` or `last`, `derive_column` (including derived targets), and
`fill_values` (imputation). Explain each one in plain words before asking for approval.
Filling a target after the history end is never allowed.

## What is checked before anything is written

- The prepared file reads back with the run's settings.
- finnts' own input checks (`finnts::validate_prepared_input()`) with the real settings:
  required columns, the reserved `Date` column, a numeric target, trimmed non-blank
  series names, no duplicate series/date rows, dates on the period grid, a valid date
  type and fiscal year start, future rows and drivers, gaps and blank target values,
  series that `combo_cleanup_date` would drop, series too short for the horizon and back
  tests, and no total rows mixed with detail rows. Some items, such as gaps or short
  series, are reported but do not fail; explain them.
- Every source row and column is either used or dropped with a reason.
- The target total matches the source cells it came from; dropped total rows and
  columns match the rows they summarize (within `tolerance`).
- No total labels are left in the series columns, and no target values were filled
  after the history end.

If the installed finnts does not have `validate_prepared_input()`, preview stops and asks
the user to update finnts. The check is never skipped.

## Privacy, reuse, and limits

- The spec is never saved. Nothing is cached or reused; every new file goes through
  diagnose, propose, and approve again. Approval records only a value-free summary in
  `project.json`: each step's number, operation, and affected count; each check's rule and
  status with a fixed message; the counts; and a fingerprint of the source file. The spec,
  its descriptions and reasons, labels, validator messages, and values are never recorded.
- If the same fix is needed on every data refresh, recommend moving it into the
  upstream report, query, or workflow that produces the file. For filtered pivots,
  suggest exporting the flat, unfiltered data instead.
- Data stays on this computer. For a bug report, build a small fake file with the same
  layout; never share real data, the preview file, or the spec.
- Excel formulas are not recalculated: the file's saved (cached) values are read. Ask
  the user to open, recalculate, and save the workbook if the values may be stale.
- The checks use the settings at preview time. If settings change later (for example the
  horizon or series columns), `validate_config.R` checks the data again; prepare again
  only if it reports a problem.
