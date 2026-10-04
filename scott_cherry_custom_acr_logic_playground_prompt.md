# Scott Cherry Custom ACR Logic Playground Prompt

Use the following prompt in a custom model playground to generate a working model that reproduces the Scott Cherry custom ACR forecasting logic from the provided R script.

```text
You are building a forecasting model from a historical ACR dataset.

Primary objective:
Create a production-ready forecasting pipeline that reproduces the Scott Cherry custom ACR logic embedded in the provided R script. The model must forecast monthly ACR targets for FY26 using historical monthly ACR data, while respecting the same decision logic, caps, and fallback rules implemented in the original code.

Input data:
- Use the Excel workbook: C:\Users\mitokic\Downloads\Scott_Cherry_AHR_Pull.xlsx
- Read the Pivot sheet.
- Use this as the source dataset for the model.
- Limit the analysis to 1,000 time series maximum after deduplicating or selecting the most relevant dimensions.
- Priority series definition: unique combinations of Field Accountability Unit, Segment, Strategic Pillar, and possibly Combination/Key where appropriate.
- If there are more than 1,000 series, select the first 1,000 unique relevant series in a stable order.

Expected dataset structure:
- Columns should be interpreted as a pivoted monthly ACR table with values such as:
  - Field Accountability Unit
  - Segment
  - Strategic Pillar
  - Fiscal Month
  - Total or monthly ACR amount
- The historical period includes monthly observations across FY23-FY25 and the forecast period extends to FY26 months 38-49.
- The original logic uses variables such as:
  - AvgDailyACR
  - TotalACR
  - MoM
  - StDev
  - Median
  - StDevMonth
  - MedianMonth
  - StDevs
  - Combination
  - Key
  - FiscalMonth
  - StrategicPillar
  - Segment
  - FieldAccountabilityUnit

Business requirements:
1. Build a monthly forecast pipeline that projects FY26 target months based on historical ACR patterns.
2. Use the same general logic as the R implementation, including same-month historical comparisons, trailing median logic, and capped forecast ranges.
3. Preserve logic for AI-related and non-AI-related strategic pillars.
4. Exclude these strategic pillars from the non-AI logic path when they match exactly:
   - AIInfra
   - Copilot for Security
   - Fabric F SKU
   - GitHubCopilot
   - GitHubPlatformandSecurity
   - AzureOpenAI
   - OtherAIServices
5. For all remaining pillars, apply the custom non-AI logic branch.

Core forecasting logic to reproduce:
- Compute month-over-month (MoM) and historical variability using same-month prior-year comparisons.
- Evaluate standard deviation of the same month across FY23, FY24, and FY25.
- Determine the best historical reference month by selecting the lowest absolute standard deviation among the three prior-year same-month comparisons.
- Set:
  - resultStdevs = minimum absolute StDevs among FY23, FY24, FY25 same-month comparison
  - resultMoM = corresponding MoM value tied to that minimum-variance month
- Use a growth factor of 1.06 as the baseline growth multiplier.
- Cap forecasts using the rule:
  - lower bound = MedianMonth - 2 * abs(StDevMonth)
  - upper bound = MedianMonth + 2 * abs(StDevMonth)
  - final value = max(min(...), upper bound), lower bound
- Use weighted combinations of historical MoM and median values depending on volatility thresholds.
- Key thresholds from the original logic include:
  - abs(StDevFY25) > 2
  - abs(resultStdevs) > 2
  - abs(resultStdevs) > 1.5
  - abs(resultStdevs) > 1
  - abs(StDevFY24) < 2 and abs(StDevFY23) < 2
  - abs(StDevFY24) < 0.8 and abs(StDevFY23) < 0.8
- Several month-specific exceptions exist for July and December (j == 38 and j == 43), where the weights differ.

Representative logic patterns to implement:
- When same-month FY25 variance is high and the minimum prior-year variance is also high:
  - use a blend of median-month and a bounded growth-adjusted prior-period value
  - apply the historical month-specific branch, especially for July/December conditions
- When same-month FY25 variance is moderate to low:
  - compute a weighted combination of resultMoM and MedianMonth with a growth adjustment
- When earlier-year variance is low and FY25/FY24/FY23 relationships align with sign patterns:
  - use weighted historical combinations from FY25, FY24, and FY23 to estimate target values
- If no strong historical signal exists:
  - fall back to a simple previous-year same-month MoM * growth factor adjustment, or the median if still unavailable

Important algorithmic details from the original code:
- There are many nested if/else branches, with month-specific logic for late-year and early-year periods.
- The logic frequently calculates values like:
  - max(min((x * multiplier), upperBound), lowerBound)
  - weighted sums such as:
    - (resultMoM * 0.6 + MedianMonth * 0.4)
    - (resultMoM * 0.7 + MedianMonth * 0.3)
    - (fx[j - 12, "MoM"] * 0.25 + fx[j - 24, "MoM"] * 0.45 + fx[j - 36, "MoM"] * 0.20 + MedianMonth * 0.10)
    - (fx[j - 12, "MoM"] * 0.10 + fx[j - 24, "MoM"] * 0.55 + fx[j - 36, "MoM"] * 0.35)
- These weights and conditions must be coded consistently with the original branches.
- There are explicit checks for same-month previous-year indicators, including negative/positive patterns of MoM.

Output requirements:
- Produce a forecast table that includes the final projected values for months 38-49 for each selected series.
- Include enough metadata to explain each forecast row:
  - StrategicPillar
  - Segment
  - FieldAccountabilityUnit
  - Combination
  - FiscalMonth
  - FiscalYear
  - Date
  - AvgDailyACR
  - TotalACR
  - Days
  - NewDays
  - MoM
  - StDev
  - Median
  - StDevMonth
  - MedianMonth
  - StDevs
- The model should return the finished forecast values in a tabular structure suitable for downstream reporting.

Implementation constraints:
- Reproduce the original decision logic, not a simplified approximation.
- Keep the logic deterministic and explainable.
- The model should be implementable in Python or R, but the logic must remain faithful to the Scott Cherry custom ACR rules.
- Do not invent new business rules beyond the logic encoded in the script.
- Include robust handling of missing values, nulls, and edge cases.
- Preserve the existing code's intent for the AI-related exclusions and the cap logic around two standard deviations.

Deliverable:
Write the code that reads the workbook, filters to the first 1,000 relevant time series, computes the historical statistics needed by the logic, applies the Scott Cherry custom ACR forecast branching rules, and outputs the final FY26 forecast table.

Important note:
The custom logic in the reference script is highly conditional and nested. Your generated model should mirror the same branching structure and threshold-based decision trees as closely as possible, not merely approximate it with a generic forecasting model.
```

This prompt is designed for a custom model playground or AI-assisted code generator to convert the business logic into a working forecasting model using the provided data.
