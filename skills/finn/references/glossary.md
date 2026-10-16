# Glossary

Use these plain-language definitions when explaining Finn to a finance user. Prefer the
first sentence; add the second only if they ask for more.

| term | plain meaning |
|---|---|
| **Series** (combo) | One line you want forecast, such as "East / Laptops revenue". Finn names it by joining the series columns with `--`. |
| **Horizon** | How many periods ahead to forecast, such as 12 months. |
| **History** / `hist_end_date` | The past actuals Finn learns from. `hist_end_date` is the last period with actuals; rows after it are treated as future driver values. |
| **Back test** | Pretending it is an earlier date, forecasting periods whose actuals are already known, and scoring the result. This is how Finn judges models fairly. |
| **WMAPE** | Weighted mean absolute percentage error: the average miss as a percentage of actuals, with bigger series counting more. Lower is better; 10% means forecasts were off by about 10% on average. |
| **Accuracy** | An easier way to say it: 100% minus WMAPE, so a 10% WMAPE is roughly 90% accurate. Finn's outputs report WMAPE itself. |
| **Driver** (external regressor) | A column, like price or headcount, that helps explain the target. It needs values for every future period being forecast. |
| **Model** | One forecasting method, such as ARIMA, exponential smoothing, or a machine-learning model. Finn tries many and keeps the best. |
| **Local model** | A model trained on one series at a time. |
| **Global model** | One model trained on many series together; faster and often better for many related series. |
| **Ensemble** | A model that learns from several other models' forecasts. |
| **Simple average** | The mean of the best few models' forecasts, used when it scores better than any single model. |
| **Best model** | The model, or average of models, with the lowest back-test error for a series. |
| **Prediction interval** | A range (for example 80% or 95%) the actual is likely to fall within. It reflects past errors, not a guarantee. |
| **Hierarchy** | Series that must add up, such as products to regions to total. Finn can forecast so totals and parts agree. |
| **Fiscal year start** | The month the user's fiscal year begins; used for fiscal-year features and summaries. |
| **Standard run** | One pass: Finn trains its usual models, back tests them, and picks the best per series. Fully local. |
| **Agent run** | An AI model (by default GitHub Copilot) studies the data and accuracy, tries different settings over several rounds, and keeps the best. |
| **Iterate** | The first Agent run on a data set; it searches for good settings per series. |
| **Update** | A later Agent run with new actuals; it refits the models the Agent already chose and searches again only if accuracy drops well below earlier runs. |
| **Agent version** | Each Agent search creates a numbered version of the project's Agent; updates build on the latest one. |
| **Run id** | The name of one run, shown in status and output folders. Rerunning the same mode resumes the same run id. |
| **Project** | A folder holding one data set, its settings, all its runs, and outputs. |
| **Outlier cleaning** | Replacing unusual spikes in history before training, so one-off events do not distort the forecast. |
| **Stationarity**, **ACF/PACF** | Statistical checks of trend and of how values relate to earlier values. The Agent uses them; users rarely need them. |
