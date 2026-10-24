# 03 · Household Expenditure: the largest number is not the most important

**Business Data Lessons #3**: what explains weekly food spending in 4,083 UK households, and why you cannot compare regression coefficients directly.

📄 **Article (Spanish):** [El número más grande no era el más importante.](https://catalinacortesg.substack.com/p/el-numero-mas-grande-no-era-el-mas)

## The lesson

In a multiple regression, alcohol had the largest coefficient (£0.39 more on food per extra £1), far above housing (£0.09). But alcohol spending barely varies between households (SD ≈ £3), while housing varies a lot (SD ≈ £54). Comparing the effect of a typical change instead of a £1 change reverses the ranking:

| Variable | Per £1 | Per typical change (1 SD) |
|---|---|---|
| Housing | £0.09 | **£4.9** |
| Alcohol | £0.39 | £1.2 |
| Debt | −£0.05 | −£1.0 |

Housing is about four times as important as alcohol.

## What's in this folder

| File | Description |
|---|---|
| `HouseholdExpenditure.Rmd` | Full analysis in R: data checks, regression, standardised effects, diagnostics, robustness check and interaction test |
| `HouseholdExpenditure.md` | Rendered version with all outputs and plots (open this one to read on GitHub) |
| `HouseholdExpenditure_files/` | Plots used by the rendered version |
| `figures/` | Figures used in the article (in Spanish) |

## Methods

- Multiple linear regression (food spending on alcohol, housing, debt, household size, children, vehicles, income and sex)
- Standardised effects (coefficient × standard deviation) to compare predictors on the same scale
- Multicollinearity (VIF), residual diagnostics and a robustness check excluding zero-debt households
- Debt × alcohol interaction (not significant, p = .62)

## Data

The dataset (4,097 UK households, 4,083 after cleaning) was provided for a course in my MSc in Business Analytics at Warwick Business School and is not included in this repository.

---
*Part of the [Business Data Lessons](../README.md) series.*
