[![R-CMD-check](https://github.com/hanupavan065/lab4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hanupavan065/lab4/actions/workflows/R-CMD-check.yaml)


# lab4 — Advanced R Programming

An R package developed for the Advanced R Programming course at Linköping University. The package includes linear regression, ridge regression, and airport delay visualization, together with automated tests and documentation.

## Features

### Linear Regression
- Includes the original linear regression implementation.
- Provides a foundation for fitting and working with linear regression models.

### Ridge Regression
- Implements ridge regression using QR decomposition.
- Standardizes predictors during model fitting.
- Supports model printing, coefficient extraction, and prediction.
- Allows regularization strength to be controlled through the `lambda` parameter.

### Airport Delay Visualization
- Visualizes average arrival delays by destination airport.
- Uses flight data from the `nycflights13` package.
- Uses `dplyr` for data manipulation and `ggplot2` for visualization.

### Testing and Documentation
- Automated tests using `testthat`.
- Function documentation generated with `roxygen2`.
- A vignette demonstrating ridge regression and comparing it with ordinary linear regression and forward variable selection.

## Installation

Clone the repository:

```bash
git clone https://github.com/suryavardhan17/lab4.git
cd lab4
```

Install the package from the project directory in R:

```r
install.packages("devtools")
devtools::install()
```

Load the package:

```r
library(lab4)
```

## Usage

### Linear Regression

View the documentation for the linear regression function:

```r
?linreg
```

### Ridge Regression

Fit a ridge regression model:

```r
model <- ridgereg(y ~ ., data = my_data, lambda = 1)

print(model)
coef(model)
```

Generate predictions for new observations:

```r
predictions <- predict(model, newdata = new_data)
```

Replace `my_data` and `new_data` with your own data frames containing the response and corresponding predictor variables.

### Airport Delay Visualization

Generate the airport delay plot:

```r
visualize_airport_delays()
```

The function visualizes average arrival delays across destination airports using flight data.

## Documentation

Access function documentation in R:

```r
?ridgereg
?visualize_airport_delays
```

Build and view the package vignette:

```r
devtools::build_vignettes()
browseVignettes("lab4")
```

## Testing

Run the automated tests:

```r
devtools::test()
```

Check the package:

```r
devtools::check()
```

The package passed `devtools::check()` during development with **0 errors, 0 warnings, and 0 notes**.

## Project Structure

```text
lab4/
├── R/              # Package source code
├── man/            # Function documentation
├── tests/          # Automated tests
├── vignettes/      # Extended documentation and examples
├── DESCRIPTION     # Package metadata and dependencies
├── NAMESPACE       # Exported functions and methods
└── README.md       # Project overview and usage
```

## License

Refer to the repository's license information.

---

**Course:** Advanced R Programming  
**Institution:** Linköping University
