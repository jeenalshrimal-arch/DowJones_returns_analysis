## Dow Jones Equity Returns – Risk Analysis (R)

### Objective
Analyze the statistical and time-series properties of equity log-returns
for Dow Jones component stocks, with focus on volatility clustering,
distributional characteristics, and serial dependence.

### Data
- Dow Jones component stock prices  
- Period: 2000–2015  
- Stocks analyzed: HD, IBM, INTC  

### Methodology
- Stock price trajectory visualization  
- Log-return calculation  
- Descriptive statistics (mean, volatility, skewness, kurtosis)  
- Scatter plot matrix to explore correlations  
- Ljung–Box tests for serial independence  
- Autocorrelation analysis of returns and absolute returns  
- Monthly volatility estimation  
- Q–Q plots against normal distribution to assess fat tails  

### Tools & Libraries
- R  
- xts  
- qrmdata  
- qrmtools  
- moments  

### Key Findings
- All three stocks show **non-stationary behavior** in prices.  
- Returns exhibit **weak serial correlation**, but **absolute returns show strong autocorrelation**, indicating volatility clustering.  
- Log-returns show higher volatility during 2000–2003 and the 2008 financial crisis.  
- Scatter plot matrix indicates clustered, positively correlated returns, partially due to all stocks being US-based.  
- Descriptive statistics:  
  - **Highest expected return:** Home Depot  
  - **Lowest volatility:** IBM  
  - **Negative skewness** for all stocks  
  - **High kurtosis**, indicating fat-tailed distributions and exposure to extreme returns  
- Q–Q plots confirm deviation from normality (fat tails).  

### Conclusion
- **IBM:** lowest risk option  
- **Home Depot:** highest returns, highest risk  
- **INTC:** moderate risk and return  
- Volatility clustering and fat-tailed distributions are evident across all three stocks.  

### Files
- `r_scripts/dj_returns_analysis.R`  
  R script containing the full analysis, including plots, statistics, and tests.
