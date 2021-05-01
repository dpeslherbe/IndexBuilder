# IndexBuilder
To automate portfolios building based on different factors.

Currently composed of:

- IndexScraper(), a function that scrapes Currency, Price, Market Capitalization, 5-Year Monthly Beta, Price to Earnings Ratio, Earnings Per Share, Dividend Yield, and Insider Ownership percentage from Yahoo Finance. Price scraped is real-time during Market Hours and close price outside of Market Hours.
- CurrencyScaper(), a function that scrapes the Exchange Rate from Yahoo Finance (Real-Time Exchange Rate).
- IndexBuilder(), a function that will:
  1. Convert Prices and Market Capitalization Values to chosen currency (either USD or CAD)
  2. Calculate the appropriate weighting of the chosen tickers based on either Full Market Capitalization or Free Float Market Capitalization (Work In Progress)
  3. Calculate the number of shares necessary to approximate the chosen tickers and their corresponding weighting based on principal available for investing (Work In Progress)

Note that the ticker syntax must be the same used on Yahoo Finance.
No plans are made to include other currencies outside of USD, CAD at this point.

The following functions are inside the file IndexBuilder-deprecated:
- ~~IndexBuilder(); Function to build a portfolio based on market capitalization of the selected tickers;~~
- ~~IndexBuilder2.0(); Function 2.0 includes Beta, P/E Ratio and Dividend Ratio.~~
- ~~IndexEvaluator2.0(); Function to give values of Beta, P/E Ratio, Dividend Ratio & Annual Dividend Payout for global portfolio built from Function 2.0~~
- ~~IndexBuilder3.0(); Function 3.0 to scrape from either CAN or US stocks (Market needs to be specified)~~
- ~~IndexEvaluator3.0(); Function 3.0 to follow up on changes from Function 3.0~~
- ~~IndexBuilder3.1(); Function 3.1 (now scrapes Insider Ownership for both CAN and US stocks)~~
- ~~IndexBuilder4.0(); Function 4.0 (Choice of Market Cap or Free-Float Market Cap as weighting method)~~
They are kept for documentation purposes, but are no longer functional.

Personally used to simulate the upper 85% of the S&P/TSX Capped Financials Index.

Future code, and functions created may be used for Random Portfolio creation in parts of Index-vs-Equity-Risk-Analysis project (found at https://github.com/dpeslherbe/Index-vs-Equity-Risk-Analysis).

## *Disclaimer*

I am not a registered investment, legal or tax advisor or a broker/dealer. All investment/financial opinions from this report are from personal research and intended as educational material. Although best efforts are made to ensure all information is accurate, occasionally unintended errors and misprints may occur.
**Do your own Research.**
While these reports provides mathematical and financial comparisons between single equity, random portfolios and index investing for informational purposes only, it is very important to do your own analysis before making any investment based on your own personal circumstances. Nothing available in this report should be understood or construed as a recommendation or financial advice.
Past performance is not a guarantee of future return, nor is it necessarily indicative of future performance. Keep in mind investing involves risk. The value of your investment will fluctuate over time and you may gain or lose money.
