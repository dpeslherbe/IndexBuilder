# IndexBuilder
To automate portfolios building based on different factors.

Currently composed of

- Function to build a portfolio based on market capitalization of the selected tickers;
- Function 2.0 includes Beta, P/E Ratio and Dividend Ratio.
- Evaluator Function to give values of Beta, P/E Ratio, Dividend Ratio & Annual Dividend Payout for global portfolio built from Function 2.0
- Function 3.0 to scrape from either CAN or US stocks (Market needs to be specified)
- Evaluator Function 3.0 to follow up on changes from Function 3.0
- Function 3.1 (now scrapes Insider Ownership for both CAN and US stocks)

Personally used to simulate the upper 85% of the S&P/TSX Capped Financials Index.
Function only usable on TSX, and TSX-V tickers(notation as aaa-T or aaa-X respectively, where aaa is the ticker chosen).

Planned ahead is to create similar functions for US tickers, and possibly functions for having both possible markets.
Future functions may include other weighting methods, as opposed to simple market capitalization weighting used here).

Future code, and functions created may be used for Random Portfolio creation in parts of Index-vs-Equity-Risk-Analysis project (found at https://github.com/dpeslherbe/Index-vs-Equity-Risk-Analysis).

## *Disclaimer*

I am not a registered investment, legal or tax advisor or a broker/dealer. All investment/financial opinions from this report are from personal research and intended as educational material. Although best efforts are made to ensure all information is accurate, occasionally unintended errors and misprints may occur.
**Do your own Research.**
While these reports provides mathematical and financial comparisons between single equity, random portfolios and index investing for informational purposes only, it is very important to do your own analysis before making any investment based on your own personal circumstances. Nothing available in this report should be understood or construed as a recommendation or financial advice.
Past performance is not a guarantee of future return, nor is it necessarily indicative of future performance. Keep in mind investing involves risk. The value of your investment will fluctuate over time and you may gain or lose money.
