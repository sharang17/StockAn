shinyUI(fluidPage(
  
  withMathJax(), # to display LaTeX-like formulas
  
  tags$head( # CSS insert
    tags$style(HTML("      
      td, th, tr {
        padding-right: 10px;
        padding-left: 10px;
        border: 1px solid #d3d3d3;
      }
      border-color {
        #d3d3d3
      }
    "))
  ),
  
  titlePanel("Dynamic Portfolio Analysis"),
  
  br(),
  
  sidebarLayout(sidebarPanel(
        
    textInput("ticker", h3("Security ticker"), value="SPY"),
    
    br(),
    
    radioButtons("freq", h3("Frequency"),
                   c("Daily" = "daily",
                   "Monthly" = "monthly"),
                 inline = TRUE,
                 selected = "monthly"),
    
    br(),
    
    h3("Time period for the..."),
    
    sliderInput("ret_period", h4("returns distribution"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(1900,2015),
                sep = ""),
    
    sliderInput("beta_period", h4("beta calculations"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(2010,2015),
                sep = ""),
    
    code("recommended: 5 year interval"),
    
    sliderInput("rp_period", h4("risk premia calculations"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(1995,2015),
                sep = ""),
    
    code("recommended: 20 year interval"),
    
    br(),
    width = 3
  ),
  
  mainPanel(
    
    tabsetPanel(position=c("right"),
      tabPanel(strong("Plots"), 
               br(),
               plotOutput("histogram"),
               plotOutput("scatterplot",width=800),
               htmlOutput("metrics_stats"),
               htmlOutput("metrics_finance"),
               br()),
      
      tabPanel(strong("Data"), 
               br(),
               tableOutput("table_head"),
               p("..."),
               br(),
               tableOutput("table_tail"),
               code("Displays no more than 200 rows."),
               br()),
      
      tabPanel(strong("Annualized data"),
               br(),
               tableOutput("annualized_table"),
               br()),
      
      tabPanel("Description",
               br(),
               h4(strong("Overview")),
               p(strong("Mean return"), "is the average realized stock return."),
               p(strong("Median return"), "is the return at which half of returns are above and half are below. The median differs from the mean when distributions are not normal."),
               p(strong("Standard deviation"), "describes the dispersion of stock returns. If the distribution is normal, stock returns will fall above or below the mean return by one standard deviation about 68% of the time. About 95% of the time, stock returns will fall within two standard deviations of the mean return."),
               p(strong("Skewness"), "describes the symmetry of a distribution. A negative skew (left-tailed) implies that negative returns (relative to the mode) are less common but more extreme than positive returns; likewise, a positive skew (right-tailed) implies that positive returns (relative to the mode) are less common but more extreme than negative returns. In other words, for a right-skewed distribution, the likelihood of extreme positive returns is greater than that of extreme negative returns. In finance, securities that exhibit large negative skewness are often avoided because they imply large downside risk."),
               p(strong("Kurtosis"), "describes the peakedness of a distribution. Because a normal distribution has a kurtosis of 3, excess kurtosis is often used instead (ie. kurtosis less 3). A distribution that is platykurtic (excess kurtosis < 0) has a low peak and thin tails; one that is leptokurtic (excess kurtosis > 0) has a high peak and fat tails. A distribution that is leptokurtic will exhibit fluctuations of larger magnitude than one platykurtic distribution, rendering the security more risky."),
               p(span("From the above, it's clear that distributions which exhibit high standard deviations, negative skewness and high kurtosis are the riskiest investments", style="color:#990000")),
               p("The", strong("Capital Asset Pricing Model (CAPM)"), "is a commonly used financial pricing model. It calculates a required rate of return for a stock by adding the risk-free rate to the product of the stock's beta and the market risk premium. Unlike many other models, it assumes there is only one type of risk: market risk."),
               p("The", strong("  3 factor model"), "is a financial pricing model. It expands on the CAPM model by adding two other factors to market risk: size risk and value risk. It uses two portfolios to proxy these risks: SMB and HML, respectively. Because the   model empirically explains the variability of market returns better than the CAPM, it is believed this is the evidence that investors require size and value risk premia (or, at least, whatever these portfolios truly proxy). The SMB (small minus big market cap) and HML (high minus low book-to-value) are long-short portfolios that are long the top third of small/high stocks and short the bottom third."),
               p("Both these regression models produce \\(R^2\\) results. The \\(R^2\\) value measures to what degree the independent variable (eg. factor returns) explains the variability of the dependent variable (eg. stock returns). It is a proxy for how \"useful\" the alpha and beta values are. For example, a beta value that is high while the \\(R^2\\) is low implies that the beta value is unreliable. As a result, using this beta in a financial pricing model may not produce robust results."),
               p("The", strong("Sharpe ratio"), "is a commonly used metric to evaluate stock or portfolio performance. It is defined as mean return less the risk free rate, divided by the standard deviation. Put simply, it is how much the stock returns given each additional unit of risk."),
               p("The", strong("Sortino ratio"), "expands on the Sharpe ratio by recognizing that standard deviation of returns also includes positive returns. Typically, investors are only concerned with downside risk, not upside. As a result, Sortino updates the Sharpe ratio to use not the standard deviation of all returns, but only negative returns."),
               br()
      ),
      
     
      
      tabPanel("Background",
               br(),
               h4(strong("Background")),
               p("The motivation for this project was to calculate and visualize important financial metrics for any publicly traded security. Traditionally, these calculations are performed in Excel. This process is time-consuming, error-prone and produces static output. To my knowledge, there is no freely available online application that allows various input assumptions to be flexibly changed. Leveraging R to automatically run these calculations yields useful financial information extremely quickly."),
               p("Although some important financial statistics are readily available online, such as betas and realized returns, they provide an incomplete picture of stock returns. For example, despite being widely interpreted as a measure of risk, standard deviation provides a narrow, one-dimensional view of a stock's variability. In reality, stock returns are rarely normal, so variables like skewness and kurtosis cannot be assumed away."),
               p("Similarly, the CAPM is widely used as the de facto model for calculating required return, largely due to its simplicity. But while the CAPM empirically explains about 70% of variability in market returns, the   3 factor model explains over 90%. Despite this, CAPM calculators online are abundant while 3 factor ones are virtually nonexistent."),
               p("Notably, these calculations are not difficult to perform. Heavily tested and widely used programming libraries perform them automatically. For these reasons, this webapp offers a more comprehensive view of stock returns."),
               p(span("All errors in this project are solely my own.", style="color:#990000")),
               br(),
               
               h4(strong("Applications")),
               strong("Academic"),
               p("Students can visualize how changing their input assumptions affects their results. For example, how is calculating beta sensitive to the frequency or time period chosen? What does R\\(^2\\) mean in relation to beta? What do fat tails look like and what are examples of non-normal distributions?"),
               strong("Quality control"),
               p("Using this tool, more thorough Excel-based models can be sanity-checked throughout the production process. Checking comparable companies or changing important assumptions can be rapidly executed using this webapp."),
               strong("Portfolio Management"),
               p("For portfolio managers, risk may be more easily seen and quantified. All of the statistics calculated here on an individual basis can be aggregated to the portfolio level."),

               br(),
               h4(strong("About me")),
               p("I am an aspiring data scientist, and the Founder and CEO of Shuffl(https://shuffl.it) which will someday, knock SoundCloud of their perch")),
              
               br()
      )
    )
  ))
)