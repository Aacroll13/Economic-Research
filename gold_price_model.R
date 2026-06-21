############################################################
# GOLD PRICE PREDICTION MODEL — MONTHLY DATA (1974–present)
# Economic, Political, Geopolitical, Policy & Physical-Demand
# Indicators (jewelry, central bank, consumer electronics)
# Statistical Methods: OLS, VAR, ARDL, Random Forest, XGBoost,
# ARIMAX, Monte Carlo simulation
#
# Author: Drew (via Claude / Anthropic)
# Data Sources: FRED, Yahoo Finance, World Gold Council (WGC)
############################################################

# ══════════════════════════════════════════════════════════
# 0. INSTALL & LOAD PACKAGES
# ══════════════════════════════════════════════════════════

packages <- c(
  "fredr",       # FRED API (Federal Reserve Economic Data)
  "quantmod",    # Yahoo Finance / getSymbols
  "tidyverse",   # Data wrangling
  "lubridate",   # Date handling
  "zoo",         # Time series fill / rollmean
  "tseries",     # ADF / PP unit root tests
  "vars",        # Vector Autoregression (VAR)
  "ARDL",        # Autoregressive Distributed Lag + bounds test
  "lmtest",      # Breusch-Pagan, Durbin-Watson
  "sandwich",    # HAC-robust standard errors
  "randomForest",# Random Forest
  "xgboost",     # XGBoost (gradient boosting)
  "caret",       # Model training & cross-validation
  "forecast",    # ARIMA, auto.arima
  "ggplot2",     # Plots
  "ggcorrplot",  # Correlation heatmap
  "patchwork",   # Combine ggplot panels
  "kableExtra",  # Tables
  "stargazer",   # Regression output tables
  "moments",     # Skewness / kurtosis
  "PerformanceAnalytics" # Time series performance metrics
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed], repos = "https://cran.rstudio.com/")
}
invisible(lapply(packages, library, character.only = TRUE))

# NOTE: Several packages loaded above (e.g. MASS, pulled in as a dependency
# of caret/randomForest/ARDL) define their own generic functions named
# select()/rename()/filter() that mask dplyr's versions when attached AFTER
# dplyr. MASS::select in particular is a bare generic with NO method for
# data.frame/tibble objects, so any later `%>% select(...)` call in this
# script would silently break with "unused arguments" or "no applicable
# method" errors. Force these four verbs to always resolve to dplyr's
# implementations for the rest of this script, regardless of what loads
# afterward.
select  <- dplyr::select
rename  <- dplyr::rename
filter  <- dplyr::filter
lag     <- dplyr::lag

# ══════════════════════════════════════════════════════════
# 1. FRED API KEY
#    Get a FREE key at https://fred.stlouisfed.org/docs/api/api_key.html
# ══════════════════════════════════════════════════════════

fredr_set_key("8c8a050b086ab5f751366517e4da83c0")   # Drew's FRED API key

# ══════════════════════════════════════════════════════════
# 2. DATE RANGE
# ══════════════════════════════════════════════════════════

START_DATE <- as.Date("1974-01-01")   # Gold free-floating since Aug 1971
END_DATE   <- Sys.Date()              # Through most recent available month

# ══════════════════════════════════════════════════════════
# 3. DATA COLLECTION — FRED SERIES
#
# Series ID          | Variable                        | Source
# ─────────────────────────────────────────────────────────
# NOTE: FRED discontinued GOLDAMGBD228NLBM (LBMA gold fix) with no
# replacement series (removed ~May 2025). Gold price is instead sourced
# from Yahoo Finance COMEX gold futures (GC=F) in Section 4 below.
# DTWEXBGS           | USD Broad Real Effective Exch.  | Fed Board
# CPIAUCSL           | CPI All Urban (not seasonally adj)| BLS
# CPILFESL           | Core CPI (ex-food & energy)     | BLS
# DFF                | Federal Funds Effective Rate    | FOMC
# GS10               | 10-Yr Treasury Constant Maturity| Fed Board
# GS2                | 2-Yr Treasury Constant Maturity | Fed Board
# BAMLH0A0HYM2       | High-Yield (Junk) Spread        | ICE/BofA
# M2SL               | M2 Money Supply (Billions USD)  | Fed Board
# DCOILWTICO         | WTI Crude Oil ($/barrel)        | EIA
# VIXCLS             | CBOE VIX Volatility Index       | CBOE (from 1990)
# USEPUINDXD         | US Econ. Policy Uncertainty Idx | Baker-Bloom-Scott
# GEPUCURRENT        | Geopolitical Risk Index (GPR)   | Caldara-Iacoviello
# SP500              | S&P 500 Index                   | S&P (monthly avg)
# GFDEGDQ188S        | Federal Debt % of GDP           | OMB / BEA
# UNRATE             | Unemployment Rate (%)           | BLS
# INDPRO             | Industrial Production Index     | Fed Board
# DEXUSEU            | USD/EUR Exchange Rate           | Fed Board
# PAYEMS             | Nonfarm Payrolls (thousands)    | BLS
# WILL5000PR         | Wilshire 5000 Total Market      | Wilshire
# ══════════════════════════════════════════════════════════

fred_series <- list(
  usd_index    = "DTWEXBGS",
  cpi          = "CPIAUCSL",
  core_cpi     = "CPILFESL",
  fed_funds    = "DFF",
  t10y         = "GS10",
  t2y          = "GS2",
  # NOTE: BAMLH0A0HYM2 (ICE BofA US High Yield OAS) only has data from
  # 2023-06-19 onward in FRED (the older history was pulled following a
  # 2022 ICE licensing dispute and was not fully restored), which crushed
  # the complete-case sample to ~35 months. Using BAA10Y (Moody's Baa
  # corporate bond yield minus 10Y Treasury) instead — a long-running
  # (1986–present) credit-risk-spread proxy — so the variable named
  # "hy_spread" throughout the rest of the script keeps full history.
  hy_spread    = "BAA10Y",
  m2           = "M2SL",
  oil          = "DCOILWTICO",
  vix          = "VIXCLS",
  epu          = "USEPUINDXD",
  gpr          = "GEPUCURRENT",
  # NOTE: FRED's "SP500" series is licensed by S&P and only exposes the
  # most recent ~10 years via the API (no full history available), which
  # would crush the complete-case training sample. S&P 500 is instead
  # sourced from Yahoo Finance (^GSPC) in Section 4 below, which has
  # full history back to the 1950s. "sp500" is therefore NOT requested
  # from FRED here — it is added via Yahoo and merged under the same
  # column name so all downstream references keep working unchanged.
  debt_gdp     = "GFDEGDQ188S",
  unemp        = "UNRATE",
  indpro       = "INDPRO",
  usdeur       = "DEXUSEU",
  nfp          = "PAYEMS"
  # NOTE: wilshire5000 (WILL5000PR) was removed from this list. FRED
  # discontinued that series ID (the API now returns "series does not
  # exist"), and grep against the rest of this script confirms
  # wilshire5000 was never actually consumed by any downstream model —
  # broad-market exposure is already captured via Yahoo's ^GSPC (sp500,
  # Section 4). Dropping it avoids a download failure with no benefit.
)

# Download all series and convert to monthly
download_fred <- function(series_id, start = START_DATE, end = END_DATE) {
  tryCatch({
    # GFDEGDQ188S (Federal Debt as % of GDP) is published quarterly only —
    # FRED's API rejects frequency = "m" for this series ("Value of
    # frequency is not one of: 'q','sa','a'"). Fetch it at its native
    # quarterly frequency instead, then linearly interpolate to monthly
    # (consistent with the WGC demand-data interpolation used in Section
    # 4b) so it still merges cleanly into the monthly panel below.
    if (series_id == "GFDEGDQ188S") {
      qdata <- fredr(
        series_id         = series_id,
        observation_start = start,
        observation_end   = end,
        frequency         = "q"
      ) %>% select(date, value)

      monthly_dates <- seq(min(qdata$date), max(qdata$date), by = "month")
      tibble(
        date  = monthly_dates,
        value = approx(qdata$date, qdata$value, xout = monthly_dates, rule = 2)$y
      ) %>% rename(!!series_id := value)
    } else {
      fredr(
        series_id         = series_id,
        observation_start = start,
        observation_end   = end,
        frequency         = "m"
      ) %>%
        select(date, value) %>%
        rename(!!series_id := value)
    }
  }, error = function(e) {
    message(paste("Failed to download:", series_id, "-", e$message))
    return(NULL)
  })
}

message("Downloading FRED series...")
fred_data <- lapply(fred_series, function(id) download_fred(id))
names(fred_data) <- names(fred_series)

# Merge all into one dataframe (gold itself is added in Section 4 below,
# from Yahoo Finance, since FRED's gold series was discontinued)
df_raw <- fred_data[[1]] %>% rename(date = date)
for (nm in names(fred_data)[-1]) {
  if (!is.null(fred_data[[nm]])) {
    df_raw <- full_join(df_raw, fred_data[[nm]], by = "date")
  }
}

# Rename columns to readable names.
#
# CRITICAL: do NOT do this by blind position (e.g. `colnames(df_raw) <-
# c("date", names(fred_series))`). If any single FRED series fails to
# download, full_join() above simply has one fewer column than expected,
# and a positional rename then silently shifts every friendly name AFTER
# the failure point onto the WRONG underlying series — e.g. with one
# failed series, the column meant to be "unemp" (UNRATE) would actually
# get labeled "debt_gdp", "indpro" (INDPRO) would get labeled "unemp",
# and so on. This corrupts every model fit downstream without ever
# throwing an error. Instead, map by the actual FRED series ID, which
# download_fred() already stamped onto each value column — any series
# that failed is simply absent (not relabeled) and downstream code is
# expected to guard for its absence explicitly.
id_to_friendly <- setNames(names(fred_series), unlist(fred_series))
present_ids    <- setdiff(colnames(df_raw), "date")
df_raw <- df_raw %>%
  rename_with(~ id_to_friendly[.x], .cols = all_of(present_ids))

# ══════════════════════════════════════════════════════════
# 4. YAHOO FINANCE DATA (supplemental)
#    Silver (SLV proxy), GLD ETF (from 2004), DBC Commodities
# ══════════════════════════════════════════════════════════

message("Downloading Yahoo Finance data...")

getSymbols("GC=F",  src = "yahoo", from = START_DATE, to = END_DATE, auto.assign = TRUE)
getSymbols("SI=F",  src = "yahoo", from = START_DATE, to = END_DATE, auto.assign = TRUE)
getSymbols("DX-Y.NYB", src = "yahoo", from = START_DATE, to = END_DATE, auto.assign = TRUE)

# Monthly close for silver
silver_monthly <- to.monthly(`SI=F`, indexAt = "lastof", OHLC = FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  rename(silver = `SI=F.Close`)

df_raw <- left_join(df_raw, silver_monthly, by = "date")

# Monthly close for gold (real market data — COMEX gold futures, continuous
# front-month contract). This replaces FRED's GOLDAMGBD228NLBM, which FRED
# discontinued with no direct replacement. Yahoo's GC=F history goes back
# to ~2000, so rows before that will have NA gold and are dropped by the
# models' complete-case fitting later in the script.
gold_monthly <- to.monthly(`GC=F`, indexAt = "lastof", OHLC = FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  rename(gold = `GC=F.Close`)

df_raw <- left_join(df_raw, gold_monthly, by = "date")

# S&P 500 (^GSPC) — sourced from Yahoo Finance rather than FRED's "SP500"
# series, which is licensed by S&P and only exposes ~10 years of history
# via the API. Yahoo's ^GSPC history runs back to the 1950s, which is what
# a ~50-year model needs.
getSymbols("^GSPC", src = "yahoo", from = START_DATE, to = END_DATE, auto.assign = TRUE)

sp500_monthly <- to.monthly(GSPC, indexAt = "lastof", OHLC = FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  rename(sp500 = `GSPC.Close`)

df_raw <- left_join(df_raw, sp500_monthly, by = "date")

# ══════════════════════════════════════════════════════════
# 4b. PHYSICAL / INDUSTRIAL GOLD DEMAND INDICATORS
#     Jewelry demand, consumer-electronics (technology) demand,
#     and central bank net purchases — the three components of
#     World Gold Council "Gold Demand Trends" sector tonnage data,
#     plus a real FRED proxy for electronics fabrication volume.
# ══════════════════════════════════════════════════════════
#
# World Gold Council (WGC) publishes quarterly sector demand in
# tonnes (jewellery, technology, central banks & other institutions,
# investment) at https://www.gold.org/goldhub/data/gold-demand-by-country
# There is NO free API for this — it has to be downloaded as CSV/XLS.
# Reliable category-level data runs from ~Q1 1992 onward.
#
# To use REAL WGC data instead of the illustrative fallback below:
#   1. Visit https://www.gold.org/goldhub/data/gold-demand-by-country
#   2. Download "Gold Demand Trends — full data set" (CSV/XLS)
#   3. Save it next to this script as "wgc_demand_quarterly.csv" with
#      columns: date, jewellery_tonnes, technology_tonnes, cb_tonnes
#   4. Re-run this script — it will auto-detect and load the real file.

wgc_path <- "wgc_demand_quarterly.csv"

if (file.exists(wgc_path)) {
  wgc_demand <- read_csv(wgc_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
  message("Loaded REAL World Gold Council demand data from ", wgc_path)
} else {
  message("NOTE: '", wgc_path, "' not found in the working directory.\n",
          "  Using a small ILLUSTRATIVE example panel for jewellery/",
          "technology/central-bank demand so the pipeline still runs.\n",
          "  Download the real series from ",
          "https://www.gold.org/goldhub/data/gold-demand-by-country ",
          "and save it as '", wgc_path, "' to replace this with actual data.")

  # Illustrative example only (NOT real WGC figures) — seasonal jewellery
  # pattern (Q4 wedding/festival season bump), modest tech-demand level,
  # and a central-bank series that flips from net seller (pre-2010) to
  # net buyer (post-2010), roughly matching the well-documented real-world
  # regime shift, but the exact tonnages are illustrative placeholders.
  set.seed(42)
  wgc_dates <- seq(as.Date("1992-01-01"),
                    as.Date(format(END_DATE, "%Y-%m-01")), by = "quarter")
  wgc_demand <- tibble(
    date = wgc_dates,
    jewellery_tonnes  = 550 + 80 * sin(2 * pi * (as.numeric(format(date, "%m")) / 12)) +
                         rnorm(length(date), 0, 25),
    technology_tonnes = 80 + 0.15 * as.numeric(date - min(date)) / 90 +
                         rnorm(length(date), 0, 6),
    cb_tonnes          = ifelse(date < as.Date("2010-01-01"),
                                 -100 + rnorm(length(date), 0, 40),
                                  150 + rnorm(length(date), 0, 70))
  )
}

# Interpolate quarterly WGC tonnage to monthly via spline (demand moves
# smoothly within a quarter; standard practice for mixed-frequency merges)
wgc_monthly <- wgc_demand %>%
  arrange(date) %>%
  complete(date = seq(min(date), max(date), by = "month")) %>%
  mutate(across(c(jewellery_tonnes, technology_tonnes, cb_tonnes),
                ~ zoo::na.spline(.x, na.rm = FALSE))) %>%
  rename(jewelry_demand  = jewellery_tonnes,
         tech_demand_wgc = technology_tonnes,
         cb_demand       = cb_tonnes)

df_raw <- left_join(df_raw, wgc_monthly, by = "date")

# Consumer-electronics manufacturing output — REAL, live FRED series, used
# as a higher-frequency cross-check on technology-sector gold demand (gold
# bonding wire / plating scales with electronics production volume)
electronics_data <- download_fred("IPG334S")  # IP: Computer & Electronic Product Mfg
if (!is.null(electronics_data)) {
  colnames(electronics_data) <- c("date", "electronics_ip")
  df_raw <- left_join(df_raw, electronics_data, by = "date")
}

# ══════════════════════════════════════════════════════════
# 4c. MARKET-SCAN ADDITIONS — CANDIDATE SHORT-HORIZON (2-12mo)
#     PREDICTIVE VARIABLES, identified via market scan (see report
#     "Market Scan" section for full rationale/sourcing table).
#     These are NOT yet wired into any of the fitted models below —
#     they are pulled here so a future run can add them to ols_diff /
#     the RF & XGBoost feature lists and re-evaluate. Adding them to
#     df_raw now (rather than leaving this as a TODO) means the very
#     next execution of this script already has the data available.
# ══════════════════════════════════════════════════════════

message("Downloading market-scan candidate variables...")

# (1) 10-Year TIPS market-based real yield (FRED: DFII10). This is a
# DIRECT market quote of the real rate, vs. this script's existing
# real_rate_10y = nominal T10Y minus realized trailing CPI YoY (an
# ex-post approximation). DFII10 only starts 2003-01-02 (TIPS market
# inception), so adding it does not shrink the existing complete-case
# sample materially (already truncated to 2006+ by DTWEXBGS).
tips_real_yield <- download_fred("DFII10")
if (!is.null(tips_real_yield)) {
  colnames(tips_real_yield) <- c("date", "real_rate_market")
  df_raw <- left_join(df_raw, tips_real_yield, by = "date")
}

# (2) 5-Year breakeven inflation expectation (FRED: T5YIE) — a forward-
# looking, market-implied inflation measure, distinct from this script's
# existing yoy_cpi (backward-looking, realized inflation). Starts 2003.
breakeven_5y <- download_fred("T5YIE")
if (!is.null(breakeven_5y)) {
  colnames(breakeven_5y) <- c("date", "breakeven_5y")
  df_raw <- left_join(df_raw, breakeven_5y, by = "date")
}

# (3) Federal Reserve total assets / balance sheet (FRED: WALCL) — a
# direct QE/QT-pace measure, distinct from the existing qe_era dummy
# (which only flags 2008-11 to 2014-10). WALCL is published weekly;
# download_fred() requests frequency="m" which FRED aggregates to a
# monthly average automatically. Starts late 2002.
fed_balance_sheet <- download_fred("WALCL")
if (!is.null(fed_balance_sheet)) {
  colnames(fed_balance_sheet) <- c("date", "fed_balance_sheet")
  df_raw <- left_join(df_raw, fed_balance_sheet, by = "date")
}

# (4) Bitcoin price (Yahoo Finance: BTC-USD) — tests the "digital gold"
# substitution/competition hypothesis raised in the market scan. Yahoo's
# BTC-USD history starts 2014-09-17.
btc_ok <- tryCatch({
  getSymbols("BTC-USD", src = "yahoo", from = START_DATE, to = END_DATE, auto.assign = TRUE)
  TRUE
}, error = function(e) { message("NOTE: BTC-USD download failed - skipping: ", e$message); FALSE })

if (btc_ok) {
  btc_monthly <- to.monthly(`BTC-USD`, indexAt = "lastof", OHLC = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(as.yearmon(date))) %>%
    rename(btc_price = `BTC-USD.Close`)
  df_raw <- left_join(df_raw, btc_monthly, by = "date")
}

# (5) Basel III Tier-1 gold reclassification dummy. Effective 2025-07-01,
# U.S. banking regulators began allowing physical gold to count at 100%
# of market value toward core capital reserves (previously marked down
# 50% as a Tier-3 asset). This is a structural, one-time regulatory
# regime shift distinct from anything already in the dummy-variable set
# below — coded directly here rather than via a FRED/Yahoo pull, since
# it's a date-based regulatory fact, not a market series.
df_raw <- df_raw %>%
  mutate(basel3_tier1 = as.integer(date >= as.Date("2025-07-01")))

# NOTE on candidates NOT added here (no free/API-accessible data source —
# same category of limitation as the existing WGC jewelry/tech/CB demand
# series in Section 4b above). Documented in the report's Market Scan
# section so the rationale isn't lost even though the pull isn't coded:
#   - CFTC Commitment of Traders (COT) "Managed Money" net positioning —
#     free historical data exists at cftc.gov but only as weekly bulk
#     text/CSV files in a non-tabular layout, not a queryable API.
#   - Gold ETF holdings/flows (GLD + IAU tonnes) — published by the World
#     Gold Council (Goldhub) and SPDR/iShares directly; no free API.
#   - Mining All-In Sustaining Cost (AISC) — published quarterly by WGC /
#     S&P Global Market Intelligence; no free API, similar to the
#     existing WGC demand-tonnage limitation.
#   - Shanghai Gold Exchange premium vs. COMEX/LBMA — no free historical
#     API; available via paid data vendors or manual daily scraping.
#   - LBMA gold lease rates / GOFO — discontinued by the LBMA in 2015;
#     no longer published by anyone.

# ══════════════════════════════════════════════════════════
# 5. CLEAN & TRANSFORM
# ══════════════════════════════════════════════════════════

# Forward-fill slow/lagging-publication FRED indicators. CPI/Core-CPI can
# have a one-month gap from data-release delays; EPU/GPR/HY-spread/Debt-
# GDP/M2/Treasury-yields/Oil/Unemployment/Industrial-Production/Payrolls
# are all sometimes not yet posted for the most recent 1-2 months at pull
# time (M2SL in particular routinely lags ~6-8 weeks). Without this,
# df_model's drop_na() below silently discards EVERY row (including
# otherwise-complete recent gold price + macro data) from the first gap
# forward in ANY required column - producing a training panel and Monte
# Carlo anchor (gold0) that are stale relative to the true latest month
# (anchor_log_gold, computed straight from df). fill(.direction = "down")
# only carries an existing value into TRAILING NAs; it never fabricates
# data before a series' true historical start.
df_raw <- df_raw %>%
  arrange(date) %>%
  tidyr::fill(cpi, core_cpi, debt_gdp, epu, gpr, hy_spread,
              m2, usd_index, t10y, t2y, oil, unemp, indpro, usdeur, nfp,
              .direction = "down") %>%
  # Market-scan additions (Section 4c) — same forward-fill treatment for
  # any publication-lag gaps; columns may be absent entirely if a pull
  # failed (e.g. BTC-USD), so guard with any(... %in% colnames(.)).
  tidyr::fill(any_of(c("real_rate_market", "breakeven_5y",
                        "fed_balance_sheet", "btc_price")),
              .direction = "down")

df <- df_raw %>%
  arrange(date) %>%
  filter(date >= START_DATE, date <= END_DATE) %>%

  # ── Log levels ──
  mutate(
    log_gold     = log(gold),
    log_m2       = log(m2),
    log_oil      = log(oil),
    log_sp500    = log(sp500),
    log_indpro   = log(indpro),
    log_silver   = log(silver),
    log_usd      = log(usd_index)
  ) %>%

  # ── Monthly returns / first differences ──
  mutate(
    ret_gold     = c(NA, diff(log_gold)),
    ret_sp500    = c(NA, diff(log_sp500)),
    ret_oil      = c(NA, diff(log_oil)),
    ret_silver   = c(NA, diff(log_silver)),
    d_fed_funds  = c(NA, diff(fed_funds)),
    d_usd        = c(NA, diff(log_usd))
  ) %>%

  # ── Year-over-year changes (12-period) ──
  mutate(
    yoy_cpi      = (cpi / lag(cpi, 12) - 1) * 100,
    yoy_core_cpi = (core_cpi / lag(core_cpi, 12) - 1) * 100,
    yoy_m2       = (m2  / lag(m2, 12)  - 1) * 100,
    yoy_indpro   = (indpro / lag(indpro, 12) - 1) * 100
  ) %>%

  # ── Market-scan addition transforms (Section 4c) ──
  #    Computed defensively with if(exists)-style NA fallback so the
  #    script still runs end-to-end on a prior data pull that predates
  #    these columns, or if a given pull failed this run.
  mutate(
    log_btc           = if ("btc_price" %in% names(.)) log(btc_price) else NA_real_,
    log_fed_bs         = if ("fed_balance_sheet" %in% names(.)) log(fed_balance_sheet) else NA_real_
  ) %>%
  mutate(
    ret_btc            = if ("log_btc" %in% names(.)) c(NA, diff(log_btc)) else NA_real_,
    d_real_rate_market = if ("real_rate_market" %in% names(.)) c(NA, diff(real_rate_market)) else NA_real_,
    d_breakeven_5y      = if ("breakeven_5y" %in% names(.)) c(NA, diff(breakeven_5y)) else NA_real_,
    d_log_fed_bs        = if ("log_fed_bs" %in% names(.)) c(NA, diff(log_fed_bs)) else NA_real_
  ) %>%

  # ── Real interest rates (ex-ante approximation) ──
  mutate(
    real_rate_10y  = t10y - yoy_cpi,
    real_rate_2y   = t2y  - yoy_cpi,
    yield_curve    = t10y - t2y      # 10y-2y spread
  ) %>%

  # ── Political / event dummies ──
  mutate(
    year         = year(date),
    month        = month(date),

    # US Presidential election years (1976, 1980, ... 2024)
    election_yr  = as.integer(year %% 4 == 0),

    # Post-GFC QE era (Nov 2008 – Dec 2014)
    qe_era       = as.integer(date >= as.Date("2008-11-01") &
                              date <= as.Date("2014-10-31")),

    # COVID shock (Feb 2020 – Dec 2021)
    covid_shock  = as.integer(date >= as.Date("2020-02-01") &
                              date <= as.Date("2021-12-01")),

    # Nixon shock / Smithsonian (1971-1973 transition, already after start)
    gold_standard_end = as.integer(date < as.Date("1975-01-01")),

    # War on Terror / post-9/11 period (2001-09 to 2003-12)
    post_911     = as.integer(date >= as.Date("2001-09-01") &
                              date <= as.Date("2003-12-01")),

    # Russia-Ukraine escalation (Feb 2022 onward)
    russia_ukraine = as.integer(date >= as.Date("2022-02-01")),

    # Zero interest rate period (ZIRP): Dec 2008 - Dec 2015 + Mar 2020 - Mar 2022
    zirp         = as.integer(
      (date >= as.Date("2008-12-01") & date <= as.Date("2015-12-01")) |
      (date >= as.Date("2020-03-01") & date <= as.Date("2022-03-01"))
    )
  ) %>%

  # ── Physical / industrial demand-side transforms ──
  #    jewelry_demand, cb_demand, tech_demand_wgc (tonnes, quarterly→monthly
  #    spline from WGC) and electronics_ip (FRED IPG334S, real monthly series)
  #    are only populated from ~1992 onward — see Section 4b for sourcing.
  mutate(
    yoy_jewelry_demand = (jewelry_demand  / lag(jewelry_demand, 12)  - 1) * 100,
    yoy_tech_demand     = (tech_demand_wgc / lag(tech_demand_wgc, 12) - 1) * 100,
    cb_net_buyer        = as.integer(cb_demand > 0),   # 1 = central banks net buying that month
    log_electronics_ip  = ifelse(!is.na(electronics_ip) & electronics_ip > 0,
                                  log(electronics_ip), NA_real_),
    yoy_electronics_ip  = (electronics_ip / lag(electronics_ip, 12) - 1) * 100
  ) %>%

  # ── Lagged predictors (t-1 through t-3) ──
  mutate(
    real_rate_10y_L1 = lag(real_rate_10y, 1),
    real_rate_10y_L3 = lag(real_rate_10y, 3),
    yoy_cpi_L1       = lag(yoy_cpi, 1),
    yoy_m2_L1        = lag(yoy_m2, 1),
    log_usd_L1       = lag(log_usd, 1),
    epu_L1           = lag(epu, 1),
    gpr_L1           = lag(gpr, 1),
    ret_sp500_L1     = lag(ret_sp500, 1),
    vix_L1           = lag(vix, 1),
    hy_spread_L1     = lag(hy_spread, 1),

    # Demand-side lags (no look-ahead bias) for ML models
    jewelry_demand_L1 = lag(jewelry_demand, 1),
    cb_demand_L1       = lag(cb_demand, 1),
    yoy_tech_demand_L1 = lag(yoy_tech_demand, 1),
    yoy_electronics_ip_L1 = lag(yoy_electronics_ip, 1),
    cb_net_buyer_L1     = lag(cb_net_buyer, 1)
  ) %>%

  filter(date >= as.Date("1975-01-01"))  # Enough lags after start

message(paste("Dataset rows:", nrow(df), "| Cols:", ncol(df)))

# ══════════════════════════════════════════════════════════
# 6. EXPLORATORY DATA ANALYSIS
# ══════════════════════════════════════════════════════════

# 6a. Gold price level + log-scale time series
p1 <- ggplot(df, aes(date, gold)) +
  geom_line(color = "#D4AF37", linewidth = 0.8) +
  geom_rect(aes(xmin = as.Date("2008-09-01"), xmax = as.Date("2009-06-01"),
                ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) +
  geom_rect(aes(xmin = as.Date("2020-02-01"), xmax = as.Date("2020-06-01"),
                ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) +
  labs(title = "London Gold Price (USD/troy oz), 1975–2024",
       subtitle = "LBMA Afternoon Fix via FRED",
       x = NULL, y = "USD / troy oz") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 13)

p2 <- ggplot(df %>% drop_na(real_rate_10y, log_gold), aes(real_rate_10y, log_gold)) +
  geom_point(alpha = 0.3, color = "#D4AF37") +
  geom_smooth(method = "lm", color = "navy") +
  labs(title = "Real 10Y Rate vs Log Gold Price",
       x = "Real 10Y Rate (%)", y = "Log Gold") +
  theme_minimal(base_size = 13)

p3 <- ggplot(df %>% drop_na(log_usd, log_gold), aes(log_usd, log_gold)) +
  geom_point(alpha = 0.3, color = "#D4AF37") +
  geom_smooth(method = "lm", color = "navy") +
  labs(title = "USD Index vs Log Gold Price",
       x = "Log USD Index", y = "Log Gold") +
  theme_minimal(base_size = 13)

p4 <- ggplot(df %>% drop_na(yoy_cpi, log_gold), aes(yoy_cpi, log_gold)) +
  geom_point(alpha = 0.3, color = "#D4AF37") +
  geom_smooth(method = "lm", color = "navy") +
  labs(title = "CPI Inflation YoY vs Log Gold Price",
       x = "CPI YoY (%)", y = "Log Gold") +
  theme_minimal(base_size = 13)

tryCatch({
  print(p1 / (p2 | p3 | p4))
  ggsave("gold_eda.png", width = 14, height = 10)
}, error = function(e) message("NOTE: EDA combo plot failed (patchwork/ggplot2 version mismatch) - skipping: ", e$message))

# 6b. Correlation heatmap
cor_vars <- df %>%
  select(log_gold, real_rate_10y, log_usd, yoy_cpi, yoy_m2,
         ret_sp500, log_oil, vix, epu, gpr, hy_spread, yield_curve, debt_gdp) %>%
  drop_na() %>%
  cor(use = "complete.obs")

tryCatch({
  print(ggcorrplot(cor_vars, type = "lower", lab = TRUE, lab_size = 2.5,
             colors = c("#2166AC", "white", "#B2182B"),
             title = "Correlation Matrix — Gold & Macro Indicators") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  ggsave("gold_correlation.png", width = 10, height = 9)
}, error = function(e) message("NOTE: Correlation heatmap failed - skipping: ", e$message))

# ══════════════════════════════════════════════════════════
# 7. STATIONARITY TESTS
#    ADF (Augmented Dickey-Fuller) + KPSS for all key series
# ══════════════════════════════════════════════════════════

unit_root_tests <- function(x, name) {
  x_clean <- na.omit(x)
  adf  <- adf.test(x_clean, alternative = "stationary")
  kpss <- kpss.test(x_clean)
  tibble(
    Variable   = name,
    ADF_stat   = round(adf$statistic, 3),
    ADF_pval   = round(adf$p.value, 4),
    KPSS_stat  = round(kpss$statistic, 3),
    KPSS_pval  = round(kpss$p.value, 4),
    Conclusion = ifelse(adf$p.value < 0.05 & kpss$p.value > 0.05,
                        "Stationary", "Non-Stationary (I(1))")
  )
}

stationarity_results <- bind_rows(
  unit_root_tests(df$log_gold,     "Log Gold Price"),
  unit_root_tests(df$ret_gold,     "Gold Returns (Δlog)"),
  unit_root_tests(df$real_rate_10y,"Real 10Y Rate"),
  unit_root_tests(df$log_usd,      "Log USD Index"),
  unit_root_tests(df$d_usd,        "ΔUSD Index"),
  unit_root_tests(df$yoy_cpi,      "CPI YoY Inflation"),
  unit_root_tests(df$yoy_m2,       "M2 Growth YoY"),
  unit_root_tests(df$log_oil,      "Log Oil Price"),
  unit_root_tests(df$vix,          "VIX"),
  unit_root_tests(df$epu,          "Econ Policy Uncert."),
  unit_root_tests(df$gpr,          "Geopolitical Risk"),
  unit_root_tests(df$hy_spread,    "HY Credit Spread"),
  unit_root_tests(df$debt_gdp,     "Debt/GDP")
)

print(stationarity_results)

# ══════════════════════════════════════════════════════════
# 8. MODEL 1 — OLS BASELINE (LEVELS, LOG-LINEAR)
#    log(Gold) = β₀ + β₁·RealRate + β₂·LogUSD + β₃·Inflation
#               + β₄·LogM2 + β₅·LogOil + β₆·EPU + β₇·GPR
#               + β₈·HYSpread + β₉·DebtGDP + β₁₀·SP500Ret
#               + policy dummies + ε
# ══════════════════════════════════════════════════════════

df_model <- df %>% drop_na(
  log_gold, real_rate_10y, log_usd, yoy_cpi, log_m2,
  log_oil, epu, gpr, hy_spread, debt_gdp, ret_sp500,
  qe_era, covid_shock, post_911, zirp, yield_curve
)

# OLS Model 1: Pure levels (likely non-stationary — diagnostic purposes)
ols_levels <- lm(
  log_gold ~ real_rate_10y + log_usd + yoy_cpi + log_m2 +
             log_oil + epu + gpr + hy_spread + debt_gdp +
             ret_sp500 + yield_curve +
             qe_era + covid_shock + post_911 + zirp,
  data = df_model
)
cat("\n══ OLS Model 1: Levels (Log-Linear) ══\n")
print(summary(ols_levels))

# Robust standard errors (Newey-West HAC for autocorrelated errors)
library(sandwich)
cat("\n── Newey-West HAC robust SEs (ols_levels) ──\n")
tryCatch(print(coeftest(ols_levels, vcov = NeweyWest(ols_levels, lag = 12, prewhite = FALSE))),
         error = function(e) message("NOTE: NeweyWest coeftest (ols_levels) failed - skipping: ", e$message))

# OLS Model 2: First-difference returns specification (stationary)
df_diff <- df_model %>%
  mutate(
    d_log_gold    = c(NA, diff(log_gold)),
    d_real_rate   = c(NA, diff(real_rate_10y)),
    d_log_usd     = c(NA, diff(log_usd)),
    d_yoy_cpi     = c(NA, diff(yoy_cpi)),
    d_log_m2      = c(NA, diff(log_m2)),
    d_log_oil     = c(NA, diff(log_oil)),
    d_epu         = c(NA, diff(epu)),
    d_gpr         = c(NA, diff(gpr)),
    d_hy_spread   = c(NA, diff(hy_spread)),
    d_yield_curve = c(NA, diff(yield_curve))
  ) %>%
  drop_na()

ols_diff <- lm(
  d_log_gold ~ d_real_rate + d_log_usd + d_yoy_cpi + d_log_m2 +
               d_log_oil + d_epu + d_gpr + d_hy_spread + d_yield_curve +
               ret_sp500 + qe_era + covid_shock + post_911 + zirp,
  data = df_diff
)
cat("\n══ OLS Model 2: First Differences ══\n")
print(summary(ols_diff))
cat("\n── Newey-West HAC robust SEs (ols_diff) ──\n")
tryCatch(print(coeftest(ols_diff, vcov = NeweyWest(ols_diff, lag = 6, prewhite = FALSE))),
         error = function(e) message("NOTE: NeweyWest coeftest (ols_diff) failed - skipping: ", e$message))

# OLS diagnostics
cat("\n── Breusch-Pagan Test (heteroskedasticity) ──\n")
tryCatch(print(bptest(ols_diff)),
         error = function(e) message("NOTE: Breusch-Pagan test failed - skipping: ", e$message))
cat("\n── Durbin-Watson (autocorrelation) ──\n")
tryCatch(print(dwtest(ols_diff)),
         error = function(e) message("NOTE: Durbin-Watson test failed (likely collinear/rank-deficient design matrix) - skipping: ", e$message))
cat("\n── Jarque-Bera (normality of residuals) ──\n")
tryCatch(print(jarque.bera.test(residuals(ols_diff))),
         error = function(e) message("NOTE: Jarque-Bera test failed - skipping: ", e$message))

# Output table
tryCatch(
  stargazer(ols_levels, ols_diff,
            type = "text",
            title = "OLS Regression Results — Gold Price Models",
            column.labels = c("Levels (Log-Linear)", "First Differences"),
            dep.var.labels = c("log(Gold)", "Δlog(Gold)"),
            omit.stat = c("f", "ser"),
            digits = 4),
  error = function(e) message("NOTE: stargazer table failed - skipping: ", e$message)
)

# ══════════════════════════════════════════════════════════
# 8b. MODEL 1b — EXTENDED OLS WITH PHYSICAL DEMAND INDICATORS
#     Adds jewelry demand, central-bank net buying, and consumer-
#     electronics (tech) demand to the first-difference specification.
#     NOTE ON SAMPLE: WGC sector-demand data is only available from
#     ~1992 onward, so this model's effective sample is 1992–present
#     (~400 months) rather than the full 1975–present window used by
#     ols_levels / ols_diff above. Treat this as a robustness check /
#     supplementary model, not a replacement for the 50-year baseline.
# ══════════════════════════════════════════════════════════

df_diff_demand <- df_diff %>%
  mutate(
    d_jewelry_demand = c(NA, diff(jewelry_demand)),
    d_cb_demand       = c(NA, diff(cb_demand)),
    d_tech_demand     = c(NA, diff(tech_demand_wgc))
  ) %>%
  drop_na(d_jewelry_demand, d_cb_demand, d_tech_demand, cb_net_buyer)

ols_diff_demand <- lm(
  d_log_gold ~ d_real_rate + d_log_usd + d_yoy_cpi + d_log_m2 +
               d_log_oil + d_epu + d_gpr + d_hy_spread + d_yield_curve +
               ret_sp500 + qe_era + covid_shock + post_911 + zirp +
               d_jewelry_demand + d_cb_demand + d_tech_demand + cb_net_buyer,
  data = df_diff_demand
)
cat("\n══ OLS Model 1b: First Diff + Physical Demand (1992+) ══\n")
print(summary(ols_diff_demand))
cat("\n── Newey-West HAC robust SEs (ols_diff_demand) ──\n")
tryCatch(print(coeftest(ols_diff_demand, vcov = NeweyWest(ols_diff_demand, lag = 6, prewhite = FALSE))),
         error = function(e) message("NOTE: NeweyWest coeftest (ols_diff_demand) failed - skipping: ", e$message))

cat("\n── Demand-augmented model sample size vs. baseline ──\n")
cat("Baseline (ols_diff):        ", nrow(df_diff),        "months\n")
cat("Demand-augmented (1992+):   ", nrow(df_diff_demand), "months\n")

tryCatch(
  stargazer(ols_diff, ols_diff_demand,
            type = "text",
            title = "OLS — Baseline vs. Demand-Augmented Specification",
            column.labels = c("First Diff (1975+)", "First Diff + Demand (1992+)"),
            dep.var.labels = "Δlog(Gold)",
            omit.stat = c("f", "ser"),
            digits = 4),
  error = function(e) message("NOTE: stargazer (ols_diff vs ols_diff_demand) failed - skipping: ", e$message)
)

# ══════════════════════════════════════════════════════════
# 8c. ROBUSTNESS CHECKS & OUTLIER DIAGNOSTICS (on ols_diff,
#     the report's primary short-run inference model)
# ══════════════════════════════════════════════════════════

# ── (i) Variance Inflation Factors — multicollinearity check ──
robustness_vif <- tryCatch({
  if (!"car" %in% rownames(installed.packages())) {
    install.packages("car", repos = "https://cran.rstudio.com/")
  }
  library(car)
  vif(ols_diff)
}, error = function(e) { message("NOTE: VIF check failed - skipping: ", e$message); NULL })
if (!is.null(robustness_vif)) {
  cat("\n── Variance Inflation Factors (ols_diff) — VIF > 5 flags concerning collinearity ──\n")
  print(round(robustness_vif, 2))
}

# ── (ii) Rolling-window coefficient stability (60-month trailing OLS) ──
# Re-estimates ols_diff's specification on every available trailing
# 60-month window and tracks how much the USD and real-rate coefficients
# move. A coefficient that flips sign or swings wildly across windows is
# not one you should trust for a single full-sample point estimate.
roll_window  <- 60
roll_results <- list()
if (nrow(df_diff) > roll_window) {
  for (i in seq(roll_window, nrow(df_diff))) {
    win <- df_diff[(i - roll_window + 1):i, ]
    m <- tryCatch(
      lm(d_log_gold ~ d_real_rate + d_log_usd + d_yoy_cpi + d_log_m2 +
                     d_log_oil + d_epu + d_gpr + d_hy_spread + d_yield_curve +
                     ret_sp500 + qe_era + covid_shock + post_911 + zirp,
         data = win),
      error = function(e) NULL
    )
    if (!is.null(m)) {
      cf <- coef(m)
      roll_results[[length(roll_results) + 1]] <- tibble(
        window_end   = win$date[nrow(win)],
        b_real_rate  = unname(cf["d_real_rate"]),
        b_log_usd    = unname(cf["d_log_usd"]),
        b_yoy_cpi    = unname(cf["d_yoy_cpi"]),
        b_epu        = unname(cf["d_epu"])
      )
    }
  }
}
if (length(roll_results) > 0) {
  rolling_coefs <- bind_rows(roll_results)
  cat("\n── Rolling 60-month coefficient stability (ols_diff spec) ──\n")
  cat("USD coefficient range across all windows:    [",
      round(min(rolling_coefs$b_log_usd, na.rm = TRUE), 3), ",",
      round(max(rolling_coefs$b_log_usd, na.rm = TRUE), 3), "]\n")
  cat("Real-rate coefficient range across all windows: [",
      round(min(rolling_coefs$b_real_rate, na.rm = TRUE), 3), ",",
      round(max(rolling_coefs$b_real_rate, na.rm = TRUE), 3), "]\n")
  cat("Share of windows where USD coefficient is negative: ",
      round(mean(rolling_coefs$b_log_usd < 0, na.rm = TRUE) * 100, 1), "%\n")

  tryCatch({
    print(ggplot(rolling_coefs, aes(window_end, b_log_usd)) +
      geom_line(color = "#D4AF37", linewidth = 0.9) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      labs(title = "Rolling 60-Month USD Coefficient Stability",
           subtitle = "Re-estimated d_log_gold ~ ... on each trailing 5-year window",
           x = NULL, y = "USD coefficient (d_log_usd)") +
      theme_minimal(base_size = 13))
    ggsave("gold_rolling_coef_stability.png", width = 12, height = 6)
  }, error = function(e) message("NOTE: Rolling-coefficient plot failed - skipping: ", e$message))
}

# ── (iii) Subsample stability — pre- vs. post-COVID structural break ──
df_diff_pre  <- df_diff %>% filter(date <  as.Date("2020-02-01"))
df_diff_post <- df_diff %>% filter(date >= as.Date("2020-02-01"))

ols_diff_pre  <- tryCatch(lm(d_log_gold ~ d_real_rate + d_log_usd + d_yoy_cpi + d_log_m2 +
                                          d_log_oil + d_epu + d_gpr + d_hy_spread + d_yield_curve +
                                          ret_sp500 + qe_era + post_911 + zirp,
                              data = df_diff_pre),
                           error = function(e) NULL)
ols_diff_post <- tryCatch(lm(d_log_gold ~ d_real_rate + d_log_usd + d_yoy_cpi + d_log_m2 +
                                          d_log_oil + d_epu + d_gpr + d_hy_spread + d_yield_curve +
                                          ret_sp500 + zirp,
                              data = df_diff_post),
                           error = function(e) NULL)
if (!is.null(ols_diff_pre) && !is.null(ols_diff_post)) {
  cat("\n── Subsample stability: pre-COVID (", as.character(min(df_diff_pre$date)), "to",
      as.character(max(df_diff_pre$date)), ") vs. post-COVID (",
      as.character(min(df_diff_post$date)), "to", as.character(max(df_diff_post$date)), ") ──\n")
  tryCatch(
    stargazer(ols_diff_pre, ols_diff_post, type = "text",
              title = "Subsample Stability — Pre vs. Post COVID Structural Break",
              column.labels = c("Pre-Feb 2020", "Post-Feb 2020"),
              dep.var.labels = "Δlog(Gold)", omit.stat = c("f", "ser"), digits = 4),
    error = function(e) message("NOTE: subsample stargazer failed - skipping: ", e$message)
  )
}

# ── (iv) Outlier-robust regression — MASS::rlm() vs. OLS ──
# MM-estimator regression downweights high-leverage/high-residual months
# instead of letting them dominate the fit the way OLS does. Comparing
# coefficients tells you whether ols_diff's results are being driven by
# a handful of extreme months (e.g. Apr 2013 crash, Mar 2020 COVID
# whipsaw, Feb-Mar 2022 Russia invasion spike) or are genuinely
# representative of the typical month.
robust_ok <- tryCatch({
  library(MASS)
  ols_diff_robust <<- rlm(
    d_log_gold ~ d_real_rate + d_log_usd + d_yoy_cpi + d_log_m2 +
                d_log_oil + d_epu + d_gpr + d_hy_spread + d_yield_curve +
                ret_sp500 + qe_era + covid_shock + post_911 + zirp,
    data = df_diff, maxit = 100
  )
  TRUE
}, error = function(e) { message("NOTE: rlm robust regression failed - skipping: ", e$message); FALSE })

if (robust_ok) {
  cat("\n── Outlier-robust regression (MASS::rlm) vs. OLS — coefficient comparison ──\n")
  comparison_robust <- tibble(
    Variable      = names(coef(ols_diff)),
    OLS           = round(unname(coef(ols_diff)), 4),
    Robust_rlm    = round(unname(coef(ols_diff_robust)[names(coef(ols_diff))]), 4)
  )
  print(comparison_robust)

  # rlm's own downweighting shows which months it considers extreme —
  # lower weight = more discounted = more of an outlier to this model.
  outlier_weights <- tibble(
    date   = df_diff$date,
    weight = ols_diff_robust$w
  ) %>% arrange(weight)
  cat("\n── 10 most heavily downweighted months (lowest rlm weight = biggest outliers) ──\n")
  print(head(outlier_weights, 10))
}

# ── (v) Formal outlier/influence diagnostics on ols_diff ──
# Studentized residuals (|t| > ~2-3 flags a statistical outlier),
# Cook's distance (> 4/n flags high-influence points), and hat values
# (leverage). This is the live, row-level analogue of the event-based
# outlier discussion in the report's "Robustness & Outliers" section.
outlier_diagnostics <- tibble(
  date              = df_diff$date,
  studentized_resid = rstudent(ols_diff),
  cooks_distance    = cooks.distance(ols_diff),
  leverage          = hatvalues(ols_diff)
) %>%
  mutate(cooks_flag = cooks_distance > (4 / nrow(df_diff))) %>%
  arrange(desc(abs(studentized_resid)))

cat("\n── Top 10 outlier months by |studentized residual| (ols_diff) ──\n")
print(head(outlier_diagnostics, 10))
cat("\nMonths flagged by Cook's distance (> 4/n):", sum(outlier_diagnostics$cooks_flag), "of", nrow(outlier_diagnostics), "\n")

tryCatch({
  print(ggplot(outlier_diagnostics, aes(date, studentized_resid)) +
    geom_point(aes(color = cooks_flag), size = 2) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "grey40") +
    scale_color_manual(values = c("FALSE" = "#D4AF37", "TRUE" = "#C53030"),
                        labels = c("Normal", "High-influence (Cook's D)"),
                        name = NULL) +
    labs(title = "Studentized Residuals — ols_diff",
         subtitle = "Points beyond ±2 are statistical outliers; red = high-influence (Cook's distance)",
         x = NULL, y = "Studentized Residual") +
    theme_minimal(base_size = 13))
  ggsave("gold_outlier_diagnostics.png", width = 12, height = 6)
}, error = function(e) message("NOTE: Outlier diagnostics plot failed - skipping: ", e$message))

# ══════════════════════════════════════════════════════════
# 9. MODEL 2 — VECTOR AUTOREGRESSION (VAR)
#    Endogenous: log_gold, real_rate_10y, log_usd, yoy_cpi, log_m2
#    Exogenous:  gpr, epu, qe_era, covid_shock
#    Lag selection via AIC / BIC / HQ
# ══════════════════════════════════════════════════════════

var_data <- df_model %>%
  select(log_gold, real_rate_10y, log_usd, yoy_cpi, log_m2,
         log_oil, ret_sp500, hy_spread) %>%
  drop_na() %>%
  as.ts()

exog_data <- df_model %>%
  filter(!is.na(log_gold) & !is.na(real_rate_10y) & !is.na(log_usd) &
         !is.na(yoy_cpi) & !is.na(log_m2) & !is.na(log_oil) &
         !is.na(ret_sp500) & !is.na(hy_spread)) %>%
  select(gpr, epu, qe_era, covid_shock) %>%
  as.matrix()

# Lag selection
lag_select <- VARselect(var_data, lag.max = 12, type = "const",
                        exogen = exog_data)
optimal_lag <- lag_select$selection["AIC(n)"]
cat(paste("\nOptimal VAR lag (AIC):", optimal_lag, "\n"))
print(lag_select$criteria)

# Estimate VAR
var_model <- VAR(var_data,
                 p       = optimal_lag,
                 type    = "const",
                 exogen  = exog_data)

cat("\n══ VAR Model Summary ══\n")
tryCatch(print(summary(var_model)),
         error = function(e) message("NOTE: VAR summary failed - skipping: ", e$message))

# VAR diagnostics
cat("\n── Serial Correlation Test (Portmanteau) ──\n")
tryCatch(print(serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")),
         error = function(e) message("NOTE: VAR serial.test failed - skipping: ", e$message))
cat("\n── Normality Test ──\n")
tryCatch(print(normality.test(var_model)),
         error = function(e) message("NOTE: VAR normality.test failed - skipping: ", e$message))
cat("\n── Stability (all eigenvalues < 1?) ──\n")
tryCatch(print(roots(var_model)),
         error = function(e) message("NOTE: VAR roots() failed - skipping: ", e$message))

# Impulse Response Functions (IRF)
# How does gold respond to shocks in real rates, USD, CPI?
irf_ok <- tryCatch({
  irf_real_rate <<- irf(var_model, impulse = "real_rate_10y",
                       response = "log_gold", n.ahead = 24,
                       boot = TRUE, ci = 0.95, runs = 500)

  irf_usd <<- irf(var_model, impulse = "log_usd",
                 response = "log_gold", n.ahead = 24,
                 boot = TRUE, ci = 0.95, runs = 500)

  irf_cpi <<- irf(var_model, impulse = "yoy_cpi",
                 response = "log_gold", n.ahead = 24,
                 boot = TRUE, ci = 0.95, runs = 500)
  TRUE
}, error = function(e) { message("NOTE: IRF estimation failed - skipping: ", e$message); FALSE })

if (irf_ok) {
  tryCatch({
    par(mfrow = c(1, 3))
    plot(irf_real_rate, main = "Gold Response to Real Rate Shock")
    plot(irf_usd,       main = "Gold Response to USD Shock")
    plot(irf_cpi,       main = "Gold Response to Inflation Shock")
    par(mfrow = c(1, 1))
  }, error = function(e) message("NOTE: IRF plot failed - skipping: ", e$message))
}

# Forecast Error Variance Decomposition (FEVD)
fevd_result <- tryCatch(fevd(var_model, n.ahead = 24),
                         error = function(e) { message("NOTE: FEVD estimation failed - skipping: ", e$message); NULL })
if (!is.null(fevd_result)) {
  tryCatch(plot(fevd_result),
           error = function(e) message("NOTE: FEVD plot failed - skipping: ", e$message))
}

# VAR-based forecast (12-month ahead)
var_forecast <- tryCatch(
  predict(var_model, n.ahead = 12,
          dumvar = matrix(
            rep(colMeans(exog_data, na.rm = TRUE), 12),
            nrow = 12, byrow = TRUE
          )),
  error = function(e) { message("NOTE: VAR forecast failed - skipping: ", e$message); NULL }
)
if (!is.null(var_forecast)) {
  tryCatch(plot(var_forecast, names = "log_gold"),
           error = function(e) message("NOTE: VAR forecast plot failed - skipping: ", e$message))
}

# ══════════════════════════════════════════════════════════
# 10. MODEL 3 — ARDL BOUNDS TEST (Pesaran et al. 2001)
#    Tests for cointegration between gold and macro variables
#    without requiring them all to be I(1)
# ══════════════════════════════════════════════════════════

# Restrict to complete cases for ARDL
df_ardl <- df_model %>%
  select(log_gold, real_rate_10y, log_usd, yoy_cpi,
         log_m2, log_oil, epu, gpr, hy_spread, debt_gdp) %>%
  drop_na() %>%
  as.data.frame()

# Auto-ARDL: automatically selects optimal lag structure via AIC
ardl_ok <- tryCatch({
  ardl_auto <<- auto_ardl(
    formula = log_gold ~ real_rate_10y + log_usd + yoy_cpi +
                         log_m2 + log_oil + epu + gpr + hy_spread + debt_gdp,
    data     = df_ardl,
    max_order = 4,
    selection = "AIC"
  )
  TRUE
}, error = function(e) { message("NOTE: auto_ardl failed - skipping ARDL section: ", e$message); FALSE })

if (ardl_ok) {
  cat("\nBest ARDL order (AIC):\n")
  print(ardl_auto$best_order)

  # Extract best model
  ardl_model <- ardl_auto$best_model
  cat("\n══ ARDL Model Summary ══\n")
  tryCatch(print(summary(ardl_model)),
           error = function(e) message("NOTE: ARDL summary failed - skipping: ", e$message))

  # Bounds test for cointegration (H0: no long-run relationship)
  bounds_test <- tryCatch(bounds_f_test(ardl_model, case = 3),
                           error = function(e) { message("NOTE: ARDL bounds_f_test failed - skipping: ", e$message); NULL })
  if (!is.null(bounds_test)) {
    cat("\n── ARDL Bounds F-Test (Pesaran et al. 2001) ──\n")
    print(bounds_test)

    # Long-run coefficients (if cointegrated)
    tryCatch({
      if (bounds_test$tab["t", "I(1)"] > bounds_test$statistic[[1]]) {
        cat("\n⚠ Cannot reject null of no cointegration at 5%.\n")
      } else {
        lr_coefs <- multipliers(ardl_model, type = "lr")
        cat("\n── Long-Run Multipliers ──\n")
        print(lr_coefs)
      }
    }, error = function(e) message("NOTE: Long-run multipliers failed - skipping: ", e$message))
  }

  # Error Correction Model (ECM) — short-run dynamics toward equilibrium
  tryCatch({
    ecm_model <- uecm(ardl_model)
    cat("\n══ ECM Model Summary ══\n")
    print(summary(ecm_model))
  }, error = function(e) message("NOTE: ECM (uecm) failed - skipping: ", e$message))
} else {
  ardl_model <- NULL
}

# ══════════════════════════════════════════════════════════
# 11. MODEL 4 — RANDOM FOREST
#    Uses lagged predictors (no look-ahead bias)
#    Train: 1975–2014 | Test: 2015–2024
# ══════════════════════════════════════════════════════════

features <- c(
  "real_rate_10y_L1", "log_usd_L1",   "yoy_cpi_L1",    "yoy_m2_L1",
  "log_oil",           "ret_sp500_L1", "vix_L1",         "epu_L1",
  "gpr_L1",            "hy_spread_L1", "yield_curve",    "debt_gdp",
  "qe_era",            "covid_shock",  "zirp",           "election_yr",
  "post_911",          "russia_ukraine","month",

  # Physical / industrial demand-side indicators (jewelry, central
  # bank, consumer electronics). NB: only non-NA from ~1992 onward,
  # so adding these shrinks the effective RF/XGBoost training window
  # from 1975+ to ~1992+ — still ~400 months, plenty for tree models.
  "jewelry_demand_L1", "cb_demand_L1", "cb_net_buyer_L1",
  "yoy_tech_demand_L1", "yoy_electronics_ip_L1"
)

df_ml <- df %>%
  select(date, log_gold, ret_gold, all_of(features)) %>%
  mutate(log_gold_lag1 = lag(log_gold, 1)) %>%
  drop_na()

message("RF/XGBoost training panel after adding demand indicators: ",
        nrow(df_ml), " months, ", min(df_ml$date), " to ", max(df_ml$date),
        " (effective start shifts to ~1992 because of WGC demand data availability)")

# ─────────────────────────────────────────────────────────────
# TARGET-VARIABLE FIX: train on the MONTHLY RETURN (ret_gold =
# Δlog(gold), already computed in Section 5) instead of the raw
# log-level price. Tree models (RF, XGBoost) predict values bounded
# by the range of TARGET values seen during training. With log_gold
# as the target and a pre-2015 training window, the model never sees
# target values anywhere near today's ~$3800+ gold price, so it can't
# extrapolate and collapses to predictions near the top of its
# training range (~$600s) - this caused the negative test R²
# (-1.83 RF / -1.61 XGBoost) and the flat, wrong 12-month forecast
# in the original run. Monthly returns are roughly stationary, so a
# tree model trained on historical returns CAN generalize: a "real
# rate fell, USD weakened" feature pattern produces a similar %
# return on gold whether gold is at $400 or $3800. Price-level
# forecasts are reconstructed by compounding predicted returns onto
# the actual (or anchor) price, never by asking the tree model for a
# price level directly.
# ─────────────────────────────────────────────────────────────

# Train / test split
train <- df_ml %>% filter(date <= as.Date("2014-12-31"))
test  <- df_ml %>% filter(date >= as.Date("2015-01-01"))

X_train <- train %>% select(all_of(features))
y_train <- train$ret_gold
X_test  <- test  %>% select(all_of(features))
y_test  <- test$ret_gold

set.seed(42)

# Random Forest
rf_model <- randomForest(
  x          = X_train,
  y          = y_train,
  ntree      = 1000,
  mtry       = floor(sqrt(length(features))),
  importance = TRUE,
  nodesize   = 5
)

cat("\nRandom Forest OOB summary:\n")
print(rf_model)

# Variable importance
tryCatch(
  varImpPlot(rf_model, main = "Variable Importance — Random Forest Gold Model",
             col = "#D4AF37", pch = 16),
  error = function(e) message("NOTE: RF varImpPlot failed - skipping: ", e$message)
)

imp_df <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`))
print(imp_df)

# Predictions on test set (these are predicted MONTHLY RETURNS, Δlog(gold))
rf_pred   <- predict(rf_model, X_test)
rf_rmse   <- sqrt(mean((rf_pred - y_test)^2))
rf_mae    <- mean(abs(rf_pred - y_test))
rf_r2     <- 1 - sum((rf_pred - y_test)^2) / sum((y_test - mean(y_test))^2)

# Reconstruct one-step-ahead PRICE LEVELS from predicted returns applied
# to the actual prior month's price (no compounding of forecast error -
# standard one-step-ahead test evaluation), giving a price-space MAPE.
rf_price_pred    <- exp(test$log_gold_lag1 + rf_pred)
rf_price_actual  <- exp(test$log_gold)
rf_mape   <- mean(abs((rf_price_pred - rf_price_actual) / rf_price_actual)) * 100

cat("\n── Random Forest Test Set Performance (2015–2024) ──\n")
cat(sprintf("RMSE (return): %.4f | MAE (return): %.4f | R²: %.4f | MAPE (price): %.2f%%\n",
            rf_rmse, rf_mae, rf_r2, rf_mape))

# ══════════════════════════════════════════════════════════
# 12. MODEL 5 — XGBOOST (Gradient Boosting)
# ══════════════════════════════════════════════════════════

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest  <- xgb.DMatrix(data = as.matrix(X_test),  label = y_test)

xgb_params <- list(
  booster          = "gbtree",
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 5,
  lambda           = 1.0
)

set.seed(42)
xgb_cv <- xgb.cv(
  params   = xgb_params,
  data     = dtrain,
  nrounds  = 1000,
  nfold    = 5,
  metrics  = "rmse",
  early_stopping_rounds = 50,
  verbose  = 0
)

best_rounds <- xgb_cv$best_iteration
cat(paste("\nXGBoost best rounds (5-fold CV):", best_rounds, "\n"))

xgb_model <- xgb.train(
  params   = xgb_params,
  data     = dtrain,
  nrounds  = best_rounds,
  watchlist = list(train = dtrain, test = dtest),
  verbose  = 0
)

# XGBoost importance
xgb_imp <- xgb.importance(feature_names = features, model = xgb_model)
tryCatch(
  xgb.plot.importance(xgb_imp, top_n = 15,
                      main = "XGBoost Feature Importance — Gold Price Model"),
  error = function(e) message("NOTE: XGBoost importance plot failed - skipping: ", e$message)
)
print(xgb_imp)

# XGBoost test predictions (predicted MONTHLY RETURNS, Δlog(gold))
xgb_pred <- predict(xgb_model, dtest)
xgb_rmse <- sqrt(mean((xgb_pred - y_test)^2))
xgb_mae  <- mean(abs(xgb_pred - y_test))
xgb_r2   <- 1 - sum((xgb_pred - y_test)^2) / sum((y_test - mean(y_test))^2)

xgb_price_pred   <- exp(test$log_gold_lag1 + xgb_pred)
xgb_price_actual <- exp(test$log_gold)
xgb_mape <- mean(abs((xgb_price_pred - xgb_price_actual) / xgb_price_actual)) * 100

cat("\n── XGBoost Test Set Performance (2015–2024) ──\n")
cat(sprintf("RMSE (return): %.4f | MAE (return): %.4f | R²: %.4f | MAPE (price): %.2f%%\n",
            xgb_rmse, xgb_mae, xgb_r2, xgb_mape))

# ══════════════════════════════════════════════════════════
# 13. MODEL 6 — ARIMAX (ARIMA with Exogenous Regressors)
#    Fits ARIMA on log_gold with macro regressors as xreg
# ══════════════════════════════════════════════════════════

arimax_xreg <- df_model %>%
  select(real_rate_10y, log_usd, yoy_cpi, log_m2, log_oil,
         epu, gpr, qe_era, covid_shock) %>%
  drop_na() %>%
  as.matrix()

arimax_y <- df_model %>%
  filter(complete.cases(select(., real_rate_10y, log_usd, yoy_cpi,
                                log_m2, log_oil, epu, gpr))) %>%
  pull(log_gold)

arimax_model <- auto.arima(
  arimax_y,
  xreg       = arimax_xreg,
  seasonal   = TRUE,
  stepwise   = FALSE,
  approximation = FALSE,
  ic         = "aicc",
  max.p      = 4, max.q = 4, max.P = 2, max.Q = 2
)
cat("\n══ ARIMAX Model Summary ══\n")
print(summary(arimax_model))
tryCatch(checkresiduals(arimax_model),
         error = function(e) message("NOTE: checkresiduals plot failed - skipping: ", e$message))

# ══════════════════════════════════════════════════════════
# 14. MODEL COMPARISON
# ══════════════════════════════════════════════════════════

# Collect OLS predictions for same test period
ols_test_data <- df_diff %>%
  filter(date >= as.Date("2015-01-01")) %>%
  drop_na()

ols_pred_diff <- predict(ols_diff, newdata = ols_test_data)
ols_r2_test   <- 1 - sum((ols_pred_diff - ols_test_data$d_log_gold)^2) /
                     sum((ols_test_data$d_log_gold - mean(ols_test_data$d_log_gold))^2)

comparison_table <- tibble(
  Model           = c("OLS (First-Diff)", "ARIMAX", "VAR (equation)",
                      "Random Forest", "XGBoost"),
  `R² (Test)`     = c(round(ols_r2_test, 4), NA, NA,
                       round(rf_r2, 4),   round(xgb_r2, 4)),
  `RMSE (log)`    = c(NA, NA, NA, round(rf_rmse, 4), round(xgb_rmse, 4)),
  `MAPE (%)`      = c(NA, NA, NA, round(rf_mape, 2), round(xgb_mape, 2)),
  `AIC`           = c(AIC(ols_diff), AIC(arimax_model),
                       AIC(var_model), NA, NA),
  `Cointegration` = c("N/A", "N/A", "N/A", "N/A", "N/A")
)

print(comparison_table)

# ══════════════════════════════════════════════════════════
# 15. 12-MONTH AHEAD FORECAST (XGBoost — best model)
# ══════════════════════════════════════════════════════════

last_obs <- df %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  select(all_of(features))

anchor_date     <- df %>% filter(date == max(date, na.rm = TRUE)) %>% pull(date)
anchor_log_gold <- df %>% filter(date == max(date, na.rm = TRUE)) %>% pull(log_gold)

# Forecast by predicting the MONTHLY RETURN each step and compounding it
# onto the actual current price (anchor_log_gold), rather than asking the
# tree model for an absolute price level directly - see the target-
# variable fix note in Section 11 for why the latter produced a flat,
# wildly-wrong forecast clamped near the model's pre-2015 training range.
future_preds_ret <- numeric(12)
future_log_gold  <- numeric(12)
running_log_gold <- anchor_log_gold
for (i in 1:12) {
  pred_ret <- predict(xgb_model, xgb.DMatrix(as.matrix(last_obs)))
  future_preds_ret[i] <- pred_ret
  running_log_gold    <- running_log_gold + pred_ret
  future_log_gold[i]  <- running_log_gold
  last_obs$month <- (last_obs$month %% 12) + 1
}

future_dates  <- seq.Date(anchor_date, by = "month", length.out = 13)[-1]
forecast_df   <- tibble(
  date      = future_dates,
  ret_pred  = future_preds_ret,
  log_pred  = future_log_gold,
  gold_pred = exp(future_log_gold)
)

cat("\n══ 12-Month Gold Price Forecast (XGBoost, return-compounded) ══\n")
cat(sprintf("Anchor date: %s | Anchor price: $%.2f\n", anchor_date, exp(anchor_log_gold)))
print(forecast_df)

# ══════════════════════════════════════════════════════════
# 16. FINAL PLOT — ACTUAL VS PREDICTED + FORECAST
# ══════════════════════════════════════════════════════════

history_plot <- df %>%
  filter(date >= as.Date("2010-01-01")) %>%
  select(date, gold)

test_plot <- test %>%
  mutate(gold_rf_pred  = exp(log_gold_lag1 + rf_pred),
         gold_xgb_pred = exp(log_gold_lag1 + xgb_pred))

tryCatch({
  print(ggplot() +
    geom_line(data = history_plot,
              aes(date, gold, color = "Actual"), linewidth = 0.9) +
    geom_line(data = test_plot,
              aes(date, gold_xgb_pred, color = "XGBoost Fitted"),
              linewidth = 0.8, linetype = "dashed") +
    geom_line(data = test_plot,
              aes(date, gold_rf_pred, color = "RF Fitted"),
              linewidth = 0.8, linetype = "dotted") +
    geom_ribbon(data = forecast_df,
                aes(date,
                    ymin = gold_pred * 0.93,
                    ymax = gold_pred * 1.07,
                    fill = "95% CI"),
                alpha = 0.2) +
    geom_line(data = forecast_df,
              aes(date, gold_pred, color = "12M Forecast"),
              linewidth = 1.1) +
    scale_color_manual(values = c(
      "Actual"       = "#D4AF37",
      "XGBoost Fitted" = "#2166AC",
      "RF Fitted"    = "#4DAF4A",
      "12M Forecast" = "#E41A1C"
    )) +
    scale_fill_manual(values = c("95% CI" = "#E41A1C")) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(title    = "Gold Price: Actual vs Model Predictions & 12-Month Forecast",
         subtitle = "XGBoost & Random Forest | Train: 1975–2014 | Test: 2015–2024",
         x = NULL, y = "USD / troy oz",
         color = NULL, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom"))

  ggsave("gold_forecast.png", width = 13, height = 7)
}, error = function(e) message("NOTE: Final forecast plot failed - skipping: ", e$message))

cat("\n✅ Analysis complete. Output files:\n")
cat("  gold_eda.png\n")
cat("  gold_correlation.png\n")
cat("  gold_forecast.png\n")

############################################################
# 17. MONTE CARLO SIMULATION — MULTI-VARIABLE CORRELATED SHOCKS
#
# Simulates N forward paths of the macro state vector using a
# multivariate-normal innovation process (Cholesky / mvrnorm),
# with mean-reversion (OU) dynamics for level variables and a
# Poisson "crisis" regime that fattens tails (war/shock spikes
# in GPR, EPU, and credit spreads). Gold path is generated each
# month from the fitted OLS first-difference coefficients
# (Section 8) applied to the simulated macro shocks, plus an
# idiosyncratic residual drawn from the model's own residual SD.
############################################################

library(MASS)      # mvrnorm
set.seed(2026)

# ── 17a. Calibration: order = [realRate, dlogUSD, cpiYoY, dlogM2, dlogOil, EPU, GPR, dHYspread]
mc_vars   <- c("real_rate","d_log_usd","cpi_yoy","d_log_m2","d_log_oil","epu","gpr","d_hy_spread")
n_vars    <- length(mc_vars)

# Calibrate the Monte Carlo shock distribution DIRECTLY from this run's
# actual fitted data (df_diff - the same first-difference panel used to
# fit ols_diff above), rather than illustrative/guessed constants. This
# matters in practice: an earlier hardcoded calibration assumed
# d_hy_spread had a monthly std dev of ~40 (appropriate for the ICE
# BofA HY OAS series, quoted in basis points), but Section 3 substitutes
# BAA10Y (a percentage-point credit spread, monthly sd ~0.21) for that
# series due to a FRED licensing gap. The ~190x scale mismatch between
# the hardcoded 40 and the real ~0.21, combined with beta["d_hy_spread"],
# caused simulated gold paths to explode into the billions of dollars
# once compounded over 24 months. Pulling sd/correlation directly from
# df_diff eliminates this entire class of bug by construction - the
# calibration always matches whatever series this run actually
# downloaded and fit, with no manual re-tuning required.
mc_diff_cols <- c("d_real_rate","d_log_usd","d_yoy_cpi","d_log_m2","d_log_oil","d_epu","d_gpr","d_hy_spread")

if (all(mc_diff_cols %in% colnames(df_diff))) {
  mc_sd        <- sapply(df_diff[mc_diff_cols], sd, na.rm = TRUE)
  names(mc_sd) <- mc_vars
  mc_corr      <- cor(df_diff[mc_diff_cols], use = "complete.obs")
  rownames(mc_corr) <- mc_vars
  colnames(mc_corr) <- mc_vars
  cat("\n── Monte Carlo shock calibration (live, from df_diff) ──\n")
  cat("Std devs:\n"); print(mc_sd)
  cat("Correlation matrix:\n"); print(round(mc_corr, 2))
} else {
  message("NOTE: one or more first-difference columns needed for live Monte ",
          "Carlo calibration are missing from df_diff - falling back to ",
          "illustrative constants for mc_sd/mc_corr.")
  mc_sd     <- c(0.15, 0.015, 0.30, 0.0040, 0.080, 20, 25, 40)   # monthly std devs
  names(mc_sd) <- mc_vars

  mc_corr <- matrix(c(
    1.00, 0.35,-0.20,-0.10, 0.05, 0.10, 0.15, 0.20,
    0.35, 1.00,-0.10,-0.15,-0.40, 0.05, 0.10, 0.15,
   -0.20,-0.10, 1.00, 0.20, 0.25, 0.05, 0.05, 0.10,
   -0.10,-0.15, 0.20, 1.00, 0.10, 0.00, 0.00, 0.05,
    0.05,-0.40, 0.25, 0.10, 1.00, 0.10, 0.20, 0.15,
    0.10, 0.05, 0.05, 0.00, 0.10, 1.00, 0.45, 0.30,
    0.15, 0.10, 0.05, 0.00, 0.20, 0.45, 1.00, 0.35,
    0.20, 0.15, 0.10, 0.05, 0.15, 0.30, 0.35, 1.00
  ), nrow = n_vars, byrow = TRUE, dimnames = list(mc_vars, mc_vars))
}

# Ensure mc_corr is a valid (symmetric, positive-semidefinite) correlation
# matrix before building the covariance matrix. Empirical correlation
# matrices estimated from a finite sample across 8 variables can
# occasionally be just barely non-PSD due to sampling noise/rounding;
# project onto the nearest PSD matrix so mvrnorm() never errors out.
mc_corr <- (mc_corr + t(mc_corr)) / 2   # force exact symmetry
eig <- eigen(mc_corr, symmetric = TRUE)
if (any(eig$values < 1e-8)) {
  eig$values[eig$values < 1e-8] <- 1e-8
  mc_corr <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  d <- sqrt(diag(mc_corr))
  mc_corr <- mc_corr / outer(d, d)
  dimnames(mc_corr) <- list(mc_vars, mc_vars)
}

mc_cov <- diag(mc_sd) %*% mc_corr %*% diag(mc_sd)   # covariance matrix
dimnames(mc_cov) <- list(mc_vars, mc_vars)

# Mean-reversion targets & speeds (OU process) for LEVEL variables
mr_target <- c(real_rate = 1.5, cpi_yoy = 2.5, epu = 100, gpr = 100, hy_spread = 400)
mr_kappa  <- c(real_rate = 0.06, cpi_yoy = 0.05, epu = 0.15, gpr = 0.12, hy_spread = 0.08)

# Drift terms for cumulative/random-walk variables
drift <- c(d_log_usd = 0.0000, d_log_m2 = 0.0030, d_log_oil = 0.0015)

# Crisis regime: monthly probability of a geopolitical/financial shock.
# Sized in STANDARD-DEVIATION units (multiples of each variable's own
# live-calibrated mc_sd) rather than fixed absolute magnitudes. The
# previous hardcoded constants (gpr=50, epu=40, hy_spread=100) were
# illustrative leftovers from before mc_sd/beta were switched to live
# calibration; combined with the live (correctly-scaled) ols_diff beta
# on d_hy_spread, a fixed jump of 100 produced a single-month d_log_gold
# shock of beta_hy_spread * 100 ≈ -3.3 (i.e. gold collapsing to ~4% of
# its prior value in one month) - the root cause of the Monte Carlo
# terminal distribution's implausible left tail ($11-16 5th percentile,
# ~99.7% VaR). Expressing the jump as a multiple of mc_sd keeps the
# crisis shock proportional to each variable's actual historical
# volatility regardless of its units, so it stays economically sane
# (a rare ~3-4 SD event) no matter what gets re-calibrated upstream.
p_crisis        <- 0.015
crisis_jump_sd  <- c(gpr = 3, epu = 3, hy_spread = 3)

# Fitted OLS first-difference coefficients (Section 8) — pulled LIVE from
# the actual fitted ols_diff model object for this run (not hardcoded), so
# the simulation always reflects whatever data this run downloaded rather
# than a stale illustrative snapshot from an earlier session.
ols_coefs <- coef(ols_diff)
get_coef  <- function(nm) if (nm %in% names(ols_coefs)) unname(ols_coefs[nm]) else 0
beta <- c(
  intercept    = get_coef("(Intercept)"),
  real_rate    = get_coef("d_real_rate"),
  d_log_usd    = get_coef("d_log_usd"),
  cpi_yoy      = get_coef("d_yoy_cpi"),
  d_log_m2     = get_coef("d_log_m2"),
  d_log_oil    = get_coef("d_log_oil"),
  epu          = get_coef("d_epu"),
  gpr          = get_coef("d_gpr"),
  d_hy_spread  = get_coef("d_hy_spread")
)
cat("\n── Monte Carlo beta coefficients (live, from ols_diff) ──\n")
print(beta)
resid_sd <- sqrt(1 - summary(ols_diff)$r.squared) * sd(df_diff$d_log_gold, na.rm = TRUE)

# ── 17b. Anchor to latest observed values ──
latest        <- tail(df_model, 1)
gold0         <- exp(latest$log_gold)
state0        <- c(
  real_rate   = latest$real_rate_10y,
  d_log_usd   = 0,
  cpi_yoy     = latest$yoy_cpi,
  d_log_m2    = 0,
  d_log_oil   = 0,
  epu         = latest$epu,
  gpr         = latest$gpr,
  d_hy_spread = 0
)

# ── 17c. Simulation engine ──
n_sims    <- 10000
n_months  <- 24

gold_paths <- matrix(NA_real_, nrow = n_sims, ncol = n_months)

for (s in 1:n_sims) {
  state    <- state0
  log_gold <- log(gold0)

  for (m in 1:n_months) {
    shocks <- mvrnorm(1, mu = rep(0, n_vars), Sigma = mc_cov)
    # CRITICAL FIX: MASS::mvrnorm() does NOT propagate dimnames from Sigma
    # onto its return value when n = 1 (it returns a bare unnamed numeric
    # vector). Every line below indexes `shocks` by name (e.g.
    # shocks["gpr"], shocks["real_rate"]) - without this explicit name
    # assignment, ALL of those lookups silently return NA, which then
    # contaminates d_real_rate/d_cpi_yoy/d_epu/d_gpr/d_hy/d_logusd/
    # d_logm2/d_logoil -> d_log_gold -> log_gold for every single month of
    # every single simulation. This was the root cause of the Monte Carlo
    # terminal distribution being 100% NaN (240000 of 240000 cells).
    names(shocks) <- mc_vars

    # Crisis regime jump (fat tails for GPR/EPU/credit spread)
    if (runif(1) < p_crisis) {
      shocks["gpr"]         <- shocks["gpr"]         + crisis_jump_sd["gpr"]       * mc_sd["gpr"]
      shocks["epu"]         <- shocks["epu"]         + crisis_jump_sd["epu"]       * mc_sd["epu"]
      shocks["d_hy_spread"] <- shocks["d_hy_spread"] + crisis_jump_sd["hy_spread"] * mc_sd["d_hy_spread"]
    }

    # Mean-reverting level updates
    d_real_rate <- mr_kappa["real_rate"] * (mr_target["real_rate"] - state["real_rate"]) + shocks["real_rate"]
    d_cpi_yoy   <- mr_kappa["cpi_yoy"]   * (mr_target["cpi_yoy"]   - state["cpi_yoy"])   + shocks["cpi_yoy"]
    d_epu       <- mr_kappa["epu"]       * (mr_target["epu"]       - state["epu"])       + shocks["epu"]
    d_gpr       <- mr_kappa["gpr"]       * (mr_target["gpr"]       - state["gpr"])       + shocks["gpr"]
    d_hy        <- mr_kappa["hy_spread"] * (mr_target["hy_spread"] - state["d_hy_spread"]*0 - 400) + shocks["d_hy_spread"]

    d_logusd <- drift["d_log_usd"] + shocks["d_log_usd"]
    d_logm2  <- drift["d_log_m2"]  + shocks["d_log_m2"]
    d_logoil <- drift["d_log_oil"] + shocks["d_log_oil"]

    state["real_rate"] <- state["real_rate"] + d_real_rate
    state["cpi_yoy"]   <- state["cpi_yoy"]   + d_cpi_yoy
    state["epu"]       <- state["epu"]       + d_epu
    state["gpr"]       <- state["gpr"]       + d_gpr

    # Gold return this month from regression
    d_log_gold <- beta["intercept"] +
      beta["real_rate"]   * d_real_rate +
      beta["d_log_usd"]   * d_logusd +
      beta["cpi_yoy"]     * d_cpi_yoy +
      beta["d_log_m2"]    * d_logm2 +
      beta["d_log_oil"]   * d_logoil +
      beta["epu"]         * d_epu +
      beta["gpr"]         * d_gpr +
      beta["d_hy_spread"] * d_hy +
      rnorm(1, 0, resid_sd)

    log_gold <- log_gold + d_log_gold
    gold_paths[s, m] <- exp(log_gold)
  }
}

# ── 17d. Summarize: percentile fan chart ──
n_na_paths <- sum(!is.finite(gold_paths))
if (n_na_paths > 0) {
  message("NOTE: ", n_na_paths, " of ", length(gold_paths),
          " simulated gold_paths cells are NA/non-finite (likely a ",
          "stray NA in beta/resid_sd) - using na.rm = TRUE for summaries.")
}
percentiles <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
fan_chart   <- apply(gold_paths, 2, quantile, probs = percentiles, na.rm = TRUE)
rownames(fan_chart) <- paste0("p", percentiles * 100)
fan_df <- as.data.frame(t(fan_chart)) %>%
  mutate(month = 1:n_months) %>%
  pivot_longer(-month, names_to = "percentile", values_to = "price")

tryCatch({
  print(ggplot(fan_df, aes(month, price, color = percentile)) +
    geom_line(linewidth = 0.9) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(title = "Monte Carlo Gold Price Simulation — 24-Month Fan Chart",
         subtitle = paste0(n_sims, " simulations | Anchor: $", round(gold0, 0)),
         x = "Months Ahead", y = "USD / troy oz") +
    theme_minimal(base_size = 13))
  ggsave("gold_montecarlo_fanchart.png", width = 12, height = 7)
}, error = function(e) message("NOTE: Fan chart plot failed - skipping: ", e$message))

# ── 17e. Terminal distribution (month 24) ──
terminal <- gold_paths[, n_months]
cat("\n══ Monte Carlo Terminal Distribution (24 months ahead) ══\n")
cat(sprintf("Mean:    $%.0f\n", mean(terminal, na.rm = TRUE)))
cat(sprintf("Median:  $%.0f\n", median(terminal, na.rm = TRUE)))
cat(sprintf("Std Dev: $%.0f\n", sd(terminal, na.rm = TRUE)))
cat(sprintf("5th pct: $%.0f | 95th pct: $%.0f\n", quantile(terminal, .05, na.rm = TRUE), quantile(terminal, .95, na.rm = TRUE)))
cat(sprintf("VaR 5%% (loss from anchor): $%.0f (%.1f%%)\n",
            gold0 - quantile(terminal, .05, na.rm = TRUE), (1 - quantile(terminal, .05, na.rm = TRUE) / gold0) * 100))
cat(sprintf("P(gold > $5000 in 24mo): %.1f%%\n", mean(terminal > 5000, na.rm = TRUE) * 100))
cat(sprintf("P(gold < $3500 in 24mo): %.1f%%\n", mean(terminal < 3500, na.rm = TRUE) * 100))

tryCatch({
  print(ggplot(data.frame(terminal), aes(terminal)) +
    geom_histogram(bins = 60, fill = "#C9A84C", color = "white", alpha = 0.9) +
    geom_vline(xintercept = gold0, color = "navy", linetype = "dashed", linewidth = 1) +
    labs(title = "Terminal Gold Price Distribution (24 Months)",
         subtitle = paste0(n_sims, " Monte Carlo paths"),
         x = "USD / troy oz", y = "Frequency") +
    theme_minimal(base_size = 13))
  ggsave("gold_montecarlo_terminal.png", width = 10, height = 6)
}, error = function(e) message("NOTE: Terminal distribution plot failed - skipping: ", e$message))

cat("\n✅ Monte Carlo complete. Output files:\n")
cat("  gold_montecarlo_fanchart.png\n")
cat("  gold_montecarlo_terminal.png\n")

############################################################
# 18. SCENARIO ANALYSIS — comparative statics on ols_diff
#
# Applies named macro shocks to the LIVE fitted ols_diff coefficients
# (Section 8) to get a single-shock, first-order point estimate of the
# implied price move. This is deliberately NOT a re-estimated model and
# NOT a Monte Carlo draw — it is the simplest possible "what does this
# report's own validated model say about scenario X" calculation, used
# in the report's Scenario Analysis section. Because ols_diff's R² is
# ~0.21, treat every number here as a directional/magnitude indicator,
# not a forecast with quantified confidence — about 79% of monthly
# variance is unexplained by this model.
############################################################

run_scenario <- function(label, d_real_rate = 0, d_log_usd = 0, d_yoy_cpi = 0,
                          d_epu = 0, anchor_price = NULL) {
  cf <- coef(ols_diff)
  get_b <- function(nm) if (nm %in% names(cf)) unname(cf[nm]) else 0

  d_log_gold <- get_b("d_real_rate") * d_real_rate +
                get_b("d_log_usd")   * d_log_usd   +
                get_b("d_yoy_cpi")   * d_yoy_cpi   +
                get_b("d_epu")       * d_epu

  if (is.null(anchor_price)) anchor_price <- exp(tail(df_model$log_gold, 1))
  implied_price <- anchor_price * exp(d_log_gold)

  tibble(
    Scenario        = label,
    `d_log_gold`    = round(d_log_gold, 4),
    `Implied % chg` = paste0(round((exp(d_log_gold) - 1) * 100, 1), "%"),
    `Anchor price`  = round(anchor_price, 0),
    `Implied price` = round(implied_price, 0)
  )
}

anchor_now <- exp(tail(df_model$log_gold, 1))  # live anchor, whatever this run's latest month is

scenario_results <- bind_rows(
  run_scenario("1. Fed cutting cycle accelerates (real 10Y rate -100bp)",
               d_real_rate = -1.00, anchor_price = anchor_now),
  run_scenario("2. Dollar breaks down (log USD index -10%)",
               d_log_usd = -0.10, anchor_price = anchor_now),
  run_scenario("3. Dollar/rate bear case (log USD +8%, real rate +75bp)",
               d_real_rate = 0.75, d_log_usd = 0.08, anchor_price = anchor_now),
  run_scenario("4. Stagflation surprise (CPI YoY +1.8pp, counterintuitive sign)",
               d_yoy_cpi = 1.8, anchor_price = anchor_now),
  run_scenario("5. Geopolitical/policy-uncertainty spike (EPU +50pts)",
               d_epu = 50, anchor_price = anchor_now)
)

cat("\n══ Scenario Analysis (live, from this run's fitted ols_diff) ══\n")
print(scenario_results)
cat("\nNOTE: ols_diff R² =", round(summary(ols_diff)$r.squared, 4),
    "- these are single-shock point estimates holding everything else\n",
    "constant, not probabilistic forecasts. Scenario 4's negative sign is\n",
    "the model's own documented CPI-coefficient quirk (Section 8), not a\n",
    "general claim that inflation is bearish for gold. Central-bank-buying/\n",
    "de-dollarization acceleration is deliberately NOT included here as a\n",
    "quantified scenario - cb_demand and cb_net_buyer are statistically\n",
    "insignificant in ols_diff_demand (Section 8b), so this model has no\n",
    "validated coefficient to apply; see the report's Policy Evolution and\n",
    "Central Bank Gold sections for the qualitative case instead.\n")

write_csv(scenario_results, "gold_scenario_analysis.csv")
cat("\n✅ Scenario analysis complete. Output file: gold_scenario_analysis.csv\n")
