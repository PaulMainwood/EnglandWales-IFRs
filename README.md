# EnglandWales-IFRs
IFR estimates from ONS data using Bayesian Structural Time Series

Date sources: 

ONS incidence - https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/previousReleases 

ONS deaths: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveycumulativeincidenceofthepercentageofpeoplewhohavetestedpositiveforcovid19infectionsengland

Write-up of the approach and results here: https://paulmainwood.substack.com/p/using-a-sledgehammer-to-crack-a-peanut

Scripts written in R. Assuming you're using RStudio:

1) Run Download_incidence_EnglandWales.R and Download_ONSdeaths_EnglandWales.R to get the main files in your workspace.
2) If you want to optimise the lags, then run Optimise_Lags_IFR_EnglandWales.R and use the values spat out from that - you'll need to edit them into the final script before running.
3) If you trust my lags. go straight to IFREnglandWales.R to reproduce the analysis and figures in the substack.
