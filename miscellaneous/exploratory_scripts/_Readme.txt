bvar_overprediction.R
	- investigates and plots overprediction instances for the BVAR model
	- resulting exemplary plot for Italy is in bvar_overprediction.pdf

compare-afterbefore-horizonshift.R
	- this file was run before and after correcting horizon sampling. Before, the analysis used data that was not available in real-time, namely it used forecast errors that had not realized yet for horizons > 1 year, e.g. the forecast error for target year 2023 when making a forecast for 2024 within the year 2023
	- results are in compare-after-before-horizonshift (not automatically generated)

corr_tv05tv1.R
	- investigates the correlation between the first and second releases for the truth value
	- result plot is in tv05tv1.png

investigate_error_direction.R
	- code that plots forecast intervals for loo and rolling window method, for both absolute and directional errors, as well as distribution of error values, for a given country
	- to rerun this, note that location of qufcs and scores (at top of file) might have changed

length_ci_directionalvsabsolute
	- empirically checks the proportion of cases where the absolute intervals are longer (for the currently generated quantile forecasts in directory uqimf/quantile_forecasts)


length_ci_dirvsabs: 
	- illustrate that ci lengths are the same in the limit
	- illustrate why they are dissimilar in small samples [note: probably wrong]
	- small simulation study that shows that they are dissimilar for a normal distribution

plot_horizon_violators.R
	- empirically checks how many times the horizon monotonicity condition is violated given the different methods 

plot_series_with_forecasts.R
	- small function to plot series and forecasts for single year, for all horizons, error methods, and target series

plot_shiny-vis.R
	- code that (roughly, might be a previous version) plots the output in the shiny app locally
	- manually set season and target in the beginning of the script