This folder contains score files generated by the files here("scripts", "score-forecasts.R") and here("scripts", "score-forecasts-directionalsymmetric.R")

ci_scores.csv
	scores for the quantile forecasts generated by here("scripts", "make-forecasts.R")
	contains: 		WIS with decomposition, coverage deviation and coverage at the 0.5 and 0.8 central interval level 
				!!!SCORES ARE GENERATED BASED ON THE 0.5 AND 0.8 CENTRAL INTERVALS ONLY!!!! i.e. the 0.1, 0.25, 0.75 and 0.9 quantiles
	aggregation level: 	none (only aggregated over all target_years)
				scores are thus available at level source, error_method, method, country, target, horizon
	year set: 		for training set, 2001 onwards (to ensure same scoring basis for all estimation methods)
				for hold-out set, 2013 onwards
	notes:			bias and median are NA since the 0.5 quantile (median) was not generated/scored

****************************************************************************************************************************
FROM HERE ON ONWARDS, only deviations from the above are noted

ci_scores_allyears.csv
	year set: 		1990 onwards (leave-one-out is thus scored on more years (1990-1998) than the window methods)


ci_scores_avgcnt.csv
	aggregation level: 	additionally over countries 
				scores are thus available at level source, error_method, method, target, horizon

cvg_pooled.csv
	aggregation level:	additionally over countries and horizon 
				scores are thus available at level source, error_method, method, target
	notes: 			name change from the above stems from the fact that only the coverage levels are really interpretable after averaging over horizon
				other scores will be dominated by larger horizons due to the rising scale of the forecast errors

ci_scores_directionalsymmetric.csv
	scores for the experimentally generated quantile forecasts that are centered (as in absolute method) but are constrained to have the same length as the directional method
	also generated by here("scripts", "make-forecasts.R")

ci_scores_directionalsymmetric_avgcnt.csv
	same as directly above, but averaged over countries (see ci_scores_avgcnt.csv above)

cvg_pooled_directionalsymmetric.csv
	same again, but averaged over countries and horizon (see cvg_pooled.csv above)


bvar_****.csv
	scores for the quantile forecasts in here("benchmarks", "forecast_quantiles.csv"), which are benchmark forecasts generated by a BVAR model
	otherwise, same as respective files above


pointfc_scores.csv
	scores for the point forecasts from the IMF WEO and the benchmark (AR, BVAR) forecasts in here("benchmarks", "forecast_wide.csv")
	contains: 		Absolute Error, Squared Error
	aggregation level: 	none (only aggregated over target_years)
				scores are thus available at level source, country, target, horizon
				(point forecasts are not affected by error_method and method)
	year set: 		1999 onwards (could score all years here without loss of information, but done for consistency)



