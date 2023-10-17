
scores_to_table <- function(scoredat, trgt){
#bring into long format
  gsc <- gather_scores(scoredat, trgt)


  gsc <- gsc |>
    data.table::copy() |>
    gt(rowname_col = "combms", groupname_col = "horizon") |>
    cols_label(
      IMF = "IMF",
      ar = "ar",
      bvar = "bvar"
    )

  return(gsc)
}


gather_scores <- function(scoredat, trgt){

  gsc <- scoredat |>
    data.table::copy() |>
    .d(target == trgt) |>
    .d(,target := NULL) |>
    .d(, .(horizon, method, source, score, interval_score)) |>
    setnames("score", "sample_crps") |>
    melt(id.vars = c("horizon", "method", "source"), variable.name = "score_type", value.name = "score") |>
    .d(, combms := paste0(method,"_", score_type)) |>
    .d(, .(horizon, source, combms, score)) |>
    .d(, horizon := paste0("horizon = ", horizon)) |>
    .d(, score := round(score, 3)) |> #enough to distinguish
    dcast(horizon + combms ~ source, value.var = "score")


  return(gsc)
}


tile_plot <- function(scoredat, hor, scoretype = "interval"){

  if(scoretype == "interval"){

    plotname <- " - Weighted Interval Score"

  } else {

    plotname <- " - CRPS by sample"
  }

  plotdat <- scoredat |>
    .d(horizon == paste0("horizon = ", hor)) |>
    .d(grep(scoretype, combms)) |>
    melt(id.vars = c("horizon", "combms"), variable.name = "source", value.name = "score") |>
    .d(, c("method", "scorem", "extra") := tstrsplit(combms, "_", fixed=TRUE))

  plt <- ggplot(plotdat, aes(x = source, y = method)) +
    geom_tile(aes(fill = score)) +
    guides(fill = FALSE) +
    ggtitle(paste0("horizon = ", hor, plotname))

  return(plt)
}
