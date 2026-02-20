
add_tail_covariates <- function(new_data) {
  
  # 1. TILFØJ DUMMY-KOLONNER FOR AT TILFREDSSTILLE TIDYMODELS ----
  # Disse ignoreres/slettes af vores step_rm(), men skal eksistere for at undgå fejl.
  new_data_padded <- new_data
  
  if (!"target" %in% names(new_data_padded)) {
    new_data_padded <- new_data_padded |> dplyr::mutate(target = NA_real_)
  }
  
  if (!"y_binary" %in% names(new_data_padded)) {
    new_data_padded <- new_data_padded |> dplyr::mutate(y_binary = as.factor("normal"))
  }
  
  # 2. indlæs og PAK UD (unbundle) trænede modeller fra disk ----
  m_freq <- bundle::unbundle(readr::read_rds("exploratory/oskar/freq_model.rds"))
  m_tail <- bundle::unbundle(readr::read_rds("exploratory/oskar/tail_model.rds"))
  
  # 3. forudsig sandsynlighed for spike (frekvens) ----
  prob_preds <- stats::predict(
    object = m_freq, 
    new_data = new_data_padded, 
    type = "prob"
  ) |> 
    dplyr::pull(.pred_spike)
  
  # 4. forudsig forventet pris i halen (skadesstørrelse) ----
  sev_preds <- stats::predict(
    object = m_tail, 
    new_data = new_data_padded
  ) |> 
    dplyr::pull(.pred)
  
  # 5. berig data med de nye covariater og ryd dummy-kolonner op ----
  out_data <- new_data |> 
    dplyr::mutate(
      feature_prob_spike = prob_preds,
      feature_expected_severity = sev_preds
    )
  
  return(out_data)
}