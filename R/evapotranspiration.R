#' Calcul de l'Évapotranspiration de Référence (ETo) - Méthode FAO-56
#'
#' Calcule l'évapotranspiration de référence journalière selon la méthode
#' de Penman-Monteith simplifiée (FAO-56).
#'
#' @param tmax Température maximale journalière (°C)
#' @param tmin Température minimale journalière (°C)
#' @param solar_rad Rayonnement solaire journalier (MJ/m²/jour)
#' @param wind_speed Vitesse du vent à 2m (m/s)
#' @param humidity Humidité relative moyenne (\%)
#' @param altitude Altitude du site (m). Par défaut 0.
#'
#' @return Valeur numérique de l'ETo en mm/jour
#'
#' @examples
#' # ETo pour une journée d'été en plaine
#' eto <- calc_eto(tmax = 32, tmin = 18, solar_rad = 22,
#'                 wind_speed = 2.5, humidity = 45)
#' print(eto)
#'
#' # Application sur un data.frame de données climatiques
#' data(climat_exemple)
#' climat_exemple$eto <- calc_eto(
#'   tmax = climat_exemple$tmax,
#'   tmin = climat_exemple$tmin,
#'   solar_rad = climat_exemple$rad,
#'   wind_speed = climat_exemple$vent,
#'   humidity = climat_exemple$hr
#' )
#'
#' @export
calc_eto <- function(tmax, tmin, solar_rad, wind_speed, humidity, altitude = 0) {

  # Validation des entrées
  if (any(tmax <= tmin)) {
    warning("tmax doit être supérieur à tmin pour toutes les observations")
  }
  if (any(humidity < 0 | humidity > 100)) {
    stop("L'humidité doit être entre 0 et 100 %")
  }

  # Température moyenne
  tmean <- (tmax + tmin) / 2

  # Pression atmosphérique (kPa) selon l'altitude
  P <- 101.3 * ((293 - 0.0065 * altitude) / 293) ^ 5.26

  # Constante psychrométrique
  gamma <- 0.000665 * P

  # Pente de la courbe de pression de vapeur saturante
  delta <- 4098 * (0.6108 * exp((17.27 * tmean) / (tmean + 237.3))) / (tmean + 237.3) ^ 2

  # Pression de vapeur saturante
  es <- (0.6108 * exp(17.27 * tmax / (tmax + 237.3)) +
           0.6108 * exp(17.27 * tmin / (tmin + 237.3))) / 2

  # Pression de vapeur réelle
  ea <- es * humidity / 100

  # Flux de chaleur du sol (simplifié = 0 pour journalier)
  G <- 0

  # Formule Penman-Monteith FAO-56
  eto <- (0.408 * delta * (solar_rad - G) +
            gamma * (900 / (tmean + 273)) * wind_speed * (es - ea)) /
    (delta + gamma * (1 + 0.34 * wind_speed))

  return(round(eto, 2))
}
