test_that("calc_eto retourne des valeurs positives dans des conditions normales", {
  result <- calc_eto(tmax = 30, tmin = 18, solar_rad = 20,
                     wind_speed = 2, humidity = 50)
  expect_true(result > 0)
})

test_that("calc_eto retourne une erreur si humidité hors plage", {
  expect_error(
    calc_eto(tmax = 30, tmin = 18, solar_rad = 20,
             wind_speed = 2, humidity = 150)
  )
})

test_that("calc_eto est cohérent (plus chaud = plus d'ETo)", {
  eto_chaud <- calc_eto(tmax = 38, tmin = 25, solar_rad = 28,
                        wind_speed = 3, humidity = 30)
  eto_frais  <- calc_eto(tmax = 20, tmin = 10, solar_rad = 12,
                         wind_speed = 1, humidity = 70)
  expect_true(eto_chaud > eto_frais)
})
