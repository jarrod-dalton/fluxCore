test_that("namespaced patches are flattened and applied without affecting flat patches", {
  schema <- c(
    default_patient_schema(),
    list(
      model_active = model_active_schema_var(c("ascvd", "hospital")),
      ascvd__ldl = list(default = 130, coerce = as.numeric, validate = function(x) length(x) == 1L && is.finite(x)),
      hospital__ldl_measured = list(default = NA_real_, coerce = as.numeric, validate = function(x) length(x) == 1L)
    )
  )

  p <- new_patient(init = list(ascvd__ldl = 130), schema = schema)

  # Flat update still works
  p$update(time = 1, event_type = "flat", changes = list(ascvd__ldl = 125))
  expect_equal(p$as_list(c("ascvd__ldl"))[[1]], 125)

  # Namespaced update updates multiple namespaces in one patch
  p$update(time = 2, event_type = "ns", changes = list(
    core = list(model_active = c(ascvd = TRUE, hospital = TRUE)),
    ascvd = list(ldl = 118),
    hospital = list(ldl_measured = 110)
  ))

  s <- p$as_list(c("model_active", "ascvd__ldl", "hospital__ldl_measured"))
  expect_true(is.logical(s$model_active))
  expect_true(all(names(s$model_active) %in% c("ascvd", "hospital")))
  expect_equal(s$ascvd__ldl, 118)
  expect_equal(s$hospital__ldl_measured, 110)
})
