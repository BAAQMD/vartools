requireNamespace("tibble")

test_data <-
  tibble::tibble(
    year = "CY1990",
    foo_id = 123L,
    foo_qty = 1000,
    foo_unit = "lb/day",
    baz_bap = "oranges",
    foo_bap = "apples")
