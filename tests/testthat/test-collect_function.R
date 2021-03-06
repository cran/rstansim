context("collect_simulations function should function correctly")

test_that("collect_simulations function fails correctly", {

  ## read in test stansim_simulation obj to test
  stansim_obj <-
    readRDS("objects/test_stansim.rds")


  # collection_name should be a character
  expect_error(collect_simulations(collection_name = 55),
               "collection_name must be of type character")

  expect_error(collect_simulations(collection_name = NULL),
               "collection_name must be of type character")

  # all non-collection name args must have proper class
  expect_error(
    collect_simulations(collection_name = "test",
            object = 55),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect_simulations(collection_name = "test",
            object = "test"),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect_simulations(collection_name = "test",
            object = stansim_obj,
            55),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect_simulations(collection_name = "test",
            object = stansim_obj,
            stansim_obj,
            stansim_obj,
            55),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  # all names should be different
  expect_error(
    collect_simulations(collection_name = "test",
            object = stansim_obj,
            stansim_obj),
    "The collection_name and simulation_name values of all arguments must be unique"
  )

  ## error if only 1 stansim_simulation is provided
  expect_error(
    collect_simulations(collection_name = "test",
            object = stansim_obj),
    "A single simulation cannot be used to make a collection."
  )

})

