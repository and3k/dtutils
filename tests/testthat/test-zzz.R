test_that(".onAttach() warns about problematic datatable.na.strings setting", {
  expect_message(.onAttach(dtutils), "datatable.na.strings")

  old_value <- getOption("datatable.na.strings")
  options(datatable.na.strings = "")
  expect_message(.onAttach(dtutils), NA)
  options(datatable.na.strings = old_value)

  expect_known_output(
    cat(capture_message(.onAttach(dtutils))$message),
    "../known_outputs/package_startup_messages.txt"
  )
})
