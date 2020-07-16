TEMP_LIB <- install_temp_github_package(
  repo = paste(c(ORG, REPO), collapse = "/"),
  ref = TAG
)

# TODO: remove the installed package with teardown_env() once testthat 3.0.0 is
# released
