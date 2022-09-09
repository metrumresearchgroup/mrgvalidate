# package 1.0.0

## New features

- Added `new_function()` for inspecting the results of `old_function()` (#457)
- Added `hello()` and `goodbye()` for communicating with metrum employees (#448, #451)
- Added `yes()`, `no()`, `maybe()`, and `so()` for cases where users are unsure what do to (#454)

## Updates to existing functionality

- `old_function()` has new `.fetch` argument for fetching a previous analysis (#452)
- `get_param()` now returns result as a named list instead of a dataframe (#455)

## Bug fixes

- `get_param()` now transforms the parameter names that come from `old_function()` (#453)


