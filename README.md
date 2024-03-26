# elm-trackjs ![CI](https://github.com/scrive/elm-trackjs/workflows/CI/badge.svg?branch=master)

**Work in Progress:** This project is a fork of `elm-rollbar`, with the
intention of adapting it to work with TrackJS.

TODOs:
- [ ] Add example implementation (will also function as a live test)
- [ ] Figure out what to include in `customer` data, in particular, the various `id`s
- [ ] Test that it works!
- [ ] Add test for version staying in sync
- [ ] Finish and check documentation
- [ ] Publish!

Possible extra features not currently implemented:
- Optional `url` information
- Optional `network` telemetry information
- Explore how maximum payload size and HTTP 413 response can be handled
  (without encumbering the API)
