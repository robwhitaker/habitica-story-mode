{ name = "habitica-story-mode"
, dependencies =
    [ "aff"
    , "affjax"
    , "console"
    , "coroutines"
    , "effect"
    , "js-timers"
    , "oak"
    , "psci-support"
    , "web-events"
    , "web-socket"
    ]
, packages = ./packages.dhall
, sources = [ "client/src/**/*.purs", "client/test/**/*.purs" ]
}
