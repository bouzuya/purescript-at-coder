{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "bigints"
    , "console"
    , "debug"
    , "effect"
    , "foreign-object"
    , "memoize"
    , "node-fs"
    , "numbers"
    , "precise"
    , "psci-support"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
