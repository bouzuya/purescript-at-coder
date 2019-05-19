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
    , "node-child-process"
    , "node-fs"
    , "node-path"
    , "node-process"
    , "numbers"
    , "precise"
    , "psci-support"
    , "rationals"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
