{ name = "project-euler-solutions"
, dependencies =
  [ "arrays"
  , "bigints"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lazy"
  , "lists"
  , "math"
  , "maybe"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
