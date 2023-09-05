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
  , "maybe"
  , "numbers"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
