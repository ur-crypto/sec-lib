# [sec-lib][]

Thanks for starting a project with Haskeleton! If you haven't heard of it
before, I suggest reading the introductory blog post. You can find it here:
<http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/>.

Before you get started, there are a few things that this template couldn't
provide for you. You should:

-   Add a synopsis to `sec-lib.cabal`. It should be a short (one sentence)
    explanation of your project.

-   Add a category to `sec-lib.cabal`. A list of categories is available on
    Hackage at <http://hackage.haskell.org/packages>.

-   Rename `library/Example.hs` to whatever you want your top-level module to
    be called. Typically this is the same as your package name but in
    `CamelCase` instead of `kebab-case`.

    -   Don't forget to rename the reference to it in
        `executable/Executable.hs`!

Once you've done that, start working on your project with the Stack commands
you know and love!

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

Thanks again, and happy hacking!

[sec-lib]: https://github.com/JBisnett/sec-lib
