Quasi-extras provides utilities for creating and using Haskell embedded DSLs.

It provides the following features:

  * Language.Haskell.TH.Builders

    Implementation of new versions of the AST quoters ([e| ... |], [p| ... |], etc).  These quasi-quoters allow splices in more places, and provide pattern matching capabilities.  These resolve Part A and D of (New Directions for Template Haskell)[http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal].  Reinerp's comment on that page observes that what I've implemented here is possible and perhaps preferable.

  * Language.Haskell.TH.Desuger

    Implementation of the desugarings specified in the standard report.

  * Language.Haskell.TH.Convenience

    Provides overloaded conversion operators which make for convenient AST construction, similarly to th-build.  These are used for every splice of one variant of the AST-quotation.

  * Language.Haskell.TH.Conversion

    Provides some functions for (partial) conversion between the different language domains.  For example, a pattern matcher can be turned into a construction expression, and vice versa.

  * Language.Haskell.TH.StringSplice

    Implementation of string quoter and matcher.

Other ideas:

  * Language.Haskell.TH.Generalize

    Provide a TH function which turns functions into typeclasses with one default instance.

  * Language.Haskell.TH.Embed

    Provide a TH function which constructs instances of classes, in order to (reflect on code)[http://www.ittc.ku.edu/csdlblog/?p=88].  Consider providing one that also invokes Generalize?

  * Language.Haskell.TH.Partial

    Provide a TH function which creates all of the partially constructed versions of data structures.  Or something like that.  Can be used to represent the remainder of a lens.

  * Consider moving this stuff to a different namespace?  Maybe polluting Language.Haskell.TH isn't so good.
