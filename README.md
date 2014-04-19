pcset-theory
============

These files contain Lisp code that works with the pitch-class sets of
post-tonal music theory. Specifically, imset-code-only-functions
provides a "imset-decision-maker," which is roughly described in
various pieces of documentation throughout the files.

The overall project is in rough form, but is in the process of being
refactored into more logical pieces and documented more
conventionally.

## General Explanation
A suite of functions for dealing with a new, relatively-esoteric
concept in the relatively esoteric world of post-tonal music
theory: the interval multiset (or imset), which is derived from
the concept of an 'interval function' (or IFUNC) as described by
influential music theorist David Lewin in his classic Generalized
Musical Intervals and Transformations

The following code is being used to provide the computational
underpinning for research to be published soon in a scholarly
music theory journal and, as such, is not quite ready for public
consumption. There is a considerable amount of commenting that
exists mostly to keep a kind of running note-taking for me as I
consider the issues of design and how to adjust to the changing
needs of the data which is being produced for the different
structures being inputted. the commenting style needs to be
adjusted so more of the useful information there appears in a
documentation string (most functions lack such documentation).
Consider also providing consistent appearances of examples, cf.
the highly-structured model used in How To Design Programs

While there is a very rough attempt to group functions that
belong together in the same sections, consider a more
standardized approach to semi-colon commenting. for some reason,
hash-pipe commenting blocks seems to be glitchy in Aquamacs, but is
otherwise quite useful
