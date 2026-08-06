# Regarding Modification

This help page describes the main modification semantics available in
'squarebrackets'.  
  

## Base R's Native Modification Semantics

For most average users, R's default copy-on-modify semantics are fine.  
The `_mod` methods use native semantics for atomic objects, and shallow
copy semantics for recursive objects.  
  

## Pass-by-Reference

'squarebrackets' provides the `_set` methods to modify by reference,
meaning no copy is made at all.  
Pass-by-Reference is fastest and the most memory efficient.  
But it is also more involved than the other modification forms, and
requires more thought.  
See
[squarebrackets_PassByReference](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md)
for more information.  
  

## Arguments `rp` and `tf` for Atomic Objects

**Argument `rp`**  
The `rp` argument is used to replace the values at the specified indices
with the values specified in `rp`. Using the `rp` argument in the
modification methods, corresponds to something like the following:  

    x[...] <- rp

**Argument `tf`**  
The `tf` argument is used to transform the values at the specified
indices through transformation function `tf`. Using the `tf` argument
corresponds to something like the following:  

    x[...] <- tf(x[...])

where `tf` is a function that **returns** an object of appropriate type
and size (so `tf` should not be a pass-by-reference function).  
  

## Arguments `rp` and `tf` for Recursive Objects

The `rp` and `tf` arguments work mostly in the same way for recursive
objects.  
But there are some slight differences.  
  
**Argument `rp`**  
'squarebrackets' demands that `rp` is always provided as a list in the
S3 methods for recursive vectors, matrices, and arrays (i.e. lists).  
This is to prevent ambiguity with respect to how the replacement is
recycled or distributed over the specified indices  
(See `Footnote 1` below).  
  
**Argument `tf`**  
Most functions in (base) 'R' are vectorized for atomic objects, but not
for lists  
(see `Footnote 2` below).  
'squarebrackets' will therefore apply transformation function `tf` via
`lapply`, like so:  

    x[...] <- lapply(x[...], tf)

## Arguments `rp` and `tf` for data.frame-like Objects

Replacement and transformations in data.frame-like objects are a bit
more flexible than in Lists.  
  
**Argument `rp`**  
`rp` is not always demanded to be a list for data.frame-like objects,
only when appropriate (for example, when replacing multiple columns, or
when the column itself is a list.)  
  
**Argument `tf`**  
Every column in a data.frame is like an element in a list.  
Therefore, when transforming parts of a data.frame with the `tf`
argument, lapply\` is used for transformations across multiple
columns.  
  
  

## Recycling and Coercion

Recycling is not allowed in the modification methods.  
So, for example, `length(rp)` must be equal to the length of the
selected subset, or equal to `1`.  
  
When using Pass-by-Reference semantics, the user should be extra mindful
of the auto-coercion rules.  
See
[squarebrackets_coercion](https://tony-aw.github.io/squarebrackets/reference/aaa10_squarebrackets_coercion.md)
for details.  
  

## Footnotes

**Footnote 1**  
Consider the following replacement in base 'R':

    x <-list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    x[1:2] <- 2:1

What will happen?  
Will the `x[1]` be `list(1:2)` and `x[2]` also be `list(1:2)`?  
Or will `x[1]` be `list(2)` and `x[2]` be `list(1)`?  
It turns out the latter will happen; but this is somewhat ambiguous from
the code.  
To prevent such ambiguity in your code, 'squarebrackets' demands that
`rp` is always provided as a list.  
  
**Footnote 2**  
Most functions in (base) 'R' are vectorized for atomic objects, but not
for lists.  
One of the reasons is the following:  
In an atomic vector `x` of some type `t`, every single element of `x` is
a scalar of type `t`.  
However, every element of some list `x` can be virtually anything:  
an atomic object, another list, an unevaluated expression, even dark
magic like `quote(expr =)`.  
It is difficult to make a vectorized function for an object with so many
unknowns.  
Therefore, in the vast majority of the cases, one needs to loop through
the list elements.  
  
