# Methods

This help page gives an overview of the methods available in
'squarebrackets'.  
  

## Main Methods

The main methods of 'squarebrackets' use the naming convention `A_B`:  
`A` tells you on what kind of object and what kind of indices the method
operates on;  
`B` tells you **what operation** is performed.  
  
For the `A` part, the following is available:

- `ii_`: operates on subsets of **atomic** objects by **flat/interior**
  indices.

- `ii2_`: operates on subsets of **recursive** objects by
  **flat/interior** indices.

- `ss_`: operates on subsets of **atomic** objects by (dimensional)
  **subscripts**.

- `ss2_`: operates on subsets of **recursive** objects by (dimensional)
  **subscripts**.

- `slice_`: uses **index-less**, **sequence-based**, and efficient
  operations on (`mutatomic`) long vectors.

- `slicev_`: uses **index-less**, **value-based** and efficient
  operations on (`mutatomic`) long vectors.  
    

For the `B` part, the following is available:

- `_x`: extract, exchange, or duplicate (if applicable) subsets.

- `_wo`: returns the original object **without** the provided subsets.

- `_mod`: modify subsets and return copy.

- `_set`: modify subsets using [pass-by-reference
  semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md).  
    

To illustrate, let's take the methods used for extracting subsets
(`_x`).  
  
When `y` is atomic, the following holds (roughly speaking):

- `ii_x(y, i)` corresponds to `y[i]`

- `ss_x(y, n(i, k), c(1, 3))` corresponds to `y[i, , k]`  
    

When `y` is a list (i.e. recursive), the following holds (roughly
speaking):

- `ii2_x(y, i)` corresponds to `y[i]` or `y[[i]]` (depending on the
  arguments given in
  [`ii2_x()`](https://tony-aw.github.io/squarebrackets/reference/sb_x.md))

- `ss2_x(y, n(i, k), c(1, 3))` corresponds to `y[i, , k]` or
  `y[[i, , k]]` (depending on the arguments given in
  [`ss2_x()`](https://tony-aw.github.io/squarebrackets/reference/sb_x.md))  
    

## Other Methods

Besides the main methods, 'squarebrackets' provides some additional
methods that do not neatly fit into the above methods.  
  
First, there is the
[lst\_](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)
set of methods, which deal with sub-set operations that are only
relevant for (nested) lists, but not for the other types of supported
objects.  
  
Second, there is
[idx](https://tony-aw.github.io/squarebrackets/reference/idx1.md)
method, which works on both recursive and non-recursive objects, and
transforms/translates indices to be used in R's default copy-on-modify
semantics.  
  
Finally, there are the `sb_` and `sb2_` sets of methods, which cover
miscellaneous operations for atomic and recursive objects,
respectively.  
  

## Finding the Appropriate Help Pages

With knowledge of the naming convention of the main methods, one can
easily find out information about a particular method by usign the `?`
operator.  
So to find out about modifying recursive objects by subscripts using
Pass-by-Reference semantics, type in:  
[`?ss2_set`](https://tony-aw.github.io/squarebrackets/reference/sb_set.md)  
  
