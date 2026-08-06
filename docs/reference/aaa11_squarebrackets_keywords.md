# Advanced Index Specification with Keywords

Instead of the usual indexing types, formulas can be used to specify
indices in vectors, arrays, or data.frames that can evaluate keywords.  
  
The following keywords are available:

- `.M`: the given margin/dimension; `0` if not relevant.

- `.Nms`: the (dim)names at the given margin.

- `.N`: the size of a given dimension (if `.M` is not `0`) or else the
  length of `x`.

- `.I`: equal to `seq_len(.N)`.

- `.bi(...)`: a **function** to specify bilateral indices. See the
  `Details` section below.

- `.ptrn(ptrn, start, end)`: a **function** specifying a pattern of
  indices.  
  It is defined as `(start:end)[ptrn]`, where `start` and `end` are
  numeric scalars and `ptrn` is a logical vector.  
  By default, `start = 1L` and `end = .N`.

These keywords can be used inside formulas to specify more advanced
indices.  

Here are a few examples of advanced indexing in arrays:

    x <- matrix(1:20, 5, 4)
    dimnames(x) <- list(month.abb[1:5], month.abb[1:4])
    print(x)
    #>     Jan Feb Mar Apr
    #> Jan   1   6  11  16
    #> Feb   2   7  12  17
    #> Mar   3   8  13  18
    #> Apr   4   9  14  19
    #> May   5  10  15  20


    # select first half of rows, select first & last column:
    tt_x(x, ~ 1:round(.N/2), ~ .bi(1, -1))
    #>     Jan Apr
    #> Jan   1  16
    #> Feb   2  17


    # extract where rownames contain "r" & where colnames DO NOT contain "r":
    ss_x(x, ~ grep("r", .Nms), use = c(1, -2))
    #>     Jan Feb
    #> Mar   3   8
    #> Apr   4   9


    # reverse order of all dimensions:
    ss_x(x, ~ .bi(-.I))
    #>     Apr Mar Feb Jan
    #> May  20  15  10   5
    #> Apr  19  14   9   4
    #> Mar  18  13   8   3
    #> Feb  17  12   7   2
    #> Jan  16  11   6   1


    # select every other element for each dimension:
    ss_x(x, ~ .ptrn(c(TRUE, FALSE)))
    #>     Jan Mar
    #> Jan   1  11
    #> Mar   3  13
    #> May   5  15


    # select different range from each dimension:
    x <- array(1:1000, 5:3)
    start <- c(3, 1, 2)
    ss_x(x, ~ (start[.M]):.N)
    #> , , 1
    #>
    #>      [,1] [,2] [,3] [,4]
    #> [1,]   23   28   33   38
    #> [2,]   24   29   34   39
    #> [3,]   25   30   35   40
    #>
    #> , , 2
    #>
    #>      [,1] [,2] [,3] [,4]
    #> [1,]   43   48   53   58
    #> [2,]   44   49   54   59
    #> [3,]   45   50   55   60

## Details

**Bilateral Indices**  
The `.bi(...)` function is used to specify bilateral indices.  
One can specify both positive and negative numbers.  
Positive numbers (for example `~ .bi(1:10)`) specify indices work like
normally.  
Negative numbers specify indices from the end;  
i.e. `~ .bi(-2)` would be equivalent to `length(x) - 1` if using an
`ii_` method, or `dim(x)[.M] - 1` if using a `ss_` or `tt_` method.  
  
The usage of `.bi(...)` is similar to the `c(...)` function, in that
multiple vectors can be concatenated to one.  
And one can specify both negative and positive numbers together.  
For example: `~ .bi(-10:-1, 10:1)`.  
  
One can also use the other keywords inside `.bi()`.  
For example, `~ .bi(-.I)` can be used to reverse a vector, or reverse a
dimension of an array or data.frame.  
  
