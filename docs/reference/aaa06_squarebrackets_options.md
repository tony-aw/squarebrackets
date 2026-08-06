# squarebrackets Options

This help page explains the various global options that can be set for
the 'squarebrackets' package, and how it affects the functionality.  
  

## Check Duplicates

![\[argument: chkdup\]](figures/argument-chkdup-blue.svg)  
![\[option:
squarebrackets.chkdup\]](figures/option-squarebrackets_dot_chkdup-blue.svg)  
The `_x` methods are the only methods where providing duplicate indices
actually make sense.  
For the other methods, it doesn't make sense.  
Giving duplicate indices usually won't break anything.  
However, when replacing/transforming subsets, it is almost certainly not
the intention to provide duplicate indices.  
Providing duplicate indices anyway might lead to unexpected results.  
Therefore, for the methods where giving duplicate indices does not make
sense, the `chkdup` argument is present.  
This argument controls whether the method in question checks for
duplicates (`TRUE`) or not (`FALSE`).  
  
Setting `chkdup = TRUE` means the method in question will check for
duplicate indices, and give an error when it finds them.  
Inverted indices will not be checked, as inverting indices is not
sensitive to duplicates.  
  
Setting `chkdup = FALSE` will disable these checks, which saves time and
computation power, and is thus more efficient.  
  
Since checking for duplicates can be expensive, it is set to `FALSE` by
default.  
The default can be changed in the `squarebrackets.chkdup` option.  
  
  

## Sticky

![\[argument: sticky\]](figures/argument-sticky-blue.svg)  
![\[option:
squarebrackets.sticky\]](figures/option-squarebrackets_dot_sticky-blue.svg)  
The [long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods can already handle names (through the `use.names` argument), as
well as attributes specific to the
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
class.  
  
Attributes which are not names, and not specific to
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
class - henceforth referred to as "other attributes" - are treated
differently.  
How the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods handle these "other" attributes, is determined by the `sticky`
option and argument.  
  
When `sticky = FALSE`, the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods will drop all **other** attributes.  
  
By setting `sticky = TRUE`, all these **other** attributes, except
`comment` and `tsp`, will be preserved;  
The key advantage for this, is that classes that use `static` attributes
(i.e. classes that use attributes that do not change when sub-setting),
are automatically supported if `sticky = TRUE`, and no separate methods
have to written for the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods.  
Attributes specific to classes like `difftime`, `broadcaster`, and more,
use static attributes.  
  
Instead of setting `sticky = TRUE` or `sticky = FALSE`, one can also
specify all classes that use static attributes that you'll be using in
the current R session.  
In fact, when 'squarebrackets' is **loaded**, the
`squarebrackets.sticky` option is set as follows:

    squarebrackets.sticky = c(
       "difftime", "broadcaster"
    )

So in the above default setting, `sticky = TRUE` for  
"difftime", "broadcaster".  
Also in the above default setting, `sticky = FALSE` for other classes.  
  
The reason the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods need the `sticky` option, is because of the following.  
Unlike the `ii_`, `ii_`, `ss_`, and `ss_` methods, the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods are not wrappers around the `[` and `[<-` operators.  
Therefore, most `[` - S3 methods for highly specialized classes are not
readily available for the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods.  
Which in turn means important class-specific attributes are not
automatically preserved.  
The `sticky` option is a convenient way to support a large number of
classes, without having to write specific methods for them.  
  
For specialized classes that use attributes that **do** change when
sub-setting, separate dispatches for the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods need to be written.  
Package authors are welcome to create method dispatches for their own
classes for these methods.  
  
  
