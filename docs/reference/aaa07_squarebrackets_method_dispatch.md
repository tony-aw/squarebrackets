# Ellipses Usage and Method Dispatches in 'squarebrackets'

This help page gives some additional details regarding the S3 method
dispatch used in 'squarebrackets'.  
  

## Ellipsis

Due to how the S3 method dispatch system works in 'R', all generic
methods have the ellipsis argument (`...`).  
For the user's safety, 'squarebrackets' does check that the user doesn't
accidentally add arguments that make no sense for that method.  
  
  

## 'squarebrackets' Methods that do not require explicit Dispatches

The `ii_`/`ss_`/`tt_` - `_x` methods are essentially wrappers around the
`[` operator.  
Similarly, the `ii_`/`ss_`/`tt_` - `_mod` methods are essentially
wrappers around the `[<-` operator.  
And the
[lst_rec](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)
and
[lst_recin](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)
methods are essentially wrappers around the `[[` and `[[<-` operators.  
Therefore, any custom class that has method dispatches defined for the
`[`, `[<-`, `[[`, and `[[<-` methods, have their dispatches
automatically handled by the above named methods.  
  

## 'squarebrackets' Methods that DO require explicit Dispatches

Unlike the `ii_`/`ss_`/`tt_` - `_x` methods, the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md)
method is **not** a wrapper around the `[` operator, and thus does not
detect custom method dispatches defined for `[`.  
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md) can
support classes that have static attributes (attributes that do not
depend on the type, length, or data of the vector), by adding the class
names to the
[squarebrackets.sticky](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md)
option.  
Classes with non-static attributes will explicitly require a `long_x`
method.  
  

## Class support for the Pass-By-Reference Methods

The `_set` methods only support the `data.table` and `mutatomic`
classes.  
Other mutable classes are not supported by the 'squarebrackets'
package.  
However, other 'R' package authors are welcome to add additional method
dispatches for the `_set` methods.  
  
