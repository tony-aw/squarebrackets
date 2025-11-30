
# the following code demonstrates how locked bindings,
# such as `base::letters`,
# are being safe-guarded

x <- list(a = base::letters)
myref <- x$a # view of a list
address(myref) == address(base::letters) # TRUE: point to the same memory
bindingIsLocked("letters", baseenv()) # base::letters is locked ...
bindingIsLocked("myref", environment()) # ... but this pointer is not!

if(requireNamespace("tinytest")) {
  tinytest::expect_error(
    ii_set(myref, i = 1, rp = "XXX") # this still gives an error though ...
  )
}

is.mutatomic(myref) # ... because it's not of class `mutatomic`


x <- list(
  a = as.mutatomic(base::letters) # `as.mutatomic()` makes a copy
)
myref <- x$a # view of a list
address(myref) == address(base::letters) # FALSE: it's a copy
ii_set(
  myref, i = 1, rp = "XXX"  # modifies x, does NOT modify `base::letters`
)
print(x) # x is modified
base::letters # but this still the same


