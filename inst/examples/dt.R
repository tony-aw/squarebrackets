
# Aggregate data.table ====
d <- data.table::as.data.table(iris)
dt_setcoe(d, "Sepal.Length", 2L, v = as.integer)
print(d)
fun <- rep(list(mean = mean, var = var), 2L)
cols <- rep(c("Sepal.Length", "Sepal.Width"), each = 2L)
dt_aggr(d, 1:5, cols, c(-1, 2), fun = fun, by = "Species")


# Aggregate sf-data.table ====

if(requireNamespace("sf")) {
  x <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  x <- data.table::as.data.table(x)
  
  x$region <- ifelse(x$CNTY_ID <= 2000, 'high', 'low')
  d.aggr <- dt_aggr(
    x, 0L, "geometry", fun = sf::st_union, by = "region"
  )
  
  head(d.aggr)
}



#############################################################################

# dt_setcoe ====

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
dt_setcoe(obj, is.numeric, v = as.numeric) # integers are now numeric
str(obj) # now those columns are double/numeric


#############################################################################
# dt_setmutate ====

# add new columns based on other columns:
d <- data.table::as.data.table(iris)
mutations <- ~ n(
  Ratio.Length = Sepal.Length / Petal.Length,
  Ratio.Width = Sepal.Width / Petal.Width
)
dt_setmutate(d, mutations)
d

# remove columns:
dt_setmutate(d, n(Ratio.Width = NULL)) # remove Ratio.Width
d

# transform existing columns:
mutations <- ~ n(Sepal.Length = Sepal.Length /100)
dt_setmutate(d, mutations)
d
