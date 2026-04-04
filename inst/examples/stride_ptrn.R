

x <- mutatomic(1:1e7)

long_x(x, stride_ptrn(2, 20, c(TRUE, FALSE, FALSE, TRUE)))

long_x(x, stride_ptrn(20, 2, c(TRUE, FALSE, FALSE, TRUE)))

long_x(x, stride_ptrn(2, 20, c(TRUE, FALSE, FALSE, TRUE)), -1) |> head()

long_x(x, stride_ptrn(20, 2, c(TRUE, FALSE, FALSE, TRUE)), -1) |> head()
