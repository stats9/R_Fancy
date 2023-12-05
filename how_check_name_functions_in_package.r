obj <- mget(ls("package:base"), inherits = TRUE)
fun_dp <- Filter(is.function, obj)
name_fun <- fun_dp |> names()
"data.frame" %in% name_fun
"list" %in% name_fun

library(dplyr)
obj2 <- mget(ls("package:dplyr"), inherits = TRUE)
fun_dp2 <- Filter(is.function, obj2)
name_fun2 <- fun_dp2 |> names()
"data.frame" %in% name_fun2
"list" %in% name_fun2
"case_when" %in% name_fun2
name_fun2
