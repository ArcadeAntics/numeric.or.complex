

setClassUnion(
    name    = "numeric_or_complex",
    members = c("numeric", "complex")
)





as.numeric_or_complex <- function (x, ...)
UseMethod("as.numeric_or_complex")


as.numeric_or_complex.default <- function (x, strict = TRUE, ...)
{
    if (missing(x))
        return(numeric(0))
    if (!is.numeric_or_complex(x))
        x <- as.complex(x, ...)
    if (!is.complex(x) || essentials::as.boolean(strict) && all(is.na(x) | Im(x) == 0))
        as.numeric(x, ...)
    else as.complex(x, ...)

}





methods::setMethod(
    f         = "coerce",
    signature = c(
        from = "ANY",
        to   = "numeric_or_complex"),
    definition = function(from, to = "numeric_or_complex", strict = TRUE)
as.numeric_or_complex(from, strict = strict))





is.numeric_or_complex <- function (x)
is.numeric(x) || is.complex(x)
