\name{numeric_or_complex}
\alias{numeric_or_complex}
\alias{as.numeric_or_complex}
\alias{as.numeric_or_complex.default}
\alias{is.numeric_or_complex}
\title{Numeric or Complex Vectors}
\description{
Coerce or test for objects of type "numeric_or_complex".
\code{is.numeric_or_complex} is a more general test of an object being
interpretable as numbers.
}
\usage{
as.numeric_or_complex(x, ...)
is.numeric_or_complex(x)
}
\arguments{
  \item{x}{
object to be coerced or tested.
}
  \item{...}{
further arguments passed to or from other methods.
}
}
\value{
\code{as.numeric_or_complex} attempts to coerce its argument to be of numeric or
complex type: like \code{\link[base]{as.vector}} it strips attributes including
names. The default method for \code{as.numeric_or_complex} first coerces its
argument to type complex, then coerces to type numeric if all elements are
either missing values or have no imaginary component.

\code{is.numeric_or_complex} returns \code{TRUE} or \code{FALSE} depending on
whether its argument is of numeric or complex type or not.
}
\seealso{
\code{\link[base]{double}}, \code{\link[base]{numeric}},
\code{\link[base]{complex}}
}
\examples{
as.numeric_or_complex(c(TRUE, FALSE, NA))
as.numeric_or_complex(1:100)
as.numeric_or_complex(stats::rnorm(100))
as.numeric_or_complex(exp(-4:4 * 1i))
as.numeric_or_complex(c(NA, NaN, 1:100 + 0i))
as.numeric_or_complex(c("4+5i", "7", "100"))
}
