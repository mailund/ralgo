% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_access_lists.R
\name{ral_cons}
\alias{ral_cons}
\title{Construct a new random access list by prepending an element.}
\usage{
ral_cons(elem, ral)
}
\arguments{
\item{elem}{Element to put at the front of the list}

\item{ral}{Random access list}
}
\value{
New random access list with elem at the front
}
\description{
Empty random access lists are represented as NULL, so to create lists
start from there.
}
