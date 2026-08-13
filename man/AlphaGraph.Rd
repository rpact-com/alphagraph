\name{AlphaGraph}
\alias{AlphaGraph}
\title{JSON-friendly alphagraph graph model}
\description{
R6 container for the canonical alphagraph graph definition. The contained
plain R list can be serialized with \pkg{jsonlite} and converted to
\pkg{graphicalMCP} inputs, editable transition matrices, and
\pkg{visNetwork} data.
}
\usage{
AlphaGraph
}
\details{
Create an instance with \code{AlphaGraph$new()}. Nodes represent hypotheses,
edges represent directed transitions, and their nested \code{vis} fields hold
presentation properties. Use \code{$toList()} or \code{$toJson()} for
persistence, and the \code{$to*()} and \code{$updateFrom*()} methods to keep
matrix and network views synchronized with the canonical object.
}
\value{
An R6 class generator.
}
\keyword{internal}
