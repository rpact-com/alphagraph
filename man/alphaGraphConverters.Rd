\name{alphaGraphConverters}
\alias{alphaGraphFromList}
\alias{alphaGraphFromJson}
\alias{alphaGraphFromTransitionMatrix}
\alias{alphaGraphFromGraphicalMCP}
\alias{alphaGraphFromVisNetworkData}
\title{Create an AlphaGraph from external representations}
\description{
Construct a canonical \code{AlphaGraph} from its R-list or JSON form, a
transition matrix, a \pkg{graphicalMCP} initial graph, or \pkg{visNetwork}
node and edge data frames.
}
\usage{
alphaGraphFromList(x, validate = TRUE)

alphaGraphFromJson(json, validate = TRUE)

alphaGraphFromTransitionMatrix(
  transitions,
  hypotheses = NULL,
  type = c("value", "expression", "display"),
  nodeIds = NULL,
  nodeLabels = NULL,
  id = "graph",
  label = id,
  description = NULL,
  metadata = list(),
  settings = list(),
  validate = TRUE
)

alphaGraphFromGraphicalMCP(
  graph,
  id = "graph",
  label = id,
  description = NULL,
  metadata = list(),
  settings = list(),
  validate = TRUE
)

alphaGraphFromVisNetworkData(
  nodes,
  edges,
  id = "graph",
  label = id,
  description = NULL,
  metadata = list(),
  settings = list(),
  validate = TRUE
)
}
\arguments{
\item{x}{A JSON-compatible canonical alphagraph list.}
\item{validate}{Whether to validate the resulting graph.}
\item{json}{JSON text, connection, or file path accepted by
\code{jsonlite::fromJSON()}.}
\item{transitions}{A square transition matrix or data frame.}
\item{hypotheses}{Optional hypothesis weights in matrix order.}
\item{type}{Whether matrix cells contain numeric values, expressions, or
display text.}
\item{nodeIds}{Stable node identifiers. Defaults to matrix row names.}
\item{nodeLabels}{Optional human-readable node labels.}
\item{id, label, description, metadata, settings}{Graph-level properties.}
\item{graph}{A \pkg{graphicalMCP} \code{initial_graph} or an equivalent list.}
\item{nodes, edges}{Data frames accepted by \code{visNetwork::visNetwork()}.}
}
\value{
An \code{AlphaGraph} object.
}
\keyword{internal}
