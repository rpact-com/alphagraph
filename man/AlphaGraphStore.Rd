\name{AlphaGraphStore}
\alias{AlphaGraphStore}
\alias{createAlphaGraphStore}
\title{Reactive store for a shared AlphaGraph}
\description{
Maintains one canonical graph per Shiny session. Mutations are committed
atomically and increment a revision used by graph and matrix views.
}
\usage{
AlphaGraphStore

createAlphaGraphStore(graph = createDemoAlphaGraph())
}
\arguments{
\item{graph}{Initial canonical \code{AlphaGraph}.}
}
\details{
Use \code{$transact(source, mutate)} for user changes. Nested transactions are
ignored, failed transactions are not committed, and rendering a view never
writes back to the store. This prevents bidirectional synchronization loops.
}
\value{
\code{AlphaGraphStore} is an R6 class generator;
\code{createAlphaGraphStore()} returns an instance.
}
\keyword{internal}
