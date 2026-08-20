\name{AlphaProjectStore}
\alias{AlphaProjectStore}
\alias{createAlphaProjectStore}
\title{Reactive store for a complete AlphaProject}
\description{
Owns project datasets, analysis specifications, and results while exposing the
embedded graph through the existing \code{AlphaGraphStore}.
}
\usage{
AlphaProjectStore

createAlphaProjectStore(project = createDemoAlphaProject())
}
\arguments{
\item{project}{Initial \code{AlphaProject}.}
}
\details{
A project snapshot always embeds the current graph-store snapshot. Dataset and
analysis edits invalidate affected saved results. Graph edits are detected by
the result input hash.
}
\value{
\code{AlphaProjectStore} is an R6 class generator;
\code{createAlphaProjectStore()} returns an instance.
}
\keyword{internal}
