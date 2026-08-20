\name{AlphaProject}
\alias{AlphaProject}
\alias{alphaProjectFromList}
\alias{alphaProjectFromJson}
\alias{createDemoAlphaProject}
\alias{alphaGraphInputHash}
\alias{alphaGraphPackageVersion}
\title{JSON-friendly alphagraph project model}
\description{
Stores a reusable \code{AlphaGraph} together with p-value datasets, analysis
specifications, optional derived results, project UI state, and metadata. Raw
and adjusted p-values deliberately remain outside the graph definition.
}
\usage{
AlphaProject

alphaProjectFromList(x, validate = TRUE)

alphaProjectFromJson(json, validate = TRUE)

createDemoAlphaProject()

alphaGraphInputHash(x)

alphaGraphPackageVersion(package)
}
\arguments{
\item{x}{A JSON-compatible project list, or an object to fingerprint.}

\item{validate}{Whether to validate the restored project.}

\item{json}{A serialized alphagraph project.}

\item{package}{Package name whose installed version should be returned.}
}
\details{
Create an instance with \code{AlphaProject$new()}. The project schema embeds
the graph schema and stores p-values by stable hypothesis id. Use
\code{$validate(requireComputable = TRUE, analysisId = ...)} before a
calculation and \code{$runShortcut()} for the first supported analysis path.
Derived results include an input hash and package versions.
}
\value{
\code{AlphaProject} is an R6 class generator. The conversion functions return
an \code{AlphaProject}; \code{createDemoAlphaProject()} returns an example;
the helper functions return a hash or package version.
}
\keyword{internal}
