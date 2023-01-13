\name{toyModel}
\docType{methods}
\alias{toyModel}

\title{
Toy model of coupled tensor data
}

\description{
A list object containing multiple arrays are generated.
}

\usage{
toyModel(model = "coupled_CP_Easy", seeds=123)
}

\arguments{
  \item{model}{
  "coupled_CP_Easy", "coupled_CP_Hard", "coupled_Tucker_Easy", "coupled_Tucker_Hard", "coupled_Complex_Easy", or "coupled_Complex_Hard" can be specified (Default: "coupled_CP_Easy").
  }

  \item{seeds}{
  The seed of random number (Default: 123).
  }
}

\author{Koki Tsuyuzaki}

\examples{
Xs1 <- toyModel(model = "coupled_CP_Easy", seeds=123)
Xs2 <- toyModel(model = "coupled_CP_Hard", seeds=123)
Xs3 <- toyModel(model = "coupled_Tucker_Easy", seeds=123)
Xs4 <- toyModel(model = "coupled_Tucker_Hard", seeds=123)
Xs5 <- toyModel(model = "coupled_Complex_Easy", seeds=123)
Xs6 <- toyModel(model = "coupled_Complex_Hard", seeds=123)
}

\keyword{methods}