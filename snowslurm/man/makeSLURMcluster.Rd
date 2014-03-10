\name{makeSLURMcluster}
\alias{makeSLURMcluster}
\title{makeSLURMcluster}
\description{
Create a snow cluster on a distributed computer where SLURM
is the scheduler.
On some distributed computers, users do not have direct
access to SSH into compute nodes to launch snow workers.
This package is for machines running SLURM as the scheduler.
It launches workers using SLURM commands, rather than SSH,
ensuring that workers get assigned to resources allocated
by the scheduler.
}
\usage{
makeSLURMcluster(..., options = defaultClusterOptions)
}
\arguments{
\item{options}{Options for configuring snow cluster. Users should
normally not need to change this.}
}
\seealso{
makeSOCKcluster()
}
\examples{
\dontrun{
# Minimal example
library(snowslurm)
cluster <- makeSLURMcluster()

stopCluster(cluster)
}
}
\keyword{file}
