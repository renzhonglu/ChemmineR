\name{propOB}
\alias{propOB}
\title{
	Properties from OpenBabel
}
\description{
	 Generates the following descriptors: 
	 "cansmi", "cansmiNS", "formula", "HBA1", "HBA2", "HBD", "InChI", 
	 "InChIKey", "logP", "MR", "MW", "nF","title", "TPSA".
}
\usage{
propOB(sdfSet)
}
\arguments{
  \item{sdfSet}{
	  An SDFset object.
	}
}
\value{
	A data frame with a row for each compound in the given data frame and a named
	column for each property.
}
\author{
	Kevin Horan
}

\examples{
	\dontrun{
		library(ChemmineR)
		data(sdfsample)
		propOB(sdfsample)
	}
}
