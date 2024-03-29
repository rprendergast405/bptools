% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_mvl.R
\name{scale_mvl}
\alias{scale_mvl}
\alias{scale_colour_mvl}
\alias{scale_fill_mvl}
\alias{scale_colour_mvlc}
\alias{scale_fill_mvlc}
\title{Marketview Colour Scales}
\usage{
scale_colour_mvl(..., palette = "mvl", direction = 1,
  highlight = mvl_leaf, guide = ggplot2::guide_legend(byrow = TRUE))

scale_fill_mvl(..., palette = "mvl", direction = 1, highlight = mvl_leaf,
  guide = ggplot2::guide_legend(byrow = TRUE))

scale_colour_mvlc(..., palette = "Leaf", direction = -1, values = NULL,
  na.value = "grey50", guide = "colourbar")

scale_fill_mvlc(..., palette = "Leaf", direction = -1, values = NULL,
  na.value = "grey50", guide = "colourbar")
}
\arguments{
\item{...}{Other arguments passed on to \code{\link[ggplot2]{discrete_scale}} to control name, limits, breaks, labels and so forth.}

\item{palette}{Name of the palette to use}

\item{direction}{Set the order of the colours in the scale}
}
\description{
The \code{Marketview} scales provide sequential, diverging and qualitative
colour schemes in line with the Marketview branding guides.
}
\section{Palettes}{

The following palettes are available for use with these scales:
\describe{
  \item{Diverging}{LeafPlum, TealRed, RedStone, SkyStone, PlumStone, LeafStone, RoyalStone}
  \item{Qualitative}{mvl, mcd, mcd2, mvl_classic, fssi}
  \item{Sequential}{Leaf, Plum, Teal, Blue, Sky, Citrus, Stone,
     Navy, Orange, Red, Green, Purple, Highlight}
}
If there are others that you want to be implemented, let me (Bert) know
}

