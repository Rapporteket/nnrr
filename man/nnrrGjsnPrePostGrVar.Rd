% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nnrrGjsnPrePostGrVar.R
\name{nnrrGjsnPrePostGrVar}
\alias{nnrrGjsnPrePostGrVar}
\title{Denne funksjonen generer figur på gjennomsnitt ved konsultasjon og 6mnd etter}
\usage{
nnrrGjsnPrePostGrVar(
  RegData,
  valgtVar,
  datoFra = "2012-04-01",
  datoTil = "2050-12-31",
  outfile = "",
  minald = 0,
  maxald = 130,
  gr_var = "SykehusNavn",
  enhetsUtvalg = 0,
  erMann = 99,
  reshID,
  inkl_konf = 0,
  graa = ""
)
}
\arguments{
\item{RegData}{- ei dataramme med alle nødvendige variable fra registeret}

\item{valgtVar}{- Variabel som skal vises}

\item{datoFra}{<- '2014-01-01'    # min og max dato i utvalget vises alltid i figuren.}

\item{datoTil}{<- '2050-12-31'}

\item{outfile}{- navn på fil figuren skrives til, default '' dvs. skriv til skjerm}

\item{minald}{- alder, fra og med}

\item{maxald}{- alder, til og med}

\item{erMann}{- kjønn, 1-menn, 0-kvinner, default 99, dvs. begge}

\item{reshID}{- avdelingsid for egen avdeling, standard: 0-hele landet}
}
\value{
En figur med gjennomsnitt ved konsultasjon og 6mnd etter
}
\description{
Denne funksjonen generer figur på gjennomsnitt ved konsultasjon og 6mnd etter
}
