% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeReports.R
\name{makeReports}
\alias{makeReports}
\title{Make vineyard reports}
\usage{
makeReports(
  plantListCSV = NA,
  beeDataCSV = NA,
  iNatFolder = NA,
  reportFolder = NA,
  vinePlDatCSV = NA,
  predictedBeesCSV = NA,
  dataStoragePath = NA,
  famGenPath = NA,
  orCountyShpPath = NA,
  orEcoregShpPath = NA,
  beeAbstractsPath = NA
)
}
\arguments{
\item{plantListCSV}{\emph{Required} - CSV of plants from Oregon Flora}

\item{beeDataCSV}{\emph{Required} - CSV of bee/plant interactions from OBA}

\item{iNatFolder}{\emph{Required} - Folder/subfolders containing vineyard iNaturalist CSV files}

\item{reportFolder}{\emph{Required} - Folder for writing reports to}

\item{vinePlDatCSV}{(Optional) output csv of all vineyard records. Skips writing if NA}

\item{predictedBeesCSV}{(Optional) output csv of predicted bees for each vineyard. Skips writing if NA}

\item{dataStoragePath}{(Optional) .Rdata storage path for internal function data. Skips writing if NA}

\item{famGenPath}{(Optional) Path to bee genus-family lookup csv}

\item{orCountyShpPath}{(Optional) Path to Oregon county polygons}

\item{orEcoregShpPath}{(Optional) Path to Oregon ecoregion polyogons}

\item{beeAbstractsPath}{(Optional) Path to bee/plant Abstract csv}
}
\value{
Nothing - writes to vinePlDatCSV, predictedBeesCSV, or dataStoragePath
}
\description{
Create vineyard reports from iNaturalist project data, bee/plant interactions, and plant list.
}
\examples{

makeReports(plantListCSV = './cleanedPlantList2024.csv',
  beeDataCSV = './OBA_2017_2023_v16Oct24.csv',
  iNatFolder =  './iNat records',
  reportFolder = './reports',
  vinePlDatCSV = NA, predictedBeesCSV = NA, dataStoragePath = NA)

}
