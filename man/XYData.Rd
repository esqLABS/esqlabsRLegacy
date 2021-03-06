% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XYData.R
\docType{class}
\name{XYData}
\alias{XYData}
\title{XYData}
\description{
A set of x and y value pairs
}
\section{Super classes}{
\code{\link[ospsuite.utils:Printable]{ospsuite.utils::Printable}} -> \code{\link[esqlabsRLegacy:Plotable]{esqlabsRLegacy::Plotable}} -> \code{XYData}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{xValues}}{An array of x-values. For time series, the values must be
in minutes.}

\item{\code{yValues}}{An array of y-values.}

\item{\code{yError}}{An array of arithmetic error of the y-values. Only positive
values are allowed}

\item{\code{xMax}}{Maximal value of x values plus xOffset multiplied by the
scaling factor}

\item{\code{xMin}}{Minimal value of x values plus xOffset multiplied by the
scaling factor}

\item{\code{yMax}}{Maximal value (plus error, if specified) of y values plus
yOffset multiplied by the scaling factor}

\item{\code{yMin}}{Minimal value (minus error, if specified) of y values plus
yOffset multiplied by the scaling factor}

\item{\code{dataType}}{Type of the data. See enum \code{XYDataTypes} for the list of
supported types.}

\item{\code{xDimension}}{Dimension of x values. See enum \code{ospDimensions} for the
list of supported dimensions.}

\item{\code{xUnit}}{Unit of x values}

\item{\code{yDimension}}{Dimension of y values. See enum \code{ospDimensions} for the
list of supported dimensions.}

\item{\code{yUnit}}{Unit of y values}

\item{\code{yErrorUnit}}{Unit of y error values}

\item{\code{MW}}{Molecular weight in g/mol. Required for conversion between
molar and mass dimensions. Can be \code{NULL} (default)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-XYData-new}{\code{XYData$new()}}
\item \href{#method-XYData-yMinPositive}{\code{XYData$yMinPositive()}}
\item \href{#method-XYData-xValuesProcessed}{\code{XYData$xValuesProcessed()}}
\item \href{#method-XYData-yValuesProcessed}{\code{XYData$yValuesProcessed()}}
\item \href{#method-XYData-yErrorProcessed}{\code{XYData$yErrorProcessed()}}
\item \href{#method-XYData-getAllMetaData}{\code{XYData$getAllMetaData()}}
\item \href{#method-XYData-setMetaData}{\code{XYData$setMetaData()}}
\item \href{#method-XYData-print}{\code{XYData$print()}}
\item \href{#method-XYData-clone}{\code{XYData$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-new"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the class. xVals, yVals, and
yError (optional) must be of the same length
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$new(xVals, yVals, label, yError = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xVals}}{An array of numeric x values.}

\item{\code{yVals}}{An array of numeric y values,}

\item{\code{label}}{A string that is used as a label (e.g. in the legend) for the data set}

\item{\code{yError}}{An array of numeric values of the arithmetic error. Optional}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{XYData} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-yMinPositive"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-yMinPositive}{}}}
\subsection{Method \code{yMinPositive()}}{
Returns the minimal value (minus error, if specified) of the y series that is not negative or null
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$yMinPositive()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The minimal non-negative value of the y series minus error, if
specified, plus yOffset multiplied by the scaling factor If no such
value exists, returns Inf.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-xValuesProcessed"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-xValuesProcessed}{}}}
\subsection{Method \code{xValuesProcessed()}}{
x values with all conversions applied.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$xValuesProcessed(unit = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{unit}}{Target unit. If \code{NULL} (default), no conversion between units
is applied.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Raw xValues plus xOffset multiplied by xFactor and converted to a
specified unit. It is assumed that raw xValues are in \code{xUnit}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-yValuesProcessed"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-yValuesProcessed}{}}}
\subsection{Method \code{yValuesProcessed()}}{
y values with all conversions applied.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$yValuesProcessed(unit = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{unit}}{Target unit. If \code{NULL} (default), no conversion between units
is applied.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Raw yValues plus yOffset multiplied by yFactor and converted to a
specified unit.
It is assumed that raw yValues are in \code{yUnit}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-yErrorProcessed"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-yErrorProcessed}{}}}
\subsection{Method \code{yErrorProcessed()}}{
y error values with all conversions applied.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$yErrorProcessed(unit = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{unit}}{Target unit. If \code{NULL} (default), the no conversion between
units is applied.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Raw yError multiplied by yFactor and converted to a specified
unit. It is assumed that raw yError are in \code{yUnit}. If no error is
specified, \code{NULL} is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-getAllMetaData"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-getAllMetaData}{}}}
\subsection{Method \code{getAllMetaData()}}{
Meta data list of \code{XYData} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$getAllMetaData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A named list holding the metadata of this \code{XYData}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-setMetaData"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-setMetaData}{}}}
\subsection{Method \code{setMetaData()}}{
Adds a new entry to meta data list of \code{XYData} object or changes its
value if name is already present in meta data. If only \code{name} is provided
or if \code{value} is set to NULL, entry with corresponding name is deleted
from meta data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$setMetaData(name, value = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{Name of new meta data list entry}

\item{\code{value}}{Value of new meta data list entry}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-print"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Rest arguments.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-XYData-clone"></a>}}
\if{latex}{\out{\hypertarget{method-XYData-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{XYData$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
