\name{kmlDevice}
\alias{kmlDevice}
\title{Create graphics device to generate KML content}
\description{
  This function allows us to create a regular graphics device
  for creating R plots and graphical displays.
  The content is generated as KML and can be displayed in Google
  Earth.
  The idea is that the caller specifies a geographical rectangular
  region for the device and the R plots are drawn within that
  region.
  
}
\usage{
kmlDevice(origin, w, h, filename = character(), rdims = c(1000, 1000),
           col = "white", bg = "transparent", ps = 10, ...,
            docName = "R KML", description = "R plots in KML",
            doc = createKMLDoc(docName, description, origin + c(w/2, h/2)),
             center = TRUE, folder = "Plot")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{origin}{the longitude, latitude pair giving the bottom left
    corner of the rectangular region, or the mid-point of the rectangle
    if \code{center} is \code{TRUE}.
    Longitude ranges from -180 to 180. Longitude from 90 to -90
  }
  \item{w,h}{the width and height of the rectangular region in degrees}
  \item{filename}{the name of the file to which to write the generated
    KML content when the device is closed.  This can be an empty
    character vector and the content is not written to a file. Instead,
    the caller can access the KML document via the return value from this
    function. }
  \item{rdims}{ an integer vector of dimensions giving the width and
    height to use within R for the graphics device. This is how big R
    sees the device, regardless of the size of the rectangle on Googe Earth.}
  \item{col}{the default color to use for drawing. This can be a named
    color, e.g. "red", or an RGB or RGB alpha specification,
    e.g. "#FF0000" or "#0044CC99", or a string with class \code{AsIs}
    giving a color in KML color format, i.e. alpha blue green red, e.g.
    "99cc4400".}
  \item{bg}{the background color for the graphics device}
  \item{ps}{the default size for characters}
  \item{\dots}{unused at present}
  \item{docName}{a string giving the a short description or name for the
    KML document being created. This is displayed in Google Earth's
    Place's list when the KML is loaded}
  \item{description}{a string giving a longer description of the
    document. This is available in Google Earth to the viewer.
  }
  \item{doc}{the KML document object,  or this can be an XMLInternalNode
    which allows a caller to add to an existing KML document and to
    control where the content is added.}
  \item{center}{a logical value controlling whether we center the
    rectangle or use the origin as the South-West point}
  \item{folder}{the name of the KML folder in which to collect the
    elements of the plot.  If this is an empty vector, no folder is
    created and the elements are added to the Document element of the
    KML document.  If more than one plot is created within this device,
    this is used as a prefix for each folder created to house the
    elements of each plot. }
}
\value{
  A list with two elements
  \item{dev}{an object representing the C-level graphics device
    structure}
  \item{doc}{a reference to the XML document that is updated by the
    device as graphics commands are processed}
}
\references{
  \url{http://code.google.com/apis/kml/documentation/kmlreference.html}   
  R Internals Manual
}
\author{Duncan Temple Lang}

\seealso{
  RKML package \url{http://www.omegahat.org/RKML}
}
\examples{
data(SFTemperatures)
dev = kmlDevice(c(-122.68, 37.75), 3, 3, I("sf.kml"), c(500, 800))
with(subset(SFTemperatures, dates >= as.Date("1-1-2004", "\%d-\%m-\%Y")),
             { plot(dates, temp, type = "l", main = "Temperature")
               lines(supsmu(dates, temp), col = "red", lwd = 3)
             })
dev.off()


library(lattice)
dev = kmlDevice(c(-122.68, 37.75), 3, 3, I("sfBoxplot.kml"), c(500, 800))
bwplot(temp ~ ordered(months(dates), unique(months(dates))), SFTemperatures)
dev.off()


# Now we will create three plots, one for each city in 3 different
# KML files.

data(USCities, package = "RKML") # From RKML
data(PortlandTemperatures)
data(SeattleTemperatures)
USCities$longitude = -USCities$longitude
locs = subset(USCities, name \%in\% c("SanFrancisco", "Portland", "Seattle") & state \%in\% c("CA", "OR", "WA"))[-1,]
locs = locs[, c(2:1, 3)]
cities = as.character(locs$name)
mapply(function(city, locs, data) {
         dev = kmlDevice(locs[1, 1:2], 3, 3,
                          I(sprintf("\%s.kml",   city)),dims = c(500,800))
         print(bwplot(temp ~ ordered(months(dates), unique(months(dates))), data))
         dev.off()
       }, cities, split(locs[, 1:2], locs$name),
          list(SFTemperatures, PortlandTemperatures, SeattleTemperatures))


# Now we will create the same 3 plots, but put them in a single KML
#  file. We create a KMLDoc ourselves and 

library(RKML)  # for createKMLDoc
doc = createKMLDoc("3 Boxplots", "Boxplots for 3 cities", c(-122, 42))
docNode = xmlRoot(doc)[["Document"]]
mapply(function(city, loc, temps) {
        kmlDevice(loc[1, 1:2], 3, 3, doc = docNode, dims = c(500,800), folder = city)
        print(bwplot(temp ~ ordered(months(dates), unique(months(dates))), temps))
        dev.off()
       }, cities, split(locs[, 1:2], locs$name)[cities],
          list(SFTemperatures, PortlandTemperatures, SeattleTemperatures))

saveXML(doc, I("boxplots.kml"))
}
\keyword{graphics}
\keyword{aplot}
\keyword{iplot}


