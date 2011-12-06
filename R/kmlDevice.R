
library(RGraphicsDevice)

#XXX Add styles
# reuse default style
# don't allow Polygon/polyline to create new top-level styles unless they
# are new.

kmlDevice =
function(origin, w, h, filename = character(),
         rdims = c(1000, 1000), col = "white", bg = "transparent", ps = 10, ...,
          docName = "R KML", description = "R plots in KML",
           doc = createKMLDoc(docName, description, origin + c(w/2, h/2)),
         center = TRUE, folder = "Plot")
   # we'll compute the window for the LookAt at the end of the page/document.
{

  plotNum = 1L

  origin = as.numeric(origin)
  
  if(center) 
     origin = origin + c(- w/2, h/2)

     # the node to which we add top-level
  if(is(doc, "XMLInternalNode"))
    curNode = doc
  else
    curNode = xmlRoot(doc)[["Document"]]

  if(length(folder)) 
    curNode = newXMLNode("Folder", newXMLNode("name", folder), parent = curNode)

     # these two functions map R device coordinates to longitude, latitude.
  transformX = function(x) 
    origin[1] + w * x / rdims[1]

  transformY = function(y) 
    origin[2] - (h * y / rdims[2]  )

  #XXX Have to add styles, etc. for ge.

  placeMark = function(type, parent = curNode)
        newXMLNode("Placemark", parent = parent, newXMLNode("name", type))

  addStyle = function(node, styleType, gc) {
     newXMLNode("Style",
                  sty <- newXMLNode(styleType), parent = node)

     newXMLNode("width", gc$lwd, parent = sty)
     newXMLNode("color", RKML:::rgbToKMLColor(as(gc$col, "RGB")), parent = sty)
     fill = as(gc$fill, "RGB")        
     if(fill != "transparent") {
         newXMLNode("fill", 1, parent = sty)
     }
     sty
  }
   
  
  circle = function(x, y, r, ge, dev) {
     pl = placeMark("circle")
     node = kmlCircle(transformX(x), transformY(y), w * r /rdims[1], parent = pl)
     xmlAttrs(node) = c(type = "circle")
     node
  }

  line = function(x0, y0, x1, y1, ge, dev) {
    pl = placeMark("line")
    node = kmlSegments(transformX(x0), transformY(y0), transformX(x1), transformY(y1), parent = pl)
    xmlAttrs(node[[1]]) = c(type = "line")
    pl
  }  
  
  polygon = function(num, x, y, ge, dev) {
      node = kmlPolygon(transformX(x), transformY(y), parent = curNode)
      xmlAttrs(node) = c(type = "polygon")     
     node
  }

firstPolyline = FALSE
  polyline = function(num, x, y, ge, dev) {
      #  Call kmlSegments() or some variation thereof.
    if(firstPolyline) {
      firstPolyline <<- FALSE
browser()      
    }
    coords = paste(transformX(x[1:num]), transformY(y[1:num]), 0, sep = ",", collapse = " ")
    pl = newXMLNode("Placemark", attrs = c(type = "PolyLine"), newXMLNode("name", "polyline"),
                     newXMLNode("LineString", attrs = c(type = "PolyLine"),
                                 newXMLNode("coordinates", coords)),
                     parent = curNode)

    addStyle(pl, "LineStyle", ge)

    pl
  }  
  
  text = function(x, y, text, rot, hadj, ge, dev) {
      node = kmlText(transformX(x), transformY(y), text, parent = curNode)
      xmlAttrs(node[[1]]) = c(type = "text")
      node
  }

  rect = function(x, y, w, h, ge, dev) {
    pl = placeMark("rect")
    node = kmlPolygon(transformX(c(x, x+w, x+w, x, x)), transformY(c(y, y, y+h, y+h, y)), parent = pl, style = NULL)
    xmlAttrs(node) = c(type = "rectangle")
    addStyle(pl, "PolyStyle", ge)    
    pl
  }


  numPages = 0
  newPage = function(ge, dev) {
    numPages <<- numPages + 1
    if(numPages == 1)
      return(TRUE)
    
    plotNum = plotNum + 1L
    nm = sprintf("%s%d", folder, plotNum)
    curNode <<- newXMLNode("Folder", newXMLNode("name", nm),
                            sibling = curNode)
  }

  close = function(dev) {
     if(length(filename))
        saveXML(doc, filename) # if the caller specified I(filename), then no KMZ.
  }
    
  strWidth = function(str, gc, dev)
         nchar(str) * max(10L, gc$ps) * gc$cex

  funs = dummyDevice()
  funs@circle = circle
  funs@line = line  
  funs@polygon = polygon
  funs@polyline = polyline
  funs@text = text  
  funs@rect = rect
  funs@newPage = newPage
  funs@close = close
  funs@strWidth = strWidth
  
  funs@mode = NULL
  funs@clip = NULL  
  funs@metricInfo = NULL
  funs@activate = NULL
  funs@deactivate = NULL
  funs@locator = NULL
  funs@onExit = NULL

  funs@initDevice = function(dev) {
    dev$ipr = rep(1/72.27, 2)
    dev$cra = rep(c(6, 13)/12) * 10
    dev$canClip = FALSE
    dev$canChangeGamma = TRUE
    dev$startgamma = 1
    dev$startps = ps
    dev$startcol = as(col, "RGBInt") #XXX color as KML color. Set default style.
  }

 list( dev = graphicsDevice(funs, rdims, col, bg, ps), doc = doc)
}

if(FALSE) {
  dev = kmlDevice(c(-122.68, 37.75), 3, 3, I("dev.kml"), c(500, 800))
#  plot(1:10)
  x = rnorm(1000)
  plot(density(x))
  abline(v = 0)
  dev.off()
}
