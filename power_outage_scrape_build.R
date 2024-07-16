library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflegend)
library(sf)
library(htmlwidgets)
library(htmltools)
library(janitor)

# download file

try(download.file("https://services.arcgis.com/BLN4oKB0N1YSgvY8/ArcGIS/rest/services/Power_Outages_(View)/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=",
                  "power_outages.geojson"))

# filter

power_outages <- st_read("power_outages.geojson") %>% 
  select(UtilityCompany, StartDate, EstimatedRestoreDate, Cause, ImpactedCustomers, County, OutageType, geometry)

# saved function to convert the milliseconds from UTC 

ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

# saved function to convert sting to title case

stringtotitle = function(x=County) {
  str_to_title(x)
}

# convert milliseconds to dates and clean df

power_outages1 <- power_outages %>% 
  mutate_at(vars(StartDate, EstimatedRestoreDate), ms_to_date, t0 = "1970-01-01", timezone = "America/Los_Angeles") %>% 
  mutate_at(vars(County), stringtotitle)

# format dates 

power_outages1$StartDate <- format(as.POSIXct(power_outages1$StartDate, format = "%m/%d/%Y %H:%M"), 
                  "%B %d, %Y %l:%M %p")

power_outages1$EstimatedRestoreDate <- format(as.POSIXct(power_outages1$EstimatedRestoreDate, format = "%m/%d/%Y %H:%M"), 
                  "%B %d, %Y %l:%M %p")

# replace NA values

power_outages1$Cause = power_outages1$Cause %>% replace_na('Not yet known') 

power_outages1$StartDate = power_outages1$StartDate %>% replace_na('Unknown') 

power_outages1$EstimatedRestoreDate = power_outages1$EstimatedRestoreDate %>% replace_na('Unknown') 

power_outages1$ImpactedCustomers = power_outages1$ImpactedCustomers %>% replace_na('Unknown') 

power_outages1$County = power_outages1$County %>% replace_na('Unknown') 

power_outages1$ImpactedCustomers = power_outages1$ImpactedCustomers %>% replace_na('Unknown') 

# save 

write_csv(power_outages1,"current_power_outages.csv")

# map components

popups <- paste(sep = "",
                paste(sep = "","<font size='3'><b><p style='color:#0059F6'>", power_outages1$County, " County <br> ", power_outages1$UtilityCompany, " outage </b><br> <font size='2'> <p style='color:black'> Impacted customers: <b>",  format(power_outages1$ImpactedCustomers, big.mark=","),"</b>","<br> Start time: <b>", power_outages1$StartDate, "</b> <br> Estimated restoration time: <b>", power_outages1$EstimatedRestoreDate,"</b> <br> Reason: <b>", power_outages1$Cause,"</b>")) %>% 
  lapply(htmltools::HTML)

getColor <- function(power_outages1) {
  sapply(power_outages1$ImpactedCustomers, function(ImpactedCustomers) {
    if(is.na(ImpactedCustomers)) {
      "gray"
    } else if(ImpactedCustomers <= 49) {
      "#99bcfb"
    } else if(ImpactedCustomers <= 499) {
      "#4c8af8"
    } else if(ImpactedCustomers <= 4999) {
      "#0059F6"
    } else {
      "#003593" 
    } })
}

labels <- c("1 - 49", "50 - 499", "500 - 4,999", "5,000+")

factorPal <- colorFactor(c("#99bcfb", "#4c8af8", "#0059F6", "#003593"), levels = labels, ordered = TRUE)

sort_val = factor(labels, levels = c("1 - 49", "50 - 499", "500 - 4,999", "5,000+"))

today_UTC <- as.POSIXct(Sys.time())
today_posix <- format(today_UTC, tz="America/Los_Angeles",usetz=TRUE)
today <- strptime(as.character(today_posix), format = "%Y-%m-%d %H:%M:%S")
today_display <- format(today, "%b. %d, %Y %l:%M %p")

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    top: 0.8%;
    text-align: left;
    background-color: background:#0059F6;
    width: 100%;
    border-radius: 4px 4px 4px 4px;
  }
  .leaflet-control.map-title .headline{
    font-weight: bold;
    font-size: 26px;
    color: white;
    padding: 2px;
    background-color: #F98C00;
    background:#0059F6;
    border-radius: 4px 4px 0px 0px;
  }
  .leaflet-control.map-title .subheadline {
    font-size: 12px;
    font-weight: bold;
    color: rgb(85,85,85);
    padding: 2px;
    background: white;
    border-radius: 0px 0px 4px 4px;
  }
  .leaflet-control.map-title .subheadline a {
    color: #BE0000;
    font-weight: bold;
  }
  
  @media only screen and (max-width: 550px) {
    .leaflet-control.map-title .headline {
      font-size: 26px;
    border-radius: 4px 4px 0px 0px;
    }
    .leaflet-control.map-title .subheadline {
      font-size: 12px;
    border-radius: 0px 0px 4px 4px;
    }
  @media only screen and (max-width: 420px) {
    .leaflet-control.map-title .headline {
      font-size: 26px;
    border-radius: 4px 4px 0px 0px;
    }
    .leaflet-control.map-title .subheadline {
      font-size: 12px;
    border-radius: 0px 0px 4px 4px;
    }
"))

title <- tags$div(
  tag.map.title, HTML(paste(sep="",
  "<div class='headline'>Power Outage Tracker</div>
  <div class='subheadline'>Last updated ",today_display,"</div")
  ))

tag.map.footer <- tags$style(HTML("
  .leaflet-control.map-footer {
    position: fixed !important;
    right: 0;
    bottom: 6px;
    text-align: right;
    padding: 5px;
    background: rgba(255,255,255,0.75);
    font-style: italic;
    font-size: 10px;
  }
"))

footer <- tags$div(
  tag.map.footer, HTML("<div> Source: CA Governor's Office of Emergency Services </div>"))

tag.map.box <- tags$style(HTML("
  .leaflet-control.box {
    position: fixed !important;
    width: 20.5rem;
    height: 24vh;
    background: white;
    border-radius: 4px 4px 4px 4px;
  }
  @media only screen and (max-width: 550px) {
    .leaflet-control.box {
      width: 20.5rem;
      height: 24vh;
    }
  @media only screen and (max-width: 420px) {
    .leaflet-control.box {
      width: 20.5rem;
      height: 24vh;
    }
"))

box <- tags$div(
  tag.map.box, HTML("<div> </div>"))

# actual maps

bayarea_outage_map <- leaflet(options = leafletOptions(zoomControl = FALSE, hoverToWake=FALSE, attributionControl=TRUE)) %>%
  addControl(html = box, position = "topleft", className = "box") %>% 
  addControl(position = "topleft", html = title, className="map-title") %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
   if (L.Browser.mobile) {
   map.removeControl(map.L.control.zoom);
} }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%
  addMapPane(name = "search", zIndex = 430) %>%
  addProviderTiles(providers$CartoDB.Positron, options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 16, dragging = FALSE)) %>%
  setView(-122.4484334,37.8427456, zoom = 8) %>%
  addCircleMarkers(data = power_outages1,
                 color = getColor(power_outages1),
                 fillColor = getColor(power_outages1),
                 stroke = FALSE,
                 fillOpacity = 0.7,
                 radius = 7,
                 label = popups,
                 popupOptions = popupOptions(keepInView = T,
                     autoPanPaddingTopLeft=c(100,120)),
                 labelOptions = labelOptions(
                   direction = "auto")) %>%
  addSearchOSM(options = searchOptions(collapsed=FALSE, minLength = 7,zoom=13, position="topleft", autoCollapse = TRUE,
    hideMarkerOnCollapse = TRUE, autoResize = TRUE)) %>% 
  onRender("function(el, x) {
        $('input.search-input')[0].placeholder = 'Search your address'
        }") %>%
   addLegendFactor(pal = factorPal,
                  fillOpacity = .7,
                  values = sort_val,
                  title = 'Customers Impacted',
                  position = 'topleft',
                  orientation = 'horizontal',
                  shape = 'circle',
                  className = "legend") %>%
  addControl(footer, position = "bottomright", className="map-footer")

  
  socal_outage_map <- leaflet(options = leafletOptions(zoomControl = FALSE, hoverToWake=FALSE, attributionControl=TRUE)) %>%
  addControl(html = box, position = "topleft", className = "box") %>% 
  addControl(position = "topleft", html = title, className="map-title") %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%
  addMapPane(name = "search", zIndex = 430) %>%
  addProviderTiles(providers$CartoDB.Positron, options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 16, dragging = FALSE)) %>%
  setView(-118.2426, 34.0549, zoom = 8) %>%
  addCircleMarkers(data = power_outages1,
                 color = getColor(power_outages1),
                 fillColor = getColor(power_outages1),
                 stroke = FALSE,
                 fillOpacity = 0.7,
                 radius = 7,
                 label = popups,
                 popupOptions = popupOptions(keepInView = T,
                     autoPanPaddingTopLeft=c(100,120)),
                 labelOptions = labelOptions(
                   direction = "auto")) %>%
  addSearchOSM(options = searchOptions(collapsed=FALSE, minLength = 3,zoom=13,   position="topleft", autoCollapse = TRUE,
    hideMarkerOnCollapse = TRUE)) %>% 
  onRender("function(el, x) {
        $('input.search-input')[0].placeholder = 'Search your address'
        }") %>%
   addLegendFactor(pal = factorPal,
                  fillOpacity = .7,
                  values = sort_val,
                  title = 'Customers Impacted',
                  position = 'topleft',
                  orientation = 'horizontal',
                  shape = 'circle',
                  className = "legend") %>%
  addControl(footer, position = "bottomright", className="map-footer")


fresno_outage_map <- leaflet(options = leafletOptions(zoomControl = FALSE, hoverToWake=FALSE, attributionControl=TRUE)) %>%
  addControl(html = box, position = "topleft", className = "box") %>% 
  addControl(position = "topleft", html = title, className="map-title") %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%
  addMapPane(name = "search", zIndex = 430) %>%
  addProviderTiles(providers$CartoDB.Positron, options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 16, dragging = FALSE)) %>%
  setView(-119.7871, 36.7378, zoom = 8) %>%
  addCircleMarkers(data = power_outages1,
                 color = getColor(power_outages1),
                 fillColor = getColor(power_outages1),
                 stroke = FALSE,
                 fillOpacity = 0.7,
                 radius = 7,
                 label = popups,
                 popupOptions = popupOptions(keepInView = T,
                     autoPanPaddingTopLeft=c(100,120)),
                 labelOptions = labelOptions(
                   direction = "auto")) %>%
  addSearchOSM(options = searchOptions(collapsed=FALSE, minLength = 3,zoom=13,   position="topleft", autoCollapse = TRUE,
    hideMarkerOnCollapse = TRUE)) %>% 
  onRender("function(el, x) {
        $('input.search-input')[0].placeholder = 'Search your address'
        }") %>%
   addLegendFactor(pal = factorPal,
                  fillOpacity = .7,
                  values = sort_val,
                  title = 'Customers Impacted',
                  position = 'topleft',
                  orientation = 'horizontal',
                  shape = 'circle',
                  className = "legend") %>%
  addControl(footer, position = "bottomright", className="map-footer")


ca_outage_map <- leaflet(options = leafletOptions(zoomControl = FALSE, hoverToWake=FALSE, attributionControl=TRUE)) %>%
  addControl(html = box, position = "topleft", className = "box") %>% 
  addControl(position = "topleft", html = title, className="map-title") %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%
  addMapPane(name = "search", zIndex = 430) %>%
  addProviderTiles(providers$CartoDB.Positron, options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 16, dragging = FALSE)) %>%
  setView(-119.4179, 36.7783, zoom = 6) %>%
  addCircleMarkers(data = power_outages1,
                 color = getColor(power_outages1),
                 fillColor = getColor(power_outages1),
                 stroke = FALSE,
                 fillOpacity = 0.7,
                 radius = 7,
                 label = popups,
                 popupOptions = popupOptions(keepInView = T,
                     autoPanPaddingTopLeft=c(100,120)),
                 labelOptions = labelOptions(
                   direction = "auto")) %>%
  addSearchOSM(options = searchOptions(collapsed=FALSE, minLength = 3,zoom=13,   position="topleft", autoCollapse = TRUE,
    hideMarkerOnCollapse = TRUE)) %>% 
  onRender("function(el, x) {
        $('input.search-input')[0].placeholder = 'Search your address'
        }") %>%
   addLegendFactor(pal = factorPal,
                  fillOpacity = .7,
                  values = sort_val,
                  title = 'Customers Impacted',
                  position = 'topleft',
                  orientation = 'horizontal',
                  shape = 'circle',
                  className = "legend") %>%
  addControl(footer, position = "bottomright", className="map-footer")

saveWidget(bayarea_outage_map, file="bayarea_outage_map.html")
saveWidget(socal_outage_map, file="socal_outage_map.html")
saveWidget(fresno_outage_map, file="fresno_outage_map.html")
saveWidget(ca_outage_map, file="ca_outage_map.html")
  
