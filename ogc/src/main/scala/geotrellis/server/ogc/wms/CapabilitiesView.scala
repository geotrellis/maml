package geotrellis.server.ogc.wms

import geotrellis.proj4.{CRS, LatLng}
import geotrellis.raster.CellSize
import java.net.{URI, URL}

import geotrellis.contrib.vlm.RasterSource
import geotrellis.vector.Extent
import opengis.wms.Layer
import scalaxb.CanWriteXML

import scala.xml.{Elem, NodeSeq}

/**
  *
  * @param model Model of layers we can report
  * @param serviceUrl URL where this service can be reached with addition of `?request=` query parameter
  * @param defaultCrs Common CRS, all layers must be available in at least this CRS
  */
class CapabilitiesView(model: RasterSourcesModel, serviceUrl: URL, defaultCrs: CRS = LatLng) {

  def toXML: Elem = {
    import opengis.wms._
    import CapabilitiesView._

    val service = Service(
      Name = Name.fromString("WMS", opengis.wms.defaultScope),
      Title = "GeoTrellis WMS",
      OnlineResource = OnlineResource(),
      KeywordList = Some(KeywordList(Keyword("WMS") :: Keyword("GeoTrellis") :: Nil))
    )

    val capability = {
      val getCapabilities = OperationType(
        Format = List("text/xml"),
        DCPType = List(DCPType(
          HTTP(Get = Get(OnlineResource(Map(
            "@{http://www.w3.org/1999/xlink}href" -> scalaxb.DataRecord(serviceUrl.toURI),
            "@{http://www.w3.org/1999/xlink}type" -> scalaxb.DataRecord(xlink.Simple: xlink.TypeType)))))
        )))

      val getMap = OperationType(
        Format = List("text/xml", "image/png", "image/geotiff", "image/jpeg"),
        DCPType = List(DCPType(
          HTTP(Get = Get(OnlineResource(Map(
            "@{http://www.w3.org/1999/xlink}href" -> scalaxb.DataRecord(serviceUrl.toURI),
            "@{http://www.w3.org/1999/xlink}type" -> scalaxb.DataRecord(xlink.Simple: xlink.TypeType)))))
        )))

      Capability(
        Request = Request(GetCapabilities = getCapabilities, GetMap = getMap, GetFeatureInfo = None),
        Exception = Exception(List("XML", "INIMAGE", "BLANK")),
        Layer = Some(modelAsLayer(model, defaultCrs))
      )
    }

    /**
      * Default scope generates an incorrect XML file (in the incorrect scope, prefixes all XML elements with `wms:` prefix.
      *
      * val defaultScope = scalaxb.toScope(Some("ogc") -> "http://www.opengis.net/ogc",
      * Some("wms") -> "http://www.opengis.net/wms",
      * Some("xlink") -> "http://www.w3.org/1999/xlink",
      * Some("xs") -> "http://www.w3.org/2001/XMLSchema",
      * Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance")
      */

    val ret: NodeSeq = scalaxb.toXML[opengis.wms.WMS_Capabilities](
      obj = WMS_Capabilities(service, capability, Map("@version" -> scalaxb.DataRecord("1.3.0"))),
      namespace = None,
      elementLabel = Some("WMS_Capabilities"),
      scope = scalaxb.toScope(
        Some("ogc") -> "http://www.opengis.net/ogc",
        Some("xlink") -> "http://www.w3.org/1999/xlink",
        Some("xs") -> "http://www.w3.org/2001/XMLSchema",
        Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance"
      ),
      typeAttribute = false
    )

    ret.asInstanceOf[scala.xml.Elem]
  }
}

object CapabilitiesView {
  import opengis.wms._
  implicit def toRecord[T: CanWriteXML](t: T): scalaxb.DataRecord[T] = scalaxb.DataRecord(t)

  def boundingBox(crs: CRS, extent: Extent, cellSize: CellSize): BoundingBox = {
    if (crs == LatLng) {
      BoundingBox(Map(
        "@CRS" -> s"EPSG:${crs.epsgCode.get}",
        "@minx" -> extent.ymin,
        "@miny" -> extent.xmin,
        "@maxx" -> extent.ymax,
        "@maxy" -> extent.xmax,
        "@resx" -> cellSize.width,
        "@resy" -> cellSize.height
      ))
    } else {
      BoundingBox(Map(
        "@CRS" -> s"EPSG:${crs.epsgCode.get}",
        "@minx" -> extent.xmin,
        "@miny" -> extent.ymin,
        "@maxx" -> extent.xmax,
        "@maxy" -> extent.ymax,
        "@resx" -> cellSize.width,
        "@resy" -> cellSize.height
      ))
    }
  }

  implicit class RasterSourceMethods(val self: RasterSource) {
    def toLayer(layerName: String, defaultCrs: CRS = LatLng): Layer = {
      Layer(
        Name = Some(layerName),
        Title = layerName,
        Abstract = Some(layerName),
        KeywordList = None,
        // extra CRS that is supported by this layer
        CRS = Set(defaultCrs, self.crs).flatMap(_.epsgCode).toList.map { code => s"EPSG:$code" },
        // TODO: global Extent for the CRS
        // EX_GeographicBoundingBox =   Some(self.extent.reproject(self.crs, LatLng)).map { case Extent(xmin, ymin, xmax, ymax) =>
        //  opengis.wms.EX_GeographicBoundingBox(xmin, xmax, ymin, ymax)
        // },
        BoundingBox =
          Set(self.crs, defaultCrs).toList.map { crs =>
            val rs = self.reproject(crs)
            boundingBox(crs, rs.extent, rs.cellSize)
          },
        Dimension = Nil,
        Attribution = None,
        AuthorityURL = Nil,
        Identifier = Nil,
        MetadataURL = Nil,
        DataURL = Nil,
        FeatureListURL = Nil,
        Style = Nil,
        MinScaleDenominator = None,
        MaxScaleDenominator = None,
        Layer = Nil,
        attributes = Map.empty
      )
    }
  }

  def modelAsLayer(model: RasterSourcesModel, crs: CRS = LatLng): Layer = {
    Layer(
      Name = Some("GeoTrellis WMS Layer"),
      Title = "GeoTrellis WMS Layer",
      Abstract = Some("GeoTrellis WMS Layer"),
      KeywordList = None,
      // All layers are avail at least at this CRS
      // All sublayers would have metadata in this CRS + its own
      CRS = crs.epsgCode.map { code => s"EPSG:$code" }.toList,
      // Extent of all layers in default CRS
      // Should it be world extent? To simplify tests and QGIS work it's all RasterSources extent
      // EX_GeographicBoundingBox = model.extent(LatLng).map { case Extent(xmin, ymin, xmax, ymax) =>
      //   opengis.wms.EX_GeographicBoundingBox(xmin, xmax, ymin, ymax)
      // },
      // TODO: bounding box for global layer
      BoundingBox = Nil,      Dimension = Nil,
      Attribution = None,
      AuthorityURL = Nil,
      Identifier = Nil,
      MetadataURL = Nil,
      DataURL = Nil,
      FeatureListURL = Nil,
      Style = Nil,
      MinScaleDenominator = None,
      MaxScaleDenominator = None,
      Layer = model.map.map { case (name, rs) => rs.toLayer(name, crs) }.toSeq,
      attributes = Map.empty
    )
  }
}
