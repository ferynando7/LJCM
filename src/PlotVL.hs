module PlotVL where

import qualified Graphics.Vega.VegaLite as VL
import Data.Text (Text, pack)

barPlot :: Text -> VL.VLSpec
barPlot xName = 
    let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Nominal]
            . VL.position VL.Y [VL.PName (pack "binnedData"), VL.PAggregate VL.Count, VL.PmType VL.Quantitative, VL.PAxis [VL.AxTitle (pack "count")]]
    in VL.asSpec [VL.mark VL.Bar [VL.MOpacity 1.0, VL.MColor (pack "#a3c6de")], encoding []]

linePlot :: Text -> Text -> VL.VLSpec
linePlot xName yName = 
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative]
  in VL.asSpec [VL.mark VL.Line [VL.MColor (pack "blue")], encoding []]

scatterBlue xName yName (xmin, xmax) (ymin, ymax) =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative, VL.PScale [VL.SDomain $ VL.DNumbers [xmin, xmax]]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative, VL.PScale [VL.SDomain $ VL.DNumbers [ymin, ymax]]]
  in VL.asSpec [VL.mark VL.Circle [VL.MColor (pack "blue")], encoding []]
  
scatterGreen xName yName (xmin, xmax) (ymin, ymax) =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative, VL.PScale [VL.SDomain $ VL.DNumbers [xmin, xmax]]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative, VL.PScale [VL.SDomain $ VL.DNumbers [ymin, ymax]]]
  in VL.asSpec [VL.mark VL.Circle [VL.MColor (pack "green")], encoding []]
  
scatterPlotWithColor :: Text -> Text -> Text -> (Double, Double) -> (Double, Double) -> VL.VLSpec
scatterPlotWithColor xName yName zName (xmin, xmax) (ymin, ymax) =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative, VL.PScale [VL.SDomain $ VL.DNumbers [xmin, xmax]]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative, VL.PScale [VL.SDomain $ VL.DNumbers [ymin, ymax]]]
            . VL.color [ VL.MName zName, VL.MmType VL.Quantitative, VL.MScale [VL.SScheme (pack "blues") [0.0, 1.0]]]
  in VL.asSpec [VL.mark VL.Circle [], encoding []]

density2DPlot :: Text -> Text -> (Double, Double) -> (Double, Double) -> VL.VLSpec
density2DPlot xName yName (xmin, xmax) (ymin, ymax) = 
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PBin [VL.Nice False, VL.Steps [0.05, 0.5, 5.0], VL.Extent xmin xmax], VL.PmType VL.Quantitative]
            . VL.position VL.Y [VL.PName yName, VL.PBin [VL.Nice False, VL.Steps [0.05, 0.5, 5.0], VL.Extent ymin ymax], VL.PmType VL.Quantitative]
            . VL.color [ VL.MAggregate VL.Count, VL.MName (pack "col"), VL.MmType VL.Quantitative, VL.MScale [VL.SScheme (pack "blues") [0.0, 1.0]]]
  in VL.asSpec [VL.mark VL.Rect [], encoding []]

imagePlot :: Text -> Text -> Text -> VL.VLSpec
imagePlot xName yName zName =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Nominal, VL.PAxis [VL.AxGridOpacity 0.1]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Nominal, VL.PSort [VL.Descending], VL.PAxis [VL.AxGridOpacity 0.1]]
            . VL.fill [ VL.MName zName, VL.MmType VL.Quantitative, VL.MScale [VL.SScheme (pack "blues") [0.0, 1.0]]]
            . VL.stroke [ VL.MName zName, VL.MmType VL.Quantitative, VL.MScale [VL.SScheme (pack "blues") [0.0, 1.0]],
                          VL.MLegend [VL.LType VL.GradientLegend]]
  in VL.asSpec [VL.mark VL.Rect [], encoding []]
  
imageFacetPlot :: Text -> Text -> Text -> VL.VLSpec
imageFacetPlot xName yName zName =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Ordinal, VL.PAxis [VL.AxGrid False]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Ordinal, VL.PSort [VL.Descending], VL.PAxis [VL.AxGrid False]]
            . VL.fill [ VL.MName zName, VL.MmType VL.Quantitative, VL.MScale [VL.SScheme (pack "blues") [0.0, 1.0]], VL.MLegend [VL.LOrient VL.LOBottom]]
            . VL.stroke [ VL.MName zName, VL.MmType VL.Quantitative, VL.MScale [VL.SScheme (pack "blues") [0.0, 1.0]],
                          VL.MLegend [VL.LOrient VL.LOBottom, VL.LDirection VL.Horizontal, VL.LType VL.GradientLegend]]
  in VL.asSpec [VL.mark VL.Rect [], encoding [], VL.width 200,  VL.height 100]

data SpecGrid = H [[VL.VLSpec]] | V [[VL.VLSpec]] | L [VL.VLSpec] | S VL.VLSpec | F (Text, Int, VL.VLSpec)

data InputData = Cols [(Text, VL.DataValues)]
               | File FilePath

plot :: (Double, Double) -> SpecGrid -> InputData -> VL.VegaLite
plot (figw,figh) specGrid dataPoints =
    let description = VL.description (pack "Plot")
        dat' = case dataPoints of
            Cols cols -> foldl (.) (VL.dataFromColumns []) (map (uncurry VL.dataColumn) cols) []
            File fp -> VL.dataFromSource (pack fp) []
        configure = VL.configure
            . VL.configuration (VL.Axis
                                        [ VL.Domain False,
                                          VL.LabelColor (pack "#7F7F7F"),
                                          VL.LabelPadding 4,
                                          VL.TickColor (pack "#7F7F7F"),
                                          VL.TickSize 5.67,
                                          VL.Grid True,
                                          VL.GridColor (pack "#FFFFFF")
                                          ])
        spec = case specGrid of
            S s -> VL.layer [s]
            L ls -> VL.layer ls
            H lss -> VL.hConcat (map (VL.asSpec . (:[]) . VL.layer) lss)
            V lss -> VL.vConcat (map (VL.asSpec . (:[]) . VL.layer) lss)
            F (_, _, s) -> VL.specification s
        facet = case specGrid of
            F (field, nColumns, _) -> [VL.columns $ fromIntegral nColumns, VL.facetFlow [VL.FName field, VL.FmType VL.Nominal]]
            _   -> [VL.width figw,  VL.height figh]
    in VL.toVegaLite $ [VL.background (pack "#f9f9f9"), configure [], description, dat', spec] ++ facet