// Based on code from codesuji
// https://www.codesuji.com/2019/05/24/F-and-MLNet-Anomaly/

open Microsoft.ML
open Microsoft.ML.Data
open Microsoft.ML.Transforms.TimeSeries
open XPlot.Plotly

type PriceData () =
    [<DefaultValue>]
    [<LoadColumn(0)>]
    val mutable public Date:string

    [<DefaultValue>]
    [<LoadColumn(1)>]
    val mutable public Open:float32

    [<DefaultValue>]
    [<LoadColumn(2)>]
    val mutable public High:float32

    [<DefaultValue>]
    [<LoadColumn(3)>]
    val mutable public Low:float32

    [<DefaultValue>]
    [<LoadColumn(4)>]
    val mutable public Close:float32

    [<DefaultValue>]
    [<LoadColumn(5)>]
    val mutable public AdjClose:float32

    [<DefaultValue>]
    [<LoadColumn(6)>]
    val mutable public Volume:float32
    
type PricePrediction () =
    [<DefaultValue>]
    val mutable public Date:string

    [<DefaultValue>]
    val mutable public Prediction:double[]

let dataPath = "GME.csv"

let ctx = MLContext()

let dataView = 
  ctx
    .Data
    .LoadFromTextFile<PriceData>(
      path = dataPath,
      hasHeader = true,
      separatorChar = ',')

let anomalyPValueHistoryLength = 30
let changePointPValueHistoryLength = 10
let anomalyConfidence = 95.0
let changePointConfidence = 95.0

let anomalyPipeline = 
  ctx
    .Transforms
    .DetectIidSpike(
      outputColumnName = "Prediction",
      inputColumnName = "Close",
      side = AnomalySide.TwoSided,
      confidence = anomalyConfidence, 
      pvalueHistoryLength = anomalyPValueHistoryLength)

let changePointPipeLine = 
  ctx
    .Transforms
    .DetectIidChangePoint(
      outputColumnName = "Prediction", 
      inputColumnName = "Close",
      martingale = MartingaleType.Power,
      confidence = changePointConfidence, 
      changeHistoryLength = changePointPValueHistoryLength)

let trainedAnomalyModel = anomalyPipeline.Fit(dataView)
let trainedChangePointModel = changePointPipeLine.Fit(dataView)

let transformedAnomalyData = trainedAnomalyModel.Transform(dataView);
let transformedChangePointData = trainedChangePointModel.Transform(dataView);

let anomalies = 
  ctx
    .Data
    .CreateEnumerable<PricePrediction>(transformedAnomalyData, reuseRowObject = false)

let changePoints = 
  ctx
    .Data
    .CreateEnumerable<PricePrediction>(transformedChangePointData, reuseRowObject = false)

// Build chart data
let toScatter l mode =
    let x' = l |> Seq.map(fun (x,y) -> x)
    let y' = l |> Seq.map(fun (x,y) -> y)
    Scatter(x = x', y = y', mode = mode)

let priceChartData = 
  anomalies
  |> Seq.map (fun p -> let p' = float (p.Prediction).[1]
                       (p.Date, p'))
  |> List.ofSeq 

let anomalyChartData = 
  anomalies
  |> Seq.map (fun p -> let p' = if (p.Prediction).[0] = 0. then None else Some (float (p.Prediction).[1])
                       (p.Date, p'))
  |> Seq.filter (fun (x,y) -> y.IsSome)
  |> Seq.map (fun (x,y) -> (x, y.Value))
  |> List.ofSeq 

let changePointChartData = 
  changePoints 
  |> Seq.map (fun p -> let p' = if (p.Prediction).[0] = 0. then None else Some (float (p.Prediction).[1])
                       (p.Date, p'))
  |> Seq.filter (fun (x,y) -> y.IsSome)
  |> Seq.map (fun (x,y) -> (x, y.Value))
  |> List.ofSeq


let styledLayout =
    Layout(
        title = "Gamestop Average Price Anomalies"
    )

let priceChartScatter = toScatter priceChartData "lines"
let anomalyScatter = toScatter anomalyChartData "markers"
let changePointScatter = toScatter changePointChartData "markers"

[priceChartScatter; anomalyScatter; changePointScatter]
|> Chart.Plot
|> Chart.WithLayout styledLayout 
|> Chart.WithLabels ["Price"; "Anomaly"; "ChangePoint" ]
|> Chart.WithLegend true
|> Chart.WithWidth 800
|> Chart.WithHeight 500
|> Chart.Show
