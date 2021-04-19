open System
open System.Collections
open FSharp.Data
open Microsoft.Azure.CognitiveServices.AnomalyDetector
open Microsoft.Azure.CognitiveServices.AnomalyDetector.Models

type Stocks = CsvProvider<"GME.csv">

let createClient endpoint key =
    let client = new AnomalyDetectorClient(credentials = new ApiKeyServiceClientCredentials(key), Endpoint = endpoint)
    client

let generateRequest fileName =
    let lines = Stocks.Load("GME.csv").Rows
    let l = lines |> Seq.map (fun l -> new Point(l.Date.Date, (float) l.Close)) |> Seq.toList
    let csharpl = ResizeArray l
    new Request(csharpl, Granularity.Daily)

let entireDetectSampleAsync (client:IAnomalyDetectorClient) (request:Request) =
    let detectAnomalies (client:IAnomalyDetectorClient) (request:Request) =
        async {
            let! result = (client.EntireDetectAsync(request)) |> Async.AwaitTask
            return result
        }

    printfn "Detecting anomalies in the entire time series."
    try
        let (result:EntireDetectResponse) = (detectAnomalies client request) |> Async.RunSynchronously

        let anomalies = result.IsAnomaly
                        |> Seq.zip request.Series
                        |> Seq.filter(fun (req, res) -> res = true)

        match (anomalies |> Seq.length) with
        | 0 -> printfn "No anomalies detected in the series."
        | 1 -> printfn "One anomaly was detected!"
        | x -> printfn "%A anomalies were detected!" x

        anomalies
        |> Seq.iter (fun (req, res) -> printfn "Anomalous date %A, closing value %A" req.Timestamp req.Value)
    with
      | :? System.AggregateException as ex -> printfn "%s" (ex.ToString())
    

[<EntryPoint>]
let main argv =
    // NOTE:  before you run this, you'll need to set environment variables.
    // setx ANOMALY_DETECTOR_KEY [your key]
    // setx ANOMALY_DETECTOR_ENDPOINT [your endpoint]
    let endpoint = Environment.GetEnvironmentVariable("ANOMALY_DETECTOR_ENDPOINT")
    let key = Environment.GetEnvironmentVariable("ANOMALY_DETECTOR_KEY")

    let client = createClient endpoint key
    let request = generateRequest "GME.csv"

    entireDetectSampleAsync client request

    0 // return an integer exit code