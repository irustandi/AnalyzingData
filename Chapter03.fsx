#r "System.Xml.Linq.dll"
#load "packages/FsLab/FsLab.fsx"

open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

type WorldData = XmlProvider<"http://api.worldbank.org/countries/indicators/NY.GDP.PCAP.CD?date=2010:2010">

let indUrl = "http://api.worldbank.org/countries/indicators/"

let getData year indicator = 
    let query =
        [("per_page","1000");
         ("date",sprintf "%d:%d" year year)]
    let data = Http.RequestString(indUrl + indicator, query)
    let xml = WorldData.Parse(data)
    let orNaN value =
        defaultArg (Option.map float value) nan
    series [ for d in xml.Datas -> d.Country.Value, orNaN d.Value ]

let wb = WorldBankData.GetDataContext()
let inds = wb.Countries.World.Indicators
let code = inds.``CO2 emissions (kt)``.IndicatorCode

let co2000 = getData 2000 code
let co2010 = getData 2010 code

let change = (co2010 - co2000) / co2000 * 100.0

let codes =
    [ "CO2", inds.``CO2 emissions (metric tons per capita)``
      "Univ", inds.``Gross enrolment ratio, tertiary, both sexes (%)``
      "Life", inds.``Life expectancy at birth, total (years)``
      "Growth", inds.``GDP per capita growth (annual %)``
      "Pop", inds.``Population growth (annual %)``
      "GDP", inds.``GDP per capita (current US$)``
    ]

let world =
    frame [ for name, ind in codes -> name, getData 2010 ind.IndicatorCode ]

let lo = Stats.min world
let hi = Stats.max world
let avg = Stats.mean world

let filled =
    world
    |> Frame.transpose
    |> Frame.fillMissingUsing (fun _ ind -> avg.[ind])

let norm = 
    (filled - lo) / (hi - lo)
    |> Frame.transpose

let gdp = log norm.["GDP"] |> Series.values
let life = norm.["Life"] |> Series.values


let data = [(0.0, 1.0); (1.0, 1.0); (10.0, 1.0); (13.0, 3.0); (4.0, 10.0); (5.0, 8.0)]

let distance (x1, y1) (x2, y2) :float =
    sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

let aggregate points : float * float =
    (List.averageBy fst points, List.averageBy snd points)

let clusterCount = 3

let centroids =
    let random = System.Random()
    [ for i in 1 .. clusterCount -> List.nth data (random.Next(data.Length)) ]

let closest centroids input =
    centroids
    |> List.mapi (fun i v -> i, v)
    |> List.minBy (fun (_, cent) -> distance cent input)
    |> fst

data |> List.map (fun point -> closest centroids point)

let rec update assignment = 
    let centroids =
        [ for i in 0 .. clusterCount-1 ->
            let items = 
                List.zip assignment data
                |> List.filter (fun (c, data) -> c = i)
                |> List.map snd
            aggregate items ]
    let next = List.map (closest centroids) data
    if next = assignment then assignment
    else update next

let assignment =
    update (List.map (closest centroids) data)

let kmeans distance aggregate clusterCount data = 

    let centroids =
        let rnd = System.Random()
        [ for i in 1 .. clusterCount ->
            List.nth data (rnd.Next(data.Length)) ]

    let closest centroids input = 
        centroids
        |> List.mapi (fun i v -> i, v)
        |> List.minBy (fun (_, cent) -> distance cent input)
        |> fst

    let rec update assignment =
        let centroids =
            [ for i in 0 .. clusterCount-1 -> 
                let items = 
                    List.zip assignment data
                    |> List.filter (fun (c, data) -> c = i)
                    |> List.map snd
                aggregate items ]
        let next = List.map (closest centroids) data
        if next = assignment then assignment
        else update next

    update (List.map (closest centroids) data)

let data = norm.GetRows<float>().Values |> List.ofSeq

let distance (s1:Series<string,float>) (s2:Series<string, float>) =
    (s1 - s2) * (s1 - s2) |> Stats.sum

let aggregate items = 
    items
    |> Frame.ofRowsOrdinal
    |> Stats.mean

let clrs = ColorAxis(colors=[|"red";"blue";"orange"|])
let countryClusters =
    kmeans distance aggregate 3 data

Seq.zip norm.RowKeys countryClusters
|> Chart.Geo
|> Chart.WithOptions(Options(colorAxis=clrs))