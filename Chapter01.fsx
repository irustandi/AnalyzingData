#load "packages/FsLab/FsLab.fsx"
open FSharp.Data

let wb = WorldBankData.GetDataContext()
wb.Countries.``Czech Republic``.CapitalCity
wb.Countries.``Czech Republic``.Indicators.``CO2 emissions (kt)``.[2010]

type Weather=JsonProvider<"http://api.openweathermap.org/data/2.5/forecast/daily?units=metric&q=Prague&APPID=6c082fc8cc56b48c4341e3df1d4454b6">

let w = Weather.GetSample()
printfn "%s" w.City.Country
for day in w.List do
    printfn "%f" day.Temp.Max

let baseUrl = "http://api.openweathermap.org/data/2.5"
let forecastUrl = baseUrl + "/forecast/daily?APPID=6c082fc8cc56b48c4341e3df1d4454b6&units=metric&q="

let getTomorrowTemp place = 
    let w = Weather.Load(forecastUrl + place)
    let tomorrow = Seq.head w.List
    tomorrow.Temp.Max

getTomorrowTemp "Prague"
getTomorrowTemp "Cambridge,UK"

let worldTemps = 
    [ for c in wb.Countries ->
        let place = c.CapitalCity + "," + c.Name
        printfn "Getting temperature in: %s" place
        try
            let temp = getTomorrowTemp place
            c.Name, temp
        with
            | ex -> printfn "Error getting temperature for %s" place; c.Name, System.Decimal.MinValue
    ]

open XPlot.GoogleCharts

let colors = [| "#80E000"; "#E0C000"; "#E07B00"; "#E02800" |]
let values = [| 0;+15;+30;+45 |]
let axis = ColorAxis(values=values, colors=colors)

worldTemps
|> Chart.Geo
|> Chart.WithOptions(Options(colorAxis=axis))
|> Chart.WithLabel "Temp"