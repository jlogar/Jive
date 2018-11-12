module Jive

open FSharp.Data
open System.IO

[<Literal>]
let InputSrc = __SOURCE_DIRECTORY__ + @"\input-example.csv"

type Csv = CsvProvider< InputSrc, ";",PreferOptionals=true>
type Month = int
type Year = int
type MonthInYear = {month: Month;  year: Year}
let rec last = function
        | [hd] -> hd
        | _ :: tl -> last tl
        | _ -> failwith "Empty list."

let rec win12 (agg: 'a list list) = function
        | x when (x |> List.length) < 12 ->
                agg
        | hd :: tl ->
                win12 ((hd::(tl|>List.take 11))::agg) tl
        | _ -> failwith "empty"
[<EntryPoint>]
let main args =
    if args.Length <> 1 then failwith "invalid args, expected dir name for CSV files containing invoices (invoice-sent*.csv)"
    let dir = args|>Array.head
    let files = Directory.EnumerateFiles(dir, "invoice-sent*.csv")
    printf "read %d from %s" (files|>Seq.length) dir
    let all = files
                 |> Seq.map (fun x-> Csv.Load(x).Rows)
                 |> Seq.concat
                 |> Seq.toList
    let byMonth = all
                        |> Seq.sortByDescending (fun x -> x.Date_served)
                        |> Seq.groupBy (fun x -> {month=x.Date_served.Month; year = x.Date_served.Year})
                        |> Seq.toList

    let (lastMonth, _) = byMonth |> Seq.head
    let (firstMonth, _) = last byMonth
    printfn "from %d.%d to %d.%d (%d months), %d in all" firstMonth.month firstMonth.year lastMonth.month lastMonth.year byMonth.Length all.Length
    let windowed = win12 [] byMonth
    let windowedSums = windowed
                        |> List.rev
                        |> List.map (fun x -> (x, fst x.Head, x|>List.map (fun (_, invoices)-> invoices |> Seq.sumBy (fun i->i.Amount))|>List.sum))
    printfn "done summing. got %d sums." (Seq.length windowedSums)
    windowedSums |> Seq.iter (fun (_, firstM,sum)-> printfn "%2d.%d -> SUM: %s" firstM.month firstM.year (System.String.Format("{0:C}", sum)))

    printfn "Press enter to exit."
    ignore (System.Console.ReadLine())
    0
