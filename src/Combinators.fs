namespace Doskvol

type Distribution =
    | Flat of list<string>
    | Weighted of list<float * string>

type Source = {
    Name: string
    Items: Distribution
    }

type RNG = unit -> float

type Picker =
    | Source of Distribution
    | Or of list<float * Picker>
    | Maybe of (float * Picker)

module Picker =

    let uniform<'T> (rng: RNG) (distribution: list<'T>) =
        let total = distribution |> List.length |> float
        let weight = 1. / total
        let roll = rng ()
        let rec search acc outcomes =
            match outcomes with
            | [] -> failwith "Impossible"
            | outcome :: rest ->
                let acc = acc + weight
                if acc >= roll
                then outcome
                else search acc rest
        search 0.0 distribution

    let weighted<'T> (rng: RNG) (distribution: list<float * 'T>) =
        let total = distribution |> Seq.sumBy fst
        let roll = rng () * total
        let rec search acc outcomes =
            match outcomes with
            | [] -> failwith "Impossible"
            | (weight, outcome) :: rest ->
                let acc = acc + weight
                if acc >= roll
                then outcome
                else search acc rest
        search 0.0 distribution

    let rec pick (rng: RNG) (picker: Picker) =
        match picker with
        | Source source ->
            match source with
            | Flat data ->
                uniform rng data |> Some
            | Weighted data ->
                weighted rng data |> Some
        | Or pickers ->
            weighted rng pickers
            |> pick rng
        | Maybe (proba, picker) ->
            if rng () <= proba
            then pick rng picker
            else None

module Data =

    type Expression =
        | Source of string
        | Composite of string
        | Or of list<float * Expression>
        | Maybe of float * Expression

    type NamedDefinition = {
        Name: string
        Definition: Expression
        }

    type Model = {
        Sources: list<Source>
        Definitions: list<NamedDefinition>
        }

    type Catalog = {
        NamedSources: Map<string, Distribution>
        NamedDefinitions: Map<string, Expression>
        }
        with
        static member From (model: Model) =
            // TODO validate unicity of names
            {
                NamedSources =
                    model.Sources
                    |> Seq.map (fun x -> x.Name, x.Items)
                    |> Map.ofSeq
                NamedDefinitions =
                    model.Definitions
                    |> Seq.map (fun x -> x.Name, x.Definition)
                    |> Map.ofSeq
            }

    let rec replace (catalog: Catalog) (visited: Set<string>) (expr: Expression) =
        match expr with
        | Source sourceName ->
            catalog.NamedSources
            |> Map.find sourceName
            |> Picker.Source
        | Composite compositeName ->
            if catalog.NamedDefinitions |> Map.containsKey compositeName
            then
                if visited |> Set.contains compositeName
                then failwith "Recursive definition"
                else
                    let visited = visited |> Set.add compositeName
                    catalog.NamedDefinitions
                    |> Map.find compositeName
                    |> replace catalog visited
            else failwith (sprintf "unknown definition for %s" compositeName)
        | Maybe (proba, subExpr) ->
            Picker.Maybe (proba, replace catalog visited subExpr)
        | Or choices ->
            choices
            |> List.map (fun (proba, subExpr) ->
                proba, replace catalog visited subExpr
                )
            |> Picker.Or

    let read (model: Model) =
        let catalog = Catalog.From model
        let sources =
            catalog.NamedSources
            |> Map.map (fun name source -> Picker.Source source)
        (sources, catalog.NamedDefinitions)
        ||> Map.fold (fun acc name def ->
            acc |> Map.add name (replace catalog Set.empty def)
            )
