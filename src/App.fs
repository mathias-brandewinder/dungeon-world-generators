namespace Doskvol

module App =

    open System
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fulma
    open Thoth.Fetch
    open Doskvol
    open Doskvol.Data

    let capitalize (txt: string) =
        txt.Substring(0, 1).ToUpperInvariant () +
        txt.Substring(1)

    let quoted (txt: string) =
        sprintf "\u00AB%s\u00BB" txt

    type Attribute = {
        Name: string
        Value: string
        }

    type NPC = {
        Name: string
        Attributes: list<Attribute>
        }

    let rng =
        let random = Random ()
        fun () -> random.NextDouble()

    let attribute (pickers: Map<string,Picker>) name =
        {
            Name = name
            Value =
                pickers.[name]
                |> Picker.pick rng
                |> Option.get
        }

    let npc (pickers: Map<string,Picker>) : NPC =
        {
            Name = pickers.["name"] |> Picker.pick rng |> Option.get
            Attributes = [
                attribute pickers "instinct"
                attribute pickers "knack"
                ]
        }

    type Model = {
        Data: Option<Map<string, Picker>>
        NPC: Option<NPC>
        NPCs: list<NPC>
        GameMasterMove: Option<string>
        DungeonMove: Option<string>
        }

    type Msg =
        | RollNpc
        | RollGameMasterMove
        | RollDungeonMove
        | DiscardNPC of NPC
        | RollNpcAttribute of string
        | RollNpcName
        | DataReceived of Data.Model

    let init() =

        let model : Model = {
            Data = None
            NPC = None
            NPCs = []
            GameMasterMove = None
            DungeonMove = None
            }

        let getModel () : Fable.Core.JS.Promise<Data.Model> =
            Fetch.get (@"https://dungeonworld.blob.core.windows.net/data/dungeonworld.json")

        let cmd = Cmd.OfPromise.perform getModel () DataReceived

        model, cmd

    let update (msg: Msg) (model: Model) =
        match msg with
        | RollNpc ->
            let newNPC = model.Data |> Option.map npc
            let npcs =
                match model.NPC with
                | None -> model.NPCs
                | Some npc -> npc :: model.NPCs
            { model with
                NPC = newNPC
                NPCs = npcs
            },
            Cmd.none
        | DiscardNPC npc ->
            { model with
                NPCs = model.NPCs |> List.filter (fun x -> x <> npc)
            },
            Cmd.none
        | RollNpcName ->
            let npc =
                model.NPC
                |> Option.map (fun x ->
                    let pickers = model.Data |> Option.get
                    { x with
                        Name = pickers.["name"] |> Picker.pick rng |> Option.get
                    }
                    )
            { model with NPC = npc }, Cmd.none
        | RollNpcAttribute attributeName ->
            let npc =
                model.NPC
                |> Option.map (fun x ->
                    { x with
                        Attributes =
                            x.Attributes
                            |> List.map (fun att ->
                                if att.Name = attributeName
                                then attribute (model.Data |> Option.get) attributeName
                                else att
                                )
                    }
                    )
            { model with NPC = npc }, Cmd.none

        | RollGameMasterMove ->
            let move =
                model.Data
                |> Option.map (fun pickers -> pickers.["gm move"] |> Picker.pick rng |> Option.get)
            { model with GameMasterMove = move },
            Cmd.none
        | RollDungeonMove ->
            let move =
                model.Data
                |> Option.map (fun pickers -> pickers.["dungeon move"] |> Picker.pick rng |> Option.get)
            { model with DungeonMove = move },
            Cmd.none
        | DataReceived data ->
            let pickers = data |> read
            {
                Data = Some pickers
                NPC = None
                NPCs = []
                GameMasterMove = None
                DungeonMove = None
            },
            Cmd.batch [ Cmd.ofMsg RollNpc; Cmd.ofMsg RollDungeonMove; Cmd.ofMsg RollGameMasterMove ]

    module View =

        let attribute (attribute: Attribute) dispatch =
            Control.div [ ]
                [
                    Tag.list [ Tag.List.HasAddons; Tag.List.AreMedium ]
                        [
                            Tag.tag [ Tag.Color IsDark ] [ str (capitalize attribute.Name) ]
                            Tag.tag [ ]
                                [
                                    str (capitalize attribute.Value)
                                    Delete.delete [ Delete.OnClick (fun _ -> dispatch (RollNpcAttribute attribute.Name)) ] [ ]
                                ]
                        ]
                ]

        let gameMasterMove (text: Option<string>) dispatch =
            Box.box' [ ]
                [
                    Container.container [ ]
                        [
                            Columns.columns [ ]
                                [
                                    Column.column [  ]
                                        [
                                            str "GM Move"
                                            br []
                                            Text.span [ Modifiers [ Modifier.TextSize (Screen.All, TextSize.Is4) ] ]
                                                [
                                                    text
                                                    |> Option.defaultValue ""
                                                    |> str
                                                ]
                                        ]
                                    Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                                        [
                                            Button.button [ Button.IsLight; Button.OnClick (fun _ -> dispatch RollGameMasterMove) ]
                                                [ str "Roll" ]
                                        ]
                                ]
                        ]
                ]

        let dungeonMove (text: Option<string>) dispatch =
            Box.box' [ ]
                [
                    Container.container [ ]
                        [
                            Columns.columns [ ]
                                [
                                    Column.column [  ]
                                        [
                                            str "Dungeon Move"
                                            br []
                                            Text.span [ Modifiers [ Modifier.TextSize (Screen.All, TextSize.Is4) ] ]
                                                [
                                                    text
                                                    |> Option.defaultValue ""
                                                    |> str
                                                ]
                                        ]
                                    Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                                        [
                                            Button.button [ Button.IsLight; Button.OnClick (fun _ -> dispatch RollDungeonMove) ]
                                                [ str "Roll" ]
                                        ]
                                ]
                        ]
                ]

        let npc (model: Model) dispatch =
            Box.box' [ ]
                [
                    Container.container [ ]
                        [
                            Columns.columns [ ]
                                [
                                    Column.column [  ]
                                        [
                                            match model.NPC with
                                            | None -> ignore ()
                                            | Some character ->
                                                Heading.h1 [ ]
                                                    [
                                                        str (character.Name)
                                                        Delete.delete [ Delete.OnClick (fun _ -> dispatch RollNpcName) ] [ ]
                                                    ]

                                                div [ ]
                                                    [
                                                        Field.div [ Field.IsGroupedMultiline ]
                                                            [
                                                                for desc in character.Attributes ->
                                                                    attribute desc dispatch
                                                            ]
                                                    ]
                                        ]

                                    Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                                        [
                                            Button.button [ Button.IsLight; Button.OnClick (fun _ -> dispatch RollNpc) ]
                                                [ str "Roll" ]
                                        ]
                                ]
                        ]
                ]

        let npcs model dispatch =
            match model.NPCs with
            | [] -> nothing
            | _ ->
                Box.box' [ ]
                    [
                        ul [ ] [
                            for x in model.NPCs do
                                li [ ] [
                                    Delete.delete [ Delete.OnClick (fun _ -> dispatch (DiscardNPC x)) ] [ ]
                                    str (sprintf "%s [%s]" x.Name (x.Attributes |> List.map (fun x -> x.Value) |> String.concat ", "))
                                    ]
                            ]
                    ]

    let hero () =
        Hero.hero [ Hero.Color IsDark ]
            [ Hero.body []
                [
                    Heading.h1 [] [ str "Dungeon World" ]
                    Heading.h4 [ Heading.IsSubtitle] [
                        str ""
                        a [ Href "https://dungeon-world.com/"; Style [ Color "Red" ] ]
                            [ str "Play to find out what happens."]
                        ]
                ]
            ]

    let view (model:Model) dispatch =

        Columns.columns [ Columns.IsCentered ]
            [
                Column.column [ Column.Width (Screen.All, Column.IsHalf) ]
                    [
                        Container.container []
                            [
                                hero ()

                                Section.section [] [

                                    View.npc (model) dispatch

                                    View.gameMasterMove model.GameMasterMove dispatch

                                    View.dungeonMove model.DungeonMove dispatch

                                    View.npcs model dispatch
                                    ]
                            ]
                    ]
            ]

    // App
    Program.mkProgram init update view
    |> Program.withReactSynchronous "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
