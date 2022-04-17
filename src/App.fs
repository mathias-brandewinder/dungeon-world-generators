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

    type SelectedView =
        | DungeonWorld
        | PerilousWilds

    type Model = {
        SelectedView: SelectedView
        DungeonWorld: DungeonWorld.App.Model
        PerilousWilds: PerilousWilds.App.Model
        }

    type Msg =
        | SwitchView of SelectedView
        | DW of DungeonWorld.App.Msg
        | PW of PerilousWilds.App.Msg

    let init () =

        let pwModel, pwCmd = PerilousWilds.App.init ()

        let dwModel, dwCmd = DungeonWorld.App.init ()

        let model = {
            SelectedView = DungeonWorld
            DungeonWorld = dwModel
            PerilousWilds = pwModel
            }

        // TODO batch + map
        model, Cmd.map (fun msg -> DW msg) dwCmd

    let update (msg: Msg) (model: Model) =
        match msg with
        | SwitchView selectedView->
            { model with SelectedView = selectedView }, Cmd.none
        | DW dwMsg ->
            let updated, cmd = DungeonWorld.App.update dwMsg model.DungeonWorld
            { model with DungeonWorld = updated }, Cmd.map DW cmd
        | PW pwMsg ->
            let updated, cmd = PerilousWilds.App.update pwMsg model.PerilousWilds
            { model with PerilousWilds = updated }, Cmd.map PW cmd

    let hero dispatch =
        Hero.hero [ Hero.Color IsDark ] [
            Hero.body []
                [
                    Heading.h1 [] [ str "Dungeon World" ]
                    Heading.h4 [ Heading.IsSubtitle] [
                        str ""
                        a [ Href "https://dungeon-world.com/"; Style [ Color "Red" ] ]
                            [ str "Play to find out what happens."]
                        ]

                    a [ OnClick (fun _ -> dispatch (SwitchView DungeonWorld))] [ str "Moves" ]
                    str " | "
                    a [ OnClick (fun _ -> dispatch (SwitchView PerilousWilds)) ] [ str "Dangers" ]
                ]
            ]

    let view (model:Model) (dispatch: Msg -> unit) =

        let dwDispatch (msg: DungeonWorld.App.Msg) =
            dispatch (DW msg)

        let pwDispatch (msg: PerilousWilds.App.Msg) =
            dispatch (PW msg)

        Columns.columns [ Columns.IsCentered ]
            [
                Column.column [ Column.Width (Screen.All, Column.IsHalf) ]
                    [
                        Container.container []
                            [
                                hero dispatch

                                Section.section [] [
                                    match model.SelectedView with
                                    | DungeonWorld -> DungeonWorld.App.view model.DungeonWorld dwDispatch
                                    | PerilousWilds -> PerilousWilds.App.render model.PerilousWilds pwDispatch
                                    ]
                            ]
                    ]
            ]

    // App
    Program.mkProgram init update view // PerilousWilds.App.init PerilousWilds.App.update PerilousWilds.App.render // init update view
    |> Program.withReactSynchronous "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
