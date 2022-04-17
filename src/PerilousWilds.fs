namespace PerilousWilds

module App =

    open System
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fulma
    open Thoth.Fetch

    module Types =

        type Table = | Table of string
        type Value = | Value of string

        type Exp =
            | T of Table
            | V of Value

        type Picker = {
            Table: string
            Outcomes: list<int * Exp>
            }

        type Context = {
            Sources: Map<string, Picker>
            }

        let rng = System.Random ()

        let pick (context: Context) (pickerName: string) =
            let p =
                context.Sources
                |> Map.tryFind pickerName
                |> Option.defaultValue ({ Table = pickerName; Outcomes = [ 1, V (Value "Error") ] })
            let total = p.Outcomes |> List.sumBy fst
            let outcome =
                let i = rng.Next(0, total)
                let rec find acc xs =
                    match xs with
                    | [] -> failwith "boom"
                    | (n, x) :: tl ->
                        if acc + n > i then x else find (acc + n) tl
                find 0 p.Outcomes
            outcome

        type Y =
            | Next of X
            | Final of Value
        and X = {
            ID: Guid
            Table: Table
            Value: Y
            }

        let rec roll (context: Context) (tableName: string) =
            let r = pick context tableName
            match r with
            | V v -> { ID = Guid.NewGuid(); Table = Table tableName; Value = Final v }
            | T (Table t) -> { ID = Guid.NewGuid();Table = Table tableName; Value = Next (roll context t) }

        let reroll (context: Context) (ID: Guid) (x: X) =
            let rec f (a: X) =
                if a.ID = ID
                then
                    let (Table tableName) = a.Table
                    let rerolled = roll context tableName
                    { a with Value = rerolled.Value }
                else
                    match a.Value with
                    | Final _ -> a
                    | Next next ->
                        { a with Value = Next (f next) }
            f x

    module Data =

        open Types
        let value (txt: string) = V (Value txt)

        [<RequireQualifiedAccess>]
        module General =

            let element = {
                Table = "element"
                Outcomes = [
                    1, value "air"
                    1, value "earth"
                    1, value "fire"
                    1, value "water"
                    1, value "life"
                    1, value "death"
                    ]
                }

            let oddity = {
                Table = "oddity"
                Outcomes = [
                    1, value "weird color, smell, sound"
                    1, value "geometric"
                    1, value "web, network, system"
                    1, value "crystalline, glass-like"
                    1, value "fungal"
                    1, value "gaseous, smokey"
                    1, value "mirage, illusion"
                    1, value "volcanic, explosive"
                    1, value "magnetic, repellant"
                    1, value "devoid of life"
                    1, value "unexpectedly alive"
                    // 1, Repeat 2 //value "roll twice" // TODO figure out
                    ]
                }

            let creature = {
                Table = "creature"
                Outcomes = [
                    4, value "beast"
                    2, value "human"
                    2, value "humanoid"
                    3, value "monster"
                    ]
                }

            let steading = {
                Table = "steading"
                Outcomes = [
                    5, value "village"
                    3, value "town"
                    3, value "keep"
                    1, value "city"
                    ]
                }

        [<RequireQualifiedAccess>]
        module Dungeon =

            let discovery = {
                Table = "dungeon discovery"
                Outcomes = [
                    3, T (Table "dressing")
                    6, T (Table "feature")
                    3, T (Table "find")
                    ]
                }

            let dressing = {
                Table = "dressing"
                Outcomes = [
                    1, value "junk, debris"
                    1, value "tracks, marks"
                    1, value "signs of battle"
                    1, value "writing, carving"
                    1, value "warning"
                    1, value "dead creature" // TODO Creature
                    1, value "bones, remains"
                    1, value "book, scroll, map"
                    1, value "broken door, wall"
                    1, value "breeze, wind, smell"
                    1, value "lichen, moss, fungus"
                    1, T (Table "oddity")
                    ]
                }

            let feature = {
                Table = "feature"
                Outcomes = [
                    1, value "cave-in"
                    1, value "pit, shaft, chasm"
                    1, value "pillars, columns"
                    1, value "locked door, gate"
                    1, value "alcoves, niches"
                    1, value "bridges, stairs, ramp"
                    1, value "fountain, well, pool"
                    1, value "puzzle"
                    1, value "altar, dais, platform"
                    1, value "statue, idol"
                    1, value "magic pool, statue, idol"
                    1, value "connection to another dungeon"
                    ]
                }

            let find = {
                Table = "find"
                Outcomes = [
                    1, value "trinkets"
                    1, value "tools"
                    1, value "weapons, armor"
                    1, value "supplies, trade goods"
                    1, value "coins, gems, jewelry"
                    1, value "poisons, potions"
                    1, value "adventurer, captives"
                    1, value "magic item"
                    1, value "scroll, book"
                    1, value "magic weapon, armor"
                    1, value "artifact"
                    // 1, Repeat 2 //value "roll twice" // TODO figure out
                    ]
                }

            let danger = {
                Table = "dungeon danger"
                Outcomes = [
                    4, T (Table "trap")
                    7, T (Table "dungeon creature")
                    1, T (Table "entity")
                    ]
                }

            let trap = {
                Table = "trap"
                Outcomes = [
                    1, value "alarm"
                    1, value "ensnaring, paralyzing"
                    1, value "pit"
                    1, value "crushing"
                    1, value "piercing, puncturing"
                    1, value "chopping, slashing"
                    1, value "confusing (maze, etc...)"
                    1, value "gas (poison, etc...)"
                    1, T (Table "element")
                    1, value "ambush"
                    1, value "magical"
                    // 1, Repeat 2 //value "roll twice"
                    ]
                }

            let creature = {
                Table = "dungeon creature"
                Outcomes = [
                    1, value "waiting in ambush"
                    1, value "fighting, squabbling"
                    1, value "prowling, on patrol"
                    1, value "looking for food"
                    1, value "eating, resting"
                    1, value "guarding"
                    1, value "on the move"
                    1, value "searching, scavenging"
                    1, value "returning to den"
                    1, value "making plans"
                    1, value "sleeping"
                    1, value "dying"
                    ]
                }

            let entity = {
                Table = "entity"
                Outcomes = [
                    1, value "alien interloper"
                    1, value "vermin lord"
                    1, value "criminal mastermind"
                    1, value "warlord"
                    1, value "high priest"
                    1, value "oracle"
                    1, value "wizard, witch, alchemist"
                    1, value "monster lord" // TODO Monster
                    1, value "evil spirit, ghost"
                    1, value "undead lord (lich, etc...)"
                    1, value "demon"
                    1, value "dark god"
                    ]
                }

        [<RequireQualifiedAccess>]
        module Wilderness =

            [<RequireQualifiedAccess>]
            module Discovery =
                let discovery = {
                    Table = "discovery"
                    Outcomes = [
                        1, T (Table "unnatural feature")
                        3, T (Table "natural feature")
                        2, T (Table "evidence")
                        2, T (Table "creature")
                        4, T (Table "structure")
                        ]
                    }

                let unnaturalFeature = {
                    Table = "unnatural feature"
                    Outcomes = [
                        9, T (Table "arcane")
                        2, value "planar"
                        1, value "divine"
                        ]
                    }

                let naturalFeature = {
                    Table = "natural feature"
                    Outcomes = [
                        2, T (Table "lair")
                        2, T (Table "obstacle")
                        3, T (Table "terrain change")
                        2, T (Table "water feature")
                        2, T (Table "landmark")
                        1, T (Table "resource")
                        ]
                    }

                let evidence = {
                    Table = "evidence"
                    Outcomes = [
                        6, T (Table "tracks, spoor")
                        2, T (Table "remains, debris")
                        1, T (Table "stash, cache")
                        ]
                    }

                let structure = {
                    Table = "structure"
                    Outcomes = [
                        1, T (Table "enigmatic")
                        2, T (Table "infrastructure")
                        1, T (Table "dwelling")
                        2, T (Table "burial, religious")
                        2, T (Table "steading") // TODO general table
                        4, T (Table "ruin")
                        ]
                    }

                let arcane = {
                    Table = "arcane"
                    Outcomes = [
                        2, value "residue"
                        3, value "blight"
                        2, value "alteration, mutation"
                        3, value "enchantment"
                        2, value "source, repository"
                        ]
                    }

                // TODO planar, divine

                let lair = {
                    Table = "lair"
                    Outcomes = [
                        3, value "burrow"
                        4, value "cave, tunnels"
                        2, value "nest, aerie"
                        1, value "hive"
                        2, value "ruins" // TODO branch to structure
                        ]
                    }

                let obstacle = {
                    Table = "obstacle"
                    Outcomes = [
                        5, value "difficult ground, terrain"
                        3, value "cliff, crevasse, chasm"
                        2, value "ravine, gorge"
                        2, T (Table "oddity")
                        ]
                    }

                let terrainChange = {
                    Table = "terrain change"
                    Outcomes = [
                        4, value "limited area of different terrain" // TODO table terrain?
                        2, value "crevice, hole, pit, cave"
                        2, value "altitude change"
                        2, value "canyon, valley"
                        2, value "rise, peak in distance"
                        ]
                    }

                let waterFeature = {
                    Table = "water feature"
                    Outcomes = [
                        1, value "spring, hotspring"
                        1, value "waterfall, geyser"
                        4, value "creek, stream, brook"
                        2, value "pond, lake"
                        2, value "river"
                        2, value "sea, ocean"
                        ]
                    }

                let landmark = {
                    Table = "landmark"
                    Outcomes = [
                        3, value "water based (waterfall, geyser, ...)"
                        3, value "plant based (ancient tree, giant flowers, ...)"
                        4, value "earth based (peak, formation, crater, ...)"
                        2, value "oddity" // TODO hook to oddity
                        ]
                    }

                let resource = {
                    Table = "resource"
                    Outcomes = [
                        4, value "game, fruit, vegetable"
                        2, value "herb, spice, die source"
                        3, value "timber, stone"
                        2, value "ore (copper, iron, ...)"
                        1, value "precious metal, gems"
                        ]
                    }

                let tracks = {
                    Table = "tracks, spoor"
                    Outcomes = [
                        3, value "faint, unclear"
                        3, value "definite, clear"
                        2, value "multiple"
                        2, value "signs of violence"
                        2, value "trail of blood, other"
                        ]
                    }

                let remains = {
                    Table = "remains, debris"
                    Outcomes = [
                        4, value "bones"
                        3, value "corpse, carcass"
                        2, value "site of violence"
                        1, value "junk, refuse"
                        1, value "lost supplies, cargo"
                        1, value "tools, weapons, armor"
                        ]
                    }

                let stash = {
                    Table = "stash, cache"
                    Outcomes = [
                        4, value "trinkets, coins"
                        3, value "tools, weapons, armor"
                        2, value "map"
                        1, value "food, supplies"
                        1, value "treasure" // TODO treasure table
                        ]
                    }

                let enigmatic = {
                    Table = "enigmatic"
                    Outcomes = [
                        4, value "earthworks"
                        4, value "megalith"
                        3, value "status, idol, totem"
                        1, T (Table "oddity")
                        ]
                    }

                let infrastructure = {
                    Table = "infrastructure"
                    Outcomes = [
                        4, value "track, path"
                        4, value "road"
                        2, value "bridge, ford"
                        1, value "mine, quarry"
                        1, value "aqueduct, canal, portal"
                        ]
                    }

                let dwelling = {
                    Table = "dwelling"
                    Outcomes = [
                        3, value "campsite"
                        3, value "hovel, hut"
                        2, value "farm"
                        2, value "inn, roadhouse"
                        2, value "tower, keep, estate"
                        ]
                    }

                let burial = {
                    Table = "burial, religious"
                    Outcomes = [
                        2, value "grave marker, barrow"
                        2, value "graveyard, necropolis"
                        2, value "tomb, crypt"
                        3, value "shrine"
                        2, value "temple, retreat"
                        1, value "great temple"
                        ]
                    }

                let ruin = {
                    Table = "ruin"
                    Outcomes = [
                        2, T (Table "infrastructure")
                        2, T (Table "dwelling")
                        2, T (Table "burial, religious")
                        3, T (Table "steading")
                        2, value "dungeon" // TODO
                        ]
                    }

            [<RequireQualifiedAccess>]
            module Danger =

                let danger = {
                    Table = "danger"
                    Outcomes = [
                        1, T (Table "unnatural entity")
                        3, T (Table "hazard")
                        2, T (Table "creature")
                        ]
                    }

                let unnaturalEntity = {
                    Table = "unnatural entity"
                    Outcomes = [
                        8, value "undead"
                        3, value "planar"
                        1, value "divine"
                        ]
                    }

                let hazard = {
                    Table = "hazard"
                    Outcomes = [
                        2, value "unnatural"
                        8, value "natural"
                        2, value "trap"
                        ]
                    }

        let context =
            [
                General.element
                General.oddity
                General.creature
                General.steading
                Dungeon.discovery
                Dungeon.dressing
                Dungeon.feature
                Dungeon.find
                Dungeon.danger
                Dungeon.trap
                Dungeon.creature
                Dungeon.entity
                Wilderness.Discovery.discovery
                Wilderness.Discovery.unnaturalFeature
                Wilderness.Discovery.naturalFeature
                Wilderness.Discovery.evidence
                Wilderness.Discovery.structure
                Wilderness.Discovery.arcane
                Wilderness.Discovery.lair
                Wilderness.Discovery.obstacle
                Wilderness.Discovery.terrainChange
                Wilderness.Discovery.waterFeature
                Wilderness.Discovery.landmark
                Wilderness.Discovery.resource
                Wilderness.Discovery.tracks
                Wilderness.Discovery.remains
                Wilderness.Discovery.stash
                Wilderness.Discovery.enigmatic
                Wilderness.Discovery.infrastructure
                Wilderness.Discovery.dwelling
                Wilderness.Discovery.burial
                Wilderness.Discovery.ruin
                Wilderness.Danger.danger
                Wilderness.Danger.unnaturalEntity
                Wilderness.Danger.hazard
            ]
            |> List.map (fun x -> x.Table, x)
            |> Map.ofList
            |> fun sources -> { Sources = sources }

    open Types
    open Data

    type Model = {
        Test: string
        Context: Context
        Discovery: X
        Danger: X
        Creature: X
        }

    type Roll =
        | Danger
        | Discovery
        | Creature

    type Msg =
        | Roll of Roll
        | ReRoll of (Roll * Guid)

    let init () : Model * Cmd<Msg> =
        {
            Test = "Perilous Wilds"
            Context = context
            Danger = roll context "danger"
            Discovery = roll context "discovery"
            Creature = roll context "creature"
        },
        Cmd.none

    let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
        match msg with
        | Roll rollType ->
            let model =
                match rollType with
                | Danger ->
                    { model with
                        Danger = roll context "danger"
                    }
                | Discovery ->
                    { model with
                        Discovery = roll context "discovery"
                    }
                | Creature ->
                    { model with
                        Creature = roll context "creature"
                    }
            model, Cmd.none
        | ReRoll (rollType, identifier) ->
            let model =
                match rollType with
                | Danger ->
                    { model with
                        Danger = reroll context identifier model.Danger
                    }
                | Discovery ->
                    { model with
                        Discovery = reroll context identifier model.Discovery
                    }
                | Creature ->
                    { model with
                        Discovery = reroll context identifier model.Creature
                    }
            model, Cmd.none

    let rec renderRollResult dispatch acc (x: X) (roll: Roll) =
        match x.Value with
        | Final (Value txt) ->
            let tag = Tag.tag [ Tag.Size IsMedium; Tag.Color IsDark ] [ str txt ]
            tag :: acc |> List.rev
        | Next result ->
            let (Table txt) = result.Table
            let tag =
                Tag.tag
                    [ Tag.Size IsMedium ]
                    [ str txt; Delete.delete [ Delete.OnClick (fun _ -> ReRoll (roll, result.ID) |> dispatch) ] [] ]
            renderRollResult dispatch (tag :: acc) result roll

    let render (model: Model) (dispatch: Msg -> unit) =

        Container.container [] [

            Box.box' [] [
                Heading.h2 [] [ str "In The Wild"]

                Level.level [] [
                    Level.left [ Props [ OnClick(fun _ -> dispatch (Roll Discovery)) ] ] [
                        Level.item [] [ Image.image [ Image.Is32x32 ] [ img [ Src "/d20.png" ] ] ]
                        Level.item []
                            [ Heading.h3 [ ] [ str "Discovery" ]
                            ]
                        ]
                    ]

                Field.div [ Field.IsGroupedMultiline ] [
                    Control.div [] [
                        Tag.list [] (renderRollResult dispatch [] model.Discovery Discovery)
                        ]
                    ]

                br []

                Level.level [] [
                    Level.left [ Props [ OnClick(fun _ -> dispatch (Roll Danger)) ] ] [
                        Level.item [  ] [ Image.image [ Image.Is32x32 ] [ img [ Src "/d20.png" ] ] ]
                        Level.item []
                            [ Heading.h3 [ ] [ str "Danger" ]
                            ]
                        ]
                    ]

                Field.div [ Field.IsGroupedMultiline ] [
                    Control.div [] [
                        Tag.list [] (renderRollResult dispatch [] model.Danger Danger)
                        ]
                    ]

                br []

                Level.level [] [
                    Level.left [ Props [ OnClick(fun _ -> dispatch (Roll Creature)) ] ] [
                        Level.item [  ] [ Image.image [ Image.Is32x32 ] [ img [ Src "/d20.png" ] ] ]
                        Level.item []
                            [ Heading.h3 [ ] [ str "Creature" ]
                            ]
                        ]
                    ]

                Field.div [ Field.IsGroupedMultiline ] [
                    Control.div [] [
                        Tag.list [] (renderRollResult dispatch [] model.Creature Creature)
                        ]
                    ]
                ]
            ]