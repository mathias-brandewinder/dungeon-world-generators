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
        let table (txt: string) = T (Table txt)

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
                    4, T (Table "beast")
                    2, value "human"
                    2, T (Table "humanoid")
                    3, T (Table "monster")
                    ]
                }

            let beast = {
                Table = "beast"
                Outcomes = [
                    7, T (Table "earthbound")
                    3, T (Table "airborne")
                    2, T (Table "water-going")
                    ]
                }

            let earthbound = {
                Table = "earthbound"
                Outcomes = [
                    1, value "termite, tick, louse"
                    1, value "snail, slug, worm"
                    1, value "ant, centiped, scorpion"
                    1, value "snake, lizard"
                    1, value "vole, rat, weasel"
                    1, value "boar, pig"
                    1, value "dog, fox, wolf"
                    1, value "cat, lion, panther"
                    1, value "deer, horse, camel"
                    1, value "ox, rhino"
                    1, value "bear, ape, gorilla"
                    1, value "mammoth, dinosaur"
                    ]
                }

            let airborne = {
                Table = "airborne"
                Outcomes = [
                    1, value "mosquito, firefly"
                    1, value "locust, dragonfly, moth"
                    1, value "bee, wasp"
                    1, value "chicken, duck, goose"
                    1, value "songbird, parrot"
                    1, value "gull, waterbird"
                    1, value "heron, crane, stork"
                    1, value "crow, raven"
                    1, value "hawk, falcon"
                    1, value "eagle, owl"
                    1, value "condor"
                    1, value "pteranodon"
                    ]
                }

            let waterGoing = {
                Table = "water-going"
                Outcomes = [
                    1, value "insect"
                    1, value "jelly, anemone"
                    1, value "clam, oyster, snail"
                    1, value "eel, snake"
                    1, value "frog, toad"
                    1, value "fish"
                    1, value "crab, lobster"
                    1, value "turtle"
                    1, value "alligator, crocodile"
                    1, value "dolphin, shark"
                    1, value "squid, octopus"
                    1, value "whale"
                    ]
                }

            let humanoid = {
                Table = "humanoid"
                Outcomes = [
                    7, T (Table "common")
                    3, T (Table "uncommon")
                    2, T (Table "hybrid")
                    ]
                }

            let common = {
                Table = "common"
                Outcomes = [
                    3, value "halfling" // TODO size
                    2, value "goblin, kobold"
                    2, value "dwarf, gnome"
                    2, value "orc, hobgoblin, gnoll"
                    2, value "half-elf, half-orc, ..."
                    1, value "elf"
                    ]
                }

            let uncommon = {
                Table = "uncommon"
                Outcomes = [
                    1, value "fey" // TODO size
                    2, value "catfolk, dogfolk"
                    3, value "lizardfolk, merfolk"
                    1, value "birdfolk"
                    3, value "ogre, troll"
                    2, value "cyclops, giant"
                    ]
                }

            let hybrid = {
                Table = "hybrid"
                Outcomes = [
                    2, value "centaur"
                    3, value "werewolf, werebear"
                    1, value "werecreature (human + beast)" // TODO
                    4, value "human + beast"
                    2, value "human + 2 beast"
                    ]
                }

            let monster = {
                Table = "monster"
                Outcomes = [
                    7, T (Table "unusual")
                    3, T (Table "rare")
                    2, T (Table "legendary")
                    ]
                }

            let unusual = {
                Table = "unusual"
                Outcomes = [
                    3, value "plant, fungus"
                    2, value "undead + human" // TODO
                    1, value "undead + humanoid" // TODO
                    2, value "beast + beast" // TODO
                    2, value "beast + ability" // TODO
                    2, value "beast + feature" // TODO
                    ]
                }

            let rare = {
                Table = "rare"
                Outcomes = [
                    3, value "slime, ooze" // TODO amorphous tag
                    3, value "creation (construct)" // TODO construct tag
                    3, value "beast + oddity" // TODO
                    3, table "unnatural entity"
                    ]
                }

            let legendary = {
                Table = "legendary"
                Outcomes = [
                    3, value "dragon, colossus" // TODO huge tag
                    3, value "unusual + huge" // TODO construct tag
                    3, value "rare + huge" // TODO
                    1, value "beast + dragon" // TODO
                    1, value "unusual + dragon" // TODO
                    1, value "rare + dragon" // TODO
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
                    3, table "dressing"
                    6, table "feature"
                    3, table "find"
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
                    1, table "oddity"
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
                    4, table "trap"
                    7, table "dungeon creature"
                    1, table "entity"
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
                    1, table "element"
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
                        1, table "unnatural feature"
                        3, table "natural feature"
                        2, table "evidence"
                        2, table "creature"
                        4, table "structure"
                        ]
                    }

                let unnaturalFeature = {
                    Table = "unnatural feature"
                    Outcomes = [
                        9, table "arcane"
                        2, table "planar"
                        1, table "divine"
                        ]
                    }

                let naturalFeature = {
                    Table = "natural feature"
                    Outcomes = [
                        2, table "lair"
                        2, table "obstacle"
                        3, table "terrain change"
                        2, table "water feature"
                        2, table "landmark"
                        1, table "resource"
                        ]
                    }

                let evidence = {
                    Table = "evidence"
                    Outcomes = [
                        6, table "tracks, spoor"
                        2, table "remains, debris"
                        1, table "stash, cache"
                        ]
                    }

                let structure = {
                    Table = "structure"
                    Outcomes = [
                        1, table "enigmatic"
                        2, table "infrastructure"
                        1, table "dwelling"
                        2, table "burial, religious"
                        2, table "steading"
                        4, table "ruin"
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

                let planar = {
                    Table = "planar"
                    Outcomes = [
                        4, value "distortion, warp"
                        4, value "portal, gate"
                        2, value "rift, tear"
                        2, value "outpost"
                        ]
                    }

                let divine = {
                    Table = "divine"
                    Outcomes = [
                        3, value "mark, sign"
                        3, value "cursed place"
                        3, value "hallowed place"
                        2, value "watched place"
                        1, value "presence"
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
                        2, T (Table "unnatural")
                        8, T (Table "natural")
                        2, T (Table "trap")
                        ]
                    }

                let unnatural = {
                    Table = "unnatural"
                    Outcomes = [
                        3, value "taint, blight, curse"
                        5, value "arcane trap, effect"
                        3, value "planar trap, effect"
                        1, value "divine"
                        ]
                    }

                let natural = {
                    Table = "natural"
                    Outcomes = [
                        2, value "blinding mist, fog"
                        2, value "bog, mire, quicksand"
                        3, value "pitfall, sinkhole, chasm"
                        2, value "poison, disease"
                        2, value "flood, fire, tornado"
                        1, T (Table "oddity") // TODO fix this
                        ]
                    }

                let trap = {
                    Table = "trap"
                    Outcomes = [
                        2, value "alarm"
                        3, value "ensnaring, paralyzing"
                        3, value "injurious (pit, ...)"
                        1, value "gas, fire, poison"
                        2, value "ambush"
                        ]
                    }

        let context =
            [
                General.element
                General.oddity
                General.creature
                General.beast
                General.earthbound
                General.airborne
                General.waterGoing
                General.humanoid
                General.common
                General.uncommon
                General.hybrid
                General.monster
                General.unusual
                General.rare
                General.legendary
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
                Wilderness.Discovery.planar
                Wilderness.Discovery.divine
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
                Wilderness.Danger.unnatural
                Wilderness.Danger.natural
                Wilderness.Danger.trap
            ]
            |> List.map (fun x -> x.Table, x)
            |> Map.ofList
            |> fun sources -> { Sources = sources }

    open Types
    open Data

    type Setting =
        | Wilderness
        | Dungeon

    let tableName (setting: Setting) (name: string) =
        match setting with
        | Wilderness -> name
        | Dungeon -> "dungeon " + name

    type Model = {
        Context: Context
        Setting: Setting
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
        | SwitchSetting

    let init () : Model * Cmd<Msg> =
        let setting = Wilderness
        {
            Context = context
            Setting = setting
            Danger = roll context (tableName setting "danger")
            Discovery = roll context (tableName setting "discovery")
            Creature = roll context ("creature")
        },
        Cmd.none

    let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
        match msg with
        | SwitchSetting ->
            let setting =
                match model.Setting with
                | Dungeon -> Wilderness
                | Wilderness -> Dungeon
            { model with
                Setting = setting
                Danger = roll context (tableName setting "danger")
                Discovery = roll context (tableName setting "discovery")
            }
            , Cmd.none
        | Roll rollType ->
            let model =
                match rollType with
                | Danger ->
                    { model with
                        Danger = roll context (tableName model.Setting "danger")
                    }
                | Discovery ->
                    { model with
                        Discovery = roll context (tableName model.Setting "discovery")
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
                        Creature = reroll context identifier model.Creature
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

                Level.level [] [
                    Level.left [] [
                        Heading.h2 [] [
                            match model.Setting with
                            | Wilderness -> str "Wilderness"
                            | Dungeon -> str "Dungeon"
                            ]
                        ]
                    Level.right [] [
                        Button.button [
                            Button.IsLight
                            Button.OnClick (fun _ -> dispatch SwitchSetting) ]
                            [ str "switch" ]
                        ]
                    ]

                Level.level [] [
                    Level.left [ Props [ OnClick(fun _ -> dispatch (Roll Discovery)) ] ] [
                        Level.item [] [ Image.image [ Image.Is32x32 ] [ img [ Src "d20.png" ] ] ]
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

                hr []

                Level.level [] [
                    Level.left [ Props [ OnClick(fun _ -> dispatch (Roll Danger)) ] ] [
                        Level.item [  ] [ Image.image [ Image.Is32x32 ] [ img [ Src "d20.png" ] ] ]
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

                ]

            Box.box' [] [
                Level.level [] [
                    Level.left [ Props [ OnClick(fun _ -> dispatch (Roll Creature)) ] ] [
                        Level.item [  ] [ Image.image [ Image.Is32x32 ] [ img [ Src "d20.png" ] ] ]
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