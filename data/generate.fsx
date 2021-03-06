#r "nuget: Thoth.Json.Net"
open Thoth.Json.Net

type Distribution =
    | Flat of list<string>
    | Weighted of list<float * string>

type Source = {
    Name: string
    Items: Distribution
    }

module DungeonWorld =

    let names = [
        "Finbar"; "Hywn"; "One Eye"; "Alhoro"; "Arlon"; "Yev"; "Slime"; "Jocat"; "Ewing";
        "Lim"; "Poy"; "Milo"; "Deryl"; "Medlyn"; "Astrafel"; "Daelwyn"; "Feliana"; "Damarra";
        "Sistranalle"; "Pendrell"; "Melliandre"; "Dagoliir"; "Baldric"; "Leena"; "Dunwick";
        "Willem"; "Edwyn"; "Florian"; "Seraphine"; "Quorra"; "Charlotte"; "Lily"; "Ramonde";
        "Cassandra"; "Durga"; "Aelfar"; "Gerda"; "Rurgosh"; "Bjorn"; "Drummond";
        "Helga"; "Siggrun"; "Freya"; "Wesley"; "Brinton"; "Jon"; "Sara"; "Hawthorn"; "Elise";
        "Clarke"; "Lenore"; "Piotr"; "Dahlia"; "Carmine"; "Hycorax"; "Ethanwe"; "Sinathel";
        "Demanor"; "Menoliir"; "Mithralan"; "Taeros"; "Aegor"; "Tanner"; "Dunstan";
        "Rose"; "Ivy"; "Robard"; "Mab"; "Thistle"; "Puck"; "Anne"; "Serah"; "Elana"; "Obelis";
        "Herran"; "Syla"; "Andanna"; "Siobhan"; "Aziz"; "Pelin"; "Sibel"; "Nils"; "Wei"; "Ozruk";
        "Surtur"; "Brunhilda"; "Annika"; "Janos"; "Greta"; "Dim"; "Rundrig"; "Jarl"; "Xotoq";
        "Elohiir"; "Sharaseth"; "Hasrith"; "Shevaral"; "Cadeus"; "Eldar"; "Kithracet";
        "Thelian"; "Finnegan"; "Olive"; "Randolph"; "Bartleby"; "Aubrey"; "Baldwin";
        "Becca"; "Hawke"; "Rudiger"; "Gregor"; "Brianne"; "Walton"
        ]

    let instincts = [
        "To avenge"
        "To spread the good word"
        "To reunite with a loved one"
        "To make money"
        "To make amends"
        "To explore a mysterious place"
        "To uncover a hidden truth"
        "To locate a lost thing"
        "To kill a hated foe"
        "To conquer a faraway land"
        "To cure an illness"
        "To craft a masterwork"
        "To survive just one more day"
        "To earn affection"
        "To prove a point"
        "To be smarter, faster and stronger"
        "To heal an old wound"
        "To extinguish an evil forever"
        "To hide from a shameful fact"
        "To evangelize"
        "To spread suffering"
        "To prove worth"
        "To rise in rank"
        "To be praised"
        "To discover the truth"
        "To make good on a bet"
        "To get out of an obligation"
        "To convince someone to do their dirty work"
        "To steal something valuable"
        "To overcome a bad habit"
        "To commit an atrocity"
        "To earn renown"
        "To accumulate power"
        "To save someone from a monstrosity"
        "To teach"
        "To settle down"
        "To get just one more haul"
        "To preserve the law"
        "To discover"
        "To devour"
        "To restore the family name"
        "To live a quiet life"
        "To help others"
        "To atone"
        "To prove their worth"
        "To gain honor"
        "To expand their land"
        "To gain a title"
        "To retreat from society"
        "To escape"
        "To party"
        "To return home"
        "To serve"
        "To reclaim what was taken"
        "To do what must be done"
        "To be a champion"
        "To avoid notice"
        "To help a family member"
        "To perfect a skill"
        "To travel"
        "To overcome a disadvantage"
        "To play the game"
        "To establish a dynasty"
        "To improve the realm"
        "To retire"
        "To recover a lost memory"
        "To battle"
        "To become a terror to criminals"
        "To raise dragons"
        "To live up to expectations"
        "To become someone else"
        "To do what can???t be done"
        "To be remembered in song"
        "To be forgotten"
        "To find true love"
        "To lose their mind"
        "To indulge"
        "To make the best of it"
        "To find the one"
        "To destroy an artifact"
        "To show them all"
        "To bring about unending summer"
        "To fly"
        "To find the six-fingered man"
        "To wake the ancient sleepers"
        "To entertain"
        "To follow an order"
        "To die gloriously"
        "To be careful"
        "To show kindness"
        "To not screw it all up"
        "To uncover the past"
        "To go where no man has gone before"
        "To do good"
        "To become a beast"
        "To spill blood"
        "To live forever"
        "To hunt the most dangerous game"
        "To hate"
        "To run away"
        ]

    let knacks = [
        "Criminal connections"
        "Muscle"
        "Skill with a specific weapon"
        "Hedge wizardry"
        "Comprehensive local knowledge"
        "Noble blood"
        "A one-of-a-kind item"
        "Special destiny"
        "Unique perspective"
        "Hidden knowledge"
        "Magical awareness"
        "Abnormal parentage"
        "Political leverage"
        "A tie to a monster"
        "A secret"
        "True love"
        "An innocent heart"
        "A plan for the perfect crime"
        "A one-way ticket to paradise"
        "A mysterious ore"
        "Money, money, money"
        "Divine blessing"
        "Immunity from the law"
        "Prophecy"
        "Secret martial arts techniques"
        "A ring of power"
        "A much-needed bag of taters"
        "A heart"
        "A fortified position"
        "Lawmaking"
        "Tongues"
        "A discerning eye"
        "Endurance"
        "A safe place"
        "Visions"
        "A beautiful mind"
        "A clear voice"
        "Stunning looks"
        "A catchy tune"
        "Invention"
        "Baking"
        "Brewing"
        "Smelting"
        "Woodworking"
        "Writing"
        "Immunity to fire"
        "Cooking"
        "Storytelling"
        "Ratcatching"
        "Lying"
        "Utter unremarkableness"
        "Mind-bending sexiness"
        "Undefinable coolness"
        "A way with knots"
        "Wheels of polished steel"
        "A magic carpet"
        "Endless ideas"
        "Persistence"
        "A stockpile of food"
        "A hidden path"
        "Piety"
        "Resistance to disease"
        "A library"
        "A silver tongue"
        "Bloodline"
        "An innate spell"
        "Balance"
        "Souls"
        "Speed"
        "A sense of right and wrong"
        "Certainty"
        "An eye for detail"
        "Heroic self-sacrifice"
        "Sense of direction"
        "A big idea"
        "A hidden entrance to the city"
        "The love of someone powerful"
        "Unquestioning loyalty"
        "Exotic fruit"
        "Poison"
        "Perfect memory"
        "The language of birds"
        "A key to an important door"
        "Metalworking"
        "Mysterious benefactors"
        "Steely nerves"
        "Bluffing"
        "A trained wolf"
        "A long-lost sibling, regained"
        "An arrow with your name on it"
        "A true name"
        "Luck"
        "The attention of supernatural powers"
        "Kindness"
        "Strange tattoos"
        "A majestic beard"
        "A book in a strange language"
        "Power overwhelming"
        "Delusions of grandeur"
        "The wind at his back and a spring in his step"
        ]

    let gmMoves = [
        "Use a monster, danger, or location move"
        "Reveal an unwelcome truth"
        "Show signs of an approaching threat"
        "Deal damage"
        "Use up their resources"
        "Turn their move back on them"
        "Separate them"
        "Give an opportunity that fits a class??? abilities"
        "Show a downside to their class, race, or equipment"
        "Offer an opportunity, with or without cost"
        "Put someone in a spot"
        "Tell them the requirements or consequences and ask"
        ]

    let dungeonMoves = [
        "Change the environment"
        "Point to a looming threat"
        "Introduce a new faction or type of creature"
        "Use a threat from an existing faction or type of creature"
        "Make them backtrack"
        "Present riches at a price"
        "Present a challenge to one of the characters"
        ]

open System.IO

module DataModel =

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

    let model =
        let namedSource name data =
            {
                Name = name
                Items = data
            }

        let sources = [
            DungeonWorld.names |> Flat |> namedSource "name"
            DungeonWorld.instincts |> Flat |> namedSource "instinct"
            DungeonWorld.knacks |> Flat |> namedSource "knack"
            DungeonWorld.gmMoves |> Flat |> namedSource "gm move"
            DungeonWorld.dungeonMoves |> Flat |> namedSource "dungeon move"
            ]
        let definitions = [ ]

        let model = {
            Sources = sources
            Definitions = definitions
            }
        model

DataModel.model
|> fun x -> Encode.Auto.toString(0, x)
|> fun x -> File.WriteAllText(Path.Combine(__SOURCE_DIRECTORY__, "dungeonworld.json"), x)

#load @"../src/Combinators.fs"
open Doskvol

let pickers =
    Path.Combine(__SOURCE_DIRECTORY__, "dungeonworld.json")
    |> File.ReadAllText
    |> Decode.Auto.fromString<Data.Model>
    |> fun x ->
        match x with
        | Ok data -> data |> Data.read
        | Error _ -> failwith "boom"

let rng: RNG =
    let x = System.Random ()
    fun () -> x.NextDouble ()

pickers.["name"] |> Picker.pick rng
pickers.["instinct"] |> Picker.pick rng
pickers.["knack"] |> Picker.pick rng

