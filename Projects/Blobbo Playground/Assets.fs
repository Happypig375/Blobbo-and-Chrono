namespace BlobboPlayground
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let Background = asset<TileMap> PackageName "Background"
        let Hourglass = asset<Image> PackageName "Hourglass"
        let Sand = asset<Image> PackageName "Sand"
        let WaterArrow = asset<Image> PackageName "WaterArrow"
        let WaterCursor = asset<Cursor> PackageName "WaterCursor"
