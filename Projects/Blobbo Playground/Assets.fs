namespace BlobboPlayground
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module rec Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = nameof Gameplay
        let Background = asset<TileMap> PackageName (nameof Background)
