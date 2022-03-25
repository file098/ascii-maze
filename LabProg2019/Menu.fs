
module LabProg2019.Menu

open Gfx
open Engine
open System
open Options

let W = 85
let H = 35
let engine = new engine (W,H)

type state = {
    player : sprite
}

let main() = 
    // size of options boxes
    let box_w = 35
    let box_h = 3
   
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =

        // Sound effect
        let soundH = System.Media.SystemSounds.Hand 
        let soundA = System.Media.SystemSounds.Asterisk 

        // Current coordinates 
        let (x,y) = int(st.player.x),int (st.player.y)
        let dx, dy =
            match key.KeyChar with
                |' '  -> Options.main () // Directed to options screen
                         0.,0.
                |_ -> soundA.Play()
                      0.,0.
        st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'

    // Various sprites for the aesthetic part of the menù 
    let background = engine.create_and_register_sprite (image.rectangle (W,H, pixel.filled Color.Black, pixel.filled Color.Black),0,0,0)
    let selector = engine.create_and_register_sprite(image.rectangle(15,3, pixel.filled Color.Cyan),W/2-7,H/2+4,3)
    let backgorundbox = engine.create_and_register_sprite (image.rectangle (box_w, box_h, pixel.filled Color.Black, pixel.filled Color.Black),W/2-7,H/2+4,1)
    let mutable start = engine.create_and_register_sprite (image.rectangle (box_w, box_h, pixel.filled Color.Black, pixel.filled Color.Black),W/2-7,H/2+4,2)
    start.draw_text ("START GAME", 2, 1, Color.White, Color.Black)
   
    // Title 
    let mutable background_title = engine.create_and_register_sprite (image.rectangle (85,15, pixel.filled Color.Black, pixel.filled Color.Black),3,2,0)
    let mutable title = engine.create_and_register_sprite (image.rectangle (85,15, pixel.filled Color.Black, pixel.filled Color.Black),3,2,1)
    title.draw_text (" ________  __                        __       __                                ",0,0, Color.Cyan, Color.Black) 
    title.draw_text ("|        \|  \                      |  \     /  \                               ",0,1, Color.Cyan, Color.Black) 
    title.draw_text (" \$$$$$$$$| $$____    ______        | $$\   /  $$  ______   ________   ______   ",0,2, Color.Cyan, Color.Black) 
    title.draw_text ("   | $$   | $$    \  /      \       | $$$\ /  $$$ |      \ |        \ /      \  ",0,3, Color.Cyan, Color.Black) 
    title.draw_text ("   | $$   | $$$$$$$\|  $$$$$$\      | $$$$\  $$$$  \$$$$$$\ \$$$$$$$$|  $$$$$$\ ",0,4, Color.Cyan, Color.Black) 
    title.draw_text ("   | $$   | $$  | $$| $$    $$      | $$\$$ $$ $$ /      $$  /    $$ | $$    $$ ",0,5, Color.Cyan, Color.Black) 
    title.draw_text ("   | $$   | $$  | $$| $$$$$$$$      | $$ \$$$| $$|  $$$$$$$ /  $$$$_ | $$$$$$$$ ",0,6, Color.Cyan, Color.Black)
    title.draw_text ("   | $$   | $$  | $$ \$$     \      | $$  \$ | $$ \$$    $$|  $$    \ \$$     \ ",0,7, Color.Cyan, Color.Black)
    title.draw_text ("    \$$    \$$   \$$  \$$$$$$$       \$$      \$$  \$$$$$$$ \$$$$$$$$  \$$$$$$$ ",0,8, Color.Cyan, Color.Black)
    title.draw_text ("Press SPACEBAR to begin the game",24,14, Color.White, Color.Black)


    let st0 = { 
        player = selector
    }

    engine.loop_on_key my_update st0



    