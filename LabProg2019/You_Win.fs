module LabProg2019.You_Win

open System
open Engine
open Gfx   

[< NoEquality; NoComparison >]
type state = {
    sprites : (sprite * float * float)[]

}

let W = 85
let H = 35

let main () =       
    let engine = new engine (85, 35)

    // Credit sprites 
    let n1 = engine.create_and_register_sprite (image.rectangle (20,1, pixel.filled Color.Cyan, pixel.filled Color.Cyan),2,2,2)
    n1.draw_text ("Filippo Di Gennaro",0,0, Color.Black, Color.Cyan)
    let n2 = engine.create_and_register_sprite (image.rectangle (16,1, pixel.filled Color.Cyan, pixel.filled Color.Cyan),W-20,2,2)
    n2.draw_text ("Martina Novello",0,0, Color.Black, Color.Cyan) 
    let n3 = engine.create_and_register_sprite (image.rectangle (17,1, pixel.filled Color.Cyan, pixel.filled Color.Cyan),W/2-10,2,2)
    n3.draw_text ("Filippo Villotta",0,0, Color.Black, Color.Cyan)
    let thankyou = engine.create_and_register_sprite (image.rectangle (25,1, pixel.filled Color.Cyan, pixel.filled Color.Cyan),W/2-10,H-5,3)
    thankyou.draw_text ("Thank you for playing",0,0, Color.DarkMagenta, Color.Cyan)
    let background = engine.create_and_register_sprite (image.rectangle (W,H, pixel.filled Color.Cyan, pixel.filled Color.Cyan),0,0,0)
   
    let winbox = engine.create_and_register_sprite (image.rectangle (50,4, pixel.filled Color.Cyan),20,5,4)
    winbox.draw_text (" ____ ____ ____ _________ ____ ____ ____ ____  ",0,0, Color.Black, Color.Cyan)
    winbox.draw_text ("||Y |||o |||u |||       |||w |||i |||n |||! || ",0,1, Color.Black, Color.Cyan)
    winbox.draw_text ("||__|||__|||__|||_______|||__|||__|||__|||__|| ",0,2, Color.Black, Color.Cyan)
    winbox.draw_text ("|/__\|/__\|/__\|/_______\|/__\|/__\|/__\|/__\| ",0,3, Color.Black, Color.Cyan)
    
    let my_update (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
        // move other sprites
        let sprites = [|
            for spr, dx, dy in st.sprites do
                spr.move_by (dx, dy)
                let dx = if int spr.x + spr.width >= screen.width || int spr.x <= 0 then -dx else dx
                let dy = if int spr.y + spr.height >= screen.height || int spr.y <= 0 then -dy else dy
                yield spr, dx, dy
            |]
        // calculate next state
        { sprites = sprites   
          },

        let thankyou = engine.create_and_register_sprite (image.rectangle (25,1, pixel.filled Color.Cyan, pixel.filled Color.Cyan),W/2-10,H-10,5)
        let hd = sprintf "Your score: %d\n" Score
        thankyou.draw_text (hd, 0,0, Color.Black, Color.Cyan)

        match keyo with 
        | None -> false 
        | Some k -> k.KeyChar = 'q'
        
    /// Name sprites and movement definitions 
    let moveme =[|
        (n1,0.8,0.8);
        (n2,0.5,0.5);
        (n3,0.3,0.3);
        |]  
     
    // define initial state
    let st0 = { 
        sprites = moveme
        }
    // start engine loop
    engine.loop my_update st0

   
    
