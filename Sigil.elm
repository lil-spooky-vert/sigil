{--
MIT License

Copyright (c) 2018 Mario Zavalas

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

--}

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (fromString, shape, size, huge, Text)
import Color exposing (..)
import Arithmetic exposing (..)
import Char
import Random
import List

--import Html exposing (..)

type Renderable = RenderShape Shape
                | RenderPath Path

-- from http://elm-lang.org/examples/zip
-- (,) is a shortcut from creating tuples
zip : List a -> List b -> List (a, b)
zip a b =
    List.map2 (,) a b

shift: Int -> List a -> List a
shift n list =
    List.drop n list ++ List.take n list

-- iterator
iterateN : Int -> (a -> a) -> a -> List a
iterateN n func a =
    if n <= 0 then
        []
    else
        a :: iterateN (n - 1) func (func a)

const: a -> b -> a
const a _ = a

padding: Collage msg-> Collage msg
padding n =
    spacer (width n + 5) (height n + 5)

render: Renderable -> LineStyle -> Collage msg
render r ls =
    case r of
        RenderPath path ->
            traced ls path

        RenderShape shape ->
            outlined ls shape

renderBlack: Renderable -> Collage msg
renderBlack x =
    render x <| solid thin <| uniform Color.black

-- creates a polygram
ngram : Int -> Int -> Float -> Collage msg
ngram vertices skip radius =
    let
        rotations = gcd skip vertices
        skip_ = skip // rotations
        theta =
            2 * pi / (toFloat vertices)
        points = circularPoints vertices theta radius
        lines = List.map (renderBlack << RenderPath << uncurry segment) << zip points <| shift (skip_ * rotations) points
    in
        lines
            |> group

-- ring
ring : Float -> Float -> Collage msg
ring radius width =
    let
        outer = renderBlack << RenderShape <| (circle radius)
        inner = renderBlack << RenderShape <| (circle (radius - width))
    in
        outer
            |> impose inner


circularPoints : Int -> Float -> Float -> List Point
circularPoints v theta r =
    List.map (\x -> (cos x * r, sin x * r))
    <| iterateN (v) (\rads -> rads + theta) (pi/2)


main =
    let
          original = rotate (degrees 360/8/2) <| ngram 8 3 278
--          original = ngram 16 4 278
          x = ring 200 30
          border = ngram 9 2 300
          circ = outlined (solid thin(uniform Color.red)) (circle 300)
          text = fromString "\x1F437"
                     |> size 200
                     |> rendered
    in
        padding circ
            |> impose circ
            |> impose original
            |> impose border
            |> impose x
            |> impose text
            |> svg