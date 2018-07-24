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
import Color exposing (..)
import Arithmetic exposing (..)
--import Html exposing (..)

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

padding: Collage msg-> Collage msg
padding n =
    spacer (width n + 5) (height n + 5)

renderBlack: Path -> Collage msg
renderBlack =
    traced <| solid thin <| uniform Color.black

-- creates a polygram
ngram : Int -> Int -> Float -> Collage msg
ngram vertices skip radius =
    let
        rotations = gcd skip vertices
        skip_ = skip // rotations
        theta =
            2 * pi / (toFloat vertices)
        points =
            List.map (\x -> (cos x * radius, sin x * radius))
            <| iterateN (vertices) (\rads -> rads + theta) (pi/2)

        lines = List.map (renderBlack << uncurry segment) << zip points <| shift (skip_ * rotations) points
    in
        lines
            |> group

main =
    let
          original = ngram 8 3 300
          circ = outlined (solid thin(uniform Color.red)) (circle 300)
    in
        padding circ
            |> impose circ
            |> impose original
            |> svg