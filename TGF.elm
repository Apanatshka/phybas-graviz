{- |
Module      :  TGF
Description :  Definition and functions of the Trivial Graph Format.
Copyright   :  (c) Jeff Smits
License     :  GPL-3.0

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

 | ---------------------------------------------------------------------- |
 | This program is free software: you can redistribute it and/or modify   |
 | it under the terms of the GNU General Public License as published by   |
 | the Free Software Foundation, either version 3 of the License, or      |
 | (at your option) any later version.                                    |
 |                                                                        |
 | This program is distributed in the hope that it will be useful,        |
 | but WITHOUT ANY WARRANTY; without even the implied warranty of         |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          |
 | GNU General Public License for more details.                           |
 |                                                                        |
 | You should have received a copy of the GNU General Public License      |
 | along with this program.  If not, see <http://www.gnu.org/licenses/>.  |
 | ---------------------------------------------------------------------- |
-}

module TGF where

import Char
import Maybe

type NodeID = Int

type TGFNode  = { id : NodeID, label : String }
type TGFEdge  = { idFrom : NodeID, idTo : NodeID, label : String }
type TGFGraph = { nodes : [TGFNode], edges : [TGFEdge] }

empty = { nodes = [], edges = [] }

dropWhile : (a -> Bool) -> [a] -> [a]
dropWhile pred list =
  case list of
    head :: tail -> if pred head then dropWhile pred tail else list
    []           -> []

splitOn : (a -> Bool) -> [a] -> ([a], [a])
splitOn pred list =
  case list of
    head :: tail -> if pred head
                      then ([], tail)
                      else
                        let (l,r) = splitOn pred tail
                        in (head :: l, r)
    []           -> ([], [])

fromString : String -> TGFGraph
fromString str =
  let notDigit char = not <| Char.isDigit char
      nodeFromString str =
        let (idStr, label) = splitOn notDigit str
        in case readInt idStr of
          Just id -> Just { id = id, label = label }
          Nothing -> Nothing
      edgeFromString str =
        let (idStr1, rest) = splitOn notDigit str
            (idStr2, label) = splitOn notDigit <| dropWhile notDigit rest
        in case (readInt idStr1, readInt idStr2) of
          (Just idFrom, Just idTo) -> Just { idFrom = idFrom, idTo = idTo, label = label }
          _                        -> Nothing
  in
    let lines = split "\n" str
        (nodeStrings, edgeStrings) = splitOn ((==) "#") lines
        maybeNodes = map nodeFromString nodeStrings
        maybeEdges = map edgeFromString edgeStrings
    in
      if not <| any Maybe.isNothing maybeNodes || any Maybe.isNothing maybeEdges
      then { nodes = Maybe.justs maybeNodes, edges = Maybe.justs maybeEdges }
      else empty

toString : TGFGraph -> String
toString graph =
  let nodeToString node = join " " [show node.id, node.label]
      edgeToString edge = join " " [show edge.idFrom, show edge.idTo, edge.label]
  in join "\n#\n" <| map (join "\n") [map nodeToString graph.nodes, map edgeToString graph.edges]
