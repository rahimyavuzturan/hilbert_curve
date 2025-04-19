{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import GHC.Exts.Heap (IndexTable(it_back_edge), GenClosure (n_args))
import Data.List
import Data.Ord

type Point = (Float,Float) -- 3D point locations
type Line = (Point, Point, Float) -- Two points for a line and the width
type Triangle = (Point, Point, Point) --To save STL files

sortClockwise :: [Point] -> [Point]
sortClockwise ps = sortBy (flip $ comparing (angle c)) ps
  where
    c = (avg (map fst ps), avg (map snd ps))
    avg xs = sum xs / fromIntegral (length xs)
    angle (cx, cy) (x, y) = atan2 (y - cy) (x - cx)


line_list :: [Line]
line_list = [((-5,-0.25),(5,0.25),1)]

create_triangles_from_rectangle :: [Point] -> [Triangle]
create_triangles_from_rectangle corners = [tri1, tri2, tri3, tri4]
    where
        ([x1,x2,x3,x4], [y1,y2,y3,y4]) = unzip(sortClockwise corners)
        middle_point = ((x1+x2+x3+x4) /4, (y1+y2+y3+y4)/4)
        tri1 = ((x1,y1), middle_point, (x2,y2))
        tri2 = ((x2,y2), middle_point, (x3,y3))
        tri3 = ((x3,y3), middle_point, (x4,y4))
        tri4 = ((x4,y4), middle_point, (x1,y1))


get_rectangle_corners :: Line -> [(Float,Float)]
get_rectangle_corners ((x1,y1), (x2,y2), width) = [(x1 + hwx, y1 + hwy), (x1 - hwx, y1 - hwy), (x2 - hwx, y2 - hwy), (x2 + hwx, y2 + hwy)]
    where
        length = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))
        nx = -(y2 - y1) / length
        ny = (x2 - x1) / length
        hwx = nx * width / 2
        hwy = ny * width / 2 

linelist_to_rects :: [Line] -> [Triangle]
linelist_to_rects line_list = concat [create_triangles_from_rectangle (get_rectangle_corners line) | line <- line_list]

createTriangleDef :: Triangle -> String
createTriangleDef ((x1,y1),(x2,y2),(x3,y3)) = "  facet\n" ++
                                                       "    outer loop\n" ++
                                                       "      vertex " ++ (show x1) ++ " " ++ (show y1) ++ " 0 \n" ++
                                                       "      vertex " ++ (show x2) ++ " " ++ (show y2) ++ " 0 \n" ++
                                                       "      vertex " ++ (show x3) ++ " " ++ (show y3) ++ " 0 \n" ++
                                                       "    endloop\n" ++
                                                       "  endfacet\n"                            

createObjectModelString :: [Triangle] -> String 
createObjectModelString n = "solid Object01\n" ++ concat [createTriangleDef y | y<-n] ++ "endsolid Object01"



writeObjModel :: [Triangle] -> String -> IO ()
writeObjModel x filename = do writeFile filename (createObjectModelString x)


-- User-adjustable parameters

spanSize    :: Float  -- Total span of the curve in both X and Y
spanSize     = 20
strokeWidth :: Float  -- Thickness of each line segment
strokeWidth  = 0.10


-- state, durumumuzu tutcak
type TurtleState = (Point, Float, [Line])

-- forward, 
forward :: Float -> TurtleState -> TurtleState
forward dist ((x,y), theta, ls) =
  let x'   = x + dist * cos theta
      y'   = y + dist * sin theta
      ls'  = ls ++ [((x,y),(x',y'), strokeWidth)]
  in ((x',y'), theta, ls')

-- left, right turns, stateyi değişecek
left :: Float -> TurtleState -> TurtleState
left _ (p, theta, ls) = (p, theta + pi/2, ls)

right :: Float -> TurtleState -> TurtleState
right _ (p, theta, ls) = (p, theta - pi/2, ls)

-- positive hilbert curve recursion
a :: Integer -> Float -> TurtleState -> TurtleState
a 0 _ state = state
a n len state =
  let st1  = right 0 state
      st2  = b (n-1) len st1 -- when entering with n=1, theta changes, negative hilbert added
      st3  = forward len st2
      st4  = left 0 st3
      st5  = a (n-1) len st4
      st6  = forward len st5
      st7  = a (n-1) len st6
      st8  = left 0 st7
      st9  = forward len st8
      st10 = b (n-1) len st9
      st11 = right 0 st10
  in st11

--negative hilbert curve recursion
b :: Integer -> Float -> TurtleState -> TurtleState
b 0 _ state = state
b n len state =
  let st1  = left 0 state
      st2  = a (n-1) len st1 -- when entering with n=1, theta changes, directed to positive hilbert
      st3  = forward len st2
      st4  = right 0 st3
      st5  = b (n-1) len st4
      st6  = forward len st5
      st7  = b (n-1) len st6
      st8  = right 0 st7
      st9  = forward len st8
      st10 = a (n-1) len st9
      st11 = left 0 st10
  in st11

-- main funcs
hilbertcurve :: Integer -> [Line]
hilbertcurve order =
  let dim       = 2 ^ order
      step      = spanSize / fromIntegral (dim - 1)
      start     = (-spanSize/2, -spanSize/2)
      initState = (start, 0, [])
      (_, _, ls) = a order step initState --call first hilbert
  in ls


main = do writeObjModel (linelist_to_rects (hilbertcurve 6)) "hilbert.stl"
