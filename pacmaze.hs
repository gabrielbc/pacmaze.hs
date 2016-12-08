import System.Environment   
import System.Directory  
import System.IO  
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Ord (comparing)
import Heap

type World     = [String]
type State     = [Int]
type Action    = String
type Cost      = Int
type Explored  = [Node]
type Fringe    = [Node]
type Search    = Fringe -> [Node] -> Fringe
type Goal      = (Int,Int)
type Direction = String
type FringeH   = BinaryHeap

data Node   = Node {
  state  :: State,
  action :: Action,
  cost   :: Cost,
  parent :: State,
  costH  :: Cost }
  deriving (Read, Show)

instance Eq Node where
  x == y = costH x == costH y

instance Ord Node where
  compare = comparing costH

dispatchMove :: Int -> Int -> [(String,[Int])]
dispatchMove i j = [("up",   [i-1,j]),
                    ("down", [i+1,j]),
                    ("left", [i,j-1]),
                    ("right",[i,j+1])]

printNode :: Node -> String
printNode node = foldl1 (++) ["(",a,";",i,",",j,")"]
    where
      [i,j] = map show $ state node
      a     = action node

printNodes :: [Node] -> String
printNodes ns = intercalate " " $ map printNode ns

printExpand :: Node -> String
printExpand n = foldl1 (++) ["(",a,";",i,",",j,";",c,";",x,",",y,")"]
    where
      [i,j] = map show $ state n
      [x,y] = map show $ parent n
      a     = action n
      c     = show $ cost n

printExpands :: [Node] -> String
printExpands ns = intercalate " " $ map printExpand ns

printAction :: Node -> String
printAction node = action node

printActions :: [Node] -> String
printActions ns = intercalate " " $ map printAction ns

heuristic :: State -> Cost -> Goal -> Cost
heuristic state c (x,y) = c + (abs(sx - x) + abs(sy - y))
    where
      [sx,sy] = state

move :: World -> State -> Cost -> Goal -> Direction -> Maybe Node
move w curState c g direction
    | validMove w newState  = Just $ Node newState direction newCost curState ct
    | otherwise             = Nothing
    where
      newState  = fromJust $ lookup direction (dispatchMove i j)
      [i,j]     = curState
      newCost   = c + 1
      ct        = heuristic newState newCost g

validMove :: World -> State -> Bool
validMove w [i,j] = elemAt w i j /= '#'

elemAt :: World -> Int -> Int -> Char
elemAt w i j = w !! i !! j

successor :: State -> World -> Cost -> Goal -> [Node]
successor [i,j] w c g = mapMaybe (move w [i,j] c g) ["up","down","left","right"]

expand :: Node -> World -> Goal -> [Node]
expand n w g = successor (state n) w (cost n) g

isGoal :: Node -> World -> Bool
isGoal n w = elemAt w i j == '0'
    where [i,j] = state n

searchGoal :: World -> (Maybe Int,Maybe Int)
searchGoal w = head [(elemIndex a w,elemIndex b a) | a<-w, b<-a, b == '0', elem '0' a]

notExp :: Node -> Explored -> Bool
notExp n e
    | length e == 0                 = True
    | (state n) == (state $ head e) = False
    | otherwise                     = notExp n (tail e)

graphSearch :: Fringe -> World -> Explored -> Search -> Goal -> [Node]
graphSearch (f:fs) w e search g
    | null (f:fs)  = []
    | isGoal f w   = reverse $ solution e [f]
    | notExp f e   = graphSearch newFringe w (f:e) (search) g
    | otherwise    = graphSearch fs w e (search) g
    where
      newFringe = search fs $ expand f w g

searchBFS :: Fringe -> [Node] -> Fringe
searchBFS f n = f ++ n

searchDFS :: Fringe -> [Node] -> Fringe
searchDFS f n = n ++ f

graphSearchA :: FringeH Node -> World -> Explored -> Goal -> [Node]
graphSearchA fringe w e g
    | nullH fringe  =  []
    | isGoal f w    = reverse $ solution e [f]
    | notExp f e    = graphSearchA newFringe w (f:e) g
    | otherwise     = graphSearchA fs w e g
    where
      newFringe = searchA fs $ expand f w g
      (f,fs)    = removeH fringe

removeH :: FringeH Node -> (Node,FringeH Node)
removeH f = (headH f, tailH f)

searchA :: FringeH Node -> [Node] -> FringeH Node
searchA fringe nodes
    | nullH fringe      = fromList nodes
    | null nodes        = fringe
    | otherwise         = searchA (insertH n fringe) ns
    where
      (n:ns) = nodes

solution :: Explored -> [Node] -> [Node]
solution e n 
    | lastNode == "root" = []
    | otherwise          = n ++ (solution e parentNode)
    where
      lastNode   = action $ head n
      parentNode = findParent e n

findParent :: Explored -> [Node] -> [Node]
findParent (e:es) [n]
    | null (e:es)         = []
    | action e == "root"  = [e]
    | parRef == parState  = [e]
    | otherwise           = findParent es [n]
    where
      parRef   = parent n
      parState = state e
    
main = do  
    (cmd:w:args0) <- getArgs  
    contents    <- readFile w
    let args  = map (read::String->Int) args0
        world = tail $ lines contents
        root  = Node [args !! 0, args !! 1] "root" (args !! 2) [-1,-1] 0
        (Just gx,Just gy)  = searchGoal world
    putStrLn $ case cmd of
      "successor" -> printNodes $ successor args world 0 (gx,gy)
      "expand"    -> printExpands $ expand root world (gx,gy) 
      "dfs"       -> printActions $ graphSearch [root] world [] (searchDFS) (gx,gy)
      "bfs"       -> printActions $ graphSearch [root] world [] (searchBFS) (gx,gy)
      "astar"     -> printActions $ graphSearchA (fromList [root]) world [] (gx,gy)