-- | Functions for working with two-dimensional blocks of text.
module Text.Block where

data Block = Block
  { blockLines :: [String]
  , height :: Int
  , width :: Int
  } deriving (Eq, Ord)

instance Show Block where
  show = unlines . blockLines

empty :: Block
empty = Block
  { blockLines = []
  , height = 0
  , width = 0
  }

line :: String -> Block
line s = Block
  { blockLines = [s]
  , height = 1
  , width = length s
  }

text :: String -> Block
text = foldr (vert . line) empty . lines

setHeight :: Int -> Block -> Block
setHeight h b = let w = width b in Block
  { blockLines = setLength h (replicate w ' ') (blockLines b)
  , height = h
  , width = w
  }

setWidth :: Int -> Block -> Block
setWidth w b = Block
  { blockLines = [ setLength w ' ' l | l <- blockLines b ]
  , height = height b
  , width = w
  }

horiz :: Block -> Block -> Block
horiz x y = let h = max (height x) (height y) in Block
  { blockLines =
    zipWith (++) (blockLines $ setHeight h x) (blockLines $ setHeight h y)
  , height = h
  , width = width x + width y
  }

vert :: Block -> Block -> Block
vert x y = let w = max (width x) (width y) in Block
  { blockLines = map (setLength w ' ') $ blockLines x ++ blockLines y
  , height = height x + height y
  , width = w
  }

-- | Sets the length of a list, possibly adding a filler element at the end.
setLength :: Int -> a -> [a] -> [a]
setLength n c s = take n $ s ++ repeat c
