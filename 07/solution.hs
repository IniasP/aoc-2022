import Data.List ( intercalate, stripPrefix )

data Command = Cd String | Ls [FileNode]
instance Show Command where
    show :: Command -> String
    show (Cd s)  = "> cd " ++ s
    show (Ls ss) = "> ls" ++ "\n" ++ intercalate "\n" (map show ss)

data FileNode = File Int String | Dir String [FileNode]
instance Show FileNode where
    show :: FileNode -> String
    show n = case n of
        File size name    -> "file " ++ name ++ " size " ++ show size
        Dir name []       -> "dir " ++ name
        Dir name contents -> "dir " ++ name ++ " total size " ++ show (getTotalSize (Dir name contents)) ++
                             " containing: \n" ++ concatMap (indent . show) contents

indent :: String -> String
indent = unlines . map ("    " ++) . lines

type Path = [String]

takeFirstMatching :: (a -> Bool) -> [a] -> (Maybe a, [a])
takeFirstMatching = takeFirstMatchingHelper []

takeFirstMatchingHelper :: [a] -> (a -> Bool) -> [a] -> (Maybe a, [a])
takeFirstMatchingHelper prev f []     = (Nothing, prev)
takeFirstMatchingHelper prev f (x:xs) =
    if f x
    then (Just x, addPrevBack prev xs)
    else takeFirstMatchingHelper (x:prev) f xs

addPrevBack :: [a] -> [a] -> [a]
addPrevBack xs l = foldl (flip (:)) l xs

main :: IO ()
main = do
    s <- readFile "input"
    let tree = commandsToFileTree $ linesToCommands $ lines s
    putStrLn "TREE:"
    print tree
    putStrLn $ "Sum of sizes at most 100000 (part 1): " ++ show (sumTotalSizesAtMost 100000 tree)
    let totalSpace = 70000000
    let desiredSpace = 30000000
    let usedSpace = getTotalSize tree
    let sizeToFree = desiredSpace + usedSpace - totalSpace
    putStrLn $ "Used " ++ show usedSpace ++ ", need to free " ++ show sizeToFree
    putStrLn $ "Smallest dir that frees enough (part 2): " ++ show (getSmallestDirSizeAtLeast sizeToFree tree)
    return ()

splitUntilNextCmd :: [String] -> ([String], [String])
splitUntilNextCmd = splitUntilNextCmdAcc []

splitUntilNextCmdAcc :: [String] -> [String] -> ([String], [String])
splitUntilNextCmdAcc acc [] = (acc, [])
splitUntilNextCmdAcc acc (s:ss) = case s of
    '$' : _ -> (acc, s:ss)
    _       -> splitUntilNextCmdAcc (acc ++ [s]) ss

linesToCommands :: [String] -> [Command]
linesToCommands [] = []
linesToCommands (s:ss) = case take 4 s of
    "$ cd" -> Cd (drop 5 s) : linesToCommands ss
    "$ ls" -> let (lsRes, rest) = splitUntilNextCmd ss
              in Ls (map mapLsLine lsRes) : linesToCommands rest
    c      -> error ("invalid command '" ++ c ++ "'")

mapLsLine :: String -> FileNode
mapLsLine s =
    case head s of
        'd' -> case stripPrefix "dir " s of
                  Just dirName -> Dir dirName []
                  Nothing      -> error ("invalid line '" ++ s ++ "'")
        _   -> let ws = words s
               in File (read (head ws) :: Int) (ws!!1)

commandsToFileTree :: [Command] -> FileNode
commandsToFileTree = commandsToFileTreeHelper (Dir "/" []) []

commandsToFileTreeHelper :: FileNode -> Path -> [Command] -> FileNode
commandsToFileTreeHelper tree path [] = tree
commandsToFileTreeHelper tree path (c:cs) = case c of
    Cd dir   -> commandsToFileTreeHelper tree (navigatePath path dir) cs
    Ls nodes -> commandsToFileTreeHelper (addToTree path nodes tree) path cs

-- addToTree :: dirPath -> newNode -> existingTree -> newTree
addToTree :: Path -> [FileNode] -> FileNode -> FileNode
addToTree []         newNodes (Dir dir contents)       = Dir dir (contents ++ newNodes)
addToTree []         _        _                        = error "can't add a child to a file node"
addToTree (dir:dirs) newNodes (Dir parentDir contents) =
    let (matchingDirNodeMaybe, restContents) = takeFirstMatching (isDirWithName dir) contents
    in case matchingDirNodeMaybe of
        Just matchingDir
            -> Dir parentDir (addToTree dirs newNodes matchingDir : restContents)
        _   -> error ("cannot find " ++ dir ++ " in " ++ parentDir)

isDirWithName :: String -> FileNode -> Bool
isDirWithName name (Dir dirName _) = name == dirName
isDirWithName _               _    = False

navigatePath :: Path -> String -> Path
navigatePath path ".." = init path
navigatePath path "/"  = []
navigatePath path dir  = path ++ [dir]

getTotalSize :: FileNode -> Int
getTotalSize (File size _)    = size
getTotalSize (Dir _ contents) = sum $ map getTotalSize contents

sumTotalSizesAtMost :: Int -> FileNode -> Int
sumTotalSizesAtMost n node = case node of
    File _ _       -> 0
    Dir _ contents -> let totalSize = getTotalSize node
                          recSize   = sum $ map (sumTotalSizesAtMost n) contents
                      in if totalSize <= n
                         then totalSize + recSize
                         else recSize

getSmallestDirSizeAtLeast :: Int -> FileNode -> Int
getSmallestDirSizeAtLeast n = minimum . getSizesAtLeastN n

getSizesAtLeastN :: Int -> FileNode -> [Int]
getSizesAtLeastN n node = case node of
    File _ _       -> []
    Dir _ contents -> let recResult = concatMap (getSizesAtLeastN n) contents
                          totalSize = getTotalSize node
                      in if getTotalSize node >= n
                         then totalSize : recResult
                         else recResult
