module Main where

import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA

import Control.Monad
import Control.Applicative

import Data.Graph
import Data.List
import Data.Maybe
import Data.Function
import Data.Array
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import System.FilePath.Posix
import System.Directory
import System.Environment


main = do
  args <- getArgs
  case args of
    [depth, startDir] -> do
        headers <- find_include_files (read depth) startDir 
        let uniqueHeaders = nubBy ((==) `on` takeFileName) headers
        headerToIncludes <- create_included_files_map uniqueHeaders
        let dotifiedOutput = mapToDot headerToIncludes
        writeFile "include_graph.dot"  dotifiedOutput
        writeFile "minimal.h" $ unlines $ (\inc -> "#include \"" ++ inc ++ "\"") <$> getRootHeaders headerToIncludes

    _ -> do
         putStrLn "Generates a dot showing how C header files include themselves. The output is written to a file called 'include_graph.dot'."
         putStrLn "Additionally a header file 'minimal.h' is generated that contains the minimal set of include files, so that all includes are somehow included."
         putStrLn ""
         putStrLn "Usage:"
         putStrLn "IncludeGraphs <depth> <directory>"
         putStrLn ""
         putStrLn "Example:"
         putStrLn "IncludeGraphs 5 ./ > output.dot"
         putStrLn ""        

t1 = Just "Hallo"
t3 = Nothing 
t2 = ["Hallo", "Welt"]
tf = (\inc -> "#include \"" ++ inc ++ "\"")

getRootHeaders headerMap =
    let 
        rootHeaderNodes = map vertexLookup rootVertices
        rootVertices = [e| e <- vertices graph, (inDegreeTable ! e) == 0]
        inDegreeTable = indegree graph
        (graph, vertexLookup) = graphFromEdges' [(header, takeFileName header, includes) | 
                                                 (header, includes) <- M.toList headerMap]
    in
      map (\(node, key, keys) -> node) rootHeaderNodes

mapToDot headerMap = 
    "digraph G {\n    rankdir=LR\n" 
     ++ unlines dotifiedHeaders ++"\n}"
    where
      dotifiedHeaders =
          ["   " ++ (dotifyLabel $ takeFileName header) ++ " -> " ++ dotifyLabel include |
           (header, includes) <- M.toList headerMap, include <- includes]

dotifyLabel l = [if c `elem` ".,+-" then '_' else c |
                 c <- l]

-- creates a map containing each header file as key, and all the included files of that as value
create_included_files_map headers = 
    foldM (\includeMap headerFile -> do
             incs <- nub <$> map takeFileName <$> catMaybes <$> (map included_file) <$> B.lines 
                     <$> B.readFile headerFile
             let relevantIncs = filter (\i -> elem i relevantHeaders) incs                 
             return $ M.insert headerFile relevantIncs includeMap) M.empty headers
  where
    relevantHeaders = map takeFileName headers
                                     

-- recursively search for include files in a directory
find_include_files 0 _ = return []
find_include_files max_depth dir = do
  ls <- map (dir </>) <$> filter (/= "..") <$> filter (/= ".") <$> getDirectoryContents dir
  let include_file_candidates = filter (ends_with ".h") ls
  include_files <- filterM doesFileExist include_file_candidates
  sub_dirs <- filterM doesDirectoryExist ls
  sub_dirs_include_files <- mapM (find_include_files (max_depth - 1)) sub_dirs
  return $ nub $ concat (include_files : sub_dirs_include_files)

ends_with suffix str = (reverse suffix) == (take (length suffix) (reverse str))

-- If a line of C code contains an #include statement, this function returns Just included file, otherwise Nothing
included_file :: B.ByteString -> Maybe String
included_file l = 
    let        
        l_str = B.unpack l
        (_,_,_,ms) = match include_regex l_str :: (String, String, String, [String])
    in 
    case ms of
      [fname] -> Just fname
      _ -> Nothing
 
include_regex = makeRegex "#include +\"([^\"]+)\"" :: Regex

-- ------------------------------------------------------------------------------
-- TESTS
-- ------------------------------------------------------------------------------


included_file_test_01 = (included_file $ B.pack "sdfkljsdlfjsldjf") == Nothing
included_file_test_02 = (included_file  $ B.pack "#include <sdfkljsdlfjsldjf>") == Nothing
included_file_test_03 = (included_file  $ B.pack "#include \"test.h\"") == Just "test.h"

ends_with_test_01 = ends_with ".h" "test.h" == True