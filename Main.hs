module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either
import System.Environment
import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess
import Data.List (sortBy)
import Data.Ord
import Data.Either
import qualified Data.ByteString as BS

mergeHeaders :: [CTranslUnit] -> CTranslUnit
mergeHeaders units = let (mps,rests) = unzip [ generateTypeMap decl | CTranslUnit decl _ <- units ]
                         nmps = zipWith (\mp i -> fmap (\d -> (d,i)) mp) mps [0..]
                         decls = (Map.elems $ Map.unions nmps)++concat (zipWith (\rest i -> fmap (\d -> (d,i)) rest) rests [0..])
                     in CTranslUnit (fmap fst $ sortBy (comparing (\(d,i) -> (i,posOf d))) decls) internalNode

generateTypeMap :: [CExtDecl] -> (Map String CExtDecl,[CExtDecl])
generateTypeMap decls = let (mp,rest) = partitionEithers $ fmap (\decl -> case declName decl of
                                                                    Nothing -> Right decl
                                                                    Just n -> Left (n,decl)) decls
                        in (Map.fromList mp,rest)

declName :: CExtDecl -> Maybe String
declName (CDeclExt (CDecl specs [(Just (CDeclr (Just ident) _ _ _ _),_,_)] _)) = Just $ identToString ident
declName _ = Nothing

getC :: [String] -> FilePath -> IO CTranslUnit
getC args fp = do
  res <- parseCFile (newGCC "gcc") Nothing args fp
  case res of
    Left err -> error $ show err
    Right unit -> return unit

classifyArgument :: String -> Either FilePath String
classifyArgument arg
  | null arg        = Left arg
  | head arg == '-' = Right arg
  | otherwise       = Left arg

main = do
  args <- getArgs
  let (files,cpp_args) = partitionEithers $ fmap classifyArgument args
  pre_res <- runPreprocessor (newGCC "gcc") (rawCppArgs ("-dM":cpp_args) (head files))
  case pre_res of
    Left _ -> error "Preprocessing failed"
    Right defs -> do
      cunits <- mapM (getC cpp_args) files
      putStrLn "#pragma once"
      BS.putStrLn defs
      print $ pretty $ mergeHeaders cunits