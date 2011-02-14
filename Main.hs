module Main where

import Data.Map as Map
import Data.Either
import System.Environment
import Language.C
import Language.C.System.GCC

mergeHeaders :: [CTranslUnit] -> CTranslUnit
mergeHeaders units = let (mps,rests) = unzip [ generateTypeMap decl | CTranslUnit decl _ <- units ]
                     in CTranslUnit ((Map.elems $ Map.unions mps)++concat rests) internalNode

generateTypeMap :: [CExtDecl] -> (Map String CExtDecl,[CExtDecl])
generateTypeMap decls = let (mp,rest) = partitionEithers $ fmap (\decl -> case declName decl of
                                                                    Nothing -> Right decl
                                                                    Just n -> Left (n,decl)) decls
                        in (Map.fromList mp,rest)

declName :: CExtDecl -> Maybe String
declName (CDeclExt (CDecl specs [(Just (CDeclr (Just ident) _ _ _ _),_,_)] _)) = Just $ identToString ident
declName _ = Nothing

getC :: FilePath -> IO CTranslUnit
getC fp = do
  res <- parseCFile (newGCC "gcc") Nothing [] fp
  case res of
    Left err -> error $ show err
    Right unit -> return unit

main = do
  args <- getArgs
  cunits <- mapM getC args
  print $ pretty $ mergeHeaders cunits