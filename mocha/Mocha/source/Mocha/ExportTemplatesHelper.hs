module Mocha.ExportTemplatesHelper

where

import Char
import Foreign
import Language.Haskell.THSyntax
 
objCSelectorExprToHaskellFunctionName (Q ioExpr) =
   case unsafePerformIO ioExpr of
      LitE (StringL name) ->
	 removeTrailingUnderscore $ colonsToUnderscore $ uncapitalise name
      _ ->
	 error "objCSelectorExprToHaskellFunctionName: didn't get a StringL"

objCSelectorNameToHaskellFunctionName name =
   removeTrailingUnderscore $ colonsToUnderscore $ uncapitalise name

colonsToUnderscore s = map (\c -> if c == ':' then '_' else c) s

uncapitalise :: String -> String
uncapitalise "" = ""
uncapitalise (a:x) = Char.toLower a:x

removeTrailingUnderscore "" = ""
removeTrailingUnderscore s = if last s == '_' then init s else s

