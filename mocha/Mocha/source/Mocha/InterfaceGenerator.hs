module Mocha.InterfaceGenerator
(
   importObjCClassList,
   knownClasses,
   printWholeHierarchy,
   wholeHierarchyS, -- should take these out ...
   wholeHierarchy,
   wholeHierarchyQ
)

-- tree name ideas: innate? intrinsic/extrinsic? internal/external?


where

import Data.Char
import Data.FiniteMap
import Data.List
import GHC.IOBase
import Language.Haskell.THSyntax hiding
   -- we have our own implementation of ppr*
   (
      nestDepth, Precedence, appPrec, opPrec, noPrec, parensIf, pprExp,
      pprExpI, pprFields, pprMaybeExp, pprMatch, pprLit,
      pprPat, pprPatI, pprDec, pprForeign, pprClause, pprCon,
      pprTyApp, split, pprCxt, pprStmt, pprRange, pprType, pprBody,
      pprStrictType, pprVarStrictType, pprParendType, pprRangeI,
      where_clause, showtextl
   )
import Text.PrettyPrint.HughesPJ

-- The below line should really be 610, not 601, but it appears that
-- the version number in CVS hasn't been bumped yet ...

type ClassName = String

type ClassToSuperclassFM = FiniteMap ClassName ClassName

class TreeDeclaration chd where
   typeDeclarations ::
      TreeFlag -> chd -> (ClassName, ClassName) -> ClassToSuperclassFM -> [Dec]
   classDeclarations ::
      TreeFlag -> chd -> (ClassName, ClassName) -> ClassToSuperclassFM -> [Dec]
   instanceFunctionDeclarations ::
      TreeFlag -> chd -> (ClassName, ClassName) -> ClassToSuperclassFM -> [Dec]
   isInstanceTree :: chd -> Bool
   isClassTree :: chd -> Bool
   isMetaclassTree :: chd -> Bool

data TreeFlag = InternalTree | ExternalTree

-- INSTANCE TREE

data InstanceTree = InstanceTree

instance TreeDeclaration InstanceTree where
   typeDeclarations ExternalTree t (className, superclassName) _ =
      typeTemplate ExternalTree t
      (className, superclassName)
      (className, superclassName)
   typeDeclarations InternalTree t (className, superclassName) _ =
      typeTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   classDeclarations ExternalTree t (className, superclassName) _ =
      classTemplate ExternalTree t
      ("Sub" ++ className, "Sub" ++ superclassName)
      (className, superclassName)
   classDeclarations InternalTree t (className, superclassName) _ =
      classTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   instanceFunctionDeclarations ExternalTree t (className, superclassName) _ =
      instanceTemplate ExternalTree t
      ("Sub" ++ className, "Sub" ++ superclassName)
      (className, superclassName)
   instanceFunctionDeclarations InternalTree t (className, superclassName) _ =
      instanceTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   isInstanceTree _ = True
   isClassTree _ = False
   isMetaclassTree _ = False

-- CLASS TREE

data ClassTree = ClassTree

instance TreeDeclaration ClassTree where
   typeDeclarations ExternalTree t (className, superclassName) _=
      typeTemplate ExternalTree t
      (className, superclassName)
      (className ++ "ClassObject", superclassName ++ "ClassObject")
   typeDeclarations InternalTree t (className, superclassName) _ =
      typeTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   classDeclarations ExternalTree t (className, superclassName) _ =
      classTemplate ExternalTree t
      (className ++ "ClassD", superclassName ++ "ClassD")
      (className ++ "ClassObject", superclassName ++ "ClassObject")
   classDeclarations InternalTree t (className, superclassName) _ =
      classTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   instanceFunctionDeclarations ExternalTree t (className, superclassName) _ =
      instanceTemplate ExternalTree t
      (className ++ "ClassD", superclassName ++ "ClassD")
      (className ++ "ClassObject", superclassName ++ "ClassObject")
   instanceFunctionDeclarations InternalTree t (className, superclassName) _ =
      instanceTemplate InternalTree t
      (className, superclassName)
      (className ++ "ClassObject", superclassName ++ "ClassObject")
   isInstanceTree _ = False
   isClassTree _ = True
   isMetaclassTree _ = False

-- METACLASS TREE

data MetaclassTree = MetaclassTree

instance TreeDeclaration MetaclassTree where
   typeDeclarations ExternalTree t (className, superclassName) _=
      typeTemplate ExternalTree t
      (className ++ "Metaclass", superclassName ++ "Metaclass")
      (className ++ "MetaclassObject", superclassName ++ "MetaclassObject")
   typeDeclarations InternalTree t (className, superclassName) _ =
      typeTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   classDeclarations ExternalTree t (className, superclassName) _ =
      classTemplate ExternalTree t
       (className ++ "Metaclass", superclassName ++ "Metaclass")
       (className ++ "MetaclassObject", superclassName ++ "MetaclassObject")
   classDeclarations InternalTree t (className, superclassName) _ =
      classTemplate InternalTree t
      (className, superclassName)
      (className, superclassName)
   instanceFunctionDeclarations ExternalTree t (className, superclassName) _ =
      instanceTemplate ExternalTree t
      (className ++ "Metaclass", superclassName ++ "Metaclass")
      (className ++ "MetaclassObject", superclassName ++ "MetaclassObject")
   instanceFunctionDeclarations InternalTree t (className, superclassName) _ =
      instanceTemplate InternalTree t
      (className, superclassName)
      (className ++ "MetaclassObject", superclassName ++ "MetaclassObject")
   isInstanceTree _ = False
   isClassTree _ = False
   isMetaclassTree _ = True


-- TEMPLATES

typeTemplate InternalTree t (className, "ObjCClass") (instanceName, _) =
   if isClassTree t then
      [
	 SigD classObjectName
	       (ConT $ instanceName ++ "ClassObject")
	 ,
	 FunD classObjectName [Clause [] (NormalB $ VarE "getClassObject") []]
      ]
   else
      []
   where
      classObjectName = "_" ++ instanceName ++ "_"
typeTemplate InternalTree _ _ _ = []
typeTemplate ExternalTree _ (_, _) (typeName, _) =
   [
      NewtypeD [] typeName []
      (
	 NormalC typeName
	 [
	    (NotStrict, AppT (ConT  "Ptr") (ConT typeName))
	 ]
      )
      ["Eq", "Show", "Typeable"]
   ]

classTemplate InternalTree _ (_, _) (instanceName, _) = []
classTemplate ExternalTree _ (className, superclassName) (instanceName, _) =
   [ClassD maybeSuperClassCxt className ["c"] []]
   where
      maybeSuperClassCxt
	 -- This class's superclass is its own class (i.e. if it's a
	 -- root class like NSObject or NSProxy), so we don't have an
	 -- additional constraint for this class
	 | superclassName == className = upcastAndDowncastDeclarations
	 -- Superclass isn't us (the usual); put in superclass constraints
	 | otherwise = upcastAndDowncastDeclarations ++
	    [AppT (ConT superclassName) (VarT "c")]
      upcastAndDowncastDeclarations =
	 [
	    AppT (AppT (ConT "Upcast") (VarT "c"))
		 (ConT instanceName)
	    ,
	    AppT (AppT (ConT "Downcast") (ConT instanceName))
		 (VarT "c")
	 ]

castClause arg1 arg2 =
   Clause [ConP arg1 [(VarP "p")]]
   (NormalB $ AppE (ConE arg2) (AppE (VarE "castPtr") (VarE "p"))) []
idClause = Clause [] (NormalB (VarE "id")) []


instanceTemplate InternalTree t (className, "ObjCClass") (instanceName, _) =
   if isClassTree t then
      [
	 InstanceD []
	 (AppT (ConT "ObjCClass") (ConT instanceName))
	 [
	    FunD "classObjectName"
	    [
	       Clause [WildP] (NormalB $ LitE (StringL className)) []
	    ]
	 ]
      ]
      ++ castingInstanceDeclarations instanceName "ObjCClassObject"
   else
      []
instanceTemplate InternalTree t (className, "ObjCMetaclass") (instanceName, _) =
   if isMetaclassTree t then
      [
	 InstanceD []
	 (AppT (ConT "ObjCMetaclass") (ConT instanceName))
	 [
	    FunD "metaclassObjectName"
	    [
	       Clause [WildP] (NormalB $ LitE (StringL className)) []
	    ]
	 ]
      ]
      ++
      castingInstanceDeclarations instanceName "ObjCMetaclassObject"
   else
      []

   
instanceTemplate InternalTree t (className, "ObjCInstance") (instanceName, _) =
   if isInstanceTree t then
      [
	 InstanceD []
	 (AppT (ConT "ObjCInstance") (ConT instanceName))
	 [
	    FunD "getClassObjectM" 
	    [
	       Clause [WildP]
	       (
		  NormalB
		  (
		     InfixE (Just (AppE (VarE "Mocha.Base:getClassObjectFromNameM")
		                      (LitE (StringL "Object"))))
		           (VarE "Mocha.Base:>>=^")
			   (Just (VarE "Mocha.Base:cast"))
		  )
	       )
	       []
	    ]
	 ]
      ]
      ++
      castingInstanceDeclarations instanceName "ObjCInstanceObject"
   else
      []
instanceTemplate InternalTree _ (className, "ObjCObject") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCObject") (ConT instanceName))
      []
   ]
   ++ castingInstanceDeclarations instanceName "ID"
instanceTemplate InternalTree _ (className, "ObjCTypeEncoding") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCTypeEncoding") (ConT instanceName))
      []
   ]
instanceTemplate InternalTree _ (className, "ObjCArgument") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCArgument") (ConT instanceName))
      [
	 FunD ("setArgument") [Clause [] (NormalB $ VarE "setObjectArgument") []]
      ]
   ]
instanceTemplate InternalTree _ (className, "ObjCObjectArgument") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCObjectArgument") (ConT instanceName))
      []
   ]
instanceTemplate InternalTree _ (className, "ObjCArguments") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCArguments") (ConT instanceName))
      [
	 FunD ("setArguments") [Clause [] (NormalB $ AppE
	    (VarE "setObjectArgument")
	    (LitE (IntegerL 1))
	 ) []]
      ]
   ]
instanceTemplate InternalTree _ (className, "ObjCMessageReply") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCMessageReply") (ConT instanceName))
      [
	 FunD ("sendMessage") [Clause [] (NormalB $ VarE "sendObjectMessage") []]
      ]
   ]
instanceTemplate InternalTree _ (className, "ObjCObjectMessageReply") (instanceName, _) =
   [
      InstanceD []
      (AppT (ConT "ObjCObjectMessageReply") (ConT instanceName))
      []
   ]
instanceTemplate InternalTree _ (_, _) (_, _) = []
instanceTemplate ExternalTree _
   (className, superclassName) (instanceName, superclassInstanceName) =
   [
      InstanceD []
      (AppT (ConT superclassName) (ConT instanceName))
      []
   ]
   ++
   castingInstanceDeclarations instanceName superclassInstanceName


--
-- Importing the actual class hierarchy
--

importObjCClassList :: [(ClassName, ClassName)] -> [Dec]
importObjCClassList classList =
   declareClassTree ExternalTree MetaclassTree classList
   ++
   declareClassTree ExternalTree ClassTree classList
   ++
   declareClassTree ExternalTree InstanceTree classList
   ++
   declareClassTree InternalTree MetaclassTree classList
   ++
   declareClassTree InternalTree ClassTree classList
   ++
   declareClassTree InternalTree InstanceTree classList


{-declareClassTree :: TreeDeclaration td
	         => TreeFlag -> td -> [(ClassName, ClassName)] -> [Dec]-}
declareClassTree InternalTree tree classList = concat
   (
      concat $ map declarationsForTypeClass typeClasses
   )
   where
      typeClasses =
	 [ "ObjCObject"
	 , "ObjCClass"
	 , "ObjCMetaclass"
	 , "ObjCInstance"
	 , "ObjCTypeEncoding"
	 , "ObjCArgument"
	 , "ObjCObjectArgument"
	 , "ObjCArguments"
	 , "ObjCMessageReply"
	 , "ObjCObjectMessageReply"
	 ]
      declarationsForTypeClass s =
	 [
	    typeDeclarations InternalTree tree (className, s) undefined ++
	    classDeclarations InternalTree tree (className, s) undefined ++
	    instanceFunctionDeclarations InternalTree tree (className, s) undefined
	    | (className, _) <- classList
	 ]

declareClassTree ExternalTree tree classList = 
   let
      emptyClassToSuperclassFM = emptyFM :: ClassToSuperclassFM
      classToSuperclassFM = addListToFM emptyClassToSuperclassFM classList
   in
      concat
         [ treeDeclarations ExternalTree tree (className, superclassName) classToSuperclassFM
           | (className, superclassName) <- classList ]
   where
      treeDeclarations ExternalTree tree (className, superclassName) classHierarchy =
	 concat (map (\declarations ->
	    declarations ExternalTree tree (className, superclassName) classHierarchy)
	    [typeDeclarations, classDeclarations])
	 ++
	 inheritanceDeclarations ExternalTree (instanceFunctionDeclarations ExternalTree tree)
	    (className, superclassName) classHierarchy


knownClasses =
   [ ("NSObject", "NSObject")
   , ("NSArray", "NSObject")
   , ("NSAutoreleasePool", "NSObject")
   , ("NSControl", "NSView")
   , ("NSDictionary", "NSObject")
   , ("NSImage", "NSObject")
   , ("NSImageView", "NSControl")
   , ("NSInvocation", "NSObject")
   , ("NSMethodSignature", "NSObject")
   , ("NSMovie", "NSObject")
   , ("NSMovieView", "NSView")
   , ("NSNumber", "NSValue")
   , ("NSResponder", "NSObject")
   , ("NSString", "NSObject")
   , ("NSText", "NSView")
   , ("NSTextField", "NSControl")
   , ("NSURL", "NSObject")
   , ("NSValue", "NSObject")
   , ("NSView", "NSResponder")
   --
   , ("HSProxy", "NSObject")
   ]

{-
knownClasses =
   [ ("NSObject", "NSObject")
   , ("NSArray", "NSObject")
   ]
-}

toNyah :: IO ()
toNyah = do
   writeFile
      "Nyah.hs"
      ("{-# OPTIONS -fglasgow-exts #-}\nmodule Nyah where\nimport Mocha.Base\nimport Foreign.Ptr\nimport Mocha.Messaging\nimport Mocha.TypeEncodings\n" ++
       wholeHierarchyS)

printWholeHierarchy :: IO ()
printWholeHierarchy = putStrLn $ wholeHierarchyS

wholeHierarchyS :: String
wholeHierarchyS =
   let
      foundationClassHierarchyDoc = map pprDec
         (importObjCClassList knownClasses)
   in
      show (vcat foundationClassHierarchyDoc)

wholeHierarchy =
   importObjCClassList knownClasses

wholeHierarchyQ = Q (return wholeHierarchy)

-- TODO: can we replace this with fix?
inheritanceDeclarations treeFlag declarations (className, superclassName)
   classHierarchy =
   inheritanceDeclarations' treeFlag classHierarchy declarations
      className (className, superclassName)

inheritanceDeclarations' treeFlag classHierarchy declarations originalClassName
   (currentClassName, currentSuperclassName) =
      (declarations (originalClassName, currentClassName) classHierarchy)
      ++
      case (currentClassName == currentSuperclassName) of
	 (True) -> []
	 (False) -> recurseMakeDeclarations
      where
	 recurseMakeDeclarations = inheritanceDeclarations' treeFlag
	    classHierarchy declarations originalClassName
	    (currentSuperclassName, (lookupWithDefaultFM classHierarchy
				     currentSuperclassName
				     currentSuperclassName))


castingInstanceDeclarations sub super =
   castClauses
   ++
   [
      InstanceD [] (AppT (AppT (ConT "Upcast")
	 (ConT sub)) (ConT super)) []
      ,
      InstanceD [] (AppT (AppT (ConT "Downcast")
	 (ConT super)) (ConT sub)) []
   ]
   where
      castClauses =
	 if sub /= super then
	 [
	    InstanceD [] (AppT (AppT (ConT "Cast")
	       (ConT ( sub))) (ConT ( super)))
	    [FunD ("cast") [castClause sub super]]
	    ,
	    InstanceD [] (AppT (AppT (ConT ( "Cast"))
	       (ConT ( super))) (ConT ( sub)))
	    [FunD ("cast") [castClause super sub]]
	 ]
	 else
	 [
	    InstanceD [] (AppT (AppT (ConT ( "Cast"))
	       (ConT ( sub))) (ConT ( super)))
	    [FunD ("cast") [idClause]]
	 ]



-- thanks to Derek Elkins for this
tappsArr :: [Type] -> Type
tappsArr = foldr1 (\x y -> AppT (AppT  ArrowT x) y)


{-

pprDecLn x = text "\n" <+> pprDec x
pprDecs decs = putStrLn (show $ vcat $ map pprDec decs)

--------------------------------------------------------------
-- A pretty printer (due to Ian Lynagh)
-- (with modifications by Andre Pang
--------------------------------------------------------------

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, noPrec :: Precedence
appPrec = 2	-- Argument of a function application
opPrec  = 1	-- Argument of an infix operator
noPrec  = 0	-- Others

parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d

------------------------------
pprExp :: Exp -> Doc
pprExp = pprExpI noPrec

pprExpI :: Precedence -> Exp -> Doc
pprExpI _ (VarP v)     = text variableNameWithNoColons
   where
      variableNameWithNoColons = case break (== ':') v of
	 (v, "") -> v
	 (_, v) -> tail v
pprExpI _ (ConE c)     = text c
pprExpI i (LitE l)     = pprLit i l
pprExpI i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExpI opPrec e1
                                        <+> pprExpI appPrec e2
pprExpI i (InfixE (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExpI opPrec e1
                          <+> pprExp op
                          <+> pprExpI opPrec e2
pprExpI _ (InfixE me1 op me2) = parens $ pprMaybeExp noPrec me1
                                    <+> pprExp op
                                    <+> pprMaybeExp noPrec me2
pprExpI i (LamE ps e) = parensIf (i > noPrec) $ char '\\'
                                       <> hsep (map pprPat ps)
                                      <+> text "->" <+> pprExp e
pprExpI _ (Tup es) = parens $ sep $ punctuate comma $ map pprExp es
-- Nesting in Cond is to avoid potential problems in do statments
pprExpI i (Cond guard true false)
 = parensIf (i > noPrec) $ sep [text "if" <+> pprExp guard,
                           nest 1 $ text "then" <+> pprExp true,
                           nest 1 $ text "else" <+> pprExp false]
pprExpI i (Let ds e) = parensIf (i > noPrec) $ text "let" <+> vcat (map pprDec ds)
                                       $$ text " in" <+> pprExp e
pprExpI i (Case e ms)
 = parensIf (i > noPrec) $ text "case" <+> pprExp e <+> text "of"
                   $$ nest nestDepth (vcat $ map pprMatch ms)
pprExpI i (Do ss) = parensIf (i > noPrec) $ text "do"
                                   <+> vcat (map pprStatement ss)
pprExpI _ (Comp []) = error "Can't happen: pprExpI (Comp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExpI _ (Comp ss) = text "[" <> pprStatement s
                  <+> text "|"
                  <+> (sep $ punctuate comma $ map pprStatement ss')
                   <> text "]"
  where s = last ss
        ss' = init ss
pprExpI _ (ArithSeq d) = pprDotDot d
pprExpI _ (ListExp es) = brackets $ sep $ punctuate comma $ map pprExp es
	-- 5 :: Int :: Int will break, but that's a silly thing to do anyway
pprExpI i (SigExp e t)
 = parensIf (i > noPrec) $ pprExp e <+> text "::" <+> pprTyp t
pprExpI _ (RecCon nm fs) = text nm <> braces (pprFields fs)
pprExpI _ (RecUpd e fs) = pprExpI appPrec e <> braces (pprFields fs)

pprFields :: [(String,Exp)] -> Doc
pprFields = sep . punctuate comma
          . map (\(s,e) -> text s <+> equals <+> pprExp e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExpI i e

------------------------------
pprStatement :: Statement -> Doc
pprStatement (BindSt p e) = pprPat p <+> text "<-" <+> pprExp e
pprStatement (LetSt ds) = text "let" <+> vcat (map pprDec ds)
pprStatement (NoBindSt e) = pprExp e
pprStatement (ParSt sss) = sep $ punctuate (text "|")
                         $ map (sep . punctuate comma . map pprStatement) sss

------------------------------
pprMatch :: Match -> Doc
pprMatch (Match p rhs ds) = pprPat p <+> pprRhs False rhs
                         $$ where_clause ds

------------------------------
pprRhs :: Bool -> RightHandSide -> Doc
pprRhs eq (Guarded xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (lhs, rhs) = text "|" <+> pprExp lhs <+> eqd <+> pprExp rhs
pprRhs eq (NormalB e) = (if eq then text "=" else text "->")
                   <+> pprExp e

------------------------------
pprLit :: Precedence -> LitE -> Doc
pprLit i (IntPrim x)    = parensIf (i > noPrec && x < 0)
                                   (integer x <> char '#')
pprLit i (FloatPrim x)  = parensIf (i > noPrec && x < 0)
                                   (float (fromRational x) <> char '#')
pprLit i (DoublePrim x) = parensIf (i > noPrec && x < 0)
                                   (double (fromRational x) <> text "##")
pprLit i (Integer x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (Char c)       = text (show c)
pprLit _ (StringL s)     = text (show s)
pprLit i (Rational rat) = parensIf (i > noPrec) $ rational rat

------------------------------
pprPat :: Pat -> Doc
pprPat = pprPatI noPrec

pprPatI :: Precedence -> Pat -> Doc
pprPatI i (Plit l)     = pprLit i l
pprPatI _ (VarP v)     = text v
pprPatI _ (Ptup ps)    = parens $ sep $ punctuate comma $ map pprPat ps
--pprPatI i (ConP s ps)  = parensIf (i > noPrec) $ text s <+> sep (map (pprPatI appPrec) ps)
pprPatI _ (ConP s ps)  = parens $ text s <+> sep (map (pprPatI appPrec) ps)
pprPatI i (Ptilde p)   = parensIf (i > noPrec) $ pprPatI appPrec p
pprPatI i (Paspat v p) = parensIf (i > noPrec) $ text v <> text "@" <> pprPatI appPrec p
pprPatI _ WildP = text "_"
pprPatI _ (Prec nm fs)
 = parens $     text nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> text s <+> equals <+> pprPat p) fs)

------------------------------
pprDec :: Dec -> Doc
pprDec (FunD f cs)   = vcat $ map (\c -> text f <+> pprClause c) cs
pprDec (Val p r ds) = pprPat p <+> pprRhs True r
                      $$ where_clause ds
pprDec (TySyn t xs rhs) = text "type" <+> text t <+> hsep (map text xs) 
				<+> text "=" <+> pprTyp rhs
pprDec (Data cxt t xs cs ds) = text "data"
                       <+> pprCxt cxt
                       <+> text t <+> hsep (map text xs)
                       <+> sep (pref $ map pprCon cs)
                        $$ if null ds
                           then empty
                           else nest nestDepth
                              $ text "deriving"
                            <+> parens (hsep $ punctuate comma $ map text ds)
    where pref :: [Doc] -> [Doc]
          pref [] = [char '='] -- Can't happen in H98
          pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
pprDec (NewtypeD cxt t xs c ds) = text "newtype"
                       <+> pprCxt cxt
                       <+> text t <+> hsep (map text xs)
		       <+> text "="
                       <+> pprCon c
                        $$ if null ds
                           then empty
                           else nest nestDepth
                              $ text "deriving"
                            <+> parens (hsep $ punctuate comma $ map text ds)
    where pref :: [Doc] -> [Doc]
          pref [] = [char '='] -- Can't happen in H98
          pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
pprDec (ClassD cxt c xs ds) = text "class" <+> pprCxt cxt
                         <+> text c <+> hsep (map text xs)
                          $$ where_clause ds
pprDec (InstanceD cxt i ds) = text "instance" <+> pprCxt cxt <+> pprTyp i
                          $$ where_clause ds
pprDec (SigD f t) = text f <+> text "::" <+> pprTyp t
pprDec (Foreign f) = pprForeign f

------------------------------
pprForeign :: Foreign -> Doc
pprForeign (Import callconv safety impent as typ) = text "foreign import"
                                                <+> showtextl callconv
                                                <+> showtextl safety
                                                <+> text (show impent)
                                                <+> text as
                                                <+> text "::" <+> pprTyp typ

------------------------------
pprClause :: Clause -> Doc
pprClause (Clause ps rhs ds) = hsep (map pprPat ps) <+> pprRhs True rhs
                            $$ where_clause ds

------------------------------
pprCon :: ConE -> Doc
pprCon (NormalC c sts) = text c <+> hsep (map pprStrictTyp sts)
pprCon (RecConstr c vsts) = text c
                        <+> char '{'
                         <> hsep (punctuate comma $ map pprVarStrictTyp vsts)
                         <> char '}'
pprCon (InfixEConstr st1 c st2) = pprStrictTyp st1
                             <+> text c
                             <+> pprStrictTyp st2

------------------------------
pprVarStrictTyp :: (String, Strictness, Type) -> Doc
pprVarStrictTyp (v, str, t) = text v <+> text "::" <+> text str' <> pprTyp t
    where str' = case str of
                     Strict -> "!"
                     NotStrict -> ""

------------------------------
pprStrictTyp :: (Strictness, Type) -> Doc
pprStrictTyp (Strict, t) = char '!' <> pprTyp t
pprStrictTyp (NotStrict, t) = parens (pprTyp t)

------------------------------
pprParendTyp :: Type -> Doc
pprParendTyp (VarT s) = text s
pprParendTyp (ConT t) = pprTcon t
pprParendTyp other    = parens (pprTyp other)

pprTyp :: Type -> Doc
pprTyp (TForall tvars ctxt ty) = 
  text "forall" <+> hsep (map text tvars) <+> text "." <+> 
  ctxtDoc <+> pprTyp ty
  where
    ctxtDoc | null ctxt = empty
	    | otherwise = parens (sep (punctuate comma (map pprTyp ctxt))) <+>
			  text "=>"
pprTyp ty		       = pprTyApp (split ty)

pprTyApp (ConT Arrow, [arg1,arg2])
  = sep [pprTyp arg1 <+> text "->", pprTyp arg2]

pprTyApp (ConT List, [arg])
  = brackets (pprTyp arg)

pprTyApp (ConT (Tuple n), args)
  | length args == n
  = parens (sep (punctuate comma (map pprTyp args)))

pprTyApp (fun, args)
  = pprParendTyp fun <+> sep (map pprParendTyp args)

pprTcon :: Tag -> Doc
pprTcon (Tuple 0)    = text "()"
pprTcon (Tuple n)    = parens (hcat (replicate (n-1) comma))
pprTcon Arrow	     = parens (text "->")
pprTcon List	     = text "[]"
pprTcon (s) = text s

split :: Type -> (Type, [Type])	-- Split into function and args
split t = go t []
	where
	  go (AppT t1 t2) args = go t1 (t2:args)
	  go ty		  args = (ty, args)

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = pprTyp t <+> text "=>"
pprCxt ts = parens (hsep $ punctuate comma $ map pprTyp ts) <+> text "=>"

------------------------------
pprDotDot :: DotDot -> Doc
pprDotDot = brackets . pprDotDotI

pprDotDotI :: DotDot -> Doc
pprDotDotI (From e) = pprExp e <> text ".."
pprDotDotI (FromThen e1 e2) = pprExp e1 <> text ","
                           <> pprExp e2 <> text ".."
pprDotDotI (FromTo e1 e2) = pprExp e1 <> text ".." <> pprExp e2
pprDotDotI (FromThenTo e1 e2 e3) = pprExp e1 <> text ","
                                <> pprExp e2 <> text ".."
                                <> pprExp e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = text "   where" <+> vcat (map pprDec ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show



-}



--------------------------------------------------------------
--         A pretty printer (due to Ian Lynagh)
--------------------------------------------------------------

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, noPrec :: Precedence
appPrec = 2    -- Argument of a function application
opPrec  = 1    -- Argument of an infix operator
noPrec  = 0    -- Others

parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d

------------------------------
pprExp :: Exp -> Doc
pprExp = pprExpI noPrec

pprExpI :: Precedence -> Exp -> Doc
pprExpI _ (VarE v)     = text variableNameWithNoColons
   where
      variableNameWithNoColons = case break (== ':') v of
	 (v, "") -> v
	 (_, v) -> tail v
pprExpI _ (ConE c)     = text c
pprExpI i (LitE l)     = pprLit i l
pprExpI i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExpI opPrec e1
                                               <+> pprExpI appPrec e2
pprExpI i (InfixE (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExpI opPrec e1
                        <+> pprExp op
                        <+> pprExpI opPrec e2
pprExpI _ (InfixE me1 op me2) = parens $ pprMaybeExp noPrec me1
                                     <+> pprExp op
                                     <+> pprMaybeExp noPrec me2
pprExpI i (LamE ps e) = parensIf (i > noPrec) $ char '\\'
                                             <> hsep (map pprPat ps)
                                            <+> text "->" <+> pprExp e
pprExpI _ (TupE es) = parens $ sep $ punctuate comma $ map pprExp es
-- Nesting in Cond is to avoid potential problems in do statments
pprExpI i (CondE guard true false)
 = parensIf (i > noPrec) $ sep [text "if" <+> pprExp guard,
                           nest 1 $ text "then" <+> pprExp true,
                           nest 1 $ text "else" <+> pprExp false]
pprExpI i (LetE ds e)
    = parensIf (i > noPrec) $ text "let" <+> vcat (map pprDec ds)
                           $$ text " in" <+> pprExp e
pprExpI i (CaseE e ms)
 = parensIf (i > noPrec) $ text "case" <+> pprExp e <+> text "of"
                        $$ nest nestDepth (vcat $ map pprMatch ms)
pprExpI i (DoE ss) = parensIf (i > noPrec) $ text "do"
                                         <+> vcat (map pprStmt ss)
pprExpI _ (CompE []) = error "Can't happen: pprExpI (CompExp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExpI _ (CompE ss) = text "[" <> pprStmt s
                   <+> text "|"
                   <+> (sep $ punctuate comma $ map pprStmt ss')
                    <> text "]"
  where s = last ss
        ss' = init ss
pprExpI _ (ArithSeqE d) = pprRange d
pprExpI _ (ListE es) = brackets $ sep $ punctuate comma $ map pprExp es
    -- 5 :: Int :: Int will break, but that's a silly thing to do anyway
pprExpI i (SigE e t)
 = parensIf (i > noPrec) $ pprExp e <+> text "::" <+> pprType t
pprExpI _ (RecConE nm fs) = text nm <> braces (pprFields fs)
pprExpI _ (RecUpdE e fs) = pprExpI appPrec e <> braces (pprFields fs)

pprFields :: [(String,Exp)] -> Doc
pprFields = sep . punctuate comma
          . map (\(s,e) -> text s <+> equals <+> pprExp e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExpI i e

------------------------------
pprStmt :: Stmt -> Doc
pprStmt (BindS p e) = pprPat p <+> text "<-" <+> pprExp e
pprStmt (LetS ds) = text "let" <+> vcat (map pprDec ds)
pprStmt (NoBindS e) = pprExp e
pprStmt (ParS sss) = sep $ punctuate (text "|")
                      $ map (sep . punctuate comma . map pprStmt) sss

------------------------------
pprMatch :: Match -> Doc
pprMatch (Match p rhs ds) = pprPat p <+> pprBody False rhs
                         $$ where_clause ds

------------------------------
pprBody :: Bool -> Body -> Doc
pprBody eq (GuardedB xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (lhs, rhs) = text "|" <+> pprExp lhs <+> eqd <+> pprExp rhs
pprBody eq (NormalB e) = (if eq then text "=" else text "->")
                        <+> pprExp e

------------------------------
pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrimL x)    = parensIf (i > noPrec && x < 0)
                                    (integer x <> char '#')
pprLit i (FloatPrimL x)  = parensIf (i > noPrec && x < 0)
                                    (float (fromRational x) <> char '#')
pprLit i (DoublePrimL x) = parensIf (i > noPrec && x < 0)
                                    (double (fromRational x) <> text "##")
pprLit i (IntegerL x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (CharL c)       = text (show c)
pprLit _ (StringL s)     = text (show s)
pprLit i (RationalL rat) = parensIf (i > noPrec) $ rational rat

------------------------------
pprPat :: Pat -> Doc
pprPat = pprPatI noPrec

pprPatI :: Precedence -> Pat -> Doc
pprPatI i (LitP l)     = pprLit i l
pprPatI _ (VarP v)     = text v
pprPatI _ (TupP ps)    = parens $ sep $ punctuate comma $ map pprPat ps
--pprPatI i (ConP s ps)  = parensIf (i > noPrec) $ text s <+> sep (map (pprPatI appPrec) ps)
pprPatI i (ConP s ps)  = parens $ text s <+> sep (map (pprPatI appPrec) ps)
pprPatI i (TildeP p)   = parensIf (i > noPrec) $ pprPatI appPrec p
pprPatI i (AsP v p)    = parensIf (i > noPrec) $ text v <> text "@"
                                                        <> pprPatI appPrec p
pprPatI _ WildP        = text "_"
pprPatI _ (RecP nm fs)
 = parens $     text nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> text s <+> equals <+> pprPat p) fs)
pprPatI _ (ListP ps) = brackets $ sep $ punctuate comma $ map pprPat ps

------------------------------
pprDec :: Dec -> Doc
pprDec (FunD f cs)   = vcat $ map (\c -> text f <+> pprClause c) cs
pprDec (ValD p r ds) = pprPat p <+> pprBody True r
                    $$ where_clause ds
pprDec (TySynD t xs rhs) = text "type" <+> text t <+> hsep (map text xs) 
                       <+> text "=" <+> pprType rhs
pprDec (DataD ctxt t xs cs decs)
    = text "data"
  <+> pprCxt ctxt
  <+> text t <+> hsep (map text xs)
  <+> sep (pref $ map pprCon cs)
   $$ if null decs
      then empty
      else nest nestDepth
         $ text "deriving"
       <+> parens (hsep $ punctuate comma $ map text decs)
    where pref :: [Doc] -> [Doc]
          pref [] = [char '='] -- Can't happen in H98
          pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
pprDec (NewtypeD ctxt t xs c decs)
    = text "newtype"
  <+> pprCxt ctxt
  <+> text t <+> hsep (map text xs)
  <+> char '=' <+> pprCon c
   $$ if null decs
      then empty
      else nest nestDepth
         $ text "deriving"
       <+> parens (hsep $ punctuate comma $ map text decs)
pprDec (ClassD ctxt c xs ds) = text "class" <+> pprCxt ctxt
                           <+> text c <+> hsep (map text xs)
                            $$ where_clause ds
pprDec (InstanceD ctxt i ds) = text "instance" <+> pprCxt ctxt <+> pprType i
                            $$ where_clause ds
pprDec (SigD f t) = text f <+> text "::" <+> pprType t
pprDec (ForeignD f) = pprForeign f

------------------------------
pprForeign :: Foreign -> Doc
pprForeign (ImportF callconv safety impent as typ)
    = text "foreign import"
  <+> showtextl callconv
  <+> showtextl safety
  <+> text (show impent)
  <+> text as
  <+> text "::" <+> pprType typ
pprForeign (ExportF callconv        expent as typ)
    = text "foreign export"
  <+> showtextl callconv
  <+> text (show expent)
  <+> text as
  <+> text "::" <+> pprType typ

------------------------------
pprClause :: Clause -> Doc
pprClause (Clause ps rhs ds) = hsep (map pprPat ps) <+> pprBody True rhs
                            $$ where_clause ds

------------------------------
pprCon :: Con -> Doc
pprCon (NormalC c sts) = text c <+> hsep (map pprStrictType sts)
pprCon (RecC c vsts) = text c
                   <+> char '{'
                    <> hsep (punctuate comma $ map pprVarStrictType vsts)
                    <> char '}'
pprCon (InfixC st1 c st2) = pprStrictType st1
                        <+> text c
                        <+> pprStrictType st2

------------------------------
pprVarStrictType :: (String, Strict, Type) -> Doc
pprVarStrictType (v, str, t) = text v <+> text "::" <+> pprStrictType (str, t)

------------------------------
pprStrictType :: (Strict, Type) -> Doc
pprStrictType (IsStrict, t) = char '!' <> pprType t
pprStrictType (NotStrict, t) = parens (pprType t)

------------------------------
pprParendType :: Type -> Doc
pprParendType (VarT v)   = text v
pprParendType (ConT c)   = text c
pprParendType (TupleT 0) = text "()"
pprParendType (TupleT n) = parens (hcat (replicate (n-1) comma))
pprParendType ArrowT     = parens (text "->")
pprParendType ListT      = text "[]"
pprParendType other      = parens (pprType other)

pprType :: Type -> Doc
pprType (ForallT tvars ctxt ty) = 
  text "forall" <+> hsep (map text tvars) <+> text "." <+> 
  ctxtDoc <+> pprType ty
  where
    ctxtDoc | null ctxt = empty
        | otherwise = parens (sep (punctuate comma (map pprType ctxt))) <+>
              text "=>"
pprType ty               = pprTyApp (split ty)

pprTyApp :: (Type, [Type]) -> Doc
pprTyApp (ArrowT, [arg1,arg2])
  = sep [pprType arg1 <+> text "->", pprType arg2]

pprTyApp (ListT, [arg]) = brackets (pprType arg)

pprTyApp (TupleT n, args)
 | length args == n
    = parens (sep (punctuate comma (map pprType args)))

pprTyApp (fun, args)
  = pprParendType fun <+> sep (map pprParendType args)

split :: Type -> (Type, [Type])    -- Split into function and args
split t = go t []
    where
      go (AppT t1 t2) args = go t1 (t2:args)
      go ty           args = (ty, args)

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = pprType t <+> text "=>"
pprCxt ts = parens (hsep $ punctuate comma $ map pprType ts) <+> text "=>"

------------------------------
pprRange :: Range -> Doc
pprRange = brackets . pprRangeI

pprRangeI :: Range -> Doc
pprRangeI (FromR e) = pprExp e <> text ".."
pprRangeI (FromThenR e1 e2) = pprExp e1 <> text ","
                           <> pprExp e2 <> text ".."
pprRangeI (FromToR e1 e2) = pprExp e1 <> text ".." <> pprExp e2
pprRangeI (FromThenToR e1 e2 e3) = pprExp e1 <> text ","
                                <> pprExp e2 <> text ".."
                                <> pprExp e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = text (spaces ++ "where") <+> vcat (map pprDec ds)
   where
      spaces = replicate nestDepth ' '

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show

