module Mocha.MessagingTemplates where

import Language.Haskell.THSyntax

import Mocha.THLibrary

declareObjCArgumentsForTuples lengths = returnQ (map mkTupleArgs lengths)

mkTupleArgs numTuples = InstanceD
   (map mkCxtN [1..numTuples])
   (AppT (ConT "ObjCArguments") 
         (mkTup [VarT ("a" ++ show n) | n <- [1..numTuples]])
   ) [declarations]
   where
      mkCxtN n = AppT (ConT "ObjCArgument") (VarT $ "a" ++ show n)
      declarations = FunD "setArguments" [Clause
         [VarP "expr",
	  TupP [ VarP $ "a" ++ show n | n <- [1..numTuples] ]] (NormalB 
	 setArgumentExpression) []]
      setArgumentExpression = DoE setArgumentStatements
      setArgumentStatements = 
	 [ NoBindS $ AppE (AppE (AppE (VarE "setArgument")
	        (LitE $ IntegerL n)) (VarE "expr")) (VarE $ "a" ++ show n)
	 | n <- [1..numTuples] ]

declareTypeEncodingForTuples lengths = returnQ (map mkTupleEncodings lengths)

mkTupleEncodings numTuples = InstanceD
   (map mkCxtN [1..numTuples])
   (AppT (ConT "ObjCTypeEncoding") 
         (mkTup [VarT ("a" ++ show n) | n <- [1..numTuples]])
   ) [declarations]
   where
      mkCxtN n = AppT (ConT "ObjCTypeEncoding") (VarT $ "a" ++ show n)
      declarations = FunD "typeEncoding" [Clause
         [TupP [ VarP $ "a" ++ show n | n <- [1..numTuples] ]] (NormalB 
	 typeEncodingExpression) []]
      typeEncodingExpression = infixList (VarE "GHC.Base:++") [ AppE
                                    (VarE "typeEncoding")
				    (VarE $ "a" ++ show n)
                                 | n <- [1..numTuples] ]

{-
InfixE (Just (LitE (StringL "Foo")))
       (VarE "GHC.Base:++")
       (Just (InfixE (Just (LitE (StringL "Bar"))) (VarE "GHC.Base:++") (Just (LitE (StringL "Baz")))))
-}

infixList :: Exp -> [Exp] -> Exp
infixList s (a:b:[]) = InfixE (Just a) s (Just b)
infixList s (a:x) = InfixE (Just a) s (Just $ infixList s x)

