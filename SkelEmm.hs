-- Haskell module generated by the BNF converter

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelEmm where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsEmm

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsEmm.Ident -> Result
transIdent x = case x of
  AbsEmm.Ident string -> failure x

transProgram :: AbsEmm.Program -> Result
transProgram x = case x of
  AbsEmm.Program topdefs -> failure x

transTopDef :: AbsEmm.TopDef -> Result
transTopDef x = case x of
  AbsEmm.TopDef decl -> failure x

transDecl :: AbsEmm.Decl -> Result
transDecl x = case x of
  AbsEmm.Decl type_ items -> failure x
  AbsEmm.FnDecl type_ ident args block -> failure x

transArg :: AbsEmm.Arg -> Result
transArg x = case x of
  AbsEmm.Arg type_ ident -> failure x
  AbsEmm.ArgRef type_ ident -> failure x

transItem :: AbsEmm.Item -> Result
transItem x = case x of
  AbsEmm.NoInit ident -> failure x
  AbsEmm.Init ident expr -> failure x
  AbsEmm.Array ident expr -> failure x

transBlock :: AbsEmm.Block -> Result
transBlock x = case x of
  AbsEmm.Block stmts -> failure x

transStmt :: AbsEmm.Stmt -> Result
transStmt x = case x of
  AbsEmm.Empty -> failure x
  AbsEmm.BStmt block -> failure x
  AbsEmm.SDecl decl -> failure x
  AbsEmm.Ass ident expr -> failure x
  AbsEmm.AssArr ident expr1 expr2 -> failure x
  AbsEmm.Incr ident -> failure x
  AbsEmm.IncrArr ident expr -> failure x
  AbsEmm.Decr ident -> failure x
  AbsEmm.DecrArr ident expr -> failure x
  AbsEmm.Ret expr -> failure x
  AbsEmm.VRet -> failure x
  AbsEmm.Cond expr stmt -> failure x
  AbsEmm.CondElse expr stmt1 stmt2 -> failure x
  AbsEmm.While expr stmt -> failure x
  AbsEmm.Brk -> failure x
  AbsEmm.Cnt -> failure x
  AbsEmm.SExp expr -> failure x

transType :: AbsEmm.Type -> Result
transType x = case x of
  AbsEmm.Int -> failure x
  AbsEmm.Str -> failure x
  AbsEmm.Bool -> failure x
  AbsEmm.Void -> failure x
  AbsEmm.Fun type_ types -> failure x

transExpr :: AbsEmm.Expr -> Result
transExpr x = case x of
  AbsEmm.EArr ident expr -> failure x
  AbsEmm.EVar ident -> failure x
  AbsEmm.ELitInt integer -> failure x
  AbsEmm.ELitTrue -> failure x
  AbsEmm.ELitFalse -> failure x
  AbsEmm.EApp ident exprs -> failure x
  AbsEmm.EString string -> failure x
  AbsEmm.Neg expr -> failure x
  AbsEmm.Not expr -> failure x
  AbsEmm.EMul expr1 mulop expr2 -> failure x
  AbsEmm.EAdd expr1 addop expr2 -> failure x
  AbsEmm.ERel expr1 relop expr2 -> failure x
  AbsEmm.EAnd expr1 expr2 -> failure x
  AbsEmm.EOr expr1 expr2 -> failure x

transAddOp :: AbsEmm.AddOp -> Result
transAddOp x = case x of
  AbsEmm.Plus -> failure x
  AbsEmm.Minus -> failure x

transMulOp :: AbsEmm.MulOp -> Result
transMulOp x = case x of
  AbsEmm.Times -> failure x
  AbsEmm.Div -> failure x
  AbsEmm.Mod -> failure x

transRelOp :: AbsEmm.RelOp -> Result
transRelOp x = case x of
  AbsEmm.LTH -> failure x
  AbsEmm.LE -> failure x
  AbsEmm.GTH -> failure x
  AbsEmm.GE -> failure x
  AbsEmm.EQU -> failure x
  AbsEmm.NE -> failure x