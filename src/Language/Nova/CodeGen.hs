{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nova.CodeGen where

----------------------------------------------------------------------------------------------------
import Data.Loc (noLoc)
import qualified Data.Text as T
import qualified Language.C.Quote as C
import qualified Language.C.Quote.C as C
import Text.PrettyPrint.Mainland (Doc, ppr)
----------------------------------------------------------------------------------------------------
import Language.Nova.Lexer
import Language.Nova.Parser
----------------------------------------------------------------------------------------------------

codegen :: [Decl] -> Doc
codegen decls =
    let
      includes = [ include | IncludeD include <- decls ]
      funs     = [ fun | FunD fun <- decls ]

      c :: [C.Definition]
      c = map (\i -> C.EscDef ("#include \"" ++ T.unpack i ++ "\"") noLoc) includes ++
          map (\f -> codegenFun f) funs
    in
      ppr c

codegenFun :: FunDecl -> C.Definition
codegenFun (FunDecl fname params ret_ty body) =
    [C.cedecl| $spec:(codegenTy ret_ty) $id:(codegenIdent fname) ($params:(map codegenParam params)) {
                   $items:(map codegenStmt body)
               }
             |]

codegenTy :: Type -> C.DeclSpec
codegenTy (Type (Ident ty)) = C.DeclSpec [] [] (C.Tnamed (C.Id (T.unpack ty) noLoc) [] noLoc) noLoc

codegenIdent :: Ident -> C.Id
codegenIdent (Ident i) = C.Id (T.unpack i) noLoc

codegenParam :: (Ident, Type) -> C.Param
codegenParam (i, ty) = [C.cparam| $spec:(codegenTy ty) $id:(codegenIdent i) |]

codegenStmt :: Stmt -> C.BlockItem
codegenStmt (FunCallS (FunCall e es)) = [C.citem| $(codegenExpr e)($args:(map codegenExpr es)); |]
codegenStmt (IfS cond0 body0 elseifs else0) =
    C.BlockStm (C.If (codegenExpr cond0) (codegenStmts body0) (go elseifs (fmap codegenStmts else0)) noLoc)
  where
    go :: [(Expr, [Stmt])] -> Maybe C.Stm -> Maybe C.Stm
    go [] else_ = else_
    go ((cond, body) : rest) else_ = Just (C.If (codegenExpr cond) (codegenStmts body) (go rest else_) noLoc)

codegenStmt (ReturnS Nothing) = [C.citem| return; |]
codegenStmt (ReturnS (Just e)) = [C.citem| return $(codegenExpr e); |]

codegenStmts :: [Stmt] -> C.Stm
codegenStmts stmts = C.Block (map codegenStmt stmts) noLoc

codegenExpr :: Expr -> C.Exp
codegenExpr (FunCallE (FunCall e es)) = [C.cexp| $(codegenExpr e)($args:(map codegenExpr es)) |]
codegenExpr (StringE str) = C.Const (C.StringConst [show str] "" noLoc) noLoc
codegenExpr (IdentE i) = C.Var (codegenIdent i) noLoc
codegenExpr (NumberE (Number num (Just num_ty))) = C.Cast (codegenNumTy num_ty) (C.AntiExp (T.unpack num) noLoc) noLoc
codegenExpr (NumberE (Number num Nothing)) = C.AntiExp (T.unpack num) noLoc
codegenExpr (SelectE e s) = C.Member (codegenExpr e) (codegenIdent s) noLoc
codegenExpr (IndexE e1 e2) = C.Index (codegenExpr e1) (codegenExpr e2) noLoc
codegenExpr (Unop op e) = C.UnOp (codegenUnOp op) (codegenExpr e) noLoc
codegenExpr (Binop op e1 e2) = C.BinOp (codegenBinOp op) (codegenExpr e1) (codegenExpr e2) noLoc

codegenNumTy :: NumType -> C.Type
codegenNumTy t = flip C.AntiType noLoc $ case t of
    I8  -> "int8_t"
    I16 -> "int16_t"
    I32 -> "int32_t"
    I64 -> "int64_t"
    U8  -> "uint8_t"
    U16 -> "uint16_t"
    U32 -> "uint32_t"
    U64 -> "uint64_t"
    F32 -> "float"
    F64 -> "double"

codegenUnOp :: Unop -> C.UnOp
codegenUnOp op = case op of
    Neg -> C.Negate
    Not -> C.Lnot
    AddrOf -> C.AddrOf
    Deref -> C.Deref

codegenBinOp :: Binop -> C.BinOp
codegenBinOp op = case op of
    Add -> C.Add
    Sub -> C.Sub
    Mul -> C.Mul
    Div -> C.Div
    Rem -> C.Mod -- C doesn't have modulus, this is actually remainder
