module Language.Nova.CodeGen where

----------------------------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Language.C.Data.Ident as C
import Language.C.Data.Node
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Text.PrettyPrint
----------------------------------------------------------------------------------------------------
import Language.Nova.Lexer
import Language.Nova.Parser
----------------------------------------------------------------------------------------------------

codegen :: [Decl] -> Doc
codegen decls =
    let
      includes = [ include | IncludeD include <- decls ]
      funs     = [ fun | FunD fun <- decls ]
      c_transl = CTranslUnit (map (CFDefExt . codegenFun) funs) undefNode
    in
      vcat (map codegenInclude includes) $$
      pretty c_transl

codegenInclude :: T.Text -> Doc
codegenInclude t = text "#include \"" <> text (T.unpack t) <> "\""

codegenFun :: FunDecl -> CFunDef
codegenFun (FunDecl fname params ret_ty body) =
    CFunDef [CTypeSpec (codegenType ret_ty)]
            (CDeclr (Just (toCIdent fname))
                    [CFunDeclr (Right (map codegenParam params, False)) [] undefNode]
                    Nothing
                    [] undefNode)
            []
            (CCompound [] (map codegenStmt body) undefNode)
            undefNode

codegenParam :: (Ident, Type) -> CDecl
codegenParam (param, param_ty) =
    -- TODO: This is broken .. I have no idea where to put param type here
    CDecl [] [(Just (CDeclr (Just (toCIdent param)) [] Nothing [] undefNode), Nothing, Nothing)] undefNode

codegenType :: Type -> CTypeSpec
codegenType (Type (Ident t)) = CTypeDef (C.Ident (T.unpack t) 0 undefNode) undefNode
codegenType TypeApp{} = error "codegenType TypeApp"

codegenStmt :: Stmt -> CBlockItem
codegenStmt (FunCallS (FunCall e es)) =
    CBlockStmt (CExpr (Just (CCall (codegenExpr e) (map codegenExpr es) undefNode)) undefNode)

codegenExpr :: Expr -> CExpr
codegenExpr (FunCallE (FunCall e es)) = CCall (codegenExpr e) (map codegenExpr es) undefNode
codegenExpr (StringE str) = CConst (CStrConst (CString (T.unpack str) False) undefNode)
codegenExpr (IdentE t) = CVar (toCIdent t) undefNode

toCIdent :: Ident -> C.Ident
toCIdent (Ident t) = C.Ident (T.unpack t) 0 undefNode
