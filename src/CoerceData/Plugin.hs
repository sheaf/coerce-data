{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module CoerceData.Plugin
  ( plugin )
  where

-- base
import Prelude
  hiding ( cos )
import Control.Monad
  ( unless, zipWithM )
import Data.Bifunctor
  ( Bifunctor(..) )
import Data.Maybe
  ( catMaybes, maybeToList )

-- ghc
import GHC.Core.DataCon
  ( dataConRepStrictness )
import GHC.Plugins
  -- Plugin stuff
  ( Plugin(..), defaultPlugin, purePlugin
  -- Typechecker types
  , ThetaType, TyCoVar
  -- Typechecker functions
  , dataConInstSig, dataConInstUnivs, tyConDataCons_maybe, isNewTyCon
  , typeKind, tyConResKind, isConstraintLikeKind
  -- Substitutions
  , Subst, emptySubst, substTys
  -- Other utility functions
  , partitionWithM
  )
import GHC.Tc.Instance.Family
  ( tcGetFamInstEnvs, tcLookupDataFamInst )
import GHC.Tc.Utils.Monad
  ( mapAccumLM )
import GHC.Tc.Utils.TcMType
  ( newMetaTyVarTyVarX )
import GHC.Types.RepType
  ( kindPrimRep_maybe )
import GHC.Utils.Outputable
  ( ($$), (<+>), dcolon, text, vcat )

-- transformers
import Control.Monad.Trans.Class
  ( lift )
import Control.Monad.Trans.Except
  ( ExceptT )
import qualified Control.Monad.Trans.Except as Except
  ( runExceptT, throwE, withExceptT )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal
  ( unsafeLiftTcM )

--------------------------------------------------------------------------------
-- Plugin definition.

-- | A type-checking plugin that solves @Coercible X Y@ constraints when
-- @X@ and @Y@ are datatypes with isomorphic constructors.
plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin        = \ _args -> Just $ mkTcPlugin coerceDataTcPlugin
    , pluginRecompile = purePlugin
    }

coerceDataTcPlugin :: TcPlugin
coerceDataTcPlugin =
  TcPlugin
    { tcPluginInit    = pure ()
    , tcPluginSolve   = \ _ -> solver
    , tcPluginRewrite = \ _ -> emptyUFM
    , tcPluginStop    = \ _ -> pure ()
    }

--------------------------------------------------------------------------------
-- Constraint solving.

solver :: [ Ct ] -> [ Ct ] -> TcPluginM Solve TcPluginSolveResult
solver givens wanteds
  | null wanteds
  = pure $ TcPluginOk [] []
  | otherwise
  = do
      tcPluginTrace "CoerceData solver {" $
        vcat [ text "givens:" <+> ppr givens
             , text "wanteds:" <+> ppr wanteds
             ]
      famInsts <- unsafeLiftTcM tcGetFamInstEnvs
      let
        trySolveOne :: Ct -> TcPluginM Solve ( Either ( Maybe Ct ) ( ( [ Ct ], EvTerm ), Ct ) )
        trySolveOne wtd = bimap ( \ ok -> if ok then Nothing else Just wtd ) ( , wtd ) <$> solveWanted famInsts givens wtd
      ( untouched, newAndSolved ) <- partitionWithM trySolveOne wanteds
      let insols = catMaybes untouched
          new    = concatMap ( fst . fst ) newAndSolved
          solved = map ( first snd ) newAndSolved
      tcPluginTrace "CoerceData solver }" $
        vcat [ text "insols:" <+> ppr insols
             , text "solved:" <+> ppr solved
             , text "new:" <+> ppr new
             ]
      pure $
        TcPluginSolveResult
          { tcPluginInsolubleCts = insols
          , tcPluginSolvedCts    = solved
          , tcPluginNewCts       = new
          }

solveWanted :: ( FamInstEnv, FamInstEnv ) -> [ Ct ] -> Ct -> TcPluginM Solve ( Either Bool ( [ Ct ], EvTerm ) )
solveWanted famInsts givens wanted =
  case classifyPredType (ctPred wanted) of
    EqPred ReprEq lhs rhs ->
      case ( dataType_maybe famInsts lhs, dataType_maybe famInsts rhs ) of
        ( Just ( rep_tc_l, data_l ), Just ( rep_tc_r, data_r ) )
          -- Main logic: coerce between two data-types, provided that their
          -- constructors are isomorphic.
          | isNewTyCon rep_tc_l == isNewTyCon rep_tc_r
            -- Ensure both are newtypes or neither are.
            -- NB: GHC's constraint solver deals with unwrapping newtypes;
            -- we don't deal with that here.
          -> wrapExcept $
               coerceDataTyCon givens
                 ( wanted, ( lhs, rhs ) )
                 data_l data_r
        ( Just ( rep_tc_l, [ DataConWithInstSig nt_l ( [], [], [ arg_ty_l ] ) ] ), _ )
          | isNewTyCon rep_tc_l
          , isConstraintLikeKind ( tyConResKind rep_tc_l )
          -> wrapExcept do
               let msg = text "nt_l:" <+> ppr nt_l
               finish "Newtype Class L" ( lhs, rhs ) . maybeToList <$>
                 newReprEq_maybe msg ( ctLoc wanted ) arg_ty_l rhs
        ( _, Just ( rep_tc_r, [ DataConWithInstSig nt_r ( [], [], [ arg_ty_r ] ) ] ) )
          | isNewTyCon rep_tc_r
          , isConstraintLikeKind ( tyConResKind rep_tc_r )
          -> wrapExcept do
               let msg = text "nt_r:" <+> ppr nt_r
               finish "Newtype Class R" ( lhs, rhs ) . maybeToList <$>
                 newReprEq_maybe msg ( ctLoc wanted ) lhs arg_ty_r
        _ ->
          return $ Left True
    _ -> return $ Left True

wrapExcept :: Functor m => ExceptT () m a -> m ( Either Bool a )
wrapExcept = Except.runExceptT . ( Except.withExceptT $ const False )

data DataConWithInstSig
  = DataConWithInstSig
  { dcwis_dataCon :: !DataCon
  , dcwis_dataConInstSig :: !( [ TyCoVar ], ThetaType, [ Type ] )
  }

dataType_maybe :: ( FamInstEnv, FamInstEnv ) -> Type -> Maybe ( TyCon, [ DataConWithInstSig ] )
dataType_maybe famInsts ty = do
  ( tc, args ) <- splitTyConApp_maybe ty
  let ( rep_tc, rep_tc_args, _rep_co ) = tcLookupDataFamInst famInsts tc args
  dcs <- tyConDataCons_maybe rep_tc
  let dataConSigs =
        [ DataConWithInstSig
            { dcwis_dataCon =
                dc
            , dcwis_dataConInstSig =
                dataConInstSig dc $ dataConInstUnivs dc rep_tc_args
            }
        | dc <- dcs
        ]
  return ( rep_tc, dataConSigs )

coerceDataTyCon :: [ Ct ] -- ^ Given constraints
                -> ( Ct, ( TcType, TcType ) ) -- ^ Wanted constraint
                -> [ DataConWithInstSig ] -- ^ LHS data constructors
                -> [ DataConWithInstSig ] -- ^ RHS data constructors
                -> ExceptT () ( TcPluginM Solve ) ( [ Ct ] , EvTerm )
coerceDataTyCon givens ( wanted, tys ) dc_sigs_l dc_sigs_r
    | length dc_sigs_l /= length dc_sigs_r
    = Except.throwE ()
    | otherwise
    = finish "DataTyCons" tys . concat <$>
        zipWithM ( coerceDataConApp givens ( ctLoc wanted ) )
          dc_sigs_l dc_sigs_r

finish :: String -> ( TcType, TcType ) -> [ Ct ] -> ( [ Ct ], EvTerm )
finish str ( ty_l, ty_r ) newWanteds =
  let innerCos = map ( ctEvCoercion . ctEvidence ) newWanteds
      finalCo  = mkPluginUnivEvTerm ("coerce-data: " ++ str) Representational innerCos ty_l ty_r
  in ( newWanteds, finalCo )

coerceDataConApp :: [ Ct ] -- ^ Given constraints
                 -> CtLoc -- ^ Wanted 'CtLoc'
                 -> DataConWithInstSig -- ^ LHS data constructor
                 -> DataConWithInstSig -- ^ RHS data constructor
                 -> ExceptT () ( TcPluginM Solve ) [ Ct ]
coerceDataConApp _givens wantedLoc
  ( DataConWithInstSig dc_l ( ex_tvs_l, theta_l, arg_tys_l ) )
  ( DataConWithInstSig dc_r ( ex_tvs_r, theta_r, arg_tys_r ) )
    = do { -- Ensure the two data constructors have the same strictness;
           -- coercing between them isn't sound otherwise.
         ; let str_l = dataConRepStrictness dc_l
               str_r = dataConRepStrictness dc_r
         ; unless ( str_l == str_r ) do
             lift $ tcPluginTrace "CoerceData: mismatched DataCon strictness" $
               vcat [ text "dc_l:" <+> ppr dc_l
                    , text "strictness_l:" <+> ppr str_l
                    , text "dc_r:" <+> ppr dc_r
                    , text "strictness_r:" <+> ppr str_r
                    ]
             Except.throwE ()
         ; let args_l = theta_l ++ arg_tys_l
               args_r = theta_r ++ arg_tys_r
         ; unless ( length args_l == length args_r ) $
             Except.throwE ()
           -- Instantiate all existential type variables to fresh 'TyVarTv' metavariables.
         ; ( subst_l, _ex_metas_l ) <- lift $ instExistentials ex_tvs_l
         ; ( subst_r, _ex_metas_r ) <- lift $ instExistentials ex_tvs_r
         ; let subst_args_l = substTys subst_l args_l
               subst_args_r = substTys subst_r args_r
           -- Emit new representational equalities between the left and right
           -- data constructor argument types.
         ; lift $ tcPluginTrace "CoerceData: emitting DataCon Coercible constraints" $
               vcat [ text "dc_l:" <+> ppr dc_l
                    , text "dc_r:" <+> ppr dc_r
                    , text "ty_pairs:" <+> ppr ( zip subst_args_l subst_args_r )
                    ]
            -- TODO: instantiating the existential type variables to 'TyVarTv's
            -- doesn't prevent two LHS or two RHS existentials from unifying
            -- with eachother, even though we don't want to allow that.
         ; let msg :: SDoc
               msg = text "dc_l" <+> ppr dc_l $$ text "dc_r" <+> ppr dc_r
         ; newWanteds <-
              catMaybes <$>
                zipWithM ( newReprEq_maybe msg wantedLoc  )
                  subst_args_l subst_args_r
         ; return newWanteds
         }

newReprEq_maybe :: SDoc -> CtLoc -> TcType -> TcType -> ExceptT () ( TcPluginM Solve ) ( Maybe Ct )
newReprEq_maybe msg wantedLoc arg_ty_l arg_ty_r
  | arg_ty_l `eqType` arg_ty_r
  -- Avoid emitting any constraints if the two types
  -- are visibly nominally equal (just to save work).
  = return Nothing
  | Just reps_l <- kindPrimRep_maybe ki_l
  , Just reps_r <- kindPrimRep_maybe ki_r
  , reps_l /= reps_r
  -- Give up if the kinds don't (and can't) match.
  = do { lift $ tcPluginTrace "CoerceData: mismatched DataCon argument representations" $
          vcat [ msg
               , text "arg_ty_l" <+> ppr arg_ty_l <+> dcolon <+> ppr ki_l
               , text "arg_ty_r" <+> ppr arg_ty_r <+> dcolon <+> ppr ki_r
               ]
       ; Except.throwE () }
  | otherwise
  = lift $
      Just . mkNonCanonical <$>
        newWanted wantedLoc ( mkEqPredRole Representational arg_ty_l arg_ty_r )
  where
    ki_l = typeKind arg_ty_l
    ki_r = typeKind arg_ty_r

-- | Instantiate existential type variables to fresh 'TyVarTv' metavariables.
instExistentials :: [ TcTyVar ] -> TcPluginM Solve ( Subst, [ TcTyVar ] )
instExistentials =
  unsafeLiftTcM . mapAccumLM newMetaTyVarTyVarX emptySubst
