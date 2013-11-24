{-# LANGUAGE UnicodeSyntax #-}

module Test where
import Prelude.Unicode
import Data.List
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import OL
import Edit

-- Edit -----------------------------------------------------------------------
prop_reversible ∷ OL → Edit → Property
prop_reversible ol op = opOkay ol op ==> case edit ol op of
	(olNew,opUndo) → case edit olNew opUndo of
		(olRestore,opRestore) → case edit olRestore opRestore of
			(olNew',opUndo') → olRestore≡ol && olNew≡olNew' && opUndo≡opUndo'

instance Arbitrary OL where
	arbitrary = sized genOL

genAddr ∷ OL → Int → Gen Addr
genAddr ol n = liftM Addr $ r [] ol n where
	r a ol 0 = return a
	r a ol@(OL s []) n = return a
	r a ol@(OL s cs) n = do
		c ← choose(0,length cs) ∷ Gen Int
		if length cs≡c then return a else r (c:a) (cs!!c) (n-1)

genOL ∷ Int → Gen OL
genOL 0 = return $ (OL $ ols "a") []
genOL n = liftM (OL $ ols "a") l where
	l = do
		cn ← choose(0,n-1)
		mapM genOL $ take cn $ repeat (n `div` cn)

instance Arbitrary Edit where
	arbitrary = do
		ol ← sized genOL
		a1 ← sized $ genAddr ol
		a2 ← sized $ genAddr ol
		n ← choose(0,4) ∷ Gen Int
		return $ case n of
			0 → ADD a1 ol
			1 → RPL a1 ol
			2 → DEL a1
			3 → DEL a1 -- TODO MOV a1 a2
			4 → EDT a1 (ols "b")
			_ → error "This will never happen."

test = quickCheck prop_reversible

canAddHere (Addr[]) _ = False
canAddHere a@(Addr (0:_)) ol = addrOk (addrParent a) ol
canAddHere a ol = addrOk a ol

opOkay ol (ADD a _) = canAddHere a ol
opOkay ol (RPL a _) = addrOk a ol
opOkay ol (DEL a) = addrOk a ol
opOkay ol (MOV f t) = addrOk f ol && canAddHere t ol
opOkay ol (EDT a s) = addrOk a ol

main ∷ IO()
main = test
