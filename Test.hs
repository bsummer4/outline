{-# LANGUAGE UnicodeSyntax #-}

module Test where
import Prelude.Unicode
import Data.List
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import Sync
import Outline
import Edit
import Util

main ∷ IO()
main = test

test = do
	quickCheck prop_reversible
	quickCheck prop_flushable
	quickCheck prop_validPending

-- Sync -----------------------------------------------------------------------
prop_validPending ∷ Queue() → Bool
prop_validPending q@(Queue n l p cs) = all chk  p where
	chk m = l<ts m && n>user m && (not$null$filter (\c→user m≡c) cs)

prop_flushable ∷ Queue() → Bool
prop_flushable q@(Queue (ID n) _ _ _) = (Just$sort msgs)≡(mmap sort$flushed)
	where
		flushed = mmap (pending.fst) $ qMsgs q $ msgs
		msgs = map (\id → msg bigTS (ID id) ()) [0..n-1]
		bigTS = tsInc $ maxTS $ pending q

instance Arbitrary TimeStamp where
	arbitrary = arbitrary >>= return.TS

instance Arbitrary ClientId where
	arbitrary = arbitrary >>= return.ID

genMsg ∷ Arbitrary a => Queue a → a → Gen(Msg a)
genMsg (Queue _ _ _ []) m = error $ "No clients, can't make a message"
genMsg q@(Queue (ID n) (TS sync) p connected) m = do
	u ← choose(0,length connected-1) >>= return . (connected!!)
	t ← choose(sync+1,sync+3)
	return $ msg (TS t) u m

genQueue ∷ Show a => Arbitrary a => Queue a → Gen(Queue a)
genQueue q = do
	x ← choose(0,9∷Int)
	if 0≡x || ID 0≡nextId q then return$snd$qConnect q else
		arbitrary >>= mapM(genMsg q) >>= (return.fst.fromJust.qMsgs q)

instance (Show a,Arbitrary a) => Arbitrary (Queue a) where
	arbitrary = foldl (\q() → q>>=genQueue) (return qEmpty) (take 25$repeat ())

-- Edit -----------------------------------------------------------------------
prop_reversible ∷ Outline → Edit → Property
prop_reversible ol op = opOkay ol op ==> case edit' ol op of
	(olNew,opUndo) → case edit' olNew opUndo of
		(olRestore,opRestore) → case edit' olRestore opRestore of
			(olNew',opUndo') → olRestore≡ol && olNew≡olNew' && opUndo≡opUndo'

instance Arbitrary Outline where
	arbitrary = sized genOL

genAddr ∷ Outline → Int → Gen Addr
genAddr ol n = liftM Addr $ r [] ol n where
	r a ol 0 = return a
	r a ol@(OL s []) n = return a
	r a ol@(OL s cs) n = do
		c ← choose(0,length cs) ∷ Gen Int
		if length cs≡c then return a else r (c:a) (cs!!c) (n-1)

genOL ∷ Int → Gen Outline
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
		n ← choose(0,3) ∷ Gen Int
		return $ case n of
			0 → ADD a1 ol
			1 → RPL a1 ol
			2 → DEL a1
			3 → EDT a1 (ols "b")
--			4 → MOV a1 a2
			_ → error "This will never happen."

canAddHere (Addr[]) _ = False
canAddHere a@(Addr (0:_)) ol = addrOk (addrParent a) ol
canAddHere a ol = addrOk a ol

opOkay ol (ADD a _) = canAddHere a ol
opOkay ol (RPL a _) = addrOk a ol
opOkay ol (DEL a) = addrOk a ol
opOkay ol (EDT a s) = addrOk a ol
--opOkay ol (MOV f t) = addrOk f ol && canAddHere t ol
