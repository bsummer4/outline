module Sync where
	-- ( Queue, Msg, TimeStamp, ClientId
	-- , msg, epoch, tsInc, qEmpty
	-- , qConnect, qMsg, qMsgs
	-- , prop_flushable
	-- ) where

import Util
import Data.List (sort)

-- Maintain a list of messages from each client.
-- When we get a TRACE message. Produce a chunk of messages.

-- Data Types
newtype TimeStamp = TS Int deriving (Ord,Show,Eq)
data Msg dat = TRACE TimeStamp | MSG dat deriving (Show,Ord,Eq)
data Queue dat usr = Queue TimeStamp [(usr,[Msg dat])]
	deriving (Show,Eq)

-- API
epoch :: TimeStamp
epoch = TS 0

tsInc :: TimeStamp -> TimeStamp
tsInc (TS i) = TS $ i+1

qEmpty :: Queue a b
qEmpty = Queue (TS(-1)) []

qConnect :: Eq usr => Queue dat usr -> usr -> Maybe(Queue dat usr)
qConnect (Queue l p) id = case lookup id p of
	Just _ -> Nothing
	Nothing -> Just $ Queue l ((id,[]):p)

-- TODO We might lose info by dropping pending messages that others have
-- already incorperated.
qDisconnect :: Eq usr => Queue dat usr -> usr -> Maybe(Queue dat usr)
qDisconnect (Queue l p) id = mmap f $ lookup id p where
	f _ = Queue l $ filter (\(i,_) -> i==id) p

-- Internal
-- TODO This implementation is incredably naive.
qMsg :: Eq usr => Queue dat usr -> usr -> Msg dat -> Maybe(Queue dat usr)
qMsg (Queue l p) u m@(MSG d) = case lookup u p of
	Nothing -> Nothing
	Just up -> Just $ Queue l $ (u,m:up):p

qAdvance :: Queue dat usr -> Maybe (Queue dat usr,[dat])
qAdvance q@(Queue l p) = case rawr q of
	Nothing -> Nothing
	Just wat -> Just (q',yeild) where
		q' = Queue (tsInc l) $ map (\(u,(k,y)) -> (u,k)) wat
		yeild = concat $ map (\(u,(_,y)) -> y) wat

rawr :: Queue dat usr -> Maybe [(usr,([Msg dat],[dat]))]
rawr q@(Queue l p) = foldl unMaybe (Just[]) xyz where
	targetTime = tsInc l
	xyz = map (\(u,ms) -> (u,advanceUsr targetTime [] ms)) p
	unMaybe Nothing _ = Nothing
	unMaybe _ (_,Nothing) = Nothing
	unMaybe (Just a) (u,Just e) = Just ((u,e):a)

advanceUsr :: TimeStamp -> [Msg d] -> [Msg d] -> Maybe([Msg d],[d])
advanceUsr targetTime = a where
	a _ [] = Nothing
	a acc (MSG d:ms) = a (MSG d:acc) ms
	a acc (m@(TRACE t):ms) =
		if t/=targetTime then a (m:acc) ms else
			case foldl collect (Just[]) ms of
				Nothing -> Nothing
				Just yeild -> Just(reverse acc, yeild)
	collect _ (TRACE _) = Nothing
	collect Nothing _ = Nothing
	collect (Just acc) (MSG d) = Just(d:acc)
