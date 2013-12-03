-- Syncronization Model:
	-- Messages are released only when we have all information for a chunk of time.
		-- all messages f
	-- Idle clients regularly send TRACE messages.
	-- If messages are delivered, they are intact.
	-- Messages may not be delivered, so we need to know if we missed one.

-- In a Queue,
	-- nextId is the id that the next new client will be assigned, and also
		-- the number of active cleints.
	-- lastSync is a timestamp such that
		-- ¬∃ a message m yeilded by Advance gives (ts m>lastSync).
		-- ∃ a message m yeilded by Advance that gives (ts m≡lastSync || -1≡lastSync)

module Sync where
	-- ( Queue, Msg, TimeStamp, ClientId
	-- , msg, epoch, tsInc, qEmpty
	-- , qConnect, qMsg, qMsgs
	-- , prop_flushable
	-- ) where
import Util
import Data.List (sort)

-- Data Types
newtype TimeStamp = TS Int deriving (Ord,Show,Eq)
newtype ClientId = ID Int deriving (Ord,Show,Eq)

data Msg a
	= MSG { ts::TimeStamp, user::ClientId, payload::a }
	| TRACE {traceTime::TimeStamp, lastTrace::Maybe TimeStamp, msgsThisTime::Int}
	deriving (Show,Ord,Eq)

data Queue a = Queue
	{ nextId :: ClientId
	, lastSync :: TimeStamp
	, pending :: [Msg a]
	, connected :: [ClientId]
	} deriving (Show,Eq)

-- API
epoch :: TimeStamp
epoch = TS 0

tsInc :: TimeStamp -> TimeStamp
tsInc (TS i) = TS $ i+1

qEmpty :: Queue a
qEmpty = Queue { nextId=ID 0, lastSync=TS(-1), pending=[], connected=[] }

msg :: TimeStamp -> ClientId -> a -> Msg a
msg = MSG

qDisconnect :: Queue a -> ClientId -> Maybe(Queue a)
qDisconnect (Queue n l p c) id = if c==c' then Nothing else Just q' where
	q' = Queue n l p' c'
	p' = filter ((/= id) . user) p
	c' = filter (/= id) c

qConnect :: Queue a -> (ClientId,Queue a)
qConnect (Queue (ID n) l p c) = (ID n,Queue (ID $ n+1) l p (ID n:c))

qMsg :: Queue a -> Msg a -> Maybe (Queue a,[Msg a])
qMsg q m = mmap qAdvance $ qAddMsg q m

qMsgs :: Show a => Queue a -> [Msg a] -> Maybe (Queue a,[Msg a])
qMsgs queue ms = mmap qAdvance $ foldl f (Just queue) ms where
	f Nothing _ = error "this should never happen."
	-- f (Just q) m = qAddMsg q m
	f (Just q) m = case qAddMsg q m of
		Nothing -> error $
			"Fucked up\nmsg:\n\t(" ++ show m ++ ")\non queue:\n\t(" ++ show q ++ ")"
		Just a -> Just a

-- Internal
qAddMsg :: Queue a -> Msg a -> Maybe(Queue a)
qAddMsg (Queue clients sync q c) m@(MSG t id a) =
	if id<clients && t>sync then Just $ Queue clients sync (m:q) c else Nothing

qAdvance :: Queue a -> (Queue a,[Msg a])
qAdvance q@(Queue _ _ [] _) = (q,[])
qAdvance q@(Queue (ID 0) _ _ _) = (q,[])
qAdvance q@(Queue (ID clients) old msgs connected) = r where
	r = (Queue (ID clients) sync keep connected, yeild)
	TS new = minimum $ map (maxTS . byClient msgs) $ connected
	sync = TS(new-1)
	keep = filter (\m -> ts m > sync) msgs
	yeild = filter (\m -> ts m <= sync) msgs
	byClient ms i = filter (\m -> i==user m) ms

maxTS :: [Msg a] -> TimeStamp
maxTS [] = TS(-1) -- TODO Hack!
maxTS (m:ms) = ts $ foldl f m ms where
	f m1 m2 = if ts m1 > ts m2 then m1 else m2