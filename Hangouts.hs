module Hangouts where
import Prelude
import FFI

data ApiReady
data Msg
data Participant
data ParticipantsChanged

participants :: ParticipantsChanged -> [Participant]
participants = ffi "(%1).participants"

enabledParticipants :: Fay [Participant]
enabledParticipants = ffi "gapi.hangout.getEnabledParticipants()"

clientId :: Participant -> String
clientId = ffi "(%1).id"

onGapiReady :: (ApiReady -> Fay()) -> Fay()
onGapiReady = ffi "gapi.hangout.onApiReady.add(%1)"

isAppVisible :: ApiReady -> Fay Bool
isAppVisible = ffi "(%1).isAppVisible"

msgSender :: Msg -> String
msgSender = ffi "(%1).senderId"

msgData :: Msg -> String
msgData = ffi "(%1).message"

sendMsg :: String -> Fay()
sendMsg = ffi "gapi.hangout.data.sendMessage(%1)"

onMsgRecv :: (Msg -> Fay()) -> Fay()
onMsgRecv = ffi "gapi.hangout.data.onMessageReceived.add(%1)"
