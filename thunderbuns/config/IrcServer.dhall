-- config for an irc server
--
-- if serverPassword is blank, no password will be sent.
-- if nicksrvPassword is blank, not idenitify msg will be sent.
{ host : Text
, port : Natural
, tls: Bool
, serverPassword: Optional Text
, nick: Text
, fullname: Text
, nicksrvPassword: Optional Text
, channels: List Text
}