-- config for an irc server
--
-- if serverPassword is blank, no password will be sent.
-- if nicksrvPassword is blank, not idenitify msg will be sent.
{ host : Text
, port : Integer
, ssl: Bool
, serverPassword: Text
, nick: Text
, fullname: Text
, nicksrvPassword: Text
}