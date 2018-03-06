# tcql4

When it grows up, it wants to be a Cassandra CQL4 Driver

Right now it can

* Execute CQL queries with positional bind variables
* Decode the results

It can't yet

* prepare statements
* batch statements
* use named bind variables
* use options like compression codes
* use password protected connections

The above list is roughly in order of personal priority.
Also it may take a while - the plan is to do some dogfooding
with the current api before tackling these features.
