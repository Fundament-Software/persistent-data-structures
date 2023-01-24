using import Array
local a = ((Array string))
for sym in ('all (globals)) ('append a (tostring sym))
'sort a
for sym in a (print sym)
