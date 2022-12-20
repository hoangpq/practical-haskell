local _M = {}

local json = require "cjson"

-- print(_M)
-- print(json)

for k, v in pairs(json.decode('{"name": "Vampire"}')) do
    print(k, v)
end
