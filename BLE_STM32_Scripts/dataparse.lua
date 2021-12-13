function parse_data(path)
    local fp = io.open(path)

    local parsed = {}

    local Item = {
        ang = {x = 0, y = 0, z = 0},
        acc = {x = 0, y = 0, z = 0},
        omeg = {x = 0, y = 0, z = 0},
        ad = 0,
        vol = 0,
        pres = 0
    }

    local mt = {__index = Item}
    function Item.new(o)
        o = o or {}
        setmetatable(o, mt)
        return o
    end
    local fp_log = io.open("log.txt", "a+")

    local count_line = 1
    local it = Item.new()
    it.ang, it.acc, it.omeg = {}, {}, {}

    for l in fp:lines() do
        fp_log:write(count_line, l)
        fp_log:write("\n")
        --TODO: change count to: if l:contains(...) ...
        if count_line == 1 then
            it.ang.x, it.ang.y, it.ang.z = string.match(l, "^%g*%s*(%g*)%s*(%g*)%s*(%g*)%s*%g*.*$")
            fp_log:write(count_line, it.ang.x, it.ang.y, it.ang.z)
            fp_log:write("\n")
        elseif count_line == 2 then
            it.acc.x, it.acc.y, it.acc.z = string.match(l, "^%g*%s*(%g*)%s*(%g*)%s*(%g*)%s*%g*.*$")
            fp_log:write(count_line,it.acc.x, it.acc.y, it.acc.z)
            fp_log:write("\n")
        elseif count_line == 3 then
            it.omeg.x, it.omeg.y, it.omeg.z = string.match(l, "^%g*%s*(%g*)%s*(%g*)%s*(%g*)%s*%g*.*$")
            fp_log:write(count_line,it.omeg.x, it.omeg.y, it.omeg.z)
            fp_log:write("\n")
        elseif count_line == 5 then
            it.ad, it.vol, it.pres = string.match(l, "^ADval = (%g*),vol = (%g*) mv,press = (%g*) g.*$")
            fp_log:write(count_line,it.ad, it.vol, it.pres)
            fp_log:write("\n")
        elseif count_line == 6 then
            parsed[#parsed+1] = it
            count_line = 0
            it = Item.new()
            it.ang, it.acc, it.omeg = {}, {}, {}
        end
        count_line = count_line + 1
    end
    fp:close()
    fp_log:close()
    return parsed
end

-- -- usage
-- dofile("dataparse.lua")
-- t = parse_data("./data/128newdata.txt")
-- json = require "json"
-- jt = json.encode(t)
-- f = io.open("data.json","w+")
-- f:write(jt)
-- f:close()
