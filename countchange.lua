function coin(m, tab)
    if m == 0 then return 1
    elseif m < 0 then return 0
    elseif #tab == 0 then return 0
    else
        local t = {}
        table.move(tab, 2, #tab, 1, t)
        return coin(m - tab[1], tab) + coin(m, t)
    end
end

function changecoin(m)
    local c = {1, 5, 10, 25, 50}
    return coin(m, c)
end

-- using memorize techinically
-- evaled = {} -- for debug
function memorize(f)
    local evaled = {}
    return function (x, y)
        local val = evaled[x]
        if val and val[y] then return val[y]
        else local ret = f(x, y)
            evaled[x] = evaled[x] or {}
            evaled[x][y] = ret
            return ret
        end
    end
end

coins = {1, 5, 10, 25, 50}

local memo_cc
memo_cc = memorize(function (amount, kindsofcoins)
        if amount < 0 or kindsofcoins == 0 then return 0
        elseif amount == 0 then return 1
        else return memo_cc(amount, kindsofcoins - 1) + memo_cc(amount - coins[kindsofcoins], kindsofcoins)
        end
    end)

function countchange(count)
    return memo_cc(count, 5)
end