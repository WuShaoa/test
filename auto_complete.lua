
function find_method(key, tab, prefix, serchedlist)
    for k, v in pairs(tab) do
        if string.find(k, key) then
            print(prefix .. k, v)
        end
        if type(v) == "table" and (not is_in_list(serchedlist, k)) then -- type is table and avoid loop
            table.insert(serchedlist, k)
            find_method(key, tab[k], (prefix == "") and (k .. ".") or (prefix .. k .. "."), serchedlist)
        end
    end
end

function print_table(t)
    for i, j in pairs(t) do print(i, j) end
end

-- export
function complete(key)
    find_method(key, _G, "", {"_G"}) -- package.loaded.
end

function is_in_list(list, key)
    for _, v in pairs(list) do
        if v == key then
            return true
        end
    end
    return false
end