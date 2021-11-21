socket = require("socket")

host = "www.lua.org"
file = "/manual/5.3/manual.html"
function download (host, file, fp, port)
    local c = assert(socket.connect(host, port or 80))
    local count = 0
    local request = string.format(
    "GET %s HTTP/1.0\r\nhost: %s\r\n\r\n", file, host)
    c:send(request)
    local current
    if fp then current = fp:seek() end
    repeat
        local s, status, partial = receive(c)
        count = count + #s
        -- 断点续传
        if fp then 
            fp:seek("end")
            fp:write(s or partial)
            fp:flush()
        end
    until status == "closed"
    if fp then fp:seek("set", current) end
    c:close()
    io.write(file, count)
end

function receive (connection)
    connection:settimeout(0)
    local s, status, partial = connection:receive(2^10)
    if status == "timeout" then
        coroutine.yield(connection)
        -- io.write("\r\n[COROUTINE RESUMED] " .. (s or partial))
    end
    
    return s or partial, status
end

tasks = {}
function get (host, file, fp, port)
    local co = coroutine.wrap(function ()
        download(host, file, fp, port)
    end)
    table.insert(tasks, co)
end

function runall ()
    local i = 1
    local timedout = {}
    while true do
        if tasks[i] == nil then
            if tasks[1] == nil then
                break
            end
            i = 1
            timedout = {}
        end

        local res = tasks[i]()
        if not res then
            table.remove(tasks,i)
        else
            i = i + 1
            timedout[#timedout + 1] = res
            if #timedout == #tasks then
                socket.select(timedout)
            end
        end
    end
end