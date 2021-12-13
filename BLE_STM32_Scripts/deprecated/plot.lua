function eraseTerm ()
    io.write("\27[2J")
end

function mark (x, y)
    io.write(string.format("\27[%d;%dH*",math.floor(y),math.floor(x)))
end

TermSize = {w = 80, h = 24}

function plot (f)
    eraseTerm()
    for i = 1, TermSize.w do
        local x = (i / TermSize.w) * 2 - 1
        local y = (f(x) + 1) / 2 * TermSize.h
        mark(x,y)
    end
    io.read()
end
