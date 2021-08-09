Y = (F) => {
    let g = (y) => {
        return (x) => { //η-reduction
            return F(y(y))(x)
        }
    }
    return g(g)
}

// fact
F = (proc) => {
    return (x) => {
        if(x <= 1) return 1;
        else return (x * (proc(x - 1)))
    }
}
Y(F)(42)
