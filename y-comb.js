Y = (F) => {
    let g = (y) => {
        return (x) => { //η-reduction
            return F(y(y))(x)
        }
    }
    return g(g)
}

F = (proc) => {
    return (x) => {
        if(x <= 1) return 1;
        else return (x * (proc(x - 1)))
    }
}