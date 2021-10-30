pub fn transpose_2<A, B>(tuples: Vec<(A, B)>) -> (Vec<A>, Vec<B>) {
    let mut a_items: Vec<A> = Vec::with_capacity(tuples.len());
    let mut b_items: Vec<B> = Vec::with_capacity(tuples.len());
    for (a, b) in tuples.into_iter() {
        a_items.push(a);
        b_items.push(b);
    }
    (a_items, b_items)
}

pub fn transpose_3<A, B, C>(tuples: Vec<(A, B, C)>) -> (Vec<A>, Vec<B>, Vec<C>) {
    let mut a_items: Vec<A> = Vec::with_capacity(tuples.len());
    let mut b_items: Vec<B> = Vec::with_capacity(tuples.len());
    let mut c_items: Vec<C> = Vec::with_capacity(tuples.len());
    for (a, b, c) in tuples.into_iter() {
        a_items.push(a);
        b_items.push(b);
        c_items.push(c);
    }
    (a_items, b_items, c_items)
}

pub fn transpose_4<A, B, C, D>(tuples: Vec<(A, B, C, D)>) -> (Vec<A>, Vec<B>, Vec<C>, Vec<D>) {
    let mut a_items: Vec<A> = Vec::with_capacity(tuples.len());
    let mut b_items: Vec<B> = Vec::with_capacity(tuples.len());
    let mut c_items: Vec<C> = Vec::with_capacity(tuples.len());
    let mut d_items: Vec<D> = Vec::with_capacity(tuples.len());
    for (a, b, c, d) in tuples.into_iter() {
        a_items.push(a);
        b_items.push(b);
        c_items.push(c);
        d_items.push(d);
    }
    (a_items, b_items, c_items, d_items)
}
