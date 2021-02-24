chomp_macro::nibble! {
    let opt(x) = _ : None | x : Some;
    let star(x) = [rec](opt(x.rec));
    let plus(x) = (x : First).(star(x) : Rest);

    let ws = star(" ");
    let list(x, p) = (x : First).(star(p.ws.x) : Rest);

    let number = plus("0" | "1" | "2"| "3" | "4" | "5" | "6" | "7" | "8" | "9");
    let term(e) = ((number : Pos | "-".ws.number : Neg | "(".ws.(e : Inner).")" : Parens) : RawTerm).ws;
    let prod(e) = list(term(e), "*"|"/");
    let expr(e) = list(prod(e), "+"|"-");
    let arith = [e](expr(e));

    match [rec]((" ".rec : Base | arith) : Ast);
}

impl From<Ast> for i64 {
    fn from(mut a: Ast) -> Self {
        loop {
            match a.0 {
                Ast1::Base1(b) => a = *b.rec1,
                Ast1::Arith1(a) => return a.into(),
            }
        }
    }
}

impl From<Arith1> for i64 {
    fn from(a: Arith1) -> Self {
        a.0.into()
    }
}

impl From<Expr1> for i64 {
    fn from(l: Expr1) -> Self {
        let mut acc = l.prod1.into();
        let mut rest = l.rest1;
        loop {
            match rest.0 {
                Opt5::None1(_) => return acc,
                Opt5::Some1(s) => {
                    let v: i64 = s.x1.prod1.into();
                    match s.x1.p1 {
                        P2::Branch1(_) => acc += v,
                        P2::Branch2(_) => acc -= v,
                    }
                    rest = *s.rec1;
                },
            }
        }
    }
}

impl From<Prod1> for i64 {
    fn from(l: Prod1) -> Self {
        let mut acc = l.term1.into();
        let mut rest = l.rest1;
        loop {
            match rest.0 {
                Opt3::None1(_) => return acc,
                Opt3::Some1(s) => {
                    let v: i64 = s.x1.term1.into();
                    match s.x1.p1 {
                        P1::Branch1(_) => acc *= v,
                        P1::Branch2(_) => acc /= v,
                    }
                    rest = *s.rec1;
                }
            }
        }
    }
}

impl From<Prod2> for i64 {
    fn from(l: Prod2) -> Self {
        let mut acc = l.term1.into();
        let mut rest = l.rest1;
        loop {
            match rest.0 {
                Opt4::None1(_) => return acc,
                Opt4::Some1(s) => {
                    let v: i64 = s.x1.term1.into();
                    match s.x1.p1 {
                        P1::Branch1(_) => acc *= v,
                        P1::Branch2(_) => acc /= v,
                    }
                    rest = *s.rec1;
                }
            }
        }
    }
}

impl From<Term1> for i64 {
    fn from(t: Term1) -> Self {
        match t.raw_term1 {
            RawTerm1::Pos1(n) => n.into(),
            RawTerm1::Neg1(n) => -i64::from(n.number1),
            RawTerm1::Parens1(p) => (*p.e1).into(),
        }
    }
}

impl From<Pos1> for i64 {
    fn from(p: Pos1) -> Self {
        p.to_string().parse().unwrap()
    }
}
