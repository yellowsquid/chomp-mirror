use std::mem;

chomp_macro::nibble! {
    let opt  some = _ : None | some;
    let plus iter = !(/plus/ iter . (opt plus));
    let star iter = opt (plus iter);

    let ws = star " ";
    let list inner p = !(/list/ inner . opt (p . ws . list));

    let digit = "0" | "1" | "2"| "3" | "4" | "5" | "6" | "7" | "8" | "9";
    let number = plus digit;
    let term expr =
        (( number                : Pos
         | "-" . ws . number     : Neg
         | "(" . ws . expr . ")" : Parens
         ) : RawTerm) . ws;
    let prod  expr = list (term expr) ("*"|"/");
    let arith expr = list (prod expr) ("+"|"-");

    match !(/rec/ " ".rec | !arith);
}

impl From<Ast> for i64 {
    fn from(mut a: Ast) -> Self {
        loop {
            match a {
                Ast::Branch1(cat) => a = *cat.rec1,
                Ast::Branch2(arith) => return arith.into(),
            }
        }
    }
}

impl From<Arith1> for i64 {
    fn from(a: Arith1) -> Self {
        let acc = a.prod1.into();
        a.opt1.into_iter().fold(acc, |acc, (p, prod)| {
            let v: i64 = prod.into();
            match p {
                P2::Branch1(_) => acc + v,
                P2::Branch2(_) => acc - v,
            }
        })
    }
}

impl From<Prod1> for i64 {
    fn from(p: Prod1) -> Self {
        let acc = p.term1.into();
        p.opt1.into_iter().fold(acc, |acc, (p, term)| {
            let v: i64 = term.into();
            match p {
                P1::Branch1(_) => acc * v,
                P1::Branch2(_) => acc / v,
            }
        })

    }
}

impl From<Term1> for i64 {
    fn from(t: Term1) -> Self {
        match t.raw_term1 {
            RawTerm1::Pos1(n) => n.into(),
            RawTerm1::Neg1(n) => -i64::from(n.number1),
            RawTerm1::Parens1(p) => (*p.expr1).into(),
        }
    }
}

impl From<Pos1> for i64 {
    fn from(p: Pos1) -> Self {
        p.to_string().parse().unwrap()
    }
}

impl From<Number1> for i64 {
    fn from(p: Number1) -> Self {
        p.to_string().parse().unwrap()
    }
}

impl Iterator for Opt5 {
    type Item = (P2 , Prod1);

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Self::None1(Epsilon));
        match orig {
            Self::None1(_) => None,
            Self::Some1(some) => {
                *self = some.list1.opt1;
                Some((some.p1, some.list1.prod1))
            }
        }
    }
}

impl Iterator for Opt4 {
    type Item = (P1 , Term1);

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Self::None1(Epsilon));
        match orig {
            Self::None1(_) => None,
            Self::Some1(some) => {
                *self = some.list1.opt1;
                Some((some.p1, some.list1.term1))
            }
        }
    }
}
