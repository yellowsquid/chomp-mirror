use chewed::{IterWrapper, Parser};
use chomp_bench::arith::*;
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

const INPUTS: &[&str] = &[
    include_str!("data/134.arith"),
    include_str!("data/262.arith"),
    include_str!("data/556.arith"),
    include_str!("data/1066.arith"),
    include_str!("data/2180.arith"),
    include_str!("data/4371.arith"),
    include_str!("data/8380.arith"),
    include_str!("data/17850.arith"),
    include_str!("data/35389.arith"),
];

fn parse_chewed(input: &str) -> i64 {
    IterWrapper::new(input.chars())
        .parse::<nibble::Ast>()
        .unwrap()
        .into()
}

fn parse_handwritten(input: &str) -> i64 {
    parse_expr(&mut IterWrapper::new(input.chars())).unwrap()
}

fn parse_lalrpop(parser: &lalr::ExpressionParser, input: &str) -> i64 {
    parser.parse(input).unwrap()
}

fn bench_parse(c: &mut Criterion) {
    let lalr_parser = lalr::ExpressionParser::new();
    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);
    let mut group = c.benchmark_group("Arith");
    group.plot_config(plot_config);
    for (i, input) in INPUTS.iter().enumerate() {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("Chewed", i), *input, |b, i| {
            b.iter(|| parse_chewed(i))
        });
        group.bench_with_input(BenchmarkId::new("Handwritten", i), *input, |b, i| {
            b.iter(|| parse_handwritten(i))
        });
        group.bench_with_input(BenchmarkId::new("LALRPOP", i), *input, |b, i| {
            b.iter(|| parse_lalrpop(&lalr_parser, i))
        });
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
