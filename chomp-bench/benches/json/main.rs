use chewed::{IterWrapper, Parser};
use chomp_bench::json::*;
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

const INPUTS: &[&str] = &[
    include_str!("data/55.json"),
    include_str!("data/111.json"),
    include_str!("data/222.json"),
    include_str!("data/443.json"),
    include_str!("data/886.json"),
    include_str!("data/1773.json"),
    include_str!("data/3545.json"),
    include_str!("data/7091.json"),
    include_str!("data/14181.json"),
    include_str!("data/28362.json"),
];

fn parse_chewed(input: &str) -> Value {
    IterWrapper::new(input.chars())
        .parse::<nibble::Ast>()
        .unwrap()
        .into()
}

fn parse_handwritten(input: &str) -> Value {
    IterWrapper::new(input.chars()).parse().unwrap()
}

fn parse_lalrpop(parser: &lalr::ValueParser, input: &str) -> Value {
    parser.parse(input).unwrap()
}

fn bench_parse(c: &mut Criterion) {
    let lalr_parser = lalr::ValueParser::new();
    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);
    let mut group = c.benchmark_group("JSON");
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
