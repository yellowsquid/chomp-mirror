use chewed::{IterWrapper, Parser};
use chomp_bench::json::*;
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

const INPUTS: &[&str] = &[
    r#"true"#,
    r#"[true, false]"#,
    r#"{"first" : null, "second" : 123}"#,
    r#"{"first": [ true, "Hello there" ], "second": [123, -12.4e-7]}"#,
    r#"{"first": [ true, "Hello there" ], "second": [123, -12.4e-7], "third": {"left": "Random text", "right": ["\ud83c\udf24\ufe0f"]}}"#,
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

fn bench_parse(c: &mut Criterion) {
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
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
