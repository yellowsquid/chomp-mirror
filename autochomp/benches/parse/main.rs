use std::error::Error;

use chewed::{IterWrapper, Parser};
use chomp::{
    chomp::ast::NamedExpression,
    nibble::{
        self,
        convert::{Context, Convert},
    },
};
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

const INPUTS: &[&str] = &[
    include_str!("data/12.nb"),
    include_str!("data/24.nb"),
    include_str!("data/48.nb"),
    include_str!("data/97.nb"),
    include_str!("data/194.nb"),
    include_str!("data/387.nb"),
    include_str!("data/774.nb"),
    include_str!("data/1548.nb"),
    include_str!("data/3096.nb"),

];

fn parse_autochomp(input: &str) -> Result<NamedExpression, Box<dyn Error>> {
    IterWrapper::new(input.chars())
        .parse::<autochomp::Ast>()
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|ast| {
            ast.convert(&mut Context::default())
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
}

fn parse_chomp(input: &str) -> Result<NamedExpression, Box<dyn Error>> {
    syn::parse_str::<nibble::Statement>(input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|stmt| {
            stmt.convert(&mut Context::default())
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
}

fn bench_parse(c: &mut Criterion) {
    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);
    let mut group = c.benchmark_group("Parse");
    group.plot_config(plot_config);
    for (i, input) in INPUTS.iter().enumerate() {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("Chomp", i), *input, |b, i| {
            b.iter(|| parse_chomp(i))
        });
        group.bench_with_input(BenchmarkId::new("AutoChomp", i), *input, |b, i| {
            b.iter(|| parse_autochomp(i))
        });
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
