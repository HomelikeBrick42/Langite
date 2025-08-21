use langite::{parsing::parse_file, resolving::resolve_program};

fn main() {
    let filepath = "test.lang";
    let source = std::fs::read_to_string(filepath).unwrap();
    let items = parse_file(filepath.into(), &source).unwrap_or_else(|error| {
        eprintln!("{error}");
        std::process::exit(1)
    });
    let (resolved, names) = resolve_program(&items).unwrap_or_else(|error| {
        eprintln!("{error}");
        std::process::exit(1)
    });
    drop(items);
    println!("{:#?}", resolved.types);
    println!("{:#?}", resolved.functions);
    println!("{names:#?}");
}
