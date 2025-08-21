use langite::{parsing::parse_file, resolving::resolve_program};

fn main() {
    let filepath = "test.lang";
    let source = std::fs::read_to_string(filepath).unwrap();
    let items = match parse_file(filepath.into(), &source) {
        Ok(items) => items,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1)
        }
    };
    let (resolved, names) = match resolve_program(&items) {
        Ok(output) => output,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1)
        }
    };
    drop(items);
    println!("{:#?}", resolved.types);
    println!("{:#?}", resolved.functions);
    println!("{names:#?}");
}
