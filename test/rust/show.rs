use tree_sitter::{Parser, Tree, TreeCursor};
use std::process;
use std::env;
use std::io;

fn print_cursor(verbose: bool, src: &str, cursor: &mut TreeCursor, depth: usize) {
    loop {
        let node = cursor.node();
        // TODO why?
        node.end_position();
        let mut fields = vec![];
        if node.is_named() {
            fields.push(format!("\x1b[34m{}\x1b[m", node.kind().replace('\n', "\\n")))
        };
        if verbose {
            fields.push(format!(
                "\x1b[36m{}\x1b[m - \x1b[36m{}\x1b[m",
                node.start_position(),
                node.end_position()
            ));
        }
        if node.child_count() == 0 {
            let node_src = &src[node.start_byte()..node.end_byte()];
            fields.push(format!("\x1b[33m{:?}\x1b[m", node_src));
        }
        println!("{}{}", "  ".repeat(depth), fields.join(" "));
        if cursor.goto_first_child() {
            print_cursor(verbose, src, cursor, depth + 1);
            cursor.goto_parent();
        }
        if !cursor.goto_next_sibling() {
            break;
        }
    }
}

fn print_tree(verbose: bool, src: &str, tree: &Tree) {
    return print_cursor(verbose, src, &mut tree.walk(), 0);
}

fn main() {
    let mut args: Vec<String> = env::args().skip(1).collect();
    let mut verbose = false;
    if let Some(arg) = args.first() {
        if arg.as_str() == "--verbose" {
            verbose = true;
            args.remove(0);
        }
    }
    let code;

    if args.len() == 0 {
        let stdin = io::stdin();
        code = stdin.lines().map(|a| a.expect("decode error?")).collect::<Vec<String>>().join("\n");
        if code.is_empty() {
            eprintln!("Usage: show [--verbose] [string or stdin]");
            process::exit(1);
        }
    }
    else { code = args.join(" "); }

    let mut parser = Parser::new();
    parser.set_language(tree_sitter_haskell::language()).expect("Error loading grammar");

    match parser.parse(&code, None) {
        None => eprintln!("Parser timed out"),
        Some(tree) => {
            print_tree(verbose, code.as_str(), &tree);
        }
    }
}
