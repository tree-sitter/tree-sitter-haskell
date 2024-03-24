use tree_sitter::{Parser, Tree, TreeCursor};
use std::process;
use std::env;
use std::fs;

fn print_cursor(enabled: bool, src: &str, cursor: &mut TreeCursor, depth: usize) {
    loop {
        let node = cursor.node();
        node.end_position();
        let formatted_node = format!(
            "\x1b[34m{}\x1b[m \x1b[36m{}\x1b[m - \x1b[36m{}\x1b[m",
            node.kind().replace('\n', "\\n"),
            node.start_position(),
            node.end_position()
        );
        let enable = enabled || node.is_error();
        if enable {
            if node.child_count() == 0 {
                let node_src = &src[node.start_byte()..node.end_byte()];
                println!("{}{} \x1b[33m{:?}\x1b[m", "  ".repeat(depth), formatted_node, node_src);
            } else {
                println!("{}{}", "  ".repeat(depth), formatted_node,);
            }
        }
        if cursor.goto_first_child() {
            print_cursor(enable, src, cursor, depth + 1);
            cursor.goto_parent();
        }
        if !cursor.goto_next_sibling() {
            break;
        }
    }
}

fn print_tree(enabled: bool, src: &str, tree: &Tree) {
    return print_cursor(enabled, src, &mut tree.walk(), 0);
}

fn parse_file(parser: &mut Parser, dump: bool, dump_error: bool, total_count: usize, index: &usize, path: &String) -> bool {
    if !dump {
        print!("\x1b[1K\r\x1b[35m>>>\x1b[m Parsing file \x1b[33m{index}/{total_count}\x1b[m...");
    }
    match fs::read_to_string(path) {
        Err(err) => eprintln!("Invalid path {path}: {err}"),
        Ok(code) => {
            match parser.parse(&code, None) {
                None => eprintln!("Parser timed out for {path}"),
                Some(tree) => {
                    let failed = tree.root_node().has_error();
                    let result =
                        if failed { "\x1b[31mParse errors!\x1b[m" }
                        else { "\x1b[32mSuccess!\x1b[m" };
                    if dump || failed {
                        println!("\x1b[35m>>>\x1b[m \x1b[34m{path}\x1b[m: {result}\n");
                        print_tree(dump || dump_error, code.as_str(), &tree);
                    }
                    return failed;
                }
            }
        },
    }
    return true;
}

fn main() {
    let mut args: Vec<String> = env::args().skip(1).collect();
    let mut dump = false;
    let mut dump_error = false;
    if let Some(arg) = args.first() {
        if arg.as_str() == "--dump" {
            dump = true;
            args.remove(0);
        }
        else if arg.as_str() == "--dump-error" {
            dump_error = true;
            args.remove(0);
        }
    }
    let total_count = args.len();

    if total_count == 0 {
        eprintln!("Usage: parse [--dump] file [file...]");
        process::exit(1);
    }

    let mut parser = Parser::new();
    parser.set_language(tree_sitter_haskell::language()).expect("Error loading grammar");

    let (failures, successes): (Vec<(usize, &String)>, Vec<(usize, &String)>) =
                                args
                                .iter()
                                .enumerate()
                                .partition(|(index, path)| parse_file(&mut parser, dump, dump_error, total_count, &(index + 1), &path));
    if !dump { println!(""); }
    let success_count = successes.len();
    let pct: f64 = 100.0 * success_count as f64 / total_count as f64;

    if failures.len() > 0 {
        println!("\x1b[35m>>>\x1b[m Files with parse errors:");
        for (_, path) in failures {
            println!("  \x1b[33m*\x1b[m \x1b[34m{path}\x1b[m");
        }
        println!("");
    }

    println!("\x1b[35m>>>\x1b[m {pct:.1}% success ({success_count}/{total_count})");
}
