#[test]
fn add_operation() {
    compile_expr("13");
}

#[test]
fn muv_operation() {}

fn compile_expr(expr: &str) -> usize {
    use std::io::Write;

    let mut code_file = std::env::temp_dir();
    code_file.push("code.c");
    let mut file = std::fs::File::create(&code_file).unwrap();
    let code = format!("int main(){{ return {}; }}", expr);
    file.write_all(code.as_bytes()).unwrap();

    let compiler_status = std::process::Command::new("./target/debug/simple-c-compiler")
        .arg(&code_file)
        .output()
        .expect("start compilation process");
    if !compiler_status.status.success() {
        println!("{:?}", code_file);
        panic!();
    }
    0
}
