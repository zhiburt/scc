#[test]
fn add_operation() {
    assert_eq!(10, compile_expr("return 10;"));
    assert_eq!(40, compile_expr("return (10 + 10) * 2;"));
    assert_eq!(1, compile_expr("return 10 * 2 && 1 + 1;"));
}

fn compile_expr(expr: &str) -> usize {
    use std::io::Write;

    let mut code_file = std::env::temp_dir();
    code_file.push("code.c");
    let mut file = std::fs::File::create(&code_file).unwrap();
    let code = format!("int main(){{ {} }}", expr);
    file.write_all(code.as_bytes()).unwrap();

    let compiler = std::process::Command::new("./target/debug/simple-c-compiler")
        .arg(&code_file)
        .output()
        .expect("start compilation process");
    if !compiler.status.success() {
        println!("{:?}", code_file);
        panic!();
    }

    let gcc = std::process::Command::new("gcc")
        .args(&["-m64", "-o", "a.out", "assembly.s"])
        .spawn()
        .expect("Run gcc to compile asm")
        .wait()
        .expect("Waiting for gcc");
    if !gcc.success() {
        println!("{:?}", std::env::current_dir());
        panic!();
    }

    let program = std::process::Command::new("./a.out").spawn()
        .expect("Run compiled programm")
        .wait()
        .expect("Waiting for programm");

    std::fs::remove_file("assembly.s").unwrap();
    std::fs::remove_file(code_file).unwrap();
    std::fs::remove_file("a.out").unwrap();

    program.code().unwrap() as usize
}
