mod compare_gcc {
    use super::*;

    #[test]
    fn binary_operations() {
        compare_expr("return 2 + 2 + 1;");
        compare_expr("return (4 + 2) + 2;");
        compare_expr("return 4 + (2 + 2);");

        compare_expr("return 2 - 2 - 1;");
        compare_expr("return (4 - 2) - 2;");
        compare_expr("return 4 - (2 - 2);");

        compare_expr("return 2 * 2 * 1;");
        compare_expr("return (4 * 2) * 2;");
        compare_expr("return 4 * (2 * 2);");

        compare_expr("return 2 / 2 / 1;");
        compare_expr("return (4 / 2) / 2;");
        compare_expr("return 4 / (2 / 2);");
    
        compare_expr("return 2 % 2 % 1;");
        compare_expr("return (4 % 2) % 2;");
        compare_expr("return 4 % (2 % 4);");
    }

    #[test]
    fn bit_operations() {
        compare_expr("return 2 >> 1;");
        compare_expr("return 5 >> 1;");
        compare_expr("return 5 >> 3;");

        compare_expr("return 2 << 1;");
        compare_expr("return 5 << 1;");
        compare_expr("return 5 << 3;");

        compare_expr("return 1 & 2;");
        compare_expr("return 2 & 2;");

        compare_expr("return 1 | 2;");
        compare_expr("return 2 | 2;");

        compare_expr("return 1 ^ 2;");
        compare_expr("return 2 ^ 2;");
    }

    #[test]
    fn unary_operations() {
        compare_expr("return !1;");
        compare_expr("return ~1;");
        compare_expr("return -1;");
    }

    #[test]
    fn equal_operations() {
        compare_expr("return 1 == 1;");
        compare_expr("return 1 == 2;");
        compare_expr("return 2 == 1;");

        compare_expr("return 1 != 1;");
        compare_expr("return 1 != 2;");
        compare_expr("return 2 != 1;");
    }

    #[test]
    fn compare_operations() {
        compare_expr("return 1 < 1;");
        compare_expr("return 1 < 2;");
        compare_expr("return 2 < 1;");

        compare_expr("return 1 <= 1;");
        compare_expr("return 1 <= 2;");
        compare_expr("return 2 <= 1;");

        compare_expr("return 1 > 1;");
        compare_expr("return 1 > 2;");
        compare_expr("return 2 > 1;");

        compare_expr("return 1 >= 1;");
        compare_expr("return 1 >= 2;");
        compare_expr("return 2 >= 1;");
    }

    #[test]
    fn increment_operations() {
        compare_expr("int a = 1; return ++a;");
        compare_expr("int a = 1; return a++;");
        compare_expr("int a = 1; a++; return a;");
        compare_expr("int a = 1; ++a; return a;");
    }

    #[test]
    fn decrement_operations() {
        compare_expr("int a = 1; return --a;");
        compare_expr("int a = 1; return a--;");
        compare_expr("int a = 1; a--; return a;");
        compare_expr("int a = 1; --a; return a;");
    }
}

fn compare_expr(expr: &str) {
    assert_eq!(compile_gcc_expr(expr), compile_expr(expr));
}

fn compile_expr(expr: &str) -> usize {
    use std::io::Write;

    let code = format!("int main(){{ {} }}", expr);

    let code_file = random_name("code_", ".c");
    let mut file = std::fs::File::create(&code_file).unwrap();
    file.write_all(code.as_bytes()).unwrap();

    let asm_file = random_name("asm_", ".s");
    let bin_file = random_name("bin_", ".out");


    let compiler = std::process::Command::new("./target/debug/simple-c-compiler")
        .arg(&code_file)
        .arg(&asm_file)
        .output()
        .expect("start compilation process");
    if !compiler.status.success() {
        println!("{:?}", code_file);
        panic!();
    }

    let gcc = std::process::Command::new("gcc")
        .args(&["-m64", "-o", &bin_file, &asm_file])
        .output()
        .expect("Run gcc to compile asm")
        .status;

    if !gcc.success() {
        println!("{:?}", std::env::current_dir());
        panic!();
    }

    let program = std::process::Command::new(format!("./{}", bin_file))
        .output()
        .expect("Run compiled programm")
        .status;

    std::fs::remove_file(code_file).unwrap();
    std::fs::remove_file(asm_file).unwrap();
    std::fs::remove_file(bin_file).unwrap();

    program.code().unwrap() as usize
}

fn compile_gcc_expr(expr: &str) -> usize {
    use std::io::Write;

    let code = format!("int main(){{ {} }}", expr);

    let code_file = random_name("code_", ".c");
    let mut file = std::fs::File::create(&code_file).unwrap();
    file.write_all(code.as_bytes()).unwrap();

    let bin_file = random_name("bin_", ".out");

    let gcc = std::process::Command::new("gcc")
        .args(&["-m64", "-o", &bin_file, &code_file])
        .output()
        .expect("Run gcc to compile asm")
        .status;

    if !gcc.success() {
        println!("{:?}", std::env::current_dir());
        panic!();
    }

    let program = std::process::Command::new(format!("./{}", bin_file))
        .output()
        .expect("Run compiled programm")
        .status;

    std::fs::remove_file(code_file).unwrap();
    std::fs::remove_file(bin_file).unwrap();

    program.code().unwrap() as usize
}

fn random_name(prefix: &str, suffix: &str) -> String {
    lazy_static::lazy_static!{
        static ref INDEX: std::sync::Mutex<usize> = std::sync::Mutex::new(0);
    }
    let mut i = INDEX.lock().unwrap();
    *i += 1;

    format!("{}{}{}", prefix, i, suffix)
}