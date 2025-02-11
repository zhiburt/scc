#![allow(dead_code)]

pub mod gcc {
    pub fn compare_code(code: &str) {
        assert_eq!(compile_gcc_expr(&code), compile_code(&code));
    }

    pub fn compare_expr(expr: &str) {
        let code = format!("int main(){{ {} }}", expr);
        assert_eq!(compile_gcc_expr(&code), compile_code(&code));
    }

    pub fn compile_expr(exp: &str) -> usize {
        let code = format!("int main(){{ return {} }}", exp);
        compile_code(&code)
    }

    pub fn compile_code(code: &str) -> usize {
        use std::io::Write;

        let code_file = random_name("code_", ".c");
        let mut file = std::fs::File::create(&code_file).unwrap();
        file.write_all(code.as_bytes()).unwrap();

        let asm_file = random_name("asm_", ".s");
        let bin_file = random_name("bin_", ".out");

        let compiler = std::process::Command::new("./target/debug/simple-c-compiler")
            .arg(&code_file)
            .arg("-o")
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

    fn compile_gcc_expr(code: &str) -> usize {
        use std::io::Write;

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
        lazy_static::lazy_static! {
            static ref INDEX: std::sync::Mutex<usize> = std::sync::Mutex::new(0);
        }
        let mut i = INDEX.lock().unwrap();
        *i += 1;

        format!("{}{}{}", prefix, i, suffix)
    }
}
