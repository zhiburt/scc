mod compare_gcc {
    use super::*;

    mod statements {
        use super::*;

        #[test]
        fn if_statement() {
            compare_expr(r"
                int a = 10;
                if (a > 0) return 10;
                else
                    return 20;
            ");

            compare_expr(r"
                int a = 10;
                if (a > 0) return 10;
                return 20;
            ");

            compare_expr(r"
                int a = 10;
                if (a > 0) return 10;
            ");

            compare_expr(r"
                int a = 10;
                if (a > 0) { a = 20; }
                return 20;
            ");

            compare_expr(r"
                int a = -1;
                if (a > 0) {
                    a = 20;
                } else if (a < 0) {
                    a = -10;
                } else {
                    a = 0;
                }

                return a;
            ");

            compare_expr(r"
                if (1) {
                    return 201;
                } else if (0) {
                    return 10;
                } else {
                }
            ");
        }

        #[test]
        fn while_statement() {
            compare_expr(r"
                int i = 0;
                while(i < 10) {
                    i++;
                }

                return i;
            ");

            compare_expr(r"
                int i = 0;
                while(i < 10) i++;

                return i;
            ");

            compare_expr(r"
                int sum = 0;
                int i = 0;
                while(sum < 100) {
                    int i = 0;
                    while(i < 10) {
                        i++;
                    }
                    sum += i;
                }

                return i;
            ");

            compare_expr(r"
                int sum = 0;
                int i = 0;
                while(i++ < 10) {
                    sum += i;
                }

                return i;
            ");

            compare_expr(r"
                int sum = 0;
                int i = 0;
                while(1) {
                    if(i == 10)
                        break;

                    sum += i++;
                }

                return i;
            ");
        }

        #[test]
        fn for_statement() {
            compare_expr(r"
                int sum = 0;
                for(int i = 0; i < 10; i++)
                    sum++;

                return sum;
            ");

            compare_expr(r"
                int sum = 0;
                for(int i = 0; i < 10; i++){
                    sum++;
                }

                return sum;
            ");

            compare_expr(r"
                int sum = 0;
                for(int i = 0; i < 10; i++)
                    for(int i = 0; i < 10; i++)
                        sum += i;

                return sum;
            ");

            compare_expr(r"
                int sum = 0;
                int i = 2;
                for(int i = 0; i < 10; i++){
                    int i = 11;
                    sum += i;
                }

                return sum;
            ");

            compare_expr(r"
                int i;
                for(i = 0; i < 10; i++)
                    ;
                
                return i;
            ");

            compare_expr(r"
                int i = 0;
                for(;i < 10;)
                    i++;
                
                return i;
            ");

            compare_expr(r"
                int i = 0;
                for(;;)
                    if(i < 10)
                        i++;
                    else
                        break;
                
                return i;
            ");
        }

        #[test]
        fn continue_statement() {
            compare_expr(r"
                int sum = 0;
                for(int i = 0; i < 10; i++)
                    if(i % 2 == 0)
                        sum += i;
                    else
                        continue;

                return sum;
            ");

            compare_expr(r"
                int sum = 0;
                for(int i = 1; i < 10; i++)
                    for(int j = 1; j < 10; j++)
                        if(j % i == 0)
                            sum += j + i;
                        else
                            continue;

                return sum;
            ");
        }

        #[test]
        fn break_statement() {
            compare_expr(r"
                int sum = 0;
                for(int i = 0; i < 10; i++)
                    if(i % 2 == 0)
                        sum += i;
                    else
                        break;

                return sum;
            ");

            compare_expr(r"
                int sum = 0;
                for(int i = 1; i < 10; i++)
                    for(int j = 1; j < 10; j++)
                        if(j % i == 0)
                            sum += j + i;
                        else
                            break;

                return sum;
            ");
        }
    }
}

fn compare_code(code: &str) {
    assert_eq!(compile_gcc_expr(&code), compile_expr(&code));
}

fn compare_expr(expr: &str) {
    let code = format!("int main(){{ {} }}", expr);
    assert_eq!(compile_gcc_expr(&code), compile_expr(&code));
}

fn compile_expr(code: &str) -> usize {
    use std::io::Write;

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
    lazy_static::lazy_static!{
        static ref INDEX: std::sync::Mutex<usize> = std::sync::Mutex::new(0);
    }
    let mut i = INDEX.lock().unwrap();
    *i += 1;

    format!("{}{}{}", prefix, i, suffix)
}