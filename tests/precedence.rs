mod compare;
use compare::gcc;

#[test]
fn add_operation() {
    assert_eq!(40, gcc::compile_expr("(10 + 10) * 2;"));
    assert_eq!(21, gcc::compile_expr("10 * 2 + 1;"));
    assert_eq!(21, gcc::compile_expr("1 + 10 * 2;"));
}

#[test]
fn minus_operation() {
    assert_eq!(2, gcc::compile_expr("(10 - 9) * 2;"));
    assert_eq!(19, gcc::compile_expr("10 * 2 - 1;"));
    assert_eq!(4, gcc::compile_expr("10 - 3 * 2;"));
}

#[test]
fn mul_operation() {
    assert_eq!(4, gcc::compile_expr("20 / 10 * 2;"));
    assert_eq!(24, gcc::compile_expr("10 * 2 +  2 * 2;"));
}

#[test]
fn div_operation() {
    assert_eq!(1, gcc::compile_expr("20 / 10 / 2;"));
    assert_eq!(9, gcc::compile_expr("(10 / 2) +  2 * 2;"));
}
