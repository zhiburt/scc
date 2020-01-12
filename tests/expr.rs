mod compare;
use compare::gcc;

#[test]
fn binary_operations() {
    gcc::compare_expr("return 2 + 2 + 1;");
    gcc::compare_expr("return (4 + 2) + 2;");
    gcc::compare_expr("return 4 + (2 + 2);");

    gcc::compare_expr("return 2 - 2 - 1;");
    gcc::compare_expr("return (4 - 2) - 2;");
    gcc::compare_expr("return 4 - (2 - 2);");

    gcc::compare_expr("return 2 * 2 * 1;");
    gcc::compare_expr("return (4 * 2) * 2;");
    gcc::compare_expr("return 4 * (2 * 2);");

    gcc::compare_expr("return 2 / 2 / 1;");
    gcc::compare_expr("return (4 / 2) / 2;");
    gcc::compare_expr("return 4 / (2 / 2);");

    gcc::compare_expr("return 2 % 2 % 1;");
    gcc::compare_expr("return (4 % 2) % 2;");
    gcc::compare_expr("return 4 % (2 % 4);");
}

#[test]
fn bit_operations() {
    gcc::compare_expr("return 2 >> 1;");
    gcc::compare_expr("return 5 >> 1;");
    gcc::compare_expr("return 5 >> 3;");

    gcc::compare_expr("return 2 << 1;");
    gcc::compare_expr("return 5 << 1;");
    gcc::compare_expr("return 5 << 3;");

    gcc::compare_expr("return 1 & 2;");
    gcc::compare_expr("return 2 & 2;");

    gcc::compare_expr("return 1 | 2;");
    gcc::compare_expr("return 2 | 2;");

    gcc::compare_expr("return 1 ^ 2;");
    gcc::compare_expr("return 2 ^ 2;");
}

#[test]
fn unary_operations() {
    gcc::compare_expr("return !1;");
    gcc::compare_expr("return ~1;");
    gcc::compare_expr("return -1;");
}

#[test]
fn equal_operations() {
    gcc::compare_expr("return 1 == 1;");
    gcc::compare_expr("return 1 == 2;");
    gcc::compare_expr("return 2 == 1;");

    gcc::compare_expr("return 1 != 1;");
    gcc::compare_expr("return 1 != 2;");
    gcc::compare_expr("return 2 != 1;");
}

#[test]
fn compare_operations() {
    gcc::compare_expr("return 1 < 1;");
    gcc::compare_expr("return 1 < 2;");
    gcc::compare_expr("return 2 < 1;");

    gcc::compare_expr("return 1 <= 1;");
    gcc::compare_expr("return 1 <= 2;");
    gcc::compare_expr("return 2 <= 1;");

    gcc::compare_expr("return 1 > 1;");
    gcc::compare_expr("return 1 > 2;");
    gcc::compare_expr("return 2 > 1;");

    gcc::compare_expr("return 1 >= 1;");
    gcc::compare_expr("return 1 >= 2;");
    gcc::compare_expr("return 2 >= 1;");
}

#[test]
fn increment_operations() {
    gcc::compare_expr("int a = 1; return ++a;");
    gcc::compare_expr("int a = 1; return a++;");
    gcc::compare_expr("int a = 1; a++; return a;");
    gcc::compare_expr("int a = 1; ++a; return a;");
}

#[test]
fn decrement_operations() {
    gcc::compare_expr("int a = 1; return --a;");
    gcc::compare_expr("int a = 1; return a--;");
    gcc::compare_expr("int a = 1; a--; return a;");
    gcc::compare_expr("int a = 1; --a; return a;");
}

#[test]
fn assign_operations() {
    gcc::compare_expr("int a = 1; return a += 1;");
    gcc::compare_expr("int a = 1; a += 1; return a;");

    gcc::compare_expr("int a = 1; return a -= 1;");
    gcc::compare_expr("int a = 1; a -= 1; return a;");

    gcc::compare_expr("int a = 1; return a *= 2;");
    gcc::compare_expr("int a = 1; a *= 2; return a;");

    gcc::compare_expr("int a = 10; return a /= 2;");
    gcc::compare_expr("int a = 10; a /= 2; return a;");

    gcc::compare_expr("int a = 7; return a %= 2;");
    gcc::compare_expr("int a = 7; a %= 2; return a;");

    gcc::compare_expr("int a = 2; return a |= 1;");
    gcc::compare_expr("int a = 2; a |= 1; return a;");

    gcc::compare_expr("int a = 2; return a &= 1;");
    gcc::compare_expr("int a = 2; a &= 1; return a;");

    gcc::compare_expr("int a = 2; return a ^= 1;");
    gcc::compare_expr("int a = 2; a ^= 1; return a;");
}
