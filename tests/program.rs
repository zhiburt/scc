mod compare;
use compare::gcc;

#[test]
fn if_statement() {
    gcc::compare_expr(r"
        int a = 10;
        if (a > 0) return 10;
        else
            return 20;
    ");

    gcc::compare_expr(r"
        int a = 10;
        if (a > 0) return 10;
        return 20;
    ");

    gcc::compare_expr(r"
        int a = 10;
        if (a > 0) return 10;
    ");

    gcc::compare_expr(r"
        int a = 10;
        if (a > 0) { a = 20; }
        return 20;
    ");

    gcc::compare_expr(r"
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

    gcc::compare_expr(r"
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
    gcc::compare_expr(r"
        int i = 0;
        while(i < 10) {
            i++;

        return i;
    ");

    gcc::compare_expr(r"
        int i = 0;
        while(i < 10) i++;

        return i;
    ");

    gcc::compare_expr(r"
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

    gcc::compare_expr(r"
        int sum = 0;
        int i = 0;
        while(i++ < 10) {
            sum += i;
        }
        return i;
    ");

    gcc::compare_expr(r"
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
    gcc::compare_expr(r"
        int sum = 0;
        for(int i = 0; i < 10; i++)
            sum++;
        return sum;
    ");

    gcc::compare_expr(r"
        int sum = 0;
        for(int i = 0; i < 10; i++){
            sum++;
        }
        return sum;
    ");

    gcc::compare_expr(r"
        int sum = 0;
        for(int i = 0; i < 10; i++)
            for(int i = 0; i < 10; i++)
                sum += i;
        return sum;
    ");

    gcc::compare_expr(r"
        int sum = 0;
        int i = 2;
        for(int i = 0; i < 10; i++){
            int i = 11;
            sum += i;
        }
        return sum;
    ");

    gcc::compare_expr(r"
        int i;
        for(i = 0; i < 10; i++)
            ;
        return i;
    ");

    gcc::compare_expr(r"
        int i = 0;
        for(;i < 10;)
            i++;            
        return i;
    ");

    gcc::compare_expr(r"
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
    gcc::compare_expr(r"
        int sum = 0;
        for(int i = 0; i < 10; i++)
            if(i % 2 == 0)
                sum += i;
            else
                continue;
        return sum;
    ");

    gcc::compare_expr(r"
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
    gcc::compare_expr(
        r"
                int sum = 0;
                for(int i = 0; i < 10; i++)
                    if(i % 2 == 0)
                        sum += i;
                    else
                        break;

                return sum;
            ",
    );

    gcc::compare_expr(r"
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

#[test]
fn simple_fn() {
    gcc::compare_code(r"
        int add(int a, int b) {
            return a + b;
        }

        int main() {
            add(31, 12);
        }
    ");
}

#[test]
fn decl_fn() {
    gcc::compare_code(r"
        int add(int a, int b);
        
        int main() {
            add(31, 12);
        }

        int add(int a, int b) {
            return a + b;
        }
    ");

    gcc::compare_code(r"
        int add(int a, int b) {
            return a + b;
        }

        int main() {
            add(31, 12);
        }

        int add(int a, int b);
    ");
}

#[test]
fn recursive() {
    gcc::compare_code(r"
        int fib(int n) {
            if (n == 0 || n == 1) {
                return n;
            } else {
                return fib(n - 1) + fib(n - 2);
            }
        }

        int main() {
            int n = 6;
            return fib(n);
        }
    ");
}

#[test]
fn libc_call() {
    gcc::compare_code(r"
        int putchar(int c);
        
        int main() {
            putchar(72);
            putchar(101);
            putchar(108);
            putchar(108);
            putchar(111);
            putchar(44);
            putchar(32);
            putchar(87);
            putchar(111);
            putchar(114);
            putchar(108);
            putchar(100);
            putchar(33);
            putchar(10);
        }
    ");
}
