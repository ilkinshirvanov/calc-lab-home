#define _POSIX_C_SOURCE 200809L
/*

Name: Ilkin Shirvanov

GitHub: https://github.com/ilkinshirvanov/calc-lab-home

Compile: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm

Resources: course assignment and C standard library documentation.

AI tools used: OpenAI ChatGPT. I used ChatGPT to help understand

the requirements, plan the parser, and debug the implementation.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdint.h>
#include <sys/types.h>
/* Parser values retain their type so integer arithmetic stays exact. */
typedef struct {
int is_float;
long long i;
double d;
} Value;

typedef struct {
const char *text;
size_t len;
size_t pos;
size_t error;
} Parser;

static void set_error(Parser *p, size_t pos)
{
if (p->error == 0) {
p->error = pos;
}
}

static void skip_spaces(Parser *p)
{
while (p->pos < p->len &&
(p->text[p->pos] == ' ' || p->text[p->pos] == '\t' ||
p->text[p->pos] == '\r')) {
p->pos++;
}
}

static Value integer_value(long long n)
{
Value v = {0, n, (double)n};
return v;
}

static Value float_value(double n)
{
Value v = {1, 0, n};
return v;
}

/* Forward declarations encode the precedence levels. */
static Value parse_expression(Parser *p);
static Value parse_add_sub(Parser *p);
static Value parse_mul_div(Parser *p);
static Value parse_power(Parser *p);
static Value parse_primary(Parser *p);
static Value parse_primary(Parser *p)
{
skip_spaces(p);
size_t start = p->pos;

if (p->pos >= p->len) {
    set_error(p, p->len + 1);
    return integer_value(0);
}

if (p->text[p->pos] == '(') {
    p->pos++;
    Value v = parse_expression(p);
    skip_spaces(p);

    if (p->pos >= p->len || p->text[p->pos] != ')') {
        set_error(p, p->len + 1);
        return v;
    }

    p->pos++;
    return v;
}

if (!isdigit((unsigned char)p->text[p->pos])) {
    set_error(p, p->pos + 1);
    return integer_value(0);
}

long long integer_part = 0;
while (p->pos < p->len &&
       isdigit((unsigned char)p->text[p->pos])) {
    int digit = p->text[p->pos] - '0';

    if (integer_part > (LLONG_MAX - digit) / 10) {
        set_error(p, start + 1);
        return integer_value(0);
    }

    integer_part = integer_part * 10 + digit;
    p->pos++;
}

if (p->pos < p->len && p->text[p->pos] == '.') {
    p->pos++;
    if (p->pos >= p->len ||
        !isdigit((unsigned char)p->text[p->pos])) {
        set_error(p, p->pos);
        return integer_value(0);
    }

    while (p->pos < p->len &&
           isdigit((unsigned char)p->text[p->pos])) {
        p->pos++;
    }

    size_t count = p->pos - start;
    char *number = malloc(count + 1);
    if (number == NULL) {
        set_error(p, start + 1);
        return integer_value(0);
    }

    memcpy(number, p->text + start, count);
    number[count] = '\0';

    errno = 0;
    char *endptr = NULL;
    double result = strtod(number, &endptr);

    free(number);

    if (errno == ERANGE || !isfinite(result) || endptr == NULL) {
        set_error(p, start + 1);
        return integer_value(0);
    }

    return float_value(result);
}

return integer_value(integer_part);
}
static Value parse_power(Parser *p)
{
Value left = parse_primary(p);
skip_spaces(p);

if (p->pos + 1 < p->len &&
    p->text[p->pos] == '*' && p->text[p->pos + 1] == '*') {
    size_t op_pos = p->pos;
    p->pos += 2;

    skip_spaces(p);
    size_t exponent_pos = p->pos;

    Value right = parse_power(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    if (right.is_float) {
        double result = pow(left.is_float ? left.d : (double)left.i,
                            right.d);
        if (!isfinite(result)) {
            set_error(p, op_pos + 1);
            return integer_value(0);
        }
        return float_value(result);
    }

    if (right.i < 0) {
        set_error(p, exponent_pos + 1);
        return integer_value(0);
    }

    if (!left.is_float) {
        long long result = 1;
        long long base = left.i;
        unsigned long long exp = (unsigned long long)right.i;

        while (exp > 0) {
            if (exp & 1U) {
                if (base != 0 &&
                    (result > LLONG_MAX / base ||
                     result < LLONG_MIN / base)) {
                    set_error(p, op_pos + 1);
                    return integer_value(0);
                }
                result *= base;
            }

            exp >>= 1U;
            if (exp > 0) {
                if (base != 0 &&
                    (base > LLONG_MAX / base ||
                     base < LLONG_MIN / base)) {
                    set_error(p, op_pos + 1);
                    return integer_value(0);
                }
                base *= base;
            }
        }

        return integer_value(result);
    }

    double result = pow(left.d, (double)right.i);
    if (!isfinite(result)) {
        set_error(p, op_pos + 1);
        return integer_value(0);
    }

    return float_value(result);
}

return left;

}
static Value parse_mul_div(Parser *p)
{
Value left = parse_power(p);

while (p->error == 0) {
    skip_spaces(p);

    if (p->pos >= p->len ||
        p->text[p->pos] == ')' ||
        p->text[p->pos] == '+' ||
        p->text[p->pos] == '-') {
        break;
    }

    if (p->text[p->pos] != '*' && p->text[p->pos] != '/') {
        break;
    }

    size_t op_pos = p->pos;
    char op = p->text[p->pos++];

    /* A second operator is invalid; ** is handled at the power level. */
    if (op == '*' && p->pos < p->len && p->text[p->pos] == '*') {
        p->pos = op_pos;
        break;
    }

    skip_spaces(p);
    size_t right_pos = p->pos;
    Value right = parse_power(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    if (op == '/') {
        if ((right.is_float && right.d == 0.0) ||
            (!right.is_float && right.i == 0)) {
            set_error(p, right_pos + 1);
            return integer_value(0);
        }

        if (left.is_float || right.is_float) {
            left = float_value((left.is_float ? left.d : (double)left.i) /
                               (right.is_float ? right.d : (double)right.i));
        } else {
            left = integer_value(left.i / right.i);
        }
    } else {
        if (left.is_float || right.is_float) {
            left = float_value((left.is_float ? left.d : (double)left.i) *
                               (right.is_float ? right.d : (double)right.i));
        } else {
            if (right.i != 0 &&
                (left.i > LLONG_MAX / right.i ||
                 left.i < LLONG_MIN / right.i)) {
                set_error(p, op_pos + 1);
                return integer_value(0);
            }
            left = integer_value(left.i * right.i);
        }
    }

    if (left.is_float && !isfinite(left.d)) {
        set_error(p, op_pos + 1);
        return integer_value(0);
    }
}

return left;

}
static Value parse_add_sub(Parser *p)
{
Value left = parse_mul_div(p);

while (p->error == 0) {
    skip_spaces(p);

    if (p->pos >= p->len ||
        p->text[p->pos] == ')') {
        break;
    }

    char op = p->text[p->pos];
    if (op != '+' && op != '-') {
        break;
    }

    size_t op_pos = p->pos;
    p->pos++;

    skip_spaces(p);
    Value right = parse_mul_div(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    if (left.is_float || right.is_float) {
        double a = left.is_float ? left.d : (double)left.i;
        double b = right.is_float ? right.d : (double)right.i;
        double result = (op == '+') ? a + b : a - b;

        if (!isfinite(result)) {
            set_error(p, op_pos + 1);
            return integer_value(0);
        }

        left = float_value(result);
    } else {
        if (op == '+') {
            if ((right.i > 0 && left.i > LLONG_MAX - right.i) ||
                (right.i < 0 && left.i < LLONG_MIN - right.i)) {
                set_error(p, op_pos + 1);
                return integer_value(0);
            }
            left = integer_value(left.i + right.i);
        } else {
            if ((right.i < 0 && left.i > LLONG_MAX + right.i) ||
                (right.i > 0 && left.i < LLONG_MIN + right.i)) {
                set_error(p, op_pos + 1);
                return integer_value(0);
            }
            left = integer_value(left.i - right.i);
        }
    }
}

return left;

}

static Value parse_expression(Parser *p)
{
return parse_add_sub(p);
}
static int evaluate_line(const char *text, size_t len, Value *result,
size_t *error)
{
Parser p = {text, len, 0, 0};

skip_spaces(&p);

if (p.pos >= p.len) {
    *error = 0;
    return 1;
}

*result = parse_expression(&p);
skip_spaces(&p);

if (p.error == 0 && p.pos < p.len) {
    set_error(&p, p.pos + 1);
}

*error = p.error;
return p.error == 0;

}


static void print_value(FILE *output, Value v)
{
    if (v.is_float) {
        fprintf(output, "%.15g\n", v.d);
    } else {
        fprintf(output, "%lld\n", v.i);
    }
}

static int process_stream(FILE *input, FILE *output)
{
    char *line = NULL;
    size_t capacity = 0;
    ssize_t length;
    size_t global_pos = 1;
    FILE *temporary = tmpfile();

    if (temporary == NULL) {
        perror("tmpfile");
        return 1;
    }

    while ((length = getline(&line, &capacity, input)) != -1) {
        size_t len = (size_t)length;
        size_t first = 0;

        while (first < len &&
               (line[first] == ' ' || line[first] == '\t' ||
                line[first] == '\r')) {
            first++;
        }

        if (first == len || line[first] == '\n' || line[first] == '#') {
            global_pos += len;
            continue;
        }

        size_t expression_len = len;
        if (expression_len > 0 && line[expression_len - 1] == '\n')
            expression_len--;

        Value result = integer_value(0);
        size_t error = 0;

        if (!evaluate_line(line, expression_len, &result, &error)) {
            fprintf(output, "ERROR:%zu\n", global_pos + error - 1);
            free(line);
            fclose(temporary);
            return 1;
        }

        print_value(temporary, result);
        global_pos += len;
    }

    free(line);

    if (ferror(input)) {
        perror("input");
        fclose(temporary);
        return 1;
    }

    rewind(temporary);
    {
        char buffer[4096];
        size_t n;
        while ((n = fread(buffer, 1, sizeof buffer, temporary)) > 0) {
            if (fwrite(buffer, 1, n, output) != n) {
                perror("output");
                fclose(temporary);
                return 1;
            }
        }
    }

    fclose(temporary);
    return 0;
}

static int make_directory(const char *path)
{
    struct stat st;

    if (stat(path, &st) == 0)
        return S_ISDIR(st.st_mode);

    if (mkdir(path, 0777) == 0)
        return 1;

    return 0;
}

static char *join_path(const char *dir, const char *name)
{
    size_t a = strlen(dir), b = strlen(name);
    int slash = (a > 0 && dir[a - 1] != '/');
    char *p = malloc(a + (size_t)slash + b + 1);

    if (!p)
        return NULL;

    memcpy(p, dir, a);
    if (slash)
        p[a++] = '/';
    memcpy(p + a, name, b + 1);
    return p;
}

static int ends_txt(const char *name)
{
    size_t n = strlen(name);
    return n >= 4 && strcmp(name + n - 4, ".txt") == 0;
}

static int process_directory(const char *indir, const char *outdir)
{
    DIR *d = opendir(indir);
    struct dirent *entry;
    int failed = 0;

    if (!d) {
        perror(indir);
        return 1;
    }

    if (!make_directory(outdir)) {
        fprintf(stderr, "Cannot create output directory: %s\n", outdir);
        closedir(d);
        return 1;
    }

    while ((entry = readdir(d)) != NULL) {
        char *inpath, *outpath, *base;
        FILE *in, *out;
        size_t n;
        int rc;

        if (!ends_txt(entry->d_name))
            continue;

        inpath = join_path(indir, entry->d_name);
        if (!inpath) {
            failed = 1;
            break;
        }

        {
            struct stat st;
            if (stat(inpath, &st) != 0 || !S_ISREG(st.st_mode)) {
                free(inpath);
                continue;
            }
        }

        n = strlen(entry->d_name);
        base = malloc(n - 3);
        if (!base) {
            free(inpath);
            failed = 1;
            break;
        }
        memcpy(base, entry->d_name, n - 4);
        base[n - 4] = '\0';

        {
            size_t need = strlen(base) + strlen("_ilkin_shirvanov_251ADB133.txt") + 1;
            char *outname = malloc(need);
            if (!outname) {
                free(base);
                free(inpath);
                failed = 1;
                break;
            }
            snprintf(outname, need, "%s_ilkin_shirvanov_251ADB133.txt", base);
            outpath = join_path(outdir, outname);
            free(outname);
        }
        free(base);

        if (!outpath) {
            free(inpath);
            failed = 1;
            break;
        }

        in = fopen(inpath, "r");
        if (!in) {
            perror(inpath);
            free(inpath);
            free(outpath);
            failed = 1;
            continue;
        }

        out = tmpfile();
        if (!out) {
            perror("tmpfile");
            fclose(in);
            free(inpath);
            free(outpath);
            failed = 1;
            continue;
        }

        rc = process_stream(in, out);
        fclose(in);
        free(inpath);

        if (rc != 0) {
            rewind(out);
            {
                char buffer[256];
                size_t got;
                FILE *dest = fopen(outpath, "w");
                if (!dest) {
                    perror(outpath);
                    failed = 1;
                } else {
                    while ((got = fread(buffer, 1, sizeof buffer, out)) > 0)
                        fwrite(buffer, 1, got, dest);
                    fclose(dest);
                }
            }
        } else {
            FILE *dest = fopen(outpath, "w");
            if (!dest) {
                perror(outpath);
                failed = 1;
            } else {
                rewind(out);
                {
                    char buffer[4096];
                    size_t got;
                    while ((got = fread(buffer, 1, sizeof buffer, out)) > 0) {
                        if (fwrite(buffer, 1, got, dest) != got) {
                            failed = 1;
                            break;
                        }
                    }
                }
                if (fclose(dest) != 0)
                    failed = 1;
            }
        }

        fclose(out);
        free(outpath);
    }

    closedir(d);
    return failed;
}


static const char *path_basename(const char *path)
{
    const char *end = path + strlen(path);
    while (end > path && end[-1] == '/')
        end--;
    const char *begin = end;
    while (begin > path && begin[-1] != '/')
        begin--;
    size_t n = (size_t)(end - begin);
    static char name[4096];
    if (n >= sizeof name)
        n = sizeof name - 1;
    memcpy(name, begin, n);
    name[n] = '\0';
    return name;
}

static int process_one_file(const char *input_path, const char *outdir)
{
    FILE *in = fopen(input_path, "r");
    if (!in) {
        perror(input_path);
        return 1;
    }

    if (!make_directory(outdir)) {
        fprintf(stderr, "Cannot create output directory: %s\n", outdir);
        fclose(in);
        return 1;
    }

    const char *filename = path_basename(input_path);
    size_t n = strlen(filename);
    if (n < 4 || strcmp(filename + n - 4, ".txt") != 0) {
        fprintf(stderr, "Input file must end in .txt: %s\n", input_path);
        fclose(in);
        return 1;
    }

    size_t base_len = n - 4;
    size_t name_len = base_len + sizeof "_ilkin_shirvanov_251ADB133.txt";
    char *outname = malloc(name_len);
    if (!outname) {
        fclose(in);
        return 1;
    }

    snprintf(outname, name_len, "%.*s_ilkin_shirvanov_251ADB133.txt",
             (int)base_len, filename);
    char *outpath = join_path(outdir, outname);
    free(outname);
    if (!outpath) {
        fclose(in);
        return 1;
    }

    FILE *tmp = tmpfile();
    if (!tmp) {
        perror("tmpfile");
        fclose(in);
        free(outpath);
        return 1;
    }

    (void)process_stream(in, tmp);
    fclose(in);
    rewind(tmp);

    FILE *out = fopen(outpath, "w");
    if (!out) {
        perror(outpath);
        fclose(tmp);
        free(outpath);
        return 1;
    }

    char buffer[4096];
    size_t got;
    int failed = 0;
    while ((got = fread(buffer, 1, sizeof buffer, tmp)) > 0) {
        if (fwrite(buffer, 1, got, out) != got) {
            failed = 1;
            break;
        }
    }
    if (ferror(tmp))
        failed = 1;
    if (fclose(out) != 0)
        failed = 1;

    fclose(tmp);
    free(outpath);
    return failed;
}

int main(int argc, char **argv)
{
    const char *input_path = NULL;
    const char *indir = NULL;
    const char *outdir = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            printf("Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
            return 0;
        } else if (strcmp(argv[i], "-d") == 0 ||
                   strcmp(argv[i], "--dir") == 0) {
            if (++i >= argc || indir || input_path) {
                fprintf(stderr, "Invalid or repeated directory option.\n");
                return 1;
            }
            indir = argv[i];
        } else if (strcmp(argv[i], "-o") == 0 ||
                   strcmp(argv[i], "--output-dir") == 0) {
            if (++i >= argc || outdir) {
                fprintf(stderr, "Invalid or repeated output directory option.\n");
                return 1;
            }
            outdir = argv[i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else if (input_path) {
            fprintf(stderr, "Only one input file is allowed.\n");
            return 1;
        } else {
            input_path = argv[i];
        }
    }

    if (indir && input_path) {
        fprintf(stderr, "Cannot combine -d with an input file.\n");
        return 1;
    }

    if (!indir && !input_path) {
        fprintf(stderr, "Usage: calc [-d DIR] [-o OUTDIR] [input.txt]\n");
        return 1;
    }

    char default_dir[8192];

    if (indir) {
        if (!outdir) {
            const char *base = path_basename(indir);
            if (snprintf(default_dir, sizeof default_dir,
                         "%s_ilkin_251ADB133", base) >= (int)sizeof default_dir) {
                fprintf(stderr, "Directory path too long.\n");
                return 1;
            }
            outdir = default_dir;
        }
        return process_directory(indir, outdir);
    }

    if (!outdir) {
        const char *base = path_basename(input_path);
        size_t n = strlen(base);
        if (n >= 4 && strcmp(base + n - 4, ".txt") == 0)
            n -= 4;
        if (snprintf(default_dir, sizeof default_dir,
                     "%.*s_ilkin_251ADB133", (int)n, base)
            >= (int)sizeof default_dir) {
            fprintf(stderr, "Directory path too long.\n");
            return 1;
        }
        outdir = default_dir;
    }

    return process_one_file(input_path, outdir);
}
