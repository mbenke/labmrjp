int printf(const char *,...);
int mymain(void);
void printInt(int n) { printf("%d\n", n);}
void printInt3(int a, int b, int c) { printf("%d %d %d\n", a, b, c);}
int main() { printInt(mymain()); return 0;}
