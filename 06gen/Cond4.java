public class Cond4 {
    static void printInt(int i) { System.out.println(i); }
    static void test() {
	int p=1, m=-1;
     if( (p>0&&m>0)||(p<0)&&(m<0))
	printInt(9);
     else
        printInt(1);
     if( (p>0&&p>0)||(p<0)&&(p<0))
	printInt(9);
     else
        printInt(1);
     if( (m>0&&p>0)||(m<0)&&(p<0))
	printInt(9);
     else
        printInt(1);
     if( (m>0&&m>0)||(m<0)&&(m<0))
	printInt(9);
     else
        printInt(1);
    }
    public static void main(String[] args) {
	test();
    }
}