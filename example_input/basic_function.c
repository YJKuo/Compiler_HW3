int foo(int a) {
   a += 6;
   return a;
}

void lol(int a) {
   print(a);
	return;
}
int add(int a, int b){
   return a+b;
}
void main(){
   int a=6;
   a = foo(4);
   lol(a);
   int b=4;
   int c = add(a,104.32);
   lol(c);
   return;
}

