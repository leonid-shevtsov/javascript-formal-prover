/*
  PRE: b>=0;
       !(b<0) && !(b>0) => b==0
  POST: c == a+b
 */

c = a;
i = b;

/*
  INV: c + i == a + b
  BOUND: i + 1
*/
while (i>0) {
  c = c + 1;
  i = i - 1
}
