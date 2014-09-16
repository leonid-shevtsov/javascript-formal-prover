/*
  PRE: true
  POST:
      y > r;
      x == r + y * q
*/

r = x;
q = 0;

while (y<r) {
    r = r-y;
    q = 1+q;
}
