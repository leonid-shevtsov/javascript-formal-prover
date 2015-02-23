/*
  PRE:
      x >= 0;
      y > 0
  POST:
      x == q * y + r;
      r < y;
      q >= 0;
      r >= 0;
*/

r = x;
q = 0;

/* INV: x == r + y * q */
while (y<r) {
    r = r-y;
    q = 1+q;
}
