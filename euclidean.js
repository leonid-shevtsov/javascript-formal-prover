/*
    PRE: a>0; b>0;
         (a>0 && b>-1) => a+b>-1
    POST: a==0; b==0; gcd2==gcd; gcd>0
*/

gcd=a;
gcd2=b;

/* INV: gcd>0 && gcd2>0
   BOUND: gcd+gcd2+1 */
while (gcd!=gcd2) {
    if (gcd2>gcd) {
        gcd2 = gcd2 - gcd
    } else {
        gcd = gcd - gcd2
    }
};

// check divisibility

/* INV: a>=0
   BOUND: a+1 */
while (a!=0) {
    a = a - gcd
};

/* INV: b>=0
   BOUND: b+1 */
while (b!=0) {
    b = b - gcd
}