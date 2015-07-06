/*
    PRE: x >= 1;
         epsilon >= 0;
    POST: x - (root*root) < epsilon
*/

root = 1;

/* INV: root*root < x
   BOUND: x - (root*root) */
while (x - (root*root) > epsilon) {
    root = (root + x/root) / 2
}