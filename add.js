/*
PRE: y > 0; x > 0
POST: z == x + y
*/

z = x;
n = y;

/*
 INV: z + n == x + y
 BOUND: n
 */
while (n > 0) {
 z = z + 1;
 n = n - 1;
}
