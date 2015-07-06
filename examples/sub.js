/*
PRE: y>0; x>0
POST: z == y - x
*/

z = y;
rem = x;

/*
 INV: z - rem == y - x
 BOUND: rem + 1
 */
while (rem > 0) {
 z = z - 1;
 rem = rem - 1;
}
