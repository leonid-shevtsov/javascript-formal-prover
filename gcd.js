/*
    PRE:
        a > 0;
        b > 0;
    POST:

*/

while (a != 0) {
    if (a > b) {
        a = a - b
    } else {
        b = b - a
    }
}

gcd = b

//(defn extended-euclidean
//  "Extended euclidean algorithm.
//  Find gcd, x, y, such that GCD(a,b) = gcd = ax + by"
//  [a b]
//  (loop [r0 a
//         r1 b
//         q 0
//         s0 1
//         s1 0
//         t0 0
//         t1 1]
//    (if (zero? r1)
//      {:gcd r0, :x s0, :y t0}
//      (let [q (unchecked-divide-int r0 r1)]
//        (recur r1
//               (mod r0 r1)
//               q
//               s1
//               (- s0 (* q s1))
//               t1
//               (- t0 (* q t1)))))))