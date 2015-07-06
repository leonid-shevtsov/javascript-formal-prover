# Formal proof of certain programs in the Javascript programming language

A graduate thesis by [Leonid Shevtsov](http://leonid.shevtsov.me), completed at the Dnipropetrovsk National University's Computer Technology chair, under the supervision of [Alexander Khizha](http://khizha.dp.ua)

## Abstract

This thesis is concerned with developing an automatic system for sound formal proofs of programs written in a certain imperative subset of Javascript with logical annotations.

[Text supplement (in Ukrainian)](http://leonid.shevtsov.me/en/javascript-formal-prover)

## Usage

Binary (requires Java): download the binary (.jar) from the project's Releases page, then

```java -jar javascript-formal-prover.jar <script-to-prove.js>```

From source: [install Leiningen](http://leiningen.org/#install), clone the project, go to the directory and run

```lein run <script-to-prove.js>```

## Examples

See `/examples` for source files that can be proven.

* * *

(c) 2015 Leonid Shevtsov under the MIT License

