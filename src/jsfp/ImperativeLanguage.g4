grammar ImperativeLanguage;

@header { package jsfp; }

provingStructure: assertionComment commands EOF;

assertionComment: '/*' preconditions postconditions '*/';

preconditions: 'PRE:' predicates;

postconditions: 'POST:' predicates;

predicates: predicate ( SEMICOLON predicate)* SEMICOLON?;

commands: command ( SEMICOLON command)* SEMICOLON?;

loopComment: '/*' invariant  boundFunction '*/';

invariant: 'INV:' predicate;
boundFunction: 'BOUND:' expression;

command:
  identifier '=' expression # assignmentCommand
  | '{' commands  '}' # sequenceCommand
  | 'if' '(' predicate ')' command # conditionalCommand
  | 'if' '(' predicate ')' command 'else' command # fullConditionalCommand
  | loopComment 'while' '(' predicate ')' command # loopCommand
  ;

// Expression grammar

expression:
  expression MULT_OPERATOR expression # multExpression
  | expression SUM_OPERATOR expression # sumExpression
  | atom # atomExpression
  ;

atom: negAtom | numericConstant | identifier | parensExpression;

negAtom: NEG_OPERATOR atom;

parensExpression: '(' expression ')';

// Predicate grammar

predicate:
  predicate AND_OPERATOR predicate # andPredicate
  | predicate OR_OPERATOR predicate # orPredicate
  | predicate IMPLIES_OPERATOR predicate # orPredicate
  | predicateAtom # atomPredicate
  ;

notAtom: NOT_OPERATOR predicateAtom;

predicateAtom: notAtom | booleanConstant | comparison | parensPredicate;

comparison:
  expression COMPARISON_OPERATOR expression;

parensPredicate: '(' predicate ')';

// Primitives

booleanConstant: 'true' | 'false';

numericConstant: NUMBER;

identifier: ID;

COMPARISON_OPERATOR: '<' | '>' | '==' | '>=' | '<=' | '!=';

MULT_OPERATOR: MULTIPLY | DIVIDE;
MULTIPLY: '*';
DIVIDE: '/';

SUM_OPERATOR: PLUS | MINUS;
PLUS: '+';
MINUS: '-';

NEG_OPERATOR: MINUS;

AND_OPERATOR: '&&';
OR_OPERATOR: '||';
NOT_OPERATOR: '!';
IMPLIES_OPERATOR: '=>';

NUMBER: '-'?[0-9]+;

ID: [a-zA-Z_][a-zA-Z_0-9]*;

SEMICOLON: ';';

WS: ('//'  ~( '\r' | '\n' )*  | [\n\r\b\t\f ]+) -> skip;
