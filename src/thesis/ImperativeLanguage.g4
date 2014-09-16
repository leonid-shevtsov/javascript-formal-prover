grammar ImperativeLanguage;

@header { package thesis; }

provingStructure: assertionComment WS? commands EOF;

assertionComment: '/*' WS? preconditions WS? postconditions WS? '*/';

preconditions: 'PRE:' WS? predicates;

postconditions: 'POST:' WS? predicates;

predicates: predicate ( WS? SEMICOLON WS? predicate)* SEMICOLON?;

commands: command ( WS? SEMICOLON WS? command)* SEMICOLON?;

command:
  assignmentCommand |
  sequenceCommand |
  conditionalCommand |
  loopCommand;

assignmentCommand: identifier WS? '=' WS? expression;

sequenceCommand: '{' WS? commands  WS? '}';

conditionalCommand: 'if' WS? '(' WS? predicate WS? ')' WS? command;

loopCommand: 'while' WS? '(' WS? predicate WS? ')' WS? command;

// Expression grammar

expression: multExpression;

multExpression: sumExpression (WS? multOperator WS? multExpression)*;

sumExpression: negExpression (WS? sumOperator WS? sumExpression)*;

negExpression: '-' WS? atom | atom;

atom: numericConstant | identifier | parensExpression;

parensExpression: '(' WS? expression WS? ')';

// Predicate grammar

predicate: andPredicate;

andPredicate: orPredicate (WS? '&&' WS? andPredicate)*;

orPredicate: notPredicate (WS? '||' WS? orPredicate)*;

notPredicate: '!' WS? predicateAtom | predicateAtom;

predicateAtom: booleanConstant | comparison | parensPredicate;

comparison:
  expression WS? comparisonOperator WS? expression;

parensPredicate: '(' WS? predicate WS? ')';

// Primitives

booleanConstant: 'true' | 'false';

numericConstant: NUMBER;

identifier: ID;

comparisonOperator: '<' | '>' | '==' | '>=' | '<=' | '!=';

multOperator: '*' | '/';

sumOperator: '+' | '-';

NUMBER: [0-9]+;

ID: [a-zA-Z_][a-zA-Z_0-9]*;

SEMICOLON: ';';

WS: [\n\r\b\t\f ]+ -> skip;
