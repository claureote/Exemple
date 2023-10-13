lexer grammar NucleusLexer;

    channels {
       CSHARP_CHANNEL
    }

// Keywords
ABSTRACT:           'abstract';
AFTER:              'after';
AND_C:              'and';
ASSERT:             'assert';
BLOCK:              'block'; //*** Specific to Nucleus
BOOLEAN:            'boolean';
BUYER:              'buyer';
CHAR:               'char';
CONST:              'const';
CONTRACT:           'contract';  //*** Specific to Nucleus
DATE:               'date';
DATESPAN:           'datespan';
ELSE:               'else';
ENUM:               'enum';
EVENT:              'event'; //*** Specific to Nucleus
FLOAT:              'float';
FOR:                'for';
FUNCTION:           'function';
IF:                 'if';
IMPLEMENTS:         'implements';
IN:                 'in';
INSTANCEOF:         'instanceof';
INT:                'integer'; //*** Specific to Nucleus
INTERFACE:          'interface';
IS:                 'is';
LIST:               'list';
OBSERVATION:        'observation'; //*** Specific to Nucleus
NEW:                'new';
OF:                 'of'; //*** Specific to Nucleus
ON:                 'on';
ONCE:               'once'; //*** Specific to Nucleus
ONE :               'one';  //*** Specific to Nucleus
OPTION:             'option'; //*** Specific to Nucleus
OR_C:               'or';
PAY:                'pay'; //*** Specific to Nucleus
RECEIVE:            'receive'; //*** Specific to Nucleus
RETURN:             'return';
SELLER:             'seller';
SIGNAL:             'signal'; //*** Specific to Nucleus
SKIp:               'skip'; //*** Specific to Nucleus
STRING:             'string'; //*** Specific to Nucleus
SUBTYPE:            'subtype'; //*** Specific to Nucleus
SPECIFIES:          'specifies'; //*** Specific to Nucleus
SYNCH:              'synch'; //*** Specific to Nucleus
TEMPLATE:           'template'; //*** Specific to Nucleus
THIS:               'this';
TYPE:               'type'; //*** Specific to Nucleus
USING:              'using';
VERIFIES:           'verifies';
WHEN:               'when'; //*** Specific to Nucleus
WHERE:              'where'; //*** Specific to Nucleus
WITHIN:             'within';

// Literals

DECIMAL_LITERAL:    ('0' | [1-9] (Digits? | '_'+ Digits)) ;
HEX_LITERAL:        '0' [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])? ;
OCT_LITERAL:        '0' '_'* [0-7] ([0-7_]* [0-7])? [lL]?;
BINARY_LITERAL:     '0' [bB] [01] ([01_]* [01])? [lL]?;

FLOAT_LITERAL:      (Digits '.' Digits? | '.' Digits) ExponentPart? //[fFdD]?
             |       Digits (ExponentPart )//[fFdD]? | [fFdD])
             ;

HEX_FLOAT_LITERAL:  '0' [xX] (HexDigits '.'? | HexDigits? '.' HexDigits) [pP] [+-]? Digits [fFdD]?;

BOOL_LITERAL:       'true'
            |       'false'
            ;

CHAR_LITERAL:       '\'' (~['\\\r\n] | EscapeSequence) '\'';

STRING_LITERAL:     '"' (~["\\\r\n] | EscapeSequence)* '"';
TEXT_BLOCK:         '"""' [ \t]* [\r\n] (. | EscapeSequence)*? '"""';

TIMESPAN_LITERAL:   Digits ('D'|'W'|'M'|'Y') ; 
NULL_LITERAL:       'null';

// Separators
LPAREN:             '(';
RPAREN:             ')';
LBRACE:             '{';
RBRACE:             '}';
LBRACK:             '[';
RBRACK:             ']';
SEMI:               ';';
COMMA:              ',';
DOT:                '.';
// Operators
ASSIGN:             '=';
GT:                 '>';
LT:                 '<';
QUESTION:           '?';
COLON:              ':';
EQUAL:              '==';
LE:                 '<=';
GE:                 '>=';
NOTEQUAL:           '!=';
BITAND:             '&';
AND:                '&&';
OR:                 '||';
NOT:                '!';
INC:                '++';
DEC:                '--';
ADD:                '+';
SUB:                '-';
MUL:                '*';
DIV:                '/';
CARET:              '^';
MOD:                '%';
ADD_ASSIGN:         '+=';
SUB_ASSIGN:         '-=';
MUL_ASSIGN:         '*=';
DIV_ASSIGN:         '/=';
AND_ASSIGN:         '&=';
OR_ASSIGN:          '|=';
XOR_ASSIGN:         '^=';
MOD_ASSIGN:         '%=';
RANGE:              '..';
ARROW:              '->';
POSITION:           '#';


// Whitespace and comments
WS:                 [ \t\r\n\u000C]+ -> channel(HIDDEN);
COMMENT:            '/*' .*? '*/'    -> channel(HIDDEN);
LINE_COMMENT:       '//' ~[\r\n]*    -> channel(HIDDEN);

// C# Function
CSHARPFUNCTION: '#{' .*? '}#' ;//-> channel(CSHARP_CHANNEL);

NEWLINE
 :  ( '\r'? '\n' | '\r' | '\f' ) SPACES?
 ;

// Identifiers
IDENTIFIER:         Letter LetterOrDigit*;

// Fragment rules
fragment ExponentPart
    : [eE] [+-]? Digits
    ;

fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;
fragment HexDigits
    : HexDigit ((HexDigit | '_')* HexDigit)?
    ;
fragment HexDigit
    : [0-9a-fA-F]
    ;
fragment Digits
    : [0-9] ([0-9_]* [0-9])?
    ;
fragment LetterOrDigit
    : Letter
    | [0-9]
    ;
fragment Letter
    : [a-zA-Z$_] // these are the "java letters" below 0x7F
    | ~[\u0000-\u007F\uD800-\uDBFF] // covers all characters above 0x7F which are not a surrogate
    | [\uD800-\uDBFF] [\uDC00-\uDFFF] // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
    ;

fragment SPACES
 : [ \t]+
 ;


//Dump from Java

TILDE:              '~';

/*PRIVATE:            'private';
PROTECTED:          'protected';
PUBLIC:             'public';*/

/*
ASSERT:             'assert';
BREAK:              'break';
BYTE:               'byte';
CASE:               'case';
CATCH:              'catch';
CLASS:              'class';
CONTINUE:           'continue';
DEFAULT:            'default';
DO:                 'do';
DOUBLE:             'double';
EXTENDS:            'extends';
FINAL:              'final';
FINALLY:            'finally';
GOTO:               'goto';

IMPORT:             'import';
LONG:               'long';
NATIVE:             'native';
NEW:                'new';
PACKAGE:            'package';
SHORT:              'short';
STATIC:             'static';
STRICTFP:           'strictfp';
SUPER:              'super';
SWITCH:             'switch';
SYNCHRONIZED:       'synchronized';
THROW:              'throw';
THROWS:             'throws';
TRANSIENT:          'transient';
TRY:                'try';
VOID:               'void';
VOLATILE:           'volatile';
WHILE:              'while';

// Module related keywords
MODULE:             'module';
OPEN:               'open';
REQUIRES:           'requires';
EXPORTS:            'exports';
OPENS:              'opens';
TO:                 'to';
USES:               'uses';
PROVIDES:           'provides';
WITH:               'with';
TRANSITIVE:         'transitive';

// Local Variable Type Inference
VAR:                'var'; // reserved type name

// Switch Expressions
YIELD:              'yield';  // reserved type name from Java 14

// Records
RECORD:             'record';

// Sealed Classes
SEALED:             'sealed';
PERMITS:            'permits';
NON_SEALED:         'non-sealed';
*/


BITOR:              '|';
LSHIFT_ASSIGN:      '<<=';
RSHIFT_ASSIGN:      '>>=';
URSHIFT_ASSIGN:     '>>>=';
COLONCOLON:         '::';
// Additional symbols not defined in the lexical specification
AT:                 '@';
ELLIPSIS:           '...';
UNKNOWN : . ;
