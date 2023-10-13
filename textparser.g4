parser grammar NucleusParser;
//mettre abstract et interface?

/*
##########  TO DO #############
    - Functor
    - Position (#1 #1..3 ) for skip
*/

options { tokenVocab=NucleusLexer; }

compilationUnit
    : packageDeclaration? importDeclaration* statements* typeDeclaration*  EOF
  // | errors
    ;
statements
    : (blockStatement| deliverExpression ';')
    ;

packageDeclaration
    : WITHIN qualifiedName ';'
    ;

importDeclaration
    : USING qualifiedName ';'
    ;

typeDeclaration
    : derivedTypeDeclaration
    | blockDeclaration
    | templateDeclaration
    | functionDeclaration
    ;
   // | interfaceDeclaration
derivedTypeDeclaration
    : typeDecorator = (SUBTYPE|TYPE) ident=typeIdentifier IS derivedType ';'
    ;

derivedType
    : enumType                          #Enumeration
    | NEW? typeType (WHERE validator)?  #Derived
    | INTERFACE interface_              #Interface
    ;

validator
    : qualifiedName
    | lambdaExpression
    ;

enumType //enum or a kind of dictionary where keys are string and values are typeType
    : ENUM (OF typeType)? '{'enumConstants? '}'
    //| UNION {identifierList}
    ;

enumConstants
    : enumConstant (',' enumConstant)*
    | identifier (',' identifier)*
    ;

enumConstant
    :  ident=STRING_LITERAL  ( ':' specification | assignation)? //classBody?
    ;

assignation
    : '=' expression
    ;

specification
    : identifier (WHERE defaultings)?
   // | typeType (WHERE defaultings)?
    | '(' specification ')'
    ;

defaultings
    : defaulting (',' defaulting)*
    ;

defaulting
    : identifier('.'identifier)* (IS specification | assignation)
    ;

/*interfaceDeclaration
    : INTERFACE ident=identifier IS interface_ ';'
    ;*/

interface_
    : '{' interfaceComponentList '}'
    ;

interfaceComponentList
    : interfaceComponent (',' interfaceComponent)*
    ;

interfaceComponent
    :  identifierList ':' interfaceComponentType
    ;

identifierList
    : identifier (',' identifier) *
    ;

interfaceComponentType
    : frpType
    | classOrInterfaceType

    //| interface_
    ;

blockDeclaration
    : modifier? BLOCK ident=identifier
        (RETURN interfaceComponentType | IS specification )?
        (IMPLEMENTS interfaceTypeList)?
        blockBody
    ;

interfaceTypeList
    : interfaceType (',' interfaceType) *
    ;

interfaceType
    : interface_
    | classOrInterfaceType
    ;
//(IMPLEMENTS typeList)?

blockBody
    : ';'
    | block
    ;

block
    :   '{'
            blockStatement*
        '}'
    ;

blockStatement
    : ';'
    | propertyDefinition ';'
    | assertExpression ';'
    | assignationDefinition ';'
    | returnExpression ';'
    ;

propertyDefinition
    : identifierList ':' typeType (WHERE inputDecoratorList | assignation)?
    | identifier IS typeType (WHERE inputDecoratorList)?
    ;

assignationDefinition
    : variableDeclaratorId assignation
    ;

returnExpression
    : RETURN expression
    ;
assertExpression
    : ASSERT expression
    ;

variableDeclaratorId
    : variableDeclaratorId ('[' index=expression ?']')+
    | variableDeclaratorId bop='.' variableDeclaratorId
    | variableDeclaratorId ('(' eventExpr=expression ')')
    | identifier
    ;

inputDecoratorList
    : inputDecorator (',' inputDecorator)*
    ;
inputDecorator
    : defaulting
    | validatorInput
    ;

validatorInput
    : expression
    ;

templateDeclaration
    :  modifier? TEMPLATE ident=identifier (IS specification)?
        templateBody
    ;

templateBody
    : ';'
    | template
    ;

template
    :   '{'
            templateBodyDeclaration*
        '}'
    ;

templateBodyDeclaration
    : ';'
    | propertyDefinition ';'
   // | statementExpression=expression ';'
    | assignationDefinition ';'
    | deliverExpression ';'
    ;



deliverExpression
    : prefix= (PAY|RECEIVE) expression
    ;

modifier
    : ABSTRACT
    ;

functionDeclaration
    : FUNCTION typeType ident=identifier formalParameters
      functionBody
    ;

functionBody
    : CSHARPFUNCTION
    ;

/*variableDeclarators
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ('=' variableInitializer)?
    ;
*/
/*variableDeclaratorId
    : identifier ('[' ']')*
    ;*/

variableInitializer
    : arrayInitializer
    | expression
    ;

arrayInitializer
    : '{' (variableInitializer (',' variableInitializer)* (',')? )? '}'
    ;

formalParameters
    : '(' ( receiverParameter?
          | receiverParameter (',' formalParameterList)?
          | formalParameterList?
          ) ')'
    ;

receiverParameter
    : typeType (identifier '.')* THIS
    ;

formalParameterList
    : formalParameter (',' formalParameter)* (',' lastFormalParameter)?
    | lastFormalParameter
    ;

formalParameter
    :  typeType variableDeclaratorId
    ;

lastFormalParameter
    :  typeType  '...' variableDeclaratorId
    ;

qualifiedName
    : identifier ('.' identifier)*
    ;

literal
    : integerLiteral        #Integer
    | floatLiteral          #Float
    | CHAR_LITERAL          #Char
    | STRING_LITERAL        #String
    | BOOL_LITERAL          #Boolean
    | NULL_LITERAL          #Null
    | POSITION              #Infinity
    //| TEXT_BLOCK 7
    | TIMESPAN_LITERAL      #TimeSpan
    ;
integerLiteral
    : DECIMAL_LITERAL
    | HEX_LITERAL
    | OCT_LITERAL
    | BINARY_LITERAL
    ;

floatLiteral
    : FLOAT_LITERAL
    | HEX_FLOAT_LITERAL
    ;


// STATEMENTS / BLOCKS

identifier
    : IDENTIFIER
   /* | MODULE
    | OPEN
    | REQUIRES
    | EXPORTS
    | OPENS
    | TO
    | USES
    | PROVIDES
    | WITH
    | TRANSITIVE
    | YIELD
    | SEALED
    | PERMITS*/
   // | VAR
    ;

typeIdentifier  // Identifiers that are not restricted for type declarations
    : IDENTIFIER
   /* | MODULE
    | OPEN
    | REQUIRES
    | EXPORTS
    | OPENS
    | TO
    | USES
    | PROVIDES
    | WITH
    | TRANSITIVE
    | SEALED
    | PERMITS*/
    ;
/*
localTypeDeclaration
    :
      (blockDeclaration )
    | ';'
    ;

statement
    : blockLabel=block
  //| IF parExpression statement (ELSE statement)?
  //| FOR '(' forControl ')' statement
  //| WHILE parExpression statement
  //| DO statement WHILE parExpression ';'
    | RETURN expression? ';'
    | statementExpression=expression ';'
    ;
*/
// EXPRESSIONS

methodCall
    : identifier ('(' arguments ')')+
    //| THIS arguments
   // | SUPER '(' expressionList? ')'
    ;
arguments
   // : '(' (expressionList | assignList)? ')'
    : (expressionList|nameValueList)
    ;

expressionList
    : expression (',' expression)*
    ;

nameValueList
    : nameValue (',' nameValue)*
    ;

nameValue
    : identifier assignation
    ;
     /* (
         identifier
       | methodCall
       | STRING_LITERAL
      )*/
         // | methodCall
expression
    : methodCall                                    #FuncCall
    | expression bop='.' methodCall                 #FunctionAcces
    | expression bop='.' memberIdentifier           #MemberAcces
    | expression '[' expression ']' ('(' expression ')')?  #ElmtOfArray
    | prefix=('+'|'-'|'++'|'--') expression         #UnaryOpNum
    | prefix='!' expression                         #UnaryOpBool
    | prefix='#' expression                         #Position
   // | '['expression bop=':' expression ']'          #SkippedRange
    | expression bop='..' expression                #SkippedRange
    | expression bop='^' expression                 #BopPow
    | expression bop=('*'|'/'|'%') expression       #BopMultDivPercent
    | expression bop=('+'|'-') expression           #BopAddSub
    | expression
        bop=('<=' | '>=' | '>' | '<') expression    #BopComp
    | expression bop=('==' | '!=') expression       #BopEqual
    | expression bop='&&' expression                #BopAnd
    | expression bop='||' expression                #BopOr
    | <assoc=right> expression
        bop='?' expression ':' expression           #CondTer
    | <assoc=right> expression
        bop=IF expression ELSE expression           #IfThenElse
  //signalExpressions
    | DATE '(' expression')'                        #DateOfOcc
  //eventExpressions
    | SKIp expressionList eventExpr=expression      #Skip
    | targetEvent = expression
        AFTER triggerEvent = expression             #After
    | targetEvent = expression
        SYNCH triggerEvent = expression             #Synch
    | ONCE expression                               #Once
    | WHEN expression                               #When
  //contractExpressions
    | ONE '(' asset=expression ')'  #One
    | RECEIVE expression                            #Receive
    | expression IN asset=expression                #In
    | expression ON eventExpr=expression            #On
    | expression bop=AND_C  expression              #AddContract
    | expression bop=OR_C  expression               #Choose
    | primary                                       #Prime
    | '(' expression (',' expression)+ ')'          #ValueTuple
    | '[' expression(',' expression)*  ']'          #ValueList
    | expression '[' left=expression? ':' right=expression?  ']' #Slicing
    | '(' expression ')'                            #Paren
    | '['expression FOR  identifier IN  (enumerator = expression|left=expression ':' right=expression )  ']'   #For
    ;

memberIdentifier
    : identifier
    | STRING_LITERAL
    ;

lambdaExpression
    : lambdaParameters '->' lambdaBody
    ;

lambdaParameters
    : identifier
    | '(' formalParameterList? ')'
    | '(' identifier (',' identifier)* ')'
   // | '(' lambdaLVTIList? ')'
    ;
lambdaBody
    : expression
    | block
    ;

primary
    : literal
    | identifier
    ;
/*
guardedPattern
    : '(' guardedPattern ')'
    | typeType  identifier ('&&' expression)*
    | guardedPattern '&&' expression
    ;
*/

typeType
    :  primeType ( '[' expression? ']')*
    ;

primeType
    : classOrInterfaceType
    | primitiveType
    | frpType
    ;

classOrInterfaceType
    : ident=identifier//(identifier '.')* typeIdentifier
    ;
frpType
    : decorator = (SIGNAL|OBSERVATION|LIST) 'of' typeType
    | (direction = (BUYER|SELLER) )? decorator =OPTION  'of' typeType
    | decorator=EVENT
    | decorator=CONTRACT
    ;

primitiveType
    : BOOLEAN
    | INT
    | FLOAT
    | DATE
    | STRING
    | DATESPAN
    | tupleType
    ;

tupleType
    : '('tupleItem (','tupleItem)* ')'
    ;
tupleItem
    : (identifier ':')? primitiveType
    ;
//| LONG
//| CHAR
//| BYTE
//| SHORT
//| DOUBLE
/*
errors
: error+;

    error
    : ABSTRACT
      |AFTER
      |AND_C
      |BLOCK
      |BOOLEAN
      |BUYER
      |CHAR
      |CONST
      |CONTRACT
      |DATE
      |DATESPAN
      |ELSE
      |ENUM
      |EVENT
      |FLOAT
      |FUNCTION
      |IF
      |IMPLEMENTS
      |IN
      |INSTANCEOF
      |INT
      |INTERFACE
      |IS
      |LIST
      |OBSERVATION
      |NEW
      |OF
      |ON
      |ONCE
      |ONE
      |OPTION
      |OR_C
      |PAY
      |RECEIVE
      |RETURN
      |SELLER
      |SIGNAL
      |SKIp
      |STRING
      |SUBTYPE
      |SPECIFIES
      |SYNCH
      |TEMPLATE
      |THIS
      |TIMESPAN
      |TYPE
      |USING
      |VERIFIES
      |WHEN
      |WHERE
      |WITHIN
      |DECIMAL_LITERAL
      |HEX_LITERAL
      |OCT_LITERAL
      |BINARY_LITERAL
      |FLOAT_LITERAL
      |HEX_FLOAT_LITERAL
      |BOOL_LITERAL
      |CHAR_LITERAL
      |STRING_LITERAL
      |TEXT_BLOCK
      |TIMESPAN_LITERAL
      |NULL_LITERAL
      |LPAREN
      |RPAREN
      |LBRACE
      |RBRACE
      |LBRACK
      |RBRACK
      |SEMI
      |COMMA
      |DOT
      |ASSIGN
      |GT
      |LT
      |QUESTION
      |COLON
      |EQUAL
      |LE
      |GE
      |NOTEQUAL
      |BITAND
      |AND
      |OR
      |INC
      |DEC
      |ADD
      |SUB
      |MUL
      |DIV
      |CARET
      |MOD
      |ADD_ASSIGN
      |SUB_ASSIGN
      |MUL_ASSIGN
      |DIV_ASSIGN
      |AND_ASSIGN
      |OR_ASSIGN
      |XOR_ASSIGN
      |MOD_ASSIGN
      |RANGE
      |ARROW
      |POSITION
      |WS
      |COMMENT
      |LINE_COMMENT
      |CSHARPFUNCTION
      |NEWLINE
      |IDENTIFIER
      ;*/

/*
typeArguments
    : '<' typeArgument (',' typeArgument)* '>'
    ;

superSuffix
    : arguments
    | '.' typeArguments? identifier arguments?
    ;

explicitGenericInvocationSuffix
    : SUPER superSuffix
    | identifier arguments
    ;
*/



/*
typeArgument
    : typeType
  //  | '?' ((EXTENDS | SUPER) typeType)?
    ;
*/

/*
memberDeclaration
    : interfaceDeclaration
    | functionDeclaration
    | genericMethodDeclaration
    | fieldDeclaration
    | constructorDeclaration
    | genericConstructorDeclaration

    | blockDeclaration
    | derivedTypeDeclaration
    ;
*/

    /*lambdaLVTIList
        : lambdaLVTIParameter (',' lambdaLVTIParameter)*
        ;

    lambdaLVTIParameter
        :  VAR identifier
        ;*/
    /** Matches cases then statements, both of which are mandatory.
     *  To handle empty cases at the end, we add switchLabel* to statement.
     */
     /*
    switchBlockStatementGroup
        : switchLabel+ blockStatement+
        ;

    switchLabel
        : CASE (constantExpression=expression | enumConstantName=IDENTIFIER | typeType varName=identifier) ':'
        | DEFAULT ':'
        ;

    forControl
        : enhancedForControl
        | forInit? ';' expression? ';' forUpdate=expressionList?
        ;

    forInit
        : localVariableDeclaration
        | expressionList
        ;

    enhancedForControl
        :  (typeType | VAR) variableDeclaratorId ':' expression
        ;
    */

    // Java17
    /*switchExpression
        : SWITCH parExpression '{' switchLabeledRule* '}'
        ;*/

    // Java17
    /*switchLabeledRule
        : CASE (expressionList | NULL_LITERAL | guardedPattern) (ARROW | COLON) switchRuleOutcome
        | DEFAULT (ARROW | COLON) switchRuleOutcome
        ;*/

    // Java17


    /*
    genericConstructorDeclaration
        : typeParameters constructorDeclaration
        ;

    constructorDeclaration
        : identifier formalParameters  constructorBody=block
        ;
    */

    // ANNOTATIONS  => to be used by composer
    /*altAnnotationQualifiedName
        : (identifier DOT)* '@' identifier
        ;

    annotation
        : ('@' qualifiedName | altAnnotationQualifiedName) ('(' ( elementValuePairs | elementValue )? ')')?
        ;
    elementValuePairs
        : elementValuePair (',' elementValuePair)*
        ;

    elementValuePair
        : identifier '=' elementValue
        ;

    elementValue
        : expression
        | elementValueArrayInitializer
        ;

    elementValueArrayInitializer
        : '{' (elementValue (',' elementValue)*)? (',')? '}'
        ;
    */

/*
expression
    : primary
    | expression bop='.'
      (
         identifier
       | methodCall
       | THIS
       | NEW nonWildcardTypeArguments? innerCreator
       | SUPER superSuffix
       | explicitGenericInvocation
      )
    | expression '[' expression ']'
    | methodCall
  //| NEW creator
  //| '('  typeType ('&' typeType)* ')' expression
  //| expression postfix=('++' | '--')
    | prefix=('+'|'-'|'++'|'--') expression
  //| prefix=('~'|'!') expression
    | expression bop='^' expression
    | expression bop=('*'|'/'|'%') expression
    | expression bop=('+'|'-') expression
  //| expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    | expression bop=('<=' | '>=' | '>' | '<') expression
  //| expression bop=INSTANCEOF (typeType | pattern)
    | expression bop=('==' | '!=') expression
    | expression bop='&' expression
    | expression bop='|' expression
    | expression bop='&&' expression
    | expression bop='||' expression
    | <assoc=right> expression bop='?' expression ':' expression
  //| <assoc=right> expression
  //  bop=('=' | '+=' | '-=' | '*=' | '/=' | '&=' | '|=' | '^=' | '>>=' | '>>>=' | '<<=' | '%=')
  //  expression
  //| lambdaExpression // Java8

    | switchExpression // Java17

     Java 8 methodReference
    | expression '::' typeArguments? identifier
   | typeType '::' (typeArguments? identifier  | NEW)
    | classType '::' typeArguments?   NEW
    ;*/

    /*
    pattern
        :  typeType  identifier
        ;
    */
    // Java8


    /*
    typeTypeOrVoid
        : typeType
        ;
    */
    /*
    genericMethodDeclaration
        : derivedTypeDeclaration functionDeclaration
        ;

    fieldDeclaration
        :  variableDeclarators  ':' typeType  specification? ';'
        ;
    */
    /*
    statement
        : blockLabel=block
      //  | ASSERT expression (':' expression)? ';'
        | IF parExpression statement (ELSE statement)?
       // | FOR '(' forControl ')' statement
        | WHILE parExpression statement
        | DO statement WHILE parExpression ';'
       // | SWITCH parExpression '{' switchBlockStatementGroup* switchLabel* '}'
       // | SYNCHRONIZED parExpression block
        | RETURN expression? ';'
        | THROW expression ';'
        | BREAK identifier? ';'
        | CONTINUE identifier? ';'
        | YIELD expression ';' // Java17
        | SEMI
        | statementExpression=expression ';'
       // | switchExpression ';'? // Java17
        //| identifierLabel=identifier ':' statement
        ; */

        /*
        resourceSpecification
            : '(' resources ';'? ')'
            ;

        resources
            : resource (';' resource)*
            ;

        resource
            : ( classOrInterfaceType variableDeclaratorId |  identifier ) '=' expression
            | identifier
            ;
        */

        /*parExpression
            : '(' expression ')'
            ;
        */
