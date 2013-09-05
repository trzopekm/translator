grammar pascal3g;
options { output=AST; }

tokens {
  AND              = 'and'             ;
  BEGIN            = 'begin'           ;
  BOOLEAN          = 'boolean'         ;
  CASE             = 'case'            ;
  CHAR             = 'char'            ;
  CHR              = 'chr'             ;
  EXIT             = 'exit'            ;
  CONST            = 'const'           ;
  DIV              = 'div'             ;
  DO               = 'do'              ;
  DOWNTO           = 'downto'          ;
  ELSE             = 'else'            ;
  END              = 'end'             ;
  FOR              = 'for'             ;
  FUNCTION         = 'function'        ;
  IF               = 'if'              ;
  IN               = 'in'              ;
  INTEGER          = 'integer'         ;
  LABEL            = 'label'           ;
  MOD              = 'mod'             ;
  NIL              = 'nil'             ;
  NOT              = 'not'             ;
  OR               = 'or'              ;
  PROCEDURE        = 'procedure'       ;
  PROGRAM          = 'program'         ;
  REAL             = 'real'            ;
  REPEAT           = 'repeat'          ;
  THEN             = 'then'            ;
  TO               = 'to'              ;
  TYPE             = 'type'            ;
  UNTIL            = 'until'           ;
  VAR              = 'var'             ;
  WHILE            = 'while'           ;
  STRING           = 'string'          ;
  BLOCK;   
  IDLIST;     
  FUNC_CALL;
  PROC_CALL;
  CONSTLIST;   
  ARGDECLS; 
  VARDECL;  
  ARGDECL;  
  ARGLIST;  
  TYPEDECL; 
     
  PLUS            = '+'   ;
  MINUS           = '-'   ;
  STAR            = '*'   ;
  SLASH           = '/'   ;
  ASSIGN          = ':='  ; 
  COMMA           = ','   ;
  SEMI            = ';'   ;
  COLON           = ':'   ;
  EQUAL           = '='   ;
  NOT_EQUAL       = '<>'  ;
  LT              = '<'   ;
  LE              = '<='  ;
  GE              = '>='  ;
  GT              = '>'   ;
  LPAREN          = '('   ;
  RPAREN          = ')'   ;
  LBRACK          = '['   ;
  LBRACK2         = '(.'  ;
  RBRACK          = ']'   ;
  RBRACK2         = '.)'  ;
  POINTER         = '^'   ;
  AT              = '@'   ;
  DOT             = '.' ;
  DOTDOT          = '..';
  LCURLY          = '{' ;
  RCURLY          = '}' ;
}

@header {
	import java.io.*;
}

@members {
	PrintWriter writer;
	File f;

	public pascal3gParser(CommonTokenStream input, String fileName) {
		super(input);
		try {
			f = new File(fileName);
			if(!f.exists()){
				new File("output").mkdir();
				f.createNewFile();
				System.out.println("Output file not found, new file created");
			}
			writer = new PrintWriter(fileName);
		} catch(Exception e) {
			e.printStackTrace();
		}
  }
}


program
    : programHeading 
      block
      DOT
    ;

programHeading
    : PROGRAM! identifier (LPAREN! identifierList RPAREN!)? SEMI!
  ;

identifier
    : IDENT
    ;

block
    : ( constantDefinitionPart
      | typeDefinitionPart
      | variableDeclarationPart
      | procedureAndFunctionDeclarationPart
      )*
      compoundStatement
    | identifier
    ;

label
    : unsignedInteger
    ;

constantDefinitionPart
    : CONST^ constantDefinition ( SEMI! constantDefinition )* SEMI!
    ;

constantDefinition
    : identifier EQUAL^ constant
    ;

constantChr
    : CHR^ LPAREN! (unsignedInteger|identifier) RPAREN!
    ;

constant
    : unsignedNumber
    | s=sign n=unsignedNumber -> ^($s $n) 
    | s2=sign id=identifier -> ^($s2 $id) 
    | string
    | constantChr
    ;

unsignedNumber
    : unsignedInteger
    | unsignedReal
    ;

unsignedInteger
    : NUM_INT
    ;

unsignedReal
    : NUM_REAL
    ;

sign
    : PLUS | MINUS
    ;

string
    : STRING_LITERAL
    ;

typeDefinitionPart
    : TYPE^ typeDefinition ( SEMI! typeDefinition )* SEMI!
    ;

typeDefinition
    : identifier e=EQUAL^ {$e.setType(TYPEDECL);}
      ( type
      | functionType 
      | procedureType
      )
    ;

functionType
    : FUNCTION^ (formalParameterList)? COLON! resultType
    ;

procedureType
    : PROCEDURE^ (formalParameterList)?
    ;

type
    : CHAR
    | BOOLEAN
    | INTEGER
    | REAL
    | STRING
    ;

variableDeclarationPart
    : VAR^ variableDeclaration ( SEMI! variableDeclaration )* SEMI!
    ;

variableDeclaration
    : identifierList c=COLON^ {$c.setType(VARDECL);} type
    ;

procedureAndFunctionDeclarationPart
    : procedureOrFunctionDeclaration SEMI!
    ;

procedureOrFunctionDeclaration
    : procedureDeclaration
    | functionDeclaration
    ;

procedureDeclaration
    : PROCEDURE^ identifier (formalParameterList)? SEMI!
      ( block )
    ;

functionDeclaration
    : FUNCTION^ identifier (formalParameterList)? COLON! resultType SEMI!
      ( block )
    ;

formalParameterList
    : LPAREN formalParameterSection ( SEMI formalParameterSection )* RPAREN
    -> ^(ARGDECLS formalParameterSection+)
    ;

formalParameterSection
    : parameterGroup
    | VAR^ parameterGroup
    | FUNCTION^ parameterGroup
    | PROCEDURE^ parameterGroup
    ;

parameterGroup
    : ids=identifierList COLON t=type
    -> ^(ARGDECL identifierList type)
    ;

identifierList
    : identifier ( COMMA identifier )*
    ->^(IDLIST identifier+)
    ;

constList
    : constant ( COMMA constant )*
    ->^(CONSTLIST constant+)
    ;

resultType
    : type
    ;

statement
    : simpleStatement
    | structuredStatement
    ;

exitStatement
    : EXIT^ 
    ;


simpleStatement
    : assignmentStatement
    | procedureStatement
    | exitStatement
    | emptyStatement
    ;

assignmentStatement
    : variable ASSIGN^ expression 	{ 
										writer.println($variable.text + " = " + $expression.text);
										writer.flush();
									}
    ;

variable
    : ( AT^ identifier
      | identifier
      )
      (  LBRACK^ expression ( COMMA! expression)* RBRACK!
      | LBRACK2^ expression ( COMMA! expression)* RBRACK2!
      | DOT^ identifier
      )*
    ;

expression
    : simpleExpression
    ( (EQUAL^ | NOT_EQUAL^ | LT^ | LE^ | GE^ | GT^ | IN^) simpleExpression )* 
    ;

simpleExpression
    : term ( (PLUS^ | MINUS^ | OR^) term )*
    ;

term
  : signedFactor ( (STAR^ | SLASH^ | DIV^ | MOD^ | AND^) signedFactor )*
    ;

signedFactor
    : (PLUS^|MINUS^)? factor
    ;

factor
    : variable
    | LPAREN! expression RPAREN!
    | functionDesignator
    | unsignedConstant
    | NOT^ factor
    ;

unsignedConstant
    : unsignedNumber
    | constantChr         
    | string
    | NIL
    ;

functionDesignator
    : id=identifier LPAREN args=parameterList RPAREN
    -> ^(FUNC_CALL $id $args) 
    ;

parameterList
    : actualParameter ( COMMA actualParameter )*
    -> ^(ARGLIST actualParameter+)
    ;

element
    : expression ( DOTDOT^ expression )?
    ;

procedureStatement
    : id=identifier ( LPAREN args=parameterList RPAREN )?
    -> ^(PROC_CALL identifier parameterList?)
    ;

actualParameter
    : expression (COLON unsignedInteger)?
    ;

emptyStatement
    :
    ;

empty
    : 
    ;

structuredStatement
    : compoundStatement
    | ifStatement
    | repetetiveStatement
    ;

compoundStatement
    : BEGIN
    statements
      END
        -> ^(BLOCK statements*) 
    ;

statements
    : statement ( SEMI! statement )* 
    ;

ifStatement
    : IF^ expression THEN! statement
      (
     ELSE! statement
    )?
    ;

repetetiveStatement
    : whileStatement
    | repeatStatement
    | forStatement
    ;

whileStatement
    : WHILE^ expression DO! statement
    ;

repeatStatement
    : REPEAT^ statements UNTIL! expression
    ;

forStatement
    : FOR^ identifier ASSIGN! forList DO! statement
    ;

forList
    : initialValue (TO^ | DOWNTO^) finalValue
    ;

initialValue
    : expression
    ;

finalValue
    : expression
    ;



WS      : ( ' '
    |  '\t'
    |  '\f'
    |  (  '\r\n' 
      |  '\r'  
      |  '\n'   
      )
      {  }
    )
    { $channel=HIDDEN; }
  ;

COMMENT_1
        :   '(*'
       ( 
         { input.LA(2) != ')' }?=> '*'
           |   ~('*') 
       )*
          '*)'  
    {$channel=HIDDEN; }
  ;

COMMENT_2
        :  '{'
        (  ~('}') )*
           '}'
    {$channel=HIDDEN; }
  ;

IDENT  :  ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*   
  ;

STRING_LITERAL
  : '\'' ('\'\'' | ~('\''))* '\''
  ;

NUM_INT
  : ('0'..'9')+ 
    ( ( {(input.LA(2)!='.')&&(input.LA(2)!=')')}?=>       

        '.' {$type = NUM_REAL;} 
        ('0'..'9')+ (EXPONENT)?
      )?
          | EXPONENT {$type = NUM_REAL;}  
    )
  ;

fragment
EXPONENT
  :  ('e') ('+'|'-')? ('0'..'9')+
  ;

NUM_REAL
	:
	;
