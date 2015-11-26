/* Analyseur syntaxique pour Scala */

%{
	open Ast
%}

/* Déclaration des tokens */

%token <string> IDENT
%token <string> CONST_STR
%token <int> CONST_INT
%token CLASS DEF ELSE EQ EXTENDS FALSE IF NE NEW NULL OBJECT OVERRIDE PRINT MAIN
%token RETURN THIS TRUE VAL VAR WHILE
%token AFF OR AND EQUAL DIFF SUP SUPEQ INF INFEQ ADD SUB MULT DIV MOD NOT
%token OBRA CBRA OACC CACC OPAR CPAR COLON SUPCOL INFCOL COMMA DOT
%token SEP
%token EOF

/*priorités et associativité des tokens */

%nonassoc IF
%nonassoc ELSE
%nonassoc WHILE RETURN
%right AFF
%left OR
%left AND
%left EQ NE EQUAL DIFF
%left SUP SUPEQ INF INFEQ
%left ADD SUB
%left MULT DIV MOD
%right NOT USUB /*et le - unaire fait référence à cette règle de priorité*/ 
%left DOT

/* Point d'entrée de la grammaire */

%start fichier

/* Type des valeurs renvoyées par l'analyseur */

%type <Ast.fichier> fichier

%%

/*Règles de la grammaire*/

fichier: 
	c = classe*;  
	OBJECT; MAIN; OACC; d = separated_list(SEP, decl) ; CACC ; EOF
		{ {classes = c; main = d;} }

acces:
	| i = IDENT					 	{ AccI i }
	| e = expr; DOT; i = IDENT 		{ AccC (e, i) }

arg_typ:
	a = option(op_arg_typ) 	{ a }


op_arg_typ:
	OBRA; l = separated_nonempty_list(COMMA, typ); CBRA { l }


bloc: 
	OACC; l = separated_list(SEP, tbloc); CACC	{ l }


tbloc:
	| v = var	{ TbV v }
	| e = expr 	{ TbE e }


op_classe_ptc:
	| OBRA; ptc = separated_nonempty_list(COMMA, param_type_classe); CBRA { ptc }

op_expr:
	| OPAR; l = separated_list(COMMA, expr); CPAR { l }

op_classe_ext:
	| EXTENDS; t = typ; e = op_expr? { (t, e) }

op_param:
	| OPAR; l = separated_list (COMMA, param); CPAR { l }

classe: 
	CLASS; 
	i = IDENT; 
	ptc = op_classe_ptc?;
	p = op_param?;
	extds = op_classe_ext?;
	OACC;
	d = separated_list(SEP, decl);
	CACC;
	{ {nom = i; param_type = ptc; param = p; ext = None(*extds*); corps = d;} }


decl:
	| v = var 		{ DeclV v  }
	| m = methode  	{ DeclM m  }


desc:
	| e = CONST_INT				{ Eint e }
	| s = CONST_STR				{ Echaine s }
	| THIS					{ This }
	| NULL					{ Null }
	| TRUE					{ Ebool true }
	| FALSE					{ Ebool false }
	| OPAR; CPAR				{ Eunit }
	| OPAR; e = expr; CPAR 			{ e.desc }	
	| a = acces			 	{ Eacc a }
	| a = acces; AFF; e = expr		{ Eaff (a,e) }
	| a = acces; at = arg_typ; OPAR; l = separated_list(COMMA, expr); CPAR
						{ Emet (a,at,l) }
	| NEW; id = IDENT; at = arg_typ; OPAR; l = separated_list(COMMA, expr) ; CPAR
						{ Edecl (id, at, l) } 
	| NOT; e = expr				{ Eneg e }
	| SUB; e = expr				{ Eopp e } %prec USUB
	| e1 = expr; o = binop; e2 = expr	
						{ Ebinop (e1, o, e2) } 
	| IF; OPAR; e1 = expr; CPAR; e2 = expr
						{ Opif (e1, e2, { desc = Eunit ; loc = e2.loc } ) } %prec IF
	| IF; OPAR; e1 = expr; CPAR; e2 = expr; ELSE; e3 = expr
						{ Opif (e1, e2, e3) }
	| WHILE; OPAR; e1 = expr; CPAR; e2 = expr
						{ Opwhile (e1, e2) } %prec WHILE
	| RETURN; e = expr		 	{ Opreturn (Some e) }
	| RETURN 				{ Opreturn None}
	| PRINT; OPAR; e = expr; CPAR
						{ Opprint e }
	| b = bloc 				{ Ebloc b }


expr: 
	| d = desc	{ { desc = d; loc = $startpos, $endpos } }




%inline binop: 
	| EQ 	{ Oeq }
	| NE  	{ One }
	| EQUAL { Oequal }
	| DIFF 	{ Odiff }
	| INF  	{ Oinf }
	| SUP 	{ Osup }
	| SUPEQ	{ Osupeq }
	| INFEQ	{ Oinfeq }
	| ADD 	{ Oadd }	
	| SUB  	{ Osub }
	| MULT 	{ Omult }
	| DIV	{ Odiv }
	| MOD  	{ Omod }
	| AND	{ Oand }
	| OR  	{ Oor }


op_methode:
	| OBRA; l = separated_nonempty_list(COMMA, param_typ); CBRA { l }
methode:
	b = boption(OVERRIDE); DEF; id = IDENT; 
	l = op_methode?
	OPAR; s = separated_list(COMMA, param); CPAR; m = metcont
					{ (b, id, l, s, m) }

metcont:
	| b = bloc			{ MetContB b }
	| COLON; t = typ; AFF; e = expr	{ MetContAff (t,e) }


param: 
	i = IDENT ; COLON ; t = typ { (i, t) }


param_type_classe:
	| ADD; p = param_typ { (Some Plus, p) }
	| SUB; p = param_typ { (Some Moins, p) }
	| p = param_typ   	  { (None, p) }


op_param_typ: 
	| SUPCOL; t = typ { (Domine, t) }
	| INFCOL; t = typ { (HeriteDe, t) }
	| SUP {print_string "merde"; assert false }


param_typ:
	i = IDENT; contrainte = option(op_param_typ) { (i, contrainte) }


typ:
	i = IDENT; arg = arg_typ { T (i, arg) }


var:
	| VAR; i = IDENT; t = op_var?; AFF; e = expr {(Mut, i, t, e)}
	| VAL; i = IDENT; t = op_var?; AFF; e = expr {(Immu, i, t, e)}


op_var:
	| COLON; t = typ { t }


