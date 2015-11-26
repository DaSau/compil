{
	exception Lexing_error of string
	open Lexing 
	open Parser
	
	let keyword = Hashtbl.create 45

	let f tbl (a, b) = Hashtbl.add tbl a b

	let () = List.iter (f keyword)
	[
		"class", CLASS;
		"def", DEF;
		"else", ELSE;
		"eq", EQ;
		"extends", EXTENDS;
		"false", FALSE;
		"if", IF;
		"ne", NE;
		"new", NEW;
		"null", NULL;
		"object", OBJECT;
		"override", OVERRIDE;
		"print", PRINT;
		"return", RETURN;
		"this", THIS;
		"true", TRUE;
		"val", VAL;
		"var", VAR;
		"while", WHILE;
		"Main", MAIN;
	]

	let operator = Hashtbl.create 15
	let () = List.iter (f operator) 
	[
		"=", AFF;
		"||", OR;
		"&&", AND;
		"==", EQUAL;
		"!=", DIFF;
		">", SUP;
		">=", SUPEQ;
		"<", INF;
		"<=", INFEQ;
		"+", ADD;
		"-", SUB;
		"*", MULT;
		"/", DIV;
		"%", MOD;
		"!", NOT;
		"[", OBRA;
		"]", CBRA;
		"{", OACC;
		"}", CACC;
		"(", OPAR;
		")", CPAR;
		":", COLON;
		">:", SUPCOL;
		"<:", INFCOL;
		",", COMMA;
		".", DOT;
	]
	
	  /* fonction à appeler à chaque retour chariot (caractère '\n') */
	let newline lexbuf =
    		let pos = lexbuf.lex_curr_p in
    		lexbuf.lex_curr_p <-
      		{ pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}


let digit = [ '0'-'9' ]
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_' )*


let symboleSimple  = ['!' '%' '/' '*' '+' ',' '-' '.' ':' '<' '>' '=' ]
let symboleDouble =  "<=" | ">=" | "&&" | "||" | ">:" | ">:" | "==" | "!="
let limits  = ['(' ')' '[' ']' '{' '}']



let entier = '0' | ['1' - '9'] digit*
let car = [ ^'"' ]
let chaine = "\"" car* "\""


rule token = parse
	| [' ' '\t']+ 		{ token lexbuf }
	| '\n'   		{ newline lexbuf ; token lexbuf }
	| "/*"             	{ comment lexbuf }
	| "//"	 		{ comment_line lexbuf}
	| ident as s 		{ try Hashtbl.find keyword s with Not_found -> IDENT s}
	| entier as s	  	{ CONST_INT (int_of_string s) }
	| "\""	 		{ let s = (r_car lexbuf) in CONST_STR s } 
	| ';' 			{ SEP }
	(*| C'est tout ? *)	
    	| eof             	{ EOF }
	| symboleSimple as op		{ try Hashtbl.find operator (String.make 1 op) with
				      Not_found -> raise (Lexing_error("Ceci n'est pas un operateur : " ^ String.make 1 op)) }
	| symboleDouble as op		{ try Hashtbl.find operator op with
				      Not_found -> raise (Lexing_error("Ceci n'est pas un operateur : " ^ op)) }
	| limits as l 		{ try Hashtbl.find operator (Char.escaped l) with
				      Not_found -> raise (Lexing_error "Bug") }	
	| _ as c		{ raise (Lexing_error ("Caractere illegal : "^ Char.escaped c)) }
and comment = parse
	| "*/"  { token lexbuf }
	| '\n'	{ newline lexbuf ; comment lexbuf }
	| eof   { raise (Lexing_error "Commentaire non termine") }
	| _     { comment lexbuf }

and comment_line = parse
	| '\n' 	{ newline lexbuf ; token lexbuf}
	| eof 	{ EOF }
	| _ 	{ comment_line lexbuf}

(*Si on veut optimiser, on peut concaténer à l'aide du module buffer,
plus rapide. A voir plus tard *)
and r_car = parse 
	| "\\\\" | "\\\"" | "\\n" | "\\t" as s	
					{ let suite = r_car lexbuf in s ^ suite } 
	| '\t' 			as c	{ let suite = r_car lexbuf in (Char.escaped c) ^ suite }
	| '\n' 			as c	{ newline lexbuf ; let suite = r_car lexbuf in (Char.escaped c) ^ suite }
	| '\\'		 		{ raise (Lexing_error ("Caractere illegal" ^ Char.escaped '\\')) } 
	| '"'				{ String.make 0 'z' }	
	| _			as c	{ let n = Char.code c in 
						if n > 126 || n < 32 
							then raise (Lexing_error ("Caractere illegal" ^ Char.escaped c)) 
						else begin
							let suite = r_car lexbuf in
							(Char.escaped c) ^ suite
					  	end 
					}
					
