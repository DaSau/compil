type sens = HeriteDe | Domine 
type type_var = Mut | Immu| Bloc
type signe = Plus | Moins
type ident = string
type chaine = string (*pas vraiment, mais à priori osef pour le typage et la compil*)



type decl = DeclV of var | DeclM of methode

and var = type_var * ident * typ option * expr

and meth_contenu = MetContB of bloc | MetContAff of (typ * expr) 

and methode = bool * ident * (param_type list) option * param list * meth_contenu

and param = ident*typ

and param_type = ident * (sens * typ) option

and param_type_classe = signe option * param_type

and typ = T of (ident * arg_type)

and arg_type = typ list option

and expr = { desc: desc ; loc : Lexing.position * Lexing.position }

and desc = 	| Eint of int 
		| Echaine of chaine 
		| Ebool of bool 
		| Eunit
		| This | Null
		| Eexp of expr
		| Eacc of acces
		| Eaff of acces * expr (*affectation*)
		| Emet of acces * arg_type * expr list
		| Edecl of ident * arg_type * expr list (*déclaration*)
		| Eneg of expr
		| Eopp of expr
		| Ebinop of expr * op * expr
		| Opif of expr * expr * expr (*le deuxième vaut unit si pas de else*)
		| Opwhile of expr * expr
		| Opreturn of (expr option)
		| Opprint of expr
		| Ebloc of bloc

and tbloc = TbV of var | TbE of expr 

and bloc = tbloc list 

and op = Oeq | One | Oequal | Odiff | Oinfeq | Oinf | Osupeq | Osup | Oadd | Osub | Omult | Odiv | Omod | Oand | Oor

and acces = AccI of ident | AccC of (expr*ident) 


type fichier = { classes : classe list ; main : decl list }
and classe = { 
	nom : ident ; 
	param_type : (param_type_classe list) option ;
	param : (param list) option ;
	ext : (typ * (expr list) option) option;
	corps : decl list;
}
