/*	Definition section */
%{
extern int yylineno;
extern int yylex();
extern char* yytext;   // Get current token from lex
extern char buf[256];  // Get current code line from lex
#define TABLE_SIZE 30
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef enum{function, variable, parameter}Entrytype;
/*struct Number{
    enum{INTEGER, FLOAT}type;
    union{
        int i_val;
        float f_val;
    };
};*/
typedef struct symbol{
    int index;
    char name[50];
    Entrytype entrytype;
    char datatype[10];
    char attr[50];
    int declared; 
    int defined;
    union {
        int int_val;
        double dou_val;
        char str_val[50];
    };   
}symbol;
typedef struct symtable{
    symbol cur[30];
    int lev;
    int item_exist;
    int index;
    struct symtable *parent;
}symtable;
typedef struct par{
    char type[10];
    char id[20];
}par;
symtable *curtable = NULL;  //pointer to current table
symtable *searchtable = NULL;
int curlevel = 0;  //current table's index and current level
char parlist[50];
char parchar[20];
par parameters[20];
int cnt = 0;
int type_flag = 0;
int dump_symbol_flag = 0;
int cond_index = 0;
int while_cnt = 0, while_flag = 0;
int check_flag = 0;
int parnum=0, parnum2=0;
char argchar[100];
char argchar2[100];

FILE *file;
int error_cnt=0;

/* Symbol table function - you can add new function if needed. */
int lookup_symbol(int,char *);
void create_symbol(int);
void insert_symbol(symtable *,char *, Entrytype, char *, char *);
void dump_symbol();
void assign_symbol_val(symbol *,double,char *);
void yyerror(char *);
void semantic_error(char *);
int search_for_symbol(char *);
int lookup_type(char *);
symbol *get_symbol_entry(char *);
void transform_type(char *);
void check_func_parameters(symbol *);

%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    char* string;
    int i_val;
    double f_val;
}

/* Token without return */
%token INC DEC MTE LTE EQ NE 
%token ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token PRINT 
%token IF ELSE WHILE RET
%token SEMICOLON
%token <string>VOID INT FLOAT STRING BOOL

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STR_CONST ID
%token <f_val> TRUE
%token <f_val> FALSE

/* Nonterminal with return, which need to sepcify type */
%type <f_val> binary_expr term boolean_expr factor constant
%type <string>type lvalue
%type <f_val> expression declaration function_call assignment boolean_const

/* Token associativity */
%left '*' '/' '%'
%left '+' '-' 
%left LT MT LTE MTE EQ NE
%right '=' ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%nonassoc IFX
%nonassoc ELSE

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : stats 
    {
        dump_symbol_flag=1;
    } 
;
stats  
    :stats stat{type_flag = 0;}
    |stat{type_flag = 0;}
;
stat 
    : expression_stat     
    | selection_stat {fprintf(file, "EXIT_%d:\n",curlevel); type_flag = 0;}
    | iteration_stat {while_flag = 0; while_cnt++;type_flag = 0;}
    | compound_stat 
    | func_decl    
;

expression_stat
    : expression SEMICOLON {type_flag = 0;}
    | statement SEMICOLON {type_flag = 0;}
    | SEMICOLON {type_flag = 0;}
;
expression
    : boolean_expr      //conditional    
    | function_call
    | assignment
;
statement
    : declaration 
    | return_expr
;
function_call
    :ID '('{check_flag=1;} expr_list ')'
    {
        int i=lookup_symbol(curlevel,$1);
        symbol *sym = get_symbol_entry($1);

        if(i==-2){
            char tmp[50]="Undeclared function ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{  
            check_func_parameters(sym);
            if(strcmp(argchar,argchar2) != 0 || parnum != parnum2){
                semantic_error("function formal parameter is not the same");
            }   
            parnum=0;
            memset(argchar,0,sizeof(argchar));             
            parnum2=0;
            memset(argchar2,0,sizeof(argchar2));     
            char tmp[20],tmp2[20]; 
            transform_type(sym->attr);
            strcpy(tmp,parchar);
            transform_type(sym->datatype);
            strcpy(tmp2,parchar);
            fprintf(file, "\tinvokestatic compiler_hw3/%s(%s)%s\n", $1, tmp, tmp2);            
        }
        check_flag = 0;
    }
    |PRINT '(' binary_expr ')' 
    {
        if(type_flag == 1){
            fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n"
                          "\tswap\n"
                          "\tinvokevirtual java/io/PrintStream/print(I)V\n" );
        }
        else if(type_flag == 2){
            fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n"
                          "\tswap\n"
                          "\tinvokevirtual java/io/PrintStream/print(F)V\n" );

        }
        else{
            fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n"
                          "\tswap\n"
                          "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n" );
        }
    }
    
    |PRINT '(' STR_CONST ')'
    {
        fprintf(file, "\tldc \"%s\"\n", $3);
        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n"
	                  "\tswap\n"
	                  "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n" );
    }
;
expr_list
    :expr_list ',' expression {parnum2++;}
    |expression {parnum2++;}
    |
;
return_expr
    : RET expression 
    {
        if(type_flag == 1){
            fprintf(file, "\tireturn\n");
        }
        else if(type_flag == 2){
            fprintf(file, "\tfreturn\n");
        }
        else{
            fprintf(file, "\tireturn\n");
        }
    }
    | RET {fprintf(file, "\treturn\n");}  
;
boolean_expr
    :boolean_expr EQ binary_expr 
    { 
        $$ = $1 == $3;
        fprintf(file, "\tisub\n");
        if(while_flag == 0){            
            fprintf(file, "\tifne LABEL%d_%d\n", curlevel, ++cond_index);
        }
        else{
            fprintf(file, "\tifeq LABEL_TRUE%d\n",while_cnt); 
            fprintf(file, "\tgoto LABEL_FALSE%d\n",while_cnt);
            fprintf(file, "LABEL_TRUE%d:\n",while_cnt);
        }
    }
    |boolean_expr NE binary_expr 
    { 
        $$ = $1 != $3;
        fprintf(file, "\tisub\n");
        if(while_flag == 0){            
            fprintf(file, "\tifeq LABEL%d_%d\n", curlevel, ++cond_index);
        }
        else{
            fprintf(file, "\tifne LABEL_TRUE%d\n",while_cnt); 
            fprintf(file, "\tgoto LABEL_FALSE%d\n",while_cnt);
            fprintf(file, "LABEL_TRUE%d:\n",while_cnt);
        }
    }
    |boolean_expr LT binary_expr 
    { 
        $$ = $1 < $3;
        fprintf(file, "\tisub\n");        
        if(while_flag == 0){            
            fprintf(file, "\tifme LABEL%d_%d\n", curlevel, ++cond_index);
        }
        else{
            fprintf(file, "\tiflt LABEL_TRUE%d\n",while_cnt); 
            fprintf(file, "\tgoto LABEL_FALSE%d\n",while_cnt);
            fprintf(file, "LABEL_TRUE%d:\n",while_cnt);
        }
    }
    |boolean_expr MT binary_expr 
    { 
        $$ = $1 > $3;
        fprintf(file, "\tisub\n");        
        if(while_flag == 0){           
            fprintf(file, "\tifle LABEL%d_%d\n", curlevel, ++cond_index);
        }else{
            fprintf(file, "\tifgt LABEL_TRUE%d\n",while_cnt); 
            fprintf(file, "\tgoto LABEL_FALSE%d\n",while_cnt);
            fprintf(file, "LABEL_TRUE%d:\n",while_cnt);
        }
    }
    |boolean_expr LTE binary_expr 
    { 
        $$ = $1 <= $3;
        fprintf(file, "\tisub\n");
        if(while_flag == 0){           
            fprintf(file, "\tifgt LABEL%d_%d\n", curlevel, ++cond_index);
        }
        else{
            fprintf(file, "\tifle LABEL_TRUE%d\n",while_cnt);
            fprintf(file, "\tgoto LABEL_FALSE%d\n",while_cnt); 
            fprintf(file, "LABEL_TRUE%d:\n",while_cnt);
        }
    }
    |boolean_expr MTE binary_expr 
    { 
        $$ = $1 >= $3;
        fprintf(file, "\tisub\n");
        if(while_flag == 0){
            
            fprintf(file, "\tiflt LABEL%d_%d\n", curlevel, ++cond_index);
        }
        else{
            fprintf(file, "\tifme LABEL_TRUE%d\n",while_cnt);
            fprintf(file, "\tgoto LABEL_FALSE%d\n",while_cnt); 
            fprintf(file, "LABEL_TRUE%d:\n",while_cnt); 
        }
    }
    |binary_expr {$$ = $1;}
    |boolean_const {$$ = $1;}
;
binary_expr
    :binary_expr '+' term 
    { 
        $$ = $1 + $3;
        if(type_flag == 1)
            fprintf(file, "\tiadd\n");
        else if(type_flag == 2)
            fprintf(file, "\tfadd\n");
    }
    |binary_expr '-' term 
    { 
        $$ = $1 - $3;
        if(type_flag == 1)
            fprintf(file, "\tisub\n");
        else if(type_flag == 2)
            fprintf(file, "\tfsub\n");
    }
    |term
;
term
    :term '*' factor 
    { 
        $$ = $1 * $3;
        if(type_flag == 1)
            fprintf(file, "\timul\n");
        else if(type_flag == 2)
            fprintf(file, "\tfmul\n");                
    }
    |term '/' factor 
    { 
        $$ = $1 / $3;
        if($3 == 0){
            semantic_error("divide by zero");
        }
        else{
            if(type_flag == 1)
                fprintf(file, "\tidiv\n");
            else if(type_flag == 2)
                fprintf(file, "\tfdiv\n"); 
        }
    }
    |term '%' factor 
    {
        if($3 == 0){
            semantic_error("divide by zero");
        }
        else if(type_flag == 2){
            semantic_error("operand type is not int");
        }
        else{
            $$ = (int)$1 % (int)$3;
            fprintf(file, "\tirem\n");
        }
    }
    |factor 
;
factor
    :'(' expression ')' {$$ = $2;}
    |'-' factor {$$ = -$2;}
    |ID 
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable "; 
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{           
            int t = lookup_type($1);            
            symbol *sym = get_symbol_entry($1);
            int lev = searchtable->lev;
            if(type_flag == 0)type_flag = t;
            if(check_flag == 1){
                strcat(argchar2,sym->datatype);
            }
            if(t == 1){
                $$ = sym->int_val;                
                if(type_flag == 1){
                    if(lev == 0)
                        fprintf(file, "\tgetstatic compiler_hw3/%s I\n", $1);
                    else
                        fprintf(file, "\tiload %d\n", sym->index);                    
                }
                else if(type_flag == 2){
                    fprintf(file, "\ti2f\n");
                    if(lev == 0)
                        fprintf(file, "\tgetstatic compiler_hw3/%s F\n", $1);
                    else
                        fprintf(file, "\tfload %d\n", sym->index);
                    type_flag = 2;
                }
                else{
                    type_flag = 1;
                }
            }
            else if(t == 2){
                $$ = sym->dou_val;
                if(type_flag == 1){
                    if(lev == 0)
                        fprintf(file, "\tgetstatic compiler_hw3/%s I\n", $1);
                    else
                        fprintf(file, "\tiload %d\n", sym->index);
                    fprintf(file, "\ti2f\n");
                    type_flag = 2;
                }
                else if(type_flag == 2){
                    if(lev == 0)
                        fprintf(file, "\tgetstatic compiler_hw3/%s F\n", $1);
                    else
                        fprintf(file, "\tfload %d\n", sym->index);
                }
                else{
                    type_flag = 2;
                }
            }
            else if(t == 3){
                $$ = sym->int_val;
                 if(lev == 0)
                    fprintf(file, "\tgetstatic compiler_hw3/%s I\n", $1);
                else
                    fprintf(file, "\tiload %d\n", sym->index);
            }
            else if(t == 4){
                fprintf(file, "\taload %d\n", sym->index);
            }
        }
    }
    |constant 
    {
        $$ = $1;        
    }
    | ID INC 
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable "; 
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            symbol *sym = get_symbol_entry($1);
            int index = search_for_symbol($1);
            if(check_flag == 1){
                strcat(argchar2,sym->datatype);
            }
            int tmp = sym->int_val;
            $$ = tmp + 1;
            type_flag = 1;
            fprintf(file, "\tiload %d\n",sym->index);
            fprintf(file, "\tldc 1\n");
            fprintf(file, "\tiadd\n");
            fprintf(file, "\tistore %d\n",sym->index);
        }
    } 
    | ID DEC 
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable "; 
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            symbol *sym = get_symbol_entry($1);
            int index = search_for_symbol($1);
            if(check_flag == 1){
                strcat(argchar2,sym->datatype);
            }
            int tmp = sym->int_val;
            $$ = tmp - 1;
            type_flag = 1;
            fprintf(file, "\tiload %d\n",sym->index);
            fprintf(file, "ldc 1\n");
            fprintf(file, "isub\n");
            fprintf(file, "\tistore %d\n",sym->index);
        }
    } 
;
constant
    :I_CONST 
    {
        $$ = $1;
        if(check_flag == 1){
            strcat(argchar2,"int");
        }
        if(curlevel != 0){
            fprintf(file, "\tldc %d\n", $1);          
            if(type_flag == 2){
                fprintf(file, "\ti2f\n");
            }
            else{
                type_flag = 1;
            }
        }
        else{
            type_flag = 1;
        }
        
    }
    |F_CONST 
    {
        $$ = $1;
        if(check_flag == 1){
            strcat(argchar2,"float");
        }
        if(curlevel != 0){
            if(type_flag == 1){
                fprintf(file, "\ti2f\n");
                type_flag = 2;
            }
            else{
                type_flag = 2;
            }
            
            fprintf(file, "\tldc %f\n", $1);  
        }  
        else{
            type_flag = 2;
        } 
                     
    }
;

boolean_const
    :TRUE 
    {
        $$ = 1;
        if(check_flag == 1){
            strcat(argchar2,"bool");
        }
        fprintf(file, "ldc 1\n");
    }
    |FALSE 
    {
        $$ = 0;
        if(check_flag == 1){
            strcat(argchar2,"bool");
        }
        fprintf(file, "ldc 0\n");
    }
;

arg_list
    :arg_list ',' type ID 
    { 
        strcat(parlist, ",");
        strcat(parlist, $3);
        strcat(argchar2,$3);
        parnum2++;
        int i= lookup_symbol(curlevel,$4);
        if(i == -2){
            strcpy(parameters[cnt].type,$3);
            strcpy(parameters[cnt++].id,$4); 
        }
        else {
            char tmp[50]="Redeclared variable "; 
            strcat(tmp, $4); 
            semantic_error(tmp);
        }
    } 
    |type ID 
    { 
        strcat(parlist, $1);
        strcat(argchar2,$1);
        parnum2++;
        int i= lookup_symbol(curlevel,$2);
        if(i == -2){
            strcpy(parameters[cnt].type,$1);
            strcpy(parameters[cnt++].id,$2);  
        }
        else {
            char tmp[50]="Redeclared variable "; strcat(tmp, $2); 
            semantic_error(tmp);
        }
    }
    | {cnt=0;memset(parameters,0,sizeof(parameters));}
;

declaration
    : type ID '=' expression 
    { 
        int i= lookup_symbol(curlevel,$2);        
        if(i == -2){
            insert_symbol(curtable,$2, variable, $1, "");
            int t = lookup_type($2);
            symbol *sym = get_symbol_entry($2);
            assign_symbol_val(sym, $4,"");
            if(curlevel == 0){
                if(type_flag == 1)
                    fprintf(file, ".field public static %s I = %d\n", $2, (int)$4);
                else if(type_flag == 2)
                    fprintf(file, ".field public static %s F = %f\n", $2, $4);
            }
            else{
                if(type_flag == 1){
                    if(t == 2){
                        fprintf(file, "\ti2f\n");  
                        fprintf(file, "\tfstore %d\n",sym->index); 
                    }
                    else
                        fprintf(file, "\tistore %d\n",sym->index);
                }
                else if(type_flag == 2){
                    if(t == 1){
                        fprintf(file, "\tf2i\n"); 
                        fprintf(file, "\tistore %d\n",sym->index);  
                    }
                    else
                        fprintf(file, "\tfstore %d\n",sym->index);
                }
                else{
                    if(t==3)    //boolean
                        fprintf(file, "\tistore %d\n",sym->index);
                }
            }
        }
        else {
            char tmp[50]="Redeclared variable "; 
            strcat(tmp, $2); 
            semantic_error(tmp);
        }     
    }
    | type ID '=' STR_CONST
    {
        int i= lookup_symbol(curlevel,$2);
        if(i==-2){
            insert_symbol(curtable,$2, variable, $1, "");
            symbol *sym = get_symbol_entry($2);
            fprintf(file, "\tldc \"%s\"\n", $4);
            fprintf(file, "\tastore %d\n", sym->index);
            assign_symbol_val(sym,0,$4);
        }
        else{
            char tmp[50]="Redeclared variable "; 
            strcat(tmp, $2); 
            semantic_error(tmp);
        }
    }
    | type ID 
    { 
        int i= lookup_symbol(curlevel,$2);
        if(i == -2){
            insert_symbol(curtable,$2, variable, $1, "");
            symbol *sym = get_symbol_entry($2);
            assign_symbol_val(sym, 0,"");
            int idx = search_for_symbol($2);
            type_flag = lookup_type($2);
            if(curlevel == 0){ 
                if(type_flag == 1)
                    fprintf(file, ".field public static %s I\n", $2);
                else if(type_flag == 2)
                    fprintf(file, ".field public static %s F\n", $2);
            }
            else
                fprintf(file, "\tldc 0\n\tistore %d\n", idx); 
        }
        else {
            char tmp[50]="Redeclared variable "; 
            strcat(tmp, $2); 
            semantic_error(tmp);
        }                     
    }
;

func_decl
    : type ID '(' arg_list ')' SEMICOLON 
    { 
        int i= lookup_symbol(curlevel,$2);
        if(i == -2){
            insert_symbol(curtable,$2, function, $1, parlist);
            symbol *sym = get_symbol_entry($2);
            sym->declared=1;
            printf("declared = %d\n",sym->declared);
            memset(parlist,0,50);
        }
        else {
            symbol *sym = get_symbol_entry($2);            
            if(sym->declared == 1){
                char tmp[50]="Redeclared function "; 
                strcat(tmp, $2); 
                semantic_error(tmp);
            }
            else{
                if(strcmp(sym->datatype, $1) != 0){
                    semantic_error("function return type is not the same");
                }
                check_func_parameters(sym);
                if(strcmp(argchar,argchar2) != 0 || parnum != parnum2){
                    semantic_error("function formal parameter is not the same");
                }                
                parnum=0;
                memset(argchar,0,sizeof(argchar));                
            }
        }  
        cnt = 0;
        parnum2=0; 
        memset(argchar2,0,sizeof(argchar2));  
    }
    | type ID '(' arg_list ')' '{' 
    {
        int i= lookup_symbol(curlevel,$2);
        if(i == -2){
            insert_symbol(curtable,$2, function, $1, parlist);
            symbol *sym = get_symbol_entry($2);
            sym->defined=1; 
            memset(parlist,0,50);                                                                         
            curlevel++; 
            create_symbol(curlevel);
            for(int i=0;i<cnt; ++i){
                insert_symbol(curtable,parameters[i].id, parameter, parameters[i].type, "");
            }
        }
        else{
            symbol *sym = get_symbol_entry($2);
            if(sym->declared != 1 || sym->defined == 1){
                char tmp[50]="Redeclared function "; 
                strcat(tmp, $2); 
                semantic_error(tmp);
            }
            else{
                if(strcmp(sym->datatype, $1) != 0){
                    semantic_error("function return type is not the same");
                }
                check_func_parameters(sym);
                if(strcmp(argchar,argchar2)!=0 || parnum != parnum2){
                    semantic_error("function formal parameter is not the same");
                }               
                parnum=0;
                memset(argchar,0,sizeof(argchar));
                
                curlevel++; 
                create_symbol(curlevel);
                for(int i=0;i<cnt; ++i){
                    insert_symbol(curtable,parameters[i].id, parameter, parameters[i].type, "");
                }                
            }
        }  
        parnum2=0; 
        memset(argchar2,0,sizeof(argchar2));
        cnt = 0;
        memset(parameters,0,sizeof(parameters));    
        symbol * sym = get_symbol_entry($2);
        if(strcmp($2,"main")==0)
            fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
        else{
            char tmp[20],tmp2[20]; 
            transform_type(sym->attr);
            strcpy(tmp,parchar);
            transform_type(sym->datatype);
            strcpy(tmp2,parchar);
            fprintf(file, ".method public static %s(%s)%s\n",$2,tmp, tmp2);
        }            
        fprintf(file, ".limit stack 50\n");
        fprintf(file, ".limit locals 50\n");
        
    }
    block_item_list '}'
    {
        dump_symbol_flag=1;
        curlevel--;
        fprintf(file, ".end method\n");
    } 
;

/* actions can be taken when meet the token or rule */
type
    : INT { $$ = $1; }
    | FLOAT { $$ = $1; }
    | BOOL  { $$ = $1; }
    | STRING { $$ = $1; }
    | VOID { $$ = $1; }
;

compound_stat
    :'{' {curlevel++; create_symbol(curlevel);type_flag=0;} block_item_list '}' {dump_symbol_flag=1; curlevel--;}
;

block_item_list
    :block_item_list stat
    |
;

iteration_stat
    :WHILE {while_flag = 1;fprintf(file, "LABEL_BEGIN%d:\n",while_cnt);}'(' boolean_expr ')' 
    compound_stat 
    {
        fprintf(file, "\tgoto LABEL_BEGIN%d\n", while_cnt); 
        fprintf(file, "LABEL_FALSE%d:\n", while_cnt);       
    } 
;
selection_stat
    :IF '(' boolean_expr ')' compound_stat 
    {
        cond_index = 0;
        fprintf(file, "LABEL%d_%d:\n",curlevel, cond_index+1);
        fprintf(file, "\tgoto EXIT_%d\n", curlevel);
    }%prec IFX 
    |IF '(' boolean_expr ')' compound_stat 
    {
        fprintf(file, "\tgoto EXIT_%d\n", curlevel);
    }
    elseif_stat ELSE {fprintf(file, "LABEL%d_%d:\n",curlevel, cond_index);}compound_stat {cond_index = 0;}
;
elseif_stat
    : elseif_stat ELSE IF {fprintf(file, "LABEL%d_%d:\n",curlevel, cond_index);}'(' boolean_expr ')' compound_stat
    |
;

lvalue
    :ID
    {
        int t = lookup_type($1);
        symbol *sym = get_symbol_entry($1);
        if(t == 1)fprintf(file, "\tiload %d\n", sym->index);
        else if(t == 2)fprintf(file, "\tfload %d\n", sym->index);
    }
;
assignment
    :ID '=' expression 
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            int t = lookup_type($1);
            symbol *sym = get_symbol_entry($1);
            int lev = searchtable->lev;
            if(type_flag == 1){
                if(t == 2){
                    fprintf(file, "\ti2f\n"); 
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index);                    
                    sym->dou_val = $3;
                }
                else if(t==1){
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val = $3;
                }
            }
            else if(type_flag == 2){
                if(t == 1){
                    fprintf(file, "\tf2i\n"); 
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val = $3;  
                }
                else if(t==1){
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index);
                    sym->dou_val = $3;
                }
            }
            else{
                /***for string and bool *****/
                //fprintf(file, "\tistore %d\n", sym->index);
                //sprintf(sym->str_val,"%f",$3);
            }
        }
    }
    | ID '=' STR_CONST
    {
        symbol *sym = get_symbol_entry($1);
        fprintf(file, "\tldc \"%s\"\n", $3);
        fprintf(file, "\tastore %d\n", sym->index);
        assign_symbol_val(sym,0,$3);
    }
    | lvalue ADDASGN expression
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            int t = lookup_type($1);
            symbol *sym = get_symbol_entry($1); 
            int lev = searchtable->lev;
            if(type_flag == 1){
                if(t == 2){
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfadd\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else  
                        fprintf(file, "\tfstore %d\n", sym->index);
                    sym->dou_val += $3; 
                }
                else{
                    fprintf(file, "\tiadd\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val += (int)$3;
                }
            }
            else if(type_flag == 2){
                if(t == 1){
                    fprintf(file, "\tf2i\n"); 
                    fprintf(file, "\tiadd\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index); 
                    sym->int_val += (int)$3; 
                }
                else{
                    fprintf(file, "\tfadd\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index);
                    sym->dou_val += $3;
                }
            }
        }
    }
    | lvalue SUBASGN expression
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            int t = lookup_type($1); 
            symbol *sym = get_symbol_entry($1);
            int lev = searchtable->lev;
            if(type_flag == 1){
                if(t == 2){
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfsub\n");  
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index);
                    sym->dou_val -= $3; 
                }
                else{
                    fprintf(file, "\tisub\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val -= (int)$3;
                }
            }
            else if(type_flag == 2){
                if(t == 1){
                    fprintf(file, "\tf2i\n"); 
                    fprintf(file, "\tisub\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val -= (int)$3;  
                }
                else{
                    fprintf(file, "\tfsub\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index);
                    sym->dou_val -= $3;
                }
            }
        }
    }
    | lvalue MULASGN expression
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            int t = lookup_type($1); 
            symbol *sym = get_symbol_entry($1);
            int lev = searchtable->lev;
            if(type_flag == 1){
                if(t == 2){
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfmul\n");  
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index); 
                    sym->dou_val *= $3;
                }
                else{
                    fprintf(file, "\timul\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val *= (int)$3;
                }
            }
            else if(type_flag == 2){
                if(t == 1){
                    fprintf(file, "\tf2i\n"); 
                    fprintf(file, "\timul\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index); 
                    sym->int_val *= (int)$3; 
                }
                else{
                    fprintf(file, "\tfmul\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                    else
                        fprintf(file, "\tfstore %d\n", sym->index);
                    sym->dou_val *= $3;
                }
            }
        }
    }
    | lvalue DIVASGN expression
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            int t = lookup_type($1);
            symbol *sym = get_symbol_entry($1);
            int lev = searchtable->lev;
            if($3 == 0){
                semantic_error("divide by zero");
            }
            else{ 
                if(type_flag == 1){
                    if(t == 2){
                        fprintf(file, "\ti2f\n");
                        fprintf(file, "\tfdiv\n"); 
                        if(lev == 0) 
                            fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                        else 
                            fprintf(file, "\tfstore %d\n", sym->index); 
                        sym->dou_val /= $3;
                    }
                    else{
                        fprintf(file, "\tidiv\n");
                        if(lev == 0) 
                            fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                        else
                            fprintf(file, "\tistore %d\n", sym->index);
                        sym->int_val /= (int)$3;
                    }
                }
                else if(type_flag == 2){
                    if(t == 1){
                        fprintf(file, "\tf2i\n"); 
                        fprintf(file, "\tidiv\n");
                        if(lev == 0) 
                            fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                        else
                            fprintf(file, "\tistore %d\n", sym->index);
                        sym->int_val /= (int)$3;  
                    }
                    else{
                        fprintf(file, "\tfdiv\n");
                        if(lev == 0) 
                            fprintf(file, "\tputstatic compiler_hw3/%s F\n",$1);
                        else
                            fprintf(file, "\tfstore %d\n", sym->index);
                        sym->dou_val /= $3;
                    }
                }
            }
        }
    }
    | lvalue MODASGN expression
    {
        int i=lookup_symbol(curlevel,$1); 
        if(i==-2){
            char tmp[50]="Undeclared variable ";
            strcat(tmp, $1); semantic_error(tmp);
        }
        else{
            int t = lookup_type($1);
            symbol *sym = get_symbol_entry($1); 
            int lev = searchtable->lev;
            if(type_flag == 1){
                if(t == 2){
                    semantic_error("operand type is not int"); 
                }
                else{
                    fprintf(file, "\tirem\n");
                    if(lev == 0) 
                        fprintf(file, "\tputstatic compiler_hw3/%s I\n",$1);
                    else
                        fprintf(file, "\tistore %d\n", sym->index);
                    sym->int_val %= (int)$3;
                }
            }
            else if(type_flag == 2){
                semantic_error("operand type is not int"); 
            }
        }
    }
;

%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;    
    create_symbol(curlevel);

    file = fopen("compiler_hw3.j","w");

    fprintf(file,   ".class public compiler_hw3\n"
                    ".super java/lang/Object\n");

    yyparse();
    dump_symbol();
    if(error_cnt>0){
        remove("compiler_hw3.j");
    }
    printf("\nTotal lines: %d \n",yylineno);

    fclose(file);

    return 0;
}

void yyerror(char *s)
{
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno+1, buf);
    printf("| %s", s);
    printf("\n| Unmatched token: %s", yytext);
    printf("\n|-----------------------------------------------|\n");
    remove("compiler_hw3.j");
    exit(-1);
}

void semantic_error(char *s){
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno+1, buf);
    printf("| %s", s);
    printf("\n|-----------------------------------------------|\n\n");
    error_cnt++;
}
void create_symbol(int level) {
    symtable *newtable = (symtable *)malloc(sizeof(symtable));
    newtable->parent = curtable;
    newtable->lev = level;
    newtable->item_exist = 0;
    curtable = newtable;
}
void insert_symbol(symtable *now,char *name, Entrytype ent, char *dat, char *par) {    
    now->item_exist = 1;   
    strcpy(now->cur[now->index].name, name);
    now->cur[now->index].entrytype = ent;   
    strcpy(now->cur[now->index].datatype, dat);
    strcpy(now->cur[now->index].attr, par);
    now->cur[now->index].index = now->index;
    now->index += 1;
}
void assign_symbol_val(symbol *sym, double val,char *str){
    if(strcmp(sym->datatype, "int") == 0){
        sym->int_val = (int)val;
    }
    else if(strcmp(sym->datatype, "float") == 0){
        sym->dou_val = val;
    }
    else if(strcmp(sym->datatype, "bool")== 0){
        sym->int_val = (int)val;
    }
    else if(strcmp(sym->datatype, "string")== 0){
         strcpy(sym->str_val, str);
    }
}
int lookup_symbol(int scope, char *name) {
    for(symtable *now = curtable; now != NULL; now = now->parent){
        for(int i = 0; i<now->index;++i){ 
            if(strcmp(now->cur[i].name, name) == 0) {
                if(now->lev == scope){
                    return -1; //Redeclared
                }
                else{
                    return i; //OK
                }               
            }
        }
    }
    return -2;   //undefine
}
int search_for_symbol(char *name){
    for(int i = 0; i < curtable->index;++i){ 
        if(strcmp(curtable->cur[i].name, name) == 0)
            return i;
    }
    return -1;
    //call Undeclared ERROR
}

int lookup_type(char *name){
    for(symtable *now = curtable; now != NULL; now = now->parent){
        for(int i = 0; i<now->index;++i){ 
            if(strcmp(now->cur[i].name, name) == 0) 
                if(strcmp(now->cur[i].datatype,"int") == 0)
                    return 1;
                else if(strcmp(now->cur[i].datatype,"float") == 0)
                    return 2; 
                else if(strcmp(now->cur[i].datatype,"bool") == 0)
                    return 3; 
                else if(strcmp(now->cur[i].datatype,"string") == 0)
                    return 4; 
        }
    }
    return 0;
}
symbol *get_symbol_entry(char *name){
    for(symtable *now = curtable; now != NULL; now = now->parent){
        for(int i = 0; i<now->index;++i){ 
            if(strcmp(now->cur[i].name, name) == 0){
                searchtable = now;
                return &(now->cur[i]);
            }
        }
    }
    return NULL;
}
void transform_type(char *types){
    char *token;
    char tmp[50];
    strcpy(tmp,types);
    token = strtok(tmp,",");
    memset(parchar,0,sizeof(parchar));
    while(token != NULL){
        if(strcmp(token,"int")==0){
            strcat(parchar,"I");
        }
        else if(strcmp(token,"float")==0){
            strcat(parchar,"F");
        }
        else if(strcmp(token,"string")==0){
            strcat(parchar,"Ljava/lang/String");
        }
        else if(strcmp(token,"void")==0){
            strcat(parchar,"V");
        }
        else if(strcmp(token,"bool")==0){
            strcat(parchar,"Z");
        }
        token = strtok(NULL,",");
    }
}
char *getType(Entrytype e){
    switch(e){
        case function:
            return "function";
        case parameter:
            return "parameter";
        case variable:
            return "variable";
    }
}
void check_func_parameters(symbol *sym){
    char *tok;
    char tmp[50];
    strcpy(tmp,sym->attr);
    tok = strtok(tmp, ",");
    while(tok != NULL){
        parnum++;
        strcat(argchar,tok);
        tok = strtok(NULL, ",");
    }
}
void dump_symbol() {
    if(curtable->item_exist == 0){
        symtable *tmp;
        tmp = curtable;
        curtable = curtable->parent;
        memset(tmp,0,sizeof(*tmp));
        free(tmp);
        return;
    }
    printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
           "Index", "Name", "Kind", "Type", "Scope", "Attribute");
    
    for(int i=0;i<curtable->index;++i){
        printf("%-10d%-10s%-12s%-10s%-10d%-10s",
        curtable->cur[i].index, curtable->cur[i].name, getType(curtable->cur[i].entrytype), curtable->cur[i].datatype, curtable->lev, curtable->cur[i].attr);
        printf("\n"); 
    }    
    symtable *tmp = NULL;
    tmp = curtable;
    curtable = curtable->parent;
    memset(tmp,0,sizeof(*tmp));
    free(tmp);
}
