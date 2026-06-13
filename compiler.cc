#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <stdexcept>

#include "lexer.h"
#include "execute.h"

using namespace std;

LexicalAnalyzer lexer;


// Symbol / location tables
static map<string, int> var_location_table;   
static map<int, int> const_location_table;   
static Token current_token;

// Helpers: token management
void syntax_error(const string &msg = "Syntax Error")
{
    cerr << msg << endl;
    exit(1);
}

void get_next_token()
{
    current_token = lexer.GetToken();
}

void expect(TokenType t)
{
    if (current_token.token_type != t)
    {
        syntax_error("Unexpected token: " + current_token.lexeme);
    }
    get_next_token();
}

// Helpers: memory management for variables / constants

// Get or create a location for a variable
int get_var_location(const string &name)
{
    auto it = var_location_table.find(name);
    if (it != var_location_table.end())
        return it->second;

    int loc = next_available;
    mem[next_available] = 0;
    next_available++;
    var_location_table[name] = loc;
    return loc;
}

// Get or create a location for a constant value
int get_const_location(int value)
{
    auto it = const_location_table.find(value);
    if (it != const_location_table.end())
        return it->second;

    int loc = next_available;
    mem[next_available] = value;
    next_available++;
    const_location_table[value] = loc;
    return loc;
}

InstructionNode *append_instruction_lists(InstructionNode *list1, InstructionNode *list2)
{
    if (list1 == nullptr)
        return list2;
    InstructionNode *curr = list1;
    while (curr->next != nullptr)
        curr = curr->next;
    curr->next = list2;
    return list1;
}

InstructionNode *get_last_instruction(InstructionNode *head)
{
    if (head == nullptr)
        return nullptr;
    InstructionNode *curr = head;
    while (curr->next != nullptr)
        curr = curr->next;
    return curr;
}

// IR helper structs
struct ConditionIR
{
    ConditionalOperatorType op;
    int op1_loc;
    int op2_loc;
};

struct ExprIR
{
    ArithmeticOperatorType op;
    int op1_loc;
    int op2_loc;   
};

// Forward declarations of parse functions 

InstructionNode *parse_program();
void             parse_var_section();
void             parse_id_list();
InstructionNode *parse_body();
InstructionNode *parse_stmt_list();
InstructionNode *parse_stmt();

InstructionNode *parse_assign_stmt();
InstructionNode *parse_while_stmt();
InstructionNode *parse_if_stmt();
InstructionNode *parse_switch_stmt(); 
InstructionNode *parse_for_stmt();
InstructionNode *parse_output_stmt();
InstructionNode *parse_input_stmt();

ConditionIR      parse_condition();
ExprIR           parse_expr_or_single();
int              parse_primary();
ArithmeticOperatorType parse_op();
void             parse_inputs();


bool is_stmt_start(TokenType t)
{
    return (t == ID       ||
            t == WHILE    ||
            t == IF       ||
            t == SWITCH   ||
            t == FOR      ||
            t == OUTPUT   ||
            t == INPUT);
}

// Parsing functions

// program → var_section body inputs
InstructionNode *parse_program()
{
    if (current_token.token_type == VAR)
        expect(VAR);

    parse_var_section();
    InstructionNode *body = parse_body();
    parse_inputs();
    return body;
}

// var_section → id_list SEMICOLON
void parse_var_section()
{
    parse_id_list();
    expect(SEMICOLON);
}

// id_list → ID COMMA id_list | ID
void parse_id_list()
{
    if (current_token.token_type != ID)
        syntax_error("Expected ID in var section");

    // First ID
    string name = current_token.lexeme;
    get_var_location(name);        
    expect(ID);
    // Additional IDs
    while (current_token.token_type == COMMA)
    {
        expect(COMMA);
        if (current_token.token_type != ID)
            syntax_error("Expected ID after COMMA in var section");
        name = current_token.lexeme;
        get_var_location(name);
        expect(ID);
    }
}

// body → LBRACE stmt_list RBRACE
InstructionNode *parse_body()
{
    expect(LBRACE);
    InstructionNode *inst_list = parse_stmt_list();
    expect(RBRACE);
    return inst_list;
}

// stmt_list → stmt stmt_list | stmt
InstructionNode *parse_stmt_list()
{
    InstructionNode *first = parse_stmt();

    while (is_stmt_start(current_token.token_type))
    {
        InstructionNode *next_list = parse_stmt();
        first = append_instruction_lists(first, next_list);
    }
    return first;
}

// stmt → assign_stmt | while_stmt | if_stmt | switch_stmt | for_stmt
// stmt → output_stmt | input_stmt
InstructionNode *parse_stmt()
{
    switch (current_token.token_type)
    {
        case ID:
            return parse_assign_stmt();
        case WHILE:
            return parse_while_stmt();
        case IF:
            return parse_if_stmt();
        case SWITCH:
            return parse_switch_stmt();
        case FOR:
            return parse_for_stmt();
        case OUTPUT:
            return parse_output_stmt();
        case INPUT:
            return parse_input_stmt();
        default:
            syntax_error("Invalid statement start");
            return nullptr;
    }
}

// Assignment
// assign_stmt → ID EQUAL primary SEMICOLON
// assign_stmt → ID EQUAL expr SEMICOLON
// expr → primary op primary
// primary → ID | NUM
InstructionNode *parse_assign_stmt()
{
    // LHS ID
    if (current_token.token_type != ID)
        syntax_error("Expected ID at start of assignment");

    string lhs_name = current_token.lexeme;
    int lhs_loc = get_var_location(lhs_name);
    expect(ID);

    expect(EQUAL);

    // Parse the RHS
    int first_loc = parse_primary();

    ExprIR expr;
    expr.op1_loc = first_loc;
    expr.op2_loc = -1;
    expr.op      = OPERATOR_NONE;

    // Check if there's an operator or just a single primary
    if (current_token.token_type == PLUS ||
        current_token.token_type == MINUS ||
        current_token.token_type == MULT ||
        current_token.token_type == DIV)
    {
        expr.op = parse_op();
        expr.op2_loc = parse_primary();
    }

    expect(SEMICOLON);

    InstructionNode *inst = new InstructionNode;
    inst->type = ASSIGN;
    inst->next = nullptr;

    inst->assign_inst.lhs_loc = lhs_loc;
    inst->assign_inst.op1_loc = expr.op1_loc;
    inst->assign_inst.op2_loc = (expr.op == OPERATOR_NONE) ? 0 : expr.op2_loc;
    inst->assign_inst.op      = expr.op;

    return inst;
}

// primary → ID | NUM
int parse_primary()
{
    if (current_token.token_type == ID)
    {
        string name = current_token.lexeme;
        int loc = get_var_location(name);
        expect(ID);
        return loc;
    }
    else if (current_token.token_type == NUM)
    {
        int value = stoi(current_token.lexeme);
        int loc = get_const_location(value);
        expect(NUM);
        return loc;
    }
    else
    {
        syntax_error("Expected primary (ID or NUM)");
        return -1;
    }
}

// op → PLUS | MINUS | MULT | DIV
ArithmeticOperatorType parse_op()
{
    ArithmeticOperatorType op;
    switch (current_token.token_type)
    {
        case PLUS:
            op = OPERATOR_PLUS;
            expect(PLUS);
            break;
        case MINUS:
            op = OPERATOR_MINUS;
            expect(MINUS);
            break;
        case MULT:
            op = OPERATOR_MULT;
            expect(MULT);
            break;
        case DIV:
            op = OPERATOR_DIV;
            expect(DIV);
            break;
        default:
            syntax_error("Expected arithmetic operator");
            op = OPERATOR_NONE;
    }
    return op;
}

// Input / Output
// output_stmt → output ID SEMICOLON
// input_stmt  → input ID SEMICOLON
InstructionNode *parse_output_stmt()
{
    expect(OUTPUT);

    if (current_token.token_type != ID)
        syntax_error("Expected ID after output");

    string name = current_token.lexeme;
    int loc = get_var_location(name);
    expect(ID);
    expect(SEMICOLON);

    InstructionNode *inst = new InstructionNode;
    inst->type = OUT;
    inst->next = nullptr;
    inst->output_inst.var_loc = loc;

    return inst;
}

InstructionNode *parse_input_stmt()
{
    expect(INPUT);

    if (current_token.token_type != ID)
        syntax_error("Expected ID after input");

    string name = current_token.lexeme;
    int loc = get_var_location(name);
    expect(ID);
    expect(SEMICOLON);

    InstructionNode *inst = new InstructionNode;
    inst->type = IN;
    inst->next = nullptr;
    inst->input_inst.var_loc = loc;

    return inst;
}

// Condition
// condition → primary relop primary
// relop → GREATER | LESS | NOTEQUAL
ConditionIR parse_condition()
{
    ConditionIR cond;

    int op1_loc = parse_primary();

    if (current_token.token_type != GREATER &&
        current_token.token_type != LESS &&
        current_token.token_type != NOTEQUAL)
    {
        syntax_error("Expected relational operator");
    }

    switch (current_token.token_type)
    {
        case GREATER:
            cond.op = CONDITION_GREATER;
            expect(GREATER);
            break;
        case LESS:
            cond.op = CONDITION_LESS;
            expect(LESS);
            break;
        case NOTEQUAL:
            cond.op = CONDITION_NOTEQUAL;
            expect(NOTEQUAL);
            break;
        default:
            syntax_error("Unexpected relational operator");
    }

    int op2_loc = parse_primary();

    cond.op1_loc = op1_loc;
    cond.op2_loc = op2_loc;
    return cond;
}

// IF
// if_stmt → IF condition body
InstructionNode *parse_if_stmt()
{
    expect(IF);
    ConditionIR cond = parse_condition();

    InstructionNode *cjmp = new InstructionNode;
    cjmp->type = CJMP;
    cjmp->cjmp_inst.condition_op = cond.op;
    cjmp->cjmp_inst.op1_loc = cond.op1_loc;
    cjmp->cjmp_inst.op2_loc = cond.op2_loc;
    cjmp->next = nullptr;      // will set after parsing body

    // True branch (body)
    InstructionNode *body = parse_body();

    // NOOP after IF
    InstructionNode *noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    // Link CJMP
    cjmp->next = body;         // when condition is true

    // Append NOOP after body
    InstructionNode *last_body = get_last_instruction(body);
    if (last_body == nullptr)
    {
        // Empty body 
        cjmp->next = noop;
    }
    else
    {
        last_body->next = noop;
    }

    // When condition is false
    cjmp->cjmp_inst.target = noop;

    return cjmp;
}

// WHILE
// while_stmt → WHILE condition body
InstructionNode *parse_while_stmt()
{
    expect(WHILE);
    ConditionIR cond = parse_condition();

    InstructionNode *cjmp = new InstructionNode;
    cjmp->type = CJMP;
    cjmp->cjmp_inst.condition_op = cond.op;
    cjmp->cjmp_inst.op1_loc = cond.op1_loc;
    cjmp->cjmp_inst.op2_loc = cond.op2_loc;
    cjmp->next = nullptr;  // will point to body

    // Body
    InstructionNode *body = parse_body();

    // JMP back to CJMP
    InstructionNode *jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->next = nullptr;
    jmp->jmp_inst.target = cjmp;

    // NOOP after while
    InstructionNode *noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    // Link CJMP true branch to body
    cjmp->next = body;

    // Append JMP after body
    InstructionNode *last_body = get_last_instruction(body);
    if (last_body == nullptr)
    {
        // Empty body
        cjmp->next = jmp;
    }
    else
    {
        last_body->next = jmp;
    }

    // JMP goes to CJMP
    // NOOP after while
    jmp->next = noop;

    // On condition false, go to NOOP
    cjmp->cjmp_inst.target = noop;

    return cjmp;
}

// FOR
// for_stmt → FOR LPAREN assign_stmt condition SEMICOLON assign_stmt RPAREN body
InstructionNode *parse_for_stmt()
{
    expect(FOR);
    expect(LPAREN);

    // Initial assignment
    InstructionNode *init = parse_assign_stmt(); 

    // Condition
    ConditionIR cond = parse_condition();

    expect(SEMICOLON);

    // Update assignment
    InstructionNode *update = parse_assign_stmt();  

    expect(RPAREN);

    // Body
    InstructionNode *body = parse_body();

    // init -> [CJMP] -> body+update -> JMP back to CJMP -> NOOP

    InstructionNode *cjmp = new InstructionNode;
    cjmp->type = CJMP;
    cjmp->cjmp_inst.condition_op = cond.op;
    cjmp->cjmp_inst.op1_loc = cond.op1_loc;
    cjmp->cjmp_inst.op2_loc = cond.op2_loc;
    cjmp->next = nullptr;

    // Link init -> cjmp
    InstructionNode *last_init = get_last_instruction(init);
    last_init->next = cjmp;

    // True branch: body first
    cjmp->next = body;

    // Append update after body
    InstructionNode *last_body = get_last_instruction(body);
    last_body->next = update;

    // JMP back to CJMP after update
    InstructionNode *last_update = get_last_instruction(update);

    InstructionNode *jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = cjmp;
    jmp->next = nullptr;

    last_update->next = jmp;

    // NOOP after the loop
    InstructionNode *noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    jmp->next = noop;

    // When condition is false
    cjmp->cjmp_inst.target = noop;

    return init;
}

// SWITCH 
//   switch_stmt → SWITCH ID LBRACE case_list RBRACE
//   switch_stmt → SWITCH ID LBRACE case_list default_case RBRACE
InstructionNode *parse_switch_stmt()
{
    
    expect(SWITCH);

    // Switch variable
    if (current_token.token_type != ID)
        syntax_error("Expected ID after SWITCH");
    string switch_var = current_token.lexeme;
    int switch_loc = get_var_location(switch_var);
    expect(ID);

    expect(LBRACE);

    struct CaseInfo {
        InstructionNode *test;        
        InstructionNode *body_first;  
        InstructionNode *body_last;   
        InstructionNode *jmp_to_end;  
    };

    vector<CaseInfo> cases;
    bool has_default = false;
    InstructionNode *default_first = nullptr;
    InstructionNode *default_last  = nullptr;

    while (current_token.token_type == CASE)
    {
        expect(CASE);

        // CASE NUM
        if (current_token.token_type != NUM)
            syntax_error("Expected NUM after CASE");
        int case_value = stoi(current_token.lexeme);
        int case_const_loc = get_const_location(case_value);
        expect(NUM);

        expect(COLON);

        // Build CJMP
        InstructionNode *test = new InstructionNode;
        test->type = CJMP;
        test->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
        test->cjmp_inst.op1_loc = switch_loc;
        test->cjmp_inst.op2_loc = case_const_loc;
        test->next = nullptr;               
        test->cjmp_inst.target = nullptr;   

        
        InstructionNode *body = parse_body();
        InstructionNode *body_last = get_last_instruction(body);

       
        InstructionNode *jmp = new InstructionNode;
        jmp->type = JMP;
        jmp->jmp_inst.target = nullptr;    
        jmp->next = nullptr;

        body_last->next = jmp;

        CaseInfo ci;
        ci.test       = test;
        ci.body_first = body;
        ci.body_last  = body_last;
        ci.jmp_to_end = jmp;
        cases.push_back(ci);
    }

    if (current_token.token_type == DEFAULT)
    {
        has_default = true;
        expect(DEFAULT);
        expect(COLON);

        default_first = parse_body();
        default_last  = get_last_instruction(default_first);
    }

    expect(RBRACE);

    InstructionNode *end_noop = new InstructionNode;
    end_noop->type = NOOP;
    end_noop->next = nullptr;

    for (auto &ci : cases)
    {
        ci.jmp_to_end->jmp_inst.target = end_noop;
        ci.jmp_to_end->next = end_noop;
    }

    if (has_default && default_last != nullptr)
    {
        default_last->next = end_noop;
    }

    for (size_t i = 0; i < cases.size(); i++)
    {
        CaseInfo &ci = cases[i];

        ci.test->cjmp_inst.target = ci.body_first;

        if (i + 1 < cases.size())
        {
            ci.test->next = cases[i + 1].test;
        }
        else
        {
            if (has_default)
            {
                ci.test->next = default_first;
            }
            else
            {
                ci.test->next = end_noop;
            }
        }
    }

    InstructionNode *head = nullptr;
    if (!cases.empty())
    {
        head = cases[0].test;
    }
    else if (has_default)
    {
        head = default_first;
    }
    else
    {
        head = end_noop;
    }

    return head;

}

// Inputs
// inputs → num_list
// num_list → NUM | NUM num_list
void parse_inputs()
{
    while (current_token.token_type == NUM)
    {
        int value = stoi(current_token.lexeme);
        inputs.push_back(value);
        expect(NUM);
    }
}


InstructionNode *parse_Generate_Intermediate_Representation()
{
    get_next_token();
    return parse_program();
}
