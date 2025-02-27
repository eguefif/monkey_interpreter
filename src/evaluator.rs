// Todo: refactor into a struct to keep a mutable vec of identifiers ?
use crate::{
    object::{BoolObject, Int, Object, ObjectType, Variable},
    parser::ast_types::{
        Bool, Expression, InfixType, Integer, LetStatement, PrefixExpression, PrefixType,
        ReturnStatement, Statement,
    },
};

pub fn eval_program(statements: &Vec<Statement>) -> Result<Object, String> {
    let null = Object::new(ObjectType::Null);
    let mut retval = Object::new(ObjectType::Return(Box::new(null)));
    let mut variables: Vec<Variable> = Vec::new();
    for statement in statements {
        retval = evaluate(statement, &variables)?;
        retval = match retval.obj_type {
            ObjectType::Let(value) => {
                variables.push(*value);
                Object::new(ObjectType::Null)
            }
            ObjectType::Return(value) => return Ok(*value),
            _ => retval,
        }
    }
    Ok(retval)
}

fn evaluate(statement: &Statement, variables: &Vec<Variable>) -> Result<Object, String> {
    let retval = match statement {
        Statement::Expression(exp) => evaluate_expression(&exp.expression, variables),
        Statement::Return(exp) => evaluate_return(&exp, variables),
        Statement::Let(exp) => evaluate_let(&exp, variables),
    };
    retval
}

fn evaluate_let(exp: &LetStatement, variables: &Vec<Variable>) -> Result<Object, String> {
    let value = evaluate_expression(&exp.value, variables)?;
    let var = Variable {
        value,
        name: exp.identifier.value.clone(),
    };
    Ok(Object::new(ObjectType::Let(Box::new(var))))
}

fn evaluate_block_statement(
    statements: &Vec<Statement>,
    variables: &Vec<Variable>,
) -> Result<Object, String> {
    let null = Object::new(ObjectType::Null);
    let mut retval = Object::new(ObjectType::Return(Box::new(null)));
    for statement in statements {
        retval = evaluate(statement, variables)?;

        retval = match retval.obj_type {
            ObjectType::Return(_) => return Ok(retval),
            _ => retval,
        }
    }
    Ok(retval)
}

fn evaluate_return(exp: &ReturnStatement, variables: &Vec<Variable>) -> Result<Object, String> {
    if let Some(retval) = &exp.return_value {
        let obj = evaluate_expression(&retval, variables)?;
        Ok(Object::new(ObjectType::Return(Box::new(obj))))
    } else {
        let null = Object::new(ObjectType::Null);
        Ok(Object::new(ObjectType::Return(Box::new(null))))
    }
}

fn evaluate_expression(exp: &Expression, variables: &Vec<Variable>) -> Result<Object, String> {
    match exp {
        Expression::Identifier(ident) => {
            let value = get_value_from_variables(ident.value.clone(), &variables)?;
            Ok(value)
        }
        Expression::If(if_exp) => {
            let cond = evaluate_expression(&if_exp.condition, &variables)?;
            if is_obj_truthy(cond) {
                return evaluate_block_statement(&if_exp.consequence.statements, variables);
            } else {
                if let Some(alternative) = &if_exp.alternative {
                    return evaluate_block_statement(&alternative.statements, variables);
                } else {
                    return Ok(Object::new(ObjectType::Null));
                }
            }
        }
        Expression::PrefixOp(prefix) => {
            let right = evaluate_expression(&prefix.right, &variables)?;
            evaluate_prefix(prefix, right)
        }
        Expression::Int(int) => Ok(make_int(int)),
        Expression::Boolean(boolean) => Ok(make_bool(boolean)),
        Expression::InfixOp(infix) => {
            let right = evaluate_expression(&infix.right, &variables)?;

            let left = evaluate_expression(&infix.left, &variables)?;
            evaluate_infix(&infix.infix_type, left, right)
        }
        _ => Ok(Object::new(ObjectType::Null)),
    }
}

fn get_value_from_variables(ident: String, variables: &Vec<Variable>) -> Result<Object, String> {
    for variable in variables {
        if variable.name == ident {
            let obj = match &variable.value.obj_type {
                ObjectType::Int(int) => Object::new(ObjectType::Int(Int { value: int.value })),
                _ => return Err("Unknown type for identifier".to_string()),
            };
            return Ok(obj);
        }
    }
    Err("Unknown identifier".to_string())
}

fn is_obj_truthy(obj: Object) -> bool {
    match obj.obj_type {
        ObjectType::Bool(value) => {
            if value.value {
                true
            } else {
                false
            }
        }
        ObjectType::Int(value) => {
            if value.value == 0 {
                false
            } else {
                true
            }
        }
        ObjectType::Str(value) => {
            if value.value.len() > 0 {
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn evaluate_infix(op: &InfixType, left: Object, right: Object) -> Result<Object, String> {
    match (&left.obj_type, &right.obj_type) {
        (ObjectType::Int(left), ObjectType::Int(right)) => {
            return evaluate_infix_int_vs_int(op, left.value, right.value);
        }
        (ObjectType::Bool(left), ObjectType::Int(right)) => {
            return evaluate_infix_bool_vs_int(op, left.value, right.value);
        }
        (ObjectType::Int(left), ObjectType::Bool(right)) => {
            return evaluate_infix_int_vs_bool(op, left.value, right.value);
        }
        (ObjectType::Bool(left), ObjectType::Bool(right)) => {
            return evaluate_infix_bool_vs_bool(op, left.value, right.value);
        }
        _ => {
            return Err(format!(
                "type mismatch: cannot do {} {} {}",
                left.obj_type, op, right.obj_type
            ))
        }
    }
}

fn evaluate_infix_int_vs_int(op: &InfixType, left: i128, right: i128) -> Result<Object, String> {
    match op {
        InfixType::Add => Ok(Object::new(ObjectType::Int(Int {
            value: right + left,
        }))),
        InfixType::Sub => Ok(Object::new(ObjectType::Int(Int {
            value: left - right,
        }))),
        InfixType::Mul => Ok(Object::new(ObjectType::Int(Int {
            value: right * left,
        }))),
        InfixType::Div => {
            if right == 0 {
                return Err("Div by 0 is forbidden".to_string());
            }
            Ok(Object::new(ObjectType::Int(Int {
                value: left / right,
            })))
        }
        InfixType::Gt => Ok(Object::new(ObjectType::Bool(BoolObject {
            value: left > right,
        }))),
        InfixType::Lt => Ok(Object::new(ObjectType::Bool(BoolObject {
            value: left < right,
        }))),
        InfixType::Eq => Ok(Object::new(ObjectType::Bool(BoolObject {
            value: left == right,
        }))),
        InfixType::Noteq => Ok(Object::new(ObjectType::Bool(BoolObject {
            value: left != right,
        }))),
        _ => Err(format!(
            "type mismatch: cannot do {} {} {}",
            left, op, right
        )),
    }
}

fn evaluate_infix_bool_vs_int(op: &InfixType, left: bool, right: i128) -> Result<Object, String> {
    match op {
        InfixType::Eq => {
            if right == 0 {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: left == false,
                })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: left == true,
                })))
            }
        }
        InfixType::Noteq => {
            if right == 0 {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: left != false,
                })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: left != true,
                })))
            }
        }
        _ => Err(format!("type mismatch: cannot do BOOLEAN {} INTEGER", op)),
    }
}

fn evaluate_infix_int_vs_bool(op: &InfixType, left: i128, right: bool) -> Result<Object, String> {
    match op {
        InfixType::Eq => {
            if left == 0 {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: false == right,
                })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: true == right,
                })))
            }
        }
        InfixType::Noteq => {
            if left == 0 {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: false != right,
                })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject {
                    value: true != right,
                })))
            }
        }
        _ => Err(format!("type mismatch: cannot do INTEGER {} BOOLEAN", op)),
    }
}

fn evaluate_infix_bool_vs_bool(op: &InfixType, left: bool, right: bool) -> Result<Object, String> {
    match op {
        InfixType::Eq => {
            return Ok(Object::new(ObjectType::Bool(BoolObject {
                value: left == right,
            })))
        }
        InfixType::Noteq => Ok(Object::new(ObjectType::Bool(BoolObject {
            value: left != right,
        }))),
        _ => Err(format!("unknown operator: BOOLEAN {} BOOLEAN", op)),
    }
}

fn evaluate_prefix(prefix: &PrefixExpression, right: Object) -> Result<Object, String> {
    match prefix.prefix_type {
        PrefixType::Bang => evaluate_bang(right),
        PrefixType::Minus => evaluate_minus(right),
        _ => Ok(Object::new(ObjectType::Null)),
    }
}

fn evaluate_minus(right: Object) -> Result<Object, String> {
    match right.obj_type {
        ObjectType::Int(value) => Ok(Object::new(ObjectType::Int(Int {
            value: -value.value,
        }))),
        ObjectType::Bool(_) => Err("unknown operator: -BOOLEAN".to_string()),
        _ => Ok(Object::new(ObjectType::Null)),
    }
}

fn evaluate_bang(right: Object) -> Result<Object, String> {
    match right.obj_type {
        ObjectType::Int(value) => {
            if value.value == 0 {
                Ok(Object::new(ObjectType::Bool(BoolObject { value: true })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject { value: false })))
            }
        }
        ObjectType::Str(value) => {
            if value.value.len() == 0 {
                Ok(Object::new(ObjectType::Bool(BoolObject { value: true })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject { value: false })))
            }
        }
        ObjectType::Bool(boolean) => {
            if boolean.value {
                Ok(Object::new(ObjectType::Bool(BoolObject { value: false })))
            } else {
                Ok(Object::new(ObjectType::Bool(BoolObject { value: true })))
            }
        }
        _ => Ok(Object::new(ObjectType::Bool(BoolObject { value: true }))),
    }
}

fn make_int(int: &Integer) -> Object {
    Object::new(ObjectType::Int(Int { value: int.value }))
}

fn make_bool(boolean: &Bool) -> Object {
    Object::new(ObjectType::Bool(BoolObject {
        value: boolean.value,
    }))
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, tokenizer::lexer::Lexer};

    use super::*;

    #[test]
    fn it_should_eval_integer() {
        let tests = [("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];
        for (input, expected) in tests {
            let obj = test_eval(input);
            assert_int(obj, expected)
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let prog = parser.parse_program().expect("Expect a program");
        eval_program(&prog.statements).expect("Should be a OK result")
    }

    fn assert_int(obj: Object, expected: i128) {
        if let ObjectType::Int(integer) = obj.obj_type {
            assert_eq!(expected, integer.value)
        } else {
            panic!("Obj is not an int")
        }
    }

    #[test]
    fn it_should_eval_bool() {
        let tests = [
            ("true", true),
            ("false", false),
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            let obj = test_eval(input);
            assert_bool(obj, expected)
        }
    }

    fn assert_bool(obj: Object, expected: bool) {
        if let ObjectType::Bool(boolean) = obj.obj_type {
            assert_eq!(expected, boolean.value)
        } else {
            println!("{:} and expected: {:?}", obj, expected);
            panic!("Obj is not a bool")
        }
    }

    #[test]
    fn it_should_eval_bang_op() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let obj = test_eval(input);
            assert_bool(obj, expected)
        }
    }

    #[test]
    fn it_should_eval_infix() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let obj = test_eval(input);
            assert_int(obj, expected)
        }
    }

    #[test]
    fn it_should_evaluate_if_condition() {
        let tests = [
            ("if (true) { 10 }", ObjectType::Int(Int { value: 10 })),
            ("if (false) { 10 }", ObjectType::Null),
            ("if (1) { 10 }", ObjectType::Int(Int { value: 10 })),
            ("if (1 < 2) { 10 }", ObjectType::Int(Int { value: 10 })),
            ("if (1 > 2) { 10 }", ObjectType::Null),
            (
                "if (1 > 2) { 10 } else { 20 }",
                ObjectType::Int(Int { value: 20 }),
            ),
            (
                "if (1 < 2) { 10 } else { 20 }",
                ObjectType::Int(Int { value: 10 }),
            ),
        ];

        for (input, expected) in tests {
            let obj = test_eval(input);
            assert_eq!(obj.obj_type, expected)
        }
    }

    #[test]
    fn it_should_evaluate_return_statement() {
        let tests = [
            ("return 10;", ObjectType::Int(Int { value: 10 })),
            ("return 10; 9;", ObjectType::Int(Int { value: 10 })),
            ("return 2 * 5; 8;", ObjectType::Int(Int { value: 10 })),
            ("9; return 2 * 5; 9;", ObjectType::Int(Int { value: 10 })),
        ];

        for (input, expected) in tests {
            let obj = test_eval(input);
            println!("{:?}", obj);
            assert_return(obj, &expected);
        }
    }

    #[test]
    fn it_should_evaluate_return_statement_multiple_blocks() {
        let input = "
if (10 > 1) {
    if (10 > 1) {
        return 10;
    }
}
return 1;
        ";

        let obj = test_eval(input);
        assert_return(obj, &ObjectType::Int(Int { value: 10 }))
    }

    fn assert_return(obj: Object, expected: &ObjectType) {
        assert_eq!(obj.obj_type, *expected)
    }

    #[test]
    fn it_should_handle_error() {
        let tests = [
            ("5 + true;", "type mismatch: cannot do INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: cannot do INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "
if (10 > 1) {
if (10 > 1) {
return true + false;
}
return 1;
}",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("let a = 5; b;", "Unknown identifier"),
        ];
        for (test, expected) in tests {
            let result = test_eval_with_error(test);
            if let Err(error) = result {
                assert_eq!(error, expected)
            } else {
                panic!("Expect an error type")
            }
        }
    }

    fn test_eval_with_error(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let prog = parser.parse_program().expect("Expect a program");
        eval_program(&prog.statements)
    }

    #[test]
    fn it_should_evaluate_let_statement() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = 5; b;", 5),
            ("let a = 5; let b = 5; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            println!("result: {:?}", result);
            println!("expected: {:?}", expected);
            assert_int(result, expected)
        }
    }
}
