use crate::{
    object::{BoolObject, Int, Object, ObjectType},
    parser::ast_types::{
        Bool, Expression, InfixType, Integer, PrefixExpression, PrefixType, ReturnStatement,
        Statement,
    },
};

pub fn evaluate(statements: &Vec<Statement>) -> Object {
    let null = Object::new(ObjectType::Null);
    let mut retval = Object::new(ObjectType::Return(Box::new(null)));
    for statement in statements {
        retval = match statement {
            Statement::Expression(exp) => evaluate_expression(&exp.expression),
            Statement::Return(exp) => evaluate_return(&exp),
            _ => Object::new(ObjectType::Null),
        };
        if let ObjectType::Return(_) = retval.obj_type {
            return retval;
        }
    }
    retval
}

fn evaluate_return(exp: &ReturnStatement) -> Object {
    if let Some(retval) = &exp.return_value {
        let obj = evaluate_expression(&retval);
        Object::new(ObjectType::Return(Box::new(obj)))
    } else {
        let null = Object::new(ObjectType::Null);
        Object::new(ObjectType::Return(Box::new(null)))
    }
}

fn evaluate_expression(exp: &Expression) -> Object {
    match exp {
        Expression::If(if_exp) => {
            let cond = evaluate_expression(&if_exp.condition);
            if is_obj_truthy(cond) {
                return evaluate(&if_exp.consequence.statements);
            } else {
                if let Some(alternative) = &if_exp.alternative {
                    return evaluate(&alternative.statements);
                } else {
                    return Object::new(ObjectType::Null);
                }
            }
        }
        Expression::PrefixOp(prefix) => {
            let right = evaluate_expression(&prefix.right);
            evaluate_prefix(prefix, right)
        }
        Expression::Int(int) => make_int(int),
        Expression::Boolean(boolean) => make_bool(boolean),
        Expression::InfixOp(infix) => {
            let right = evaluate_expression(&infix.right);

            let left = evaluate_expression(&infix.left);
            evaluate_infix(&infix.infix_type, left, right)
        }
        _ => Object::new(ObjectType::Null),
    }
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

fn evaluate_infix(op: &InfixType, left: Object, right: Object) -> Object {
    match (left.obj_type, right.obj_type) {
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
        _ => return Object::new(ObjectType::Null),
    }
}

fn evaluate_infix_int_vs_int(op: &InfixType, left: i128, right: i128) -> Object {
    match op {
        InfixType::Add => {
            return Object::new(ObjectType::Int(Int {
                value: right + left,
            }))
        }
        InfixType::Sub => {
            return Object::new(ObjectType::Int(Int {
                value: left - right,
            }))
        }
        InfixType::Mul => Object::new(ObjectType::Int(Int {
            value: right * left,
        })),
        InfixType::Div => {
            if right == 0 {
                panic!("Div by 0 is forbidden")
            }
            Object::new(ObjectType::Int(Int {
                value: left / right,
            }))
        }
        InfixType::Gt => Object::new(ObjectType::Bool(BoolObject {
            value: left > right,
        })),
        InfixType::Lt => Object::new(ObjectType::Bool(BoolObject {
            value: left < right,
        })),
        InfixType::Eq => Object::new(ObjectType::Bool(BoolObject {
            value: left == right,
        })),
        InfixType::Noteq => Object::new(ObjectType::Bool(BoolObject {
            value: left != right,
        })),
        InfixType::None => Object::new(ObjectType::Null),
    }
}

fn evaluate_infix_bool_vs_int(op: &InfixType, left: bool, right: i128) -> Object {
    match op {
        InfixType::Eq => {
            if right == 0 {
                Object::new(ObjectType::Bool(BoolObject {
                    value: left == false,
                }))
            } else {
                Object::new(ObjectType::Bool(BoolObject {
                    value: left == true,
                }))
            }
        }
        InfixType::Noteq => {
            if right == 0 {
                Object::new(ObjectType::Bool(BoolObject {
                    value: left != false,
                }))
            } else {
                Object::new(ObjectType::Bool(BoolObject {
                    value: left != true,
                }))
            }
        }
        _ => return Object::new(ObjectType::Null),
    }
}

fn evaluate_infix_int_vs_bool(op: &InfixType, left: i128, right: bool) -> Object {
    match op {
        InfixType::Eq => {
            if left == 0 {
                Object::new(ObjectType::Bool(BoolObject {
                    value: false == right,
                }))
            } else {
                Object::new(ObjectType::Bool(BoolObject {
                    value: true == right,
                }))
            }
        }
        InfixType::Noteq => {
            if left == 0 {
                Object::new(ObjectType::Bool(BoolObject {
                    value: false != right,
                }))
            } else {
                Object::new(ObjectType::Bool(BoolObject {
                    value: true != right,
                }))
            }
        }
        _ => return Object::new(ObjectType::Null),
    }
}

fn evaluate_infix_bool_vs_bool(op: &InfixType, left: bool, right: bool) -> Object {
    match op {
        InfixType::Eq => {
            return Object::new(ObjectType::Bool(BoolObject {
                value: left == right,
            }))
        }
        InfixType::Noteq => Object::new(ObjectType::Bool(BoolObject {
            value: left != right,
        })),
        _ => return Object::new(ObjectType::Null),
    }
}

fn evaluate_prefix(prefix: &PrefixExpression, right: Object) -> Object {
    match prefix.prefix_type {
        PrefixType::Bang => evaluate_bang(right),
        PrefixType::Minus => evaluate_minus(right),
        _ => Object::new(ObjectType::Null),
    }
}

fn evaluate_minus(right: Object) -> Object {
    match right.obj_type {
        ObjectType::Int(value) => Object::new(ObjectType::Int(Int {
            value: -value.value,
        })),
        _ => Object::new(ObjectType::Null),
    }
}

fn evaluate_bang(right: Object) -> Object {
    match right.obj_type {
        ObjectType::Int(value) => {
            if value.value == 0 {
                Object::new(ObjectType::Bool(BoolObject { value: true }))
            } else {
                Object::new(ObjectType::Bool(BoolObject { value: false }))
            }
        }
        ObjectType::Str(value) => {
            if value.value.len() == 0 {
                Object::new(ObjectType::Bool(BoolObject { value: true }))
            } else {
                Object::new(ObjectType::Bool(BoolObject { value: false }))
            }
        }

        ObjectType::Bool(boolean) => {
            if boolean.value {
                Object::new(ObjectType::Bool(BoolObject { value: false }))
            } else {
                Object::new(ObjectType::Bool(BoolObject { value: true }))
            }
        }
        _ => Object::new(ObjectType::Bool(BoolObject { value: true })),
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
        evaluate(&prog.statements)
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
        println!("{:?}", obj);
        assert_return(obj, &ObjectType::Int(Int { value: 10 }))
    }

    fn assert_return(obj: Object, expected: &ObjectType) {
        if let ObjectType::Return(value) = obj.obj_type {
            assert_eq!(value.obj_type, *expected)
        } else {
            panic!("Not a return object");
        }
    }
}
