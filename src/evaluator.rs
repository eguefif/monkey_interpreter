use crate::{
    object::{BoolObject, Int, Null, Object, ObjectType},
    parser::ast_types::{
        Bool, Expression, InfixType, Integer, PrefixExpression, PrefixType, Statement,
    },
};

pub fn evaluate(stmt: &Statement) -> Option<Object> {
    match stmt {
        Statement::Expression(exp) => evaluate_expression(&exp.expression),
        _ => None,
    }
}

fn evaluate_expression(exp: &Expression) -> Option<Object> {
    match exp {
        Expression::PrefixOp(prefix) => {
            let right = evaluate_expression(&prefix.right)
                .expect("Error while evaluating prefix expression");
            evaluate_prefix(prefix, right)
        }
        Expression::Int(int) => Some(make_int(int)),
        Expression::Boolean(boolean) => Some(make_bool(boolean)),
        Expression::InfixOp(infix) => {
            let right =
                evaluate_expression(&infix.right).expect("error while evaluting infix expression");

            let left =
                evaluate_expression(&infix.left).expect("error while evaluting infix expression");
            evaluate_infix(&infix.infix_type, left, right)
        }
        _ => None,
    }
}

fn evaluate_infix(op: &InfixType, left: Object, right: Object) -> Option<Object> {
    if let ObjectType::Int(right) = right.obj_type {
        if let ObjectType::Int(left) = left.obj_type {
            match op {
                InfixType::Add => {
                    return Some(Object::new(ObjectType::Int(Int {
                        value: right.value + left.value,
                    })))
                }
                InfixType::Sub => {
                    return Some(Object::new(ObjectType::Int(Int {
                        value: left.value - right.value,
                    })))
                }
                InfixType::Mul => {
                    return Some(Object::new(ObjectType::Int(Int {
                        value: right.value * left.value,
                    })))
                }
                InfixType::Div => {
                    if right.value == 0 {
                        panic!("Div by 0 is forbidden")
                    }
                    return Some(Object::new(ObjectType::Int(Int {
                        value: left.value / right.value,
                    })));
                }
                InfixType::Gt => {
                    return Some(Object::new(ObjectType::Bool(BoolObject {
                        value: left.value > right.value,
                    })));
                }
                InfixType::Lt => {
                    return Some(Object::new(ObjectType::Bool(BoolObject {
                        value: left.value < right.value,
                    })));
                }
                InfixType::Eq => {
                    return Some(Object::new(ObjectType::Bool(BoolObject {
                        value: left.value == right.value,
                    })));
                }
                InfixType::Noteq => {
                    return Some(Object::new(ObjectType::Bool(BoolObject {
                        value: left.value != right.value,
                    })));
                }
                InfixType::None => return Some(Object::new(ObjectType::Null(Null {}))),
            }
        }
    }
    Some(Object::new(ObjectType::Null(Null {})))
}

fn evaluate_prefix(prefix: &PrefixExpression, right: Object) -> Option<Object> {
    match prefix.prefix_type {
        PrefixType::Bang => Some(evaluate_bang(right)),
        PrefixType::Minus => Some(evaluate_minus(right)),
        _ => None,
    }
}

fn evaluate_minus(right: Object) -> Object {
    match right.obj_type {
        ObjectType::Int(value) => Object::new(ObjectType::Int(Int {
            value: -value.value,
        })),
        _ => Object::new(ObjectType::Null(Null {})),
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
        ObjectType::Null(_) => Object::new(ObjectType::Bool(BoolObject { value: true })),
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
        evaluate(&prog.statements[0]).expect("No evaluation possible")
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
        let tests = [("true", true), ("false", false)];
        for (input, expected) in tests {
            let obj = test_eval(input);
            assert_bool(obj, expected)
        }
    }

    fn assert_bool(obj: Object, expected: bool) {
        if let ObjectType::Bool(boolean) = obj.obj_type {
            assert_eq!(expected, boolean.value)
        } else {
            panic!("Obj is not an bool")
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
}
