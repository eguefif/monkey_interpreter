use crate::{
    object::{BoolObject, Int, Null, Object, ObjectType},
    parser::ast_types::{
        Bool, Expression, ExpressionStatement, Integer, PrefixExpression, PrefixType, Statement,
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
            let right =
                evaluate_expression(&prefix.right).expect("Error while evaluating expression");
            evaluate_prefix(prefix, right)
        }
        Expression::Int(int) => Some(make_int(int)),
        Expression::Boolean(boolean) => Some(make_bool(boolean)),
        _ => None,
    }
}

fn evaluate_prefix(prefix: &PrefixExpression, right: Object) -> Option<Object> {
    match prefix.prefix_type {
        PrefixType::Bang => Some(evaluate_bang(right)),
        _ => None,
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
        let tests = [("5", 5), ("10", 10)];
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
}
