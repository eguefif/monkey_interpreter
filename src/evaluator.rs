use crate::{
    object::{Int, Object, ObjectType},
    parser::ast_types::Statement,
};

fn evaluate(stmt: &Statement) -> Option<Object> {
    Some(Object::new(ObjectType::Int(Int { value: 3 })))
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

    fn assert_int(obj: Object, expected: i64) {
        if let ObjectType::Int(integer) = obj.obj_type {
            assert_eq!(expected, integer.value)
        } else {
            panic!("Obj is not an int")
        }
    }
}
