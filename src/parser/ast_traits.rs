pub trait Node {
    fn token_litteral(&self) -> String;
}

pub trait Statement {
    fn statement_node(&self) -> String;
}

pub trait Expression {
    fn expression_node(&self) -> String;
}
