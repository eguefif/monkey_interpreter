trait Node {
    fn token_litteral(&self) -> String;
}

trait Statement {
    fn statement_node(&self) -> String;
}

trait Expression {
    fn expression_node(&self) -> String;
}
