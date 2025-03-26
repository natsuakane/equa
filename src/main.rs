use regex::Regex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::f32::consts::PI;
use std::f64::MAX_EXP;
use std::sync::{LazyLock, Mutex};

#[derive(Clone)]
struct Pos {
    col: u32,
    pos: u32,
}

#[derive(Clone)]
enum Token {
    Number(String),
    StringLiteral(String),
    Identifier(String),
    Operator(String),
    EOF,
}
impl Token {
    pub fn print(&self) {
        match &self {
            Token::Number(num) => println!("num:{}", num),
            Token::StringLiteral(lit) => println!("literal:{}", lit),
            Token::Identifier(id) => println!("id:{}", id),
            Token::Operator(op) => println!("op:{}", op),
            Token::EOF => println!("EOF"),
        }
    }
    pub fn str(&self) -> String {
        match &self {
            Token::Number(num) => num.clone(),
            Token::StringLiteral(lit) => lit.clone(),
            Token::Identifier(id) => id.clone(),
            Token::Operator(op) => op.clone(),
            Token::EOF => "EOF".to_string(),
        }
    }
}

struct Lexer {
    code: String,
    iter: i32,
    que: VecDeque<Token>,
}
impl Lexer {
    fn new(program: String) -> Self {
        Lexer {
            code: program,
            iter: 0,
            que: VecDeque::new(),
        }
    }
    pub fn read(&mut self) -> Option<Token> {
        self.que.pop_front()
    }
    pub fn peek(&self) -> Option<Token> {
        self.que.front().map(|t| t.clone())
    }
    pub fn lex(&mut self) {
        let re =
            Regex::new(r#"(?P<num>\d+(\.\d+)?)|(?P<id>[a-zA-Z][a-zA-Z0-9_]*)|(?P<literal>"(?:\\.|[^"\\])*?")|(?P<op>\S)"#)
                .unwrap();

        for cap in re.captures_iter(self.code.as_str()) {
            if let Some(m) = cap.name("num") {
                self.que.push_back(Token::Number(m.to_string()));
            } else if let Some(m) = cap.name("id") {
                self.que.push_back(Token::Identifier(m.to_string()));
            } else if let Some(m) = cap.name("literal") {
                self.que.push_back(Token::StringLiteral(m.to_string()));
            } else if let Some(m) = cap.name("op") {
                self.que.push_back(Token::Operator(m.to_string()));
            }
        }

        self.que.push_back(Token::EOF);
    }
}

#[derive(Clone)]
enum Value {
    Num(f64),
    Expression(AstNode),
    None,
}
impl Value {
    pub fn getnum(&self) -> Result<f64, String> {
        match self {
            Value::Num(n) => Ok(n.clone()),
            Value::Expression(expression) => match expression.eval()? {
                Value::Num(n) => Ok(n.clone()),
                _ => Err("Expected number but not given.".to_string()),
            },
            Value::None => Err("Expected number but none is given.".to_string()),
        }
    }
}

struct Environment {
    stack: Vec<HashMap<String, (Value, bool)>>,
}
impl Environment {
    fn new() -> Self {
        Environment {
            stack: vec![HashMap::new()],
        }
    }
    pub fn find(&self, name: String) -> Result<Value, String> {
        for i in 0..self.stack.len() {
            if let Some(value) = self.stack[self.stack.len() - i - 1].get(&name) {
                return Ok(value.clone().0);
            }
        }
        Err(format!("Variable '{}' is not defined.", name))
    }
    pub fn add(&mut self, name: String, value: Value) -> Result<(), String> {
        let pos = self.stack.len() - 1;
        if let Some((_, b)) = self.stack[pos].get(&name) {
            if !b {
                Err(format!(
                    "The variable '{}' is a constant but you are trying to reassign it.",
                    name
                ))
            } else {
                self.stack.get_mut(pos).unwrap().insert(name, (value, true));
                Ok(())
            }
        } else {
            self.stack.get_mut(pos).unwrap().insert(name, (value, true));
            Ok(())
        }
    }
    pub fn add_const(&mut self, name: String, value: Value) -> Result<(), String> {
        let pos = self.stack.len() - 1;
        let const_value = match value {
            Value::Num(n) => Value::Num(n),
            Value::Expression(exp) => {
                if let Token::Number(n) = exp.token {
                    Value::Num(n.parse::<f64>().unwrap())
                } else {
                    return Err("Constant variables can only contain constants".to_string());
                }
            }
            _ => return Err("Constant variables can only contain constants".to_string()),
        };
        if let Some((_, b)) = self.stack[pos].get(&name) {
            if !b {
                Err(format!(
                    "The variable '{}' is a constant but you are trying to reassign it.",
                    name
                ))
            } else {
                self.stack
                    .get_mut(pos)
                    .unwrap()
                    .insert(name, (const_value, false));
                Ok(())
            }
        } else {
            self.stack
                .get_mut(pos)
                .unwrap()
                .insert(name, (const_value, false));
            Ok(())
        }
    }
    pub fn new_env(&mut self) {
        self.stack.push(HashMap::new());
    }
}
static ENV: LazyLock<Mutex<Environment>> = LazyLock::new(|| Mutex::new(Environment::new()));

#[derive(Clone)]
struct AstNode {
    children: Vec<AstNode>,
    token: Token,
    pos: Pos,
}
impl AstNode {
    fn new(t: Token, p: Pos) -> AstNode {
        return AstNode {
            token: t,
            pos: p,
            children: vec![],
        };
    }
    fn new_with_children(ch: Vec<AstNode>, t: Token, p: Pos) -> AstNode {
        return AstNode {
            token: t,
            pos: p,
            children: ch,
        };
    }
    pub fn print(&self) -> String {
        let mut res: String = format!("({}", self.token.str());
        for node in self.children.iter() {
            res += &format!(" {}", node.print());
        }
        res += ")";
        res
    }
    pub fn eval(&self) -> Result<Value, String> {
        match &self.token {
            Token::Number(num) => match num.parse::<f64>() {
                Ok(n) => Ok(Value::Num(n)),
                Err(msg) => Err(msg.to_string()),
            },
            Token::Operator(op) => match op.as_str() {
                "+" => Ok(Value::Num(
                    self.children[0].eval()?.getnum()? + self.children[1].eval()?.getnum()?,
                )),
                "-" => Ok(Value::Num(
                    self.children[0].eval()?.getnum()? - self.children[1].eval()?.getnum()?,
                )),
                "*" => Ok(Value::Num(
                    self.children[0].eval()?.getnum()? * self.children[1].eval()?.getnum()?,
                )),
                "/" => Ok(Value::Num(
                    self.children[0].eval()?.getnum()? / self.children[1].eval()?.getnum()?,
                )),
                "%" => Ok(Value::Num(
                    self.children[0].eval()?.getnum()? % self.children[1].eval()?.getnum()?,
                )),
                "block" => {
                    let mut res = Value::None;
                    for ch in &self.children {
                        res = ch.eval()?;
                    }
                    Ok(res.clone())
                }
                _ => Err(format!("Operator '{}' is not defined.", op)),
            },
            Token::Identifier(id) => match id.as_str() {
                "assign" => {
                    if let Token::Identifier(id) = &self.children[0].token {
                        ENV.lock()
                            .unwrap()
                            .add(id.clone(), Value::Expression(self.children[1].clone()))?;
                        Ok(Value::None)
                    } else {
                        Err("Expected identifier.".to_string())
                    }
                }
                "const" => {
                    if let Token::Identifier(id) = &self.children[0].token {
                        ENV.lock()
                            .unwrap()
                            .add_const(id.clone(), Value::Expression(self.children[1].clone()))?;
                        Ok(Value::None)
                    } else {
                        Err("Expected identifier.".to_string())
                    }
                }
                "print" => {
                    println!("{}", self.children[0].eval()?.getnum()?);
                    Ok(Value::None)
                }
                _ => ENV.lock().unwrap().find(id.clone()),
            },
            _ => Err("Invalid token.".to_string()),
        }
    }
}

struct Parser {
    lexer: Lexer,
    operators: HashMap<String, i32>,
}
impl Parser {
    fn new(lex: Lexer) -> Self {
        let mut map: HashMap<String, i32> = HashMap::new();
        map.insert("+".to_string(), 1);
        map.insert("-".to_string(), 1);
        map.insert("*".to_string(), 2);
        map.insert("/".to_string(), 2);
        Parser {
            lexer: lex,
            operators: map,
        }
    }
    fn token(&mut self, t: &str) -> Result<(), String> {
        match self.lexer.read().unwrap() {
            Token::Operator(op) => {
                if op != t.to_string() {
                    return Err(format!("Invalid token '{}', correct token is '{}'.", op, t));
                }
            }
            Token::Identifier(op) => {
                if op != t.to_string() {
                    return Err(format!("Invalid token '{}', correct token is '{}'.", op, t));
                }
            }
            Token::StringLiteral(s) => {
                return Err(format!(
                    "Invalid token '\"{}\"', correct token is '{}'.",
                    s, t
                ));
            }
            Token::Number(n) => {
                return Err(format!("Invalid token '{}', correct token is '{}'.", n, t));
            }
            Token::EOF => {
                return Err(format!("Invalid token 'EOF', correct token is '{}'.", t));
            }
        }
        Ok(())
    }
    fn istoken(&mut self, t: &str) -> bool {
        match self.lexer.peek().unwrap() {
            Token::Operator(op) => {
                return op == t;
            }
            Token::Identifier(op) => {
                return op == t;
            }
            Token::StringLiteral(s) => {
                return false;
            }
            Token::Number(n) => {
                return false;
            }
            Token::EOF => {
                return false;
            }
        }
    }
    fn isend(&self) -> bool {
        match self.lexer.peek().unwrap() {
            Token::EOF => true,
            _ => false,
        }
    }
    fn get_id(&mut self) -> Result<String, String> {
        match self.lexer.read().unwrap() {
            Token::Identifier(id) => Ok(id),
            Token::Operator(op) => Err(format!("Operator '{}' is not identifier.", op)),
            Token::StringLiteral(s) => Err(format!("String Literal \"{}\" is not identifier.", s)),
            Token::Number(n) => Err(format!("Number '{}' is not identifier.", n)),
            Token::EOF => Err(format!("'EOF' is not identifier.")),
        }
    }

    pub fn factor(&mut self) -> Result<AstNode, String> {
        match self.lexer.read().unwrap() {
            Token::Number(num) => Ok(AstNode::new(Token::Number(num), Pos { col: 0, pos: 0 })),
            Token::StringLiteral(sl) => Ok(AstNode::new(
                Token::StringLiteral(sl),
                Pos { col: 0, pos: 0 },
            )),
            Token::Identifier(id) => {
                Ok(AstNode::new(Token::Identifier(id), Pos { col: 0, pos: 0 }))
            }
            Token::Operator(op) => {
                if op == "(" {
                    let exp = self.parse_expression(0)?;
                    self.token(")")?;
                    return Ok(exp);
                }
                Err(format!("Invalid token '{}'.", op))
            }
            Token::EOF => Err(format!("Invalid token 'EOF'.")),
        }
    }

    pub fn funccall(&mut self) -> Result<AstNode, String> {
        let mut func: AstNode = self.factor()?;
        while self.istoken("(") {
            self.token("(")?;
            let arg = self.parse_expression(0)?;
            self.token(")")?;
            func = AstNode::new_with_children(
                vec![func, arg],
                Token::Identifier("function".to_string()),
                Pos { col: 0, pos: 0 },
            );
        }
        Ok(func)
    }

    pub fn parse_expression(&mut self, min_prec: i32) -> Result<AstNode, String> {
        let mut left = self.funccall()?;
        while let Some(Token::Operator(op)) = self.lexer.peek() {
            if !self.operators.contains_key(&op) {
                break;
            }
            let precedence = self.operators[&op];
            if precedence < min_prec {
                break;
            }
            self.lexer.read();
            let right = self.parse_expression(precedence + 1)?;
            let ch: Vec<AstNode> = vec![left, right];
            left =
                AstNode::new_with_children(ch, Token::Operator(op.clone()), Pos { col: 0, pos: 0 });
        }
        Ok(left)
    }

    pub fn statement(&mut self) -> Result<AstNode, String> {
        let res: AstNode;
        res = match self.lexer.peek().unwrap() {
            Token::Identifier(id) => match id.as_str() {
                "assign" => {
                    self.token("assign")?;
                    let id = self.get_id()?;
                    self.token("=")?;
                    let exp = self.parse_expression(0)?;
                    let id_as_astnode = AstNode::new(Token::Identifier(id), Pos { col: 0, pos: 0 });
                    AstNode::new_with_children(
                        vec![id_as_astnode, exp],
                        Token::Identifier("assign".to_string()),
                        Pos { col: 0, pos: 0 },
                    )
                }
                "const" => {
                    self.token("const")?;
                    let id = self.get_id()?;
                    self.token("=")?;
                    let exp = self.parse_expression(0)?;
                    let id_as_astnode = AstNode::new(Token::Identifier(id), Pos { col: 0, pos: 0 });
                    AstNode::new_with_children(
                        vec![id_as_astnode, exp],
                        Token::Identifier("const".to_string()),
                        Pos { col: 0, pos: 0 },
                    )
                }
                "print" => {
                    self.token("print")?;
                    let exp = self.parse_expression(0)?;
                    AstNode::new_with_children(
                        vec![exp],
                        Token::Identifier("print".to_string()),
                        Pos { col: 0, pos: 0 },
                    )
                }
                /*
                "lambda" => {
                    self.token("lambda")?;
                    let args: Vec<Token> = vec![];
                    self.token("(")?;
                    while !self.istoken(")") {
                        args.push(self.get_id()?);
                    }
                    self.token(")")?;
                    self.token("=>")?;
                    let block = self.block()?;
                }
                */
                _ => self.parse_expression(0)?,
            },
            _ => self.parse_expression(0)?,
        };
        Ok(res)
    }

    pub fn block(&mut self) -> Result<AstNode, String> {
        if let Token::Operator(op) = self.lexer.peek().unwrap() {
            if op == "{".to_string() {
                let mut code: Vec<AstNode> = vec![];
                self.token("{")?;
                while !self.istoken("}") {
                    code.push(self.statement()?);
                    if self.istoken(".") {
                        self.token(".")?;
                    }
                }
                self.token("}")?;
                return Ok(AstNode::new_with_children(
                    code,
                    Token::Operator("block".to_string()),
                    Pos { col: 0, pos: 0 },
                ));
            }
        }
        Ok(AstNode::new_with_children(
            vec![self.statement()?],
            Token::Identifier("block".to_string()),
            Pos { col: 0, pos: 0 },
        ))
    }

    pub fn program(&mut self) -> Result<AstNode, String> {
        let mut code: Vec<AstNode> = vec![];
        while !self.isend() {
            code.push(self.statement()?);
            if self.istoken(".") {
                self.token(".")?;
            }
        }
        Ok(AstNode::new_with_children(
            code,
            Token::Operator("block".to_string()),
            Pos { col: 0, pos: 0 },
        ))
    }
}

fn main() {
    let mut lexer: Lexer =
        Lexer::new("assign y = x + 3. assign x = y - 3. print x. print y. 0".to_string());
    lexer.lex();
    let mut parser: Parser = Parser::new(lexer);
    let program = parser.program();
    let programnode;
    match program {
        Ok(res) => {
            println!("{}", res.print());
            programnode = res;
            match programnode.eval() {
                Ok(res) => match res.getnum() {
                    Ok(r) => println!("{}", r),
                    Err(msg) => println!("Err:{}", msg),
                },
                Err(msg) => println!("ERr:{}", msg),
            }
        }
        Err(msg) => println!("ERR:{}", msg),
    }
}
