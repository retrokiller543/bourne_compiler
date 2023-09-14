use crate::parser::ASTNode;
use petgraph::dot::{Config, Dot};
use petgraph::graph::NodeIndex;
use petgraph::Graph;

pub fn visualize_ast(ast: &ASTNode) -> String {
    let mut graph = Graph::<String, &str>::new(); // <-- Change node type to String
    build_graph(&mut graph, ast, None);
    format!("{:?}", Dot::with_config(&graph, &[Config::EdgeNoLabel]))
}

fn build_graph<'a>(
    graph: &mut Graph<String, &'a str>,
    node: &'a ASTNode,
    parent: Option<NodeIndex>,
) {
    let node_name = match node {
        ASTNode::Program(_) => "Program".to_string(),
        ASTNode::Statement(_) => "Statement".to_string(),
        ASTNode::Assign { name, .. } => format!("Assign({})", name),
        ASTNode::BinaryOp { op, .. } => format!("{:?}", op),
        ASTNode::Variable(name) => format!("Variable({})", name),
        ASTNode::Number(value) => format!("Number({})", value),
        ASTNode::BuiltInFunctionCall { name, .. } => format!("FunctionCall({:#?})", name),
        ASTNode::If { .. } => "If".to_string(),
        ASTNode::While { .. } => "While".to_string(),
        // ... add other ASTNode variants as needed
        _ => format!("{:?}", node), // Default representation
    };

    let node_idx = graph.add_node(node_name);

    if let Some(parent_idx) = parent {
        graph.add_edge(parent_idx, node_idx, "");
    }

    match node {
        ASTNode::Program(nodes) => {
            for sub_node in nodes {
                build_graph(graph, sub_node, Some(node_idx));
            }
        }
        ASTNode::Statement(boxed_node) => {
            build_graph(graph, &**boxed_node, Some(node_idx));
        }
        ASTNode::BinaryOp { left, right, .. } => {
            build_graph(graph, &**left, Some(node_idx));
            build_graph(graph, &**right, Some(node_idx));
        }
        ASTNode::If {
            condition,
            body,
            else_body,
        } => {
            let condition_idx = graph.add_node("Condition".to_string());
            graph.add_edge(node_idx, condition_idx, "Condition");
            build_graph(graph, &**condition, Some(condition_idx));

            let body_idx = graph.add_node("true".to_string());
            graph.add_edge(node_idx, body_idx, "true");
            build_graph(graph, &**body, Some(body_idx));

            if let Some(else_body) = else_body {
                let else_idx = graph.add_node("else".to_string());
                graph.add_edge(node_idx, else_idx, "else");
                build_graph(graph, &**else_body, Some(else_idx));
            }
        }
        ASTNode::While { condition, body } => {
            let condition_idx = graph.add_node("Condition".to_string());
            graph.add_edge(node_idx, condition_idx, "Condition");
            build_graph(graph, &**condition, Some(condition_idx));

            let body_idx = graph.add_node("Body".to_string());
            graph.add_edge(node_idx, body_idx, "Body");
            build_graph(graph, &**body, Some(body_idx));
        }
        ASTNode::BuiltInFunctionCall { args, .. } => {
            let args_idx = graph.add_node("Args".to_string());
            for arg in args {
                graph.add_edge(node_idx, args_idx, "Args");
                build_graph(graph, arg, Some(args_idx));
            }
        }
        ASTNode::Assign { name: _, value } => {
            build_graph(graph, &**value, Some(node_idx));
        }
        // ... handle other ASTNode variants as needed
        _ => {}
    }
}
