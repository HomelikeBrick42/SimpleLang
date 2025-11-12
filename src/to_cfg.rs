use std::sync::Arc;

use crate::{
    control_flow_graph as cfg,
    ids::{Id, IdMap, IdSecondaryMap},
    typed_tree as tt,
};

pub fn function(
    function: &tt::Function,
    body: &tt::FunctionBody,
    types: &IdMap<tt::Type>,
) -> cfg::Function {
    match body {
        tt::FunctionBody::Builtin(builtin_function) => match *builtin_function {
            tt::BuiltinFunction::PrintI32 => {
                let mut variables = IdMap::new();

                let parameters = function
                    .parameter_types
                    .iter()
                    .map(|&parameter_type| {
                        variables.insert(cfg::Variable {
                            name: None,
                            typ: parameter_type,
                        })
                    })
                    .collect::<Box<[_]>>();

                let return_value = variables.insert(cfg::Variable {
                    name: None,
                    typ: function.return_type,
                });

                let mut blocks = IdMap::new();

                let start_block = blocks.insert(cfg::Block {
                    instructions: vec![cfg::Instruction::PrintI32 {
                        variable: parameters[0],
                    }],
                    jump: cfg::Jump::Return,
                });

                cfg::Function {
                    name: Some(function.name),
                    variables,
                    parameters,
                    return_value,
                    blocks,
                    start_block,
                }
            }
        },

        tt::FunctionBody::Body {
            variables: tt_variables,
            parameters: tt_parameters,
            expression,
        } => {
            let mut variables = IdMap::new();

            let mut variable_translations = IdSecondaryMap::new();
            for (id, variable) in tt_variables.iter() {
                let variable = variables.insert(cfg::Variable {
                    name: variable.name,
                    typ: variable.typ,
                });
                variable_translations.insert(id, variable);
            }

            let mut blocks = IdMap::new();

            let start_block = blocks.insert(cfg::Block {
                instructions: vec![],
                jump: cfg::Jump::Return,
            });

            let return_value = append_expression(
                expression,
                &mut { start_block },
                &mut blocks,
                &mut variables,
                &variable_translations,
                &mut IdSecondaryMap::new(),
                types,
            );

            cfg::Function {
                name: Some(function.name),
                variables,
                parameters: tt_parameters
                    .into_iter()
                    .map(|&parameter| variable_translations[parameter])
                    .collect::<Box<[_]>>(),
                return_value,
                blocks,
                start_block,
            }
        }
    }
}

struct BlockLabel {
    block: Id<cfg::Block>,
    variable: Id<cfg::Variable>,
}

fn append_expression(
    expression: &tt::Expression,
    current_block: &mut Id<cfg::Block>,
    blocks: &mut IdMap<cfg::Block>,
    variables: &mut IdMap<cfg::Variable>,
    variable_translations: &IdSecondaryMap<tt::Variable, Id<cfg::Variable>>,
    labels: &mut IdSecondaryMap<tt::Label, BlockLabel>,
    types: &IdMap<tt::Type>,
) -> Id<cfg::Variable> {
    match expression.kind {
        tt::ExpressionKind::Place(ref place) => match *place {
            tt::Place::Function(_) => match types[expression.typ].kind {
                tt::TypeKind::FunctionItem(_) => variables.insert(cfg::Variable {
                    name: Some("function item variable".into()),
                    typ: expression.typ,
                }),

                ref t => unreachable!("somehow got {t:?} at {}", expression.location),
            },

            tt::Place::Variable(id) => variable_translations[id],

            tt::Place::StructMemberAccess {
                operand: _,
                member_index: _,
            } => todo!(),
        },

        tt::ExpressionKind::Constant(ref constant) => {
            macro_rules! integer {
                ($value:ident) => {{
                    let variable = variables.insert(cfg::Variable {
                        name: None,
                        typ: expression.typ,
                    });
                    blocks[*current_block]
                        .instructions
                        .push(cfg::Instruction::Const {
                            destination: variable,
                            bytes: Arc::new($value.to_le_bytes().map(cfg::Byte::Init)),
                        });
                    variable
                }};
            }

            match *constant {
                tt::Constant::U8(value) => integer!(value),
                tt::Constant::U16(value) => integer!(value),
                tt::Constant::U32(value) => integer!(value),
                tt::Constant::U64(value) => integer!(value),
                tt::Constant::I8(value) => integer!(value),
                tt::Constant::I16(value) => integer!(value),
                tt::Constant::I32(value) => integer!(value),
                tt::Constant::I64(value) => integer!(value),
                tt::Constant::ISize(value) => integer!(value),
                tt::Constant::USize(value) => integer!(value),
            }
        }

        tt::ExpressionKind::Block {
            label,
            ref statements,
            ref last_expression,
        } => {
            let block = {
                let jump = blocks[*current_block].jump.clone();
                let block = blocks.insert(cfg::Block {
                    instructions: vec![],
                    jump,
                });
                blocks[*current_block].jump = cfg::Jump::Block(block);
                *current_block = block;
                block
            };

            let variable = variables.insert(cfg::Variable {
                name: Some("block result value".into()),
                typ: expression.typ,
            });
            labels.insert(label, BlockLabel { block, variable });

            for statement in statements {
                append_statement(
                    statement,
                    current_block,
                    blocks,
                    variables,
                    variable_translations,
                    labels,
                    types,
                );
            }

            let result_value = append_expression(
                last_expression,
                current_block,
                blocks,
                variables,
                variable_translations,
                labels,
                types,
            );

            blocks[*current_block]
                .instructions
                .push(cfg::Instruction::Copy {
                    source: result_value,
                    destination: variable,
                });

            variable
        }

        tt::ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            let operand = append_expression(
                operand,
                current_block,
                blocks,
                variables,
                variable_translations,
                labels,
                types,
            );

            let arguments = arguments
                .iter()
                .map(|argument| {
                    append_expression(
                        argument,
                        current_block,
                        blocks,
                        variables,
                        variable_translations,
                        labels,
                        types,
                    )
                })
                .collect();

            let result = variables.insert(cfg::Variable {
                name: Some("function return value".into()),
                typ: expression.typ,
            });

            let jump = blocks[*current_block].jump.clone();
            let block = blocks.insert(cfg::Block {
                instructions: vec![],
                jump,
            });
            blocks[*current_block].jump = cfg::Jump::Call {
                operand,
                arguments,
                result,
                return_to: block,
            };
            *current_block = block;

            result
        }

        tt::ExpressionKind::StructConstructor { ref arguments } => todo!(),

        tt::ExpressionKind::EnumConstructor { ref argument } => todo!(),

        tt::ExpressionKind::Match {
            ref scruitnee,
            ref arms,
        } => todo!(),

        tt::ExpressionKind::Break { label, ref value } => todo!(),

        tt::ExpressionKind::Continue { label } => todo!(),
    }
}

fn append_statement(
    statement: &tt::Statement,
    current_block: &mut Id<cfg::Block>,
    blocks: &mut IdMap<cfg::Block>,
    variables: &mut IdMap<cfg::Variable>,
    variable_translations: &IdSecondaryMap<tt::Variable, Id<cfg::Variable>>,
    labels: &mut IdSecondaryMap<tt::Label, BlockLabel>,
    types: &IdMap<tt::Type>,
) {
    match statement.kind {
        tt::StatementKind::Expression(ref expression) => {
            append_expression(
                expression,
                current_block,
                blocks,
                variables,
                variable_translations,
                labels,
                types,
            );
        }

        tt::StatementKind::Assignment {
            ref pattern,
            ref value,
        } => todo!(),
    }
}
