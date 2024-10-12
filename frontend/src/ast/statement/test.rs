use crate::ast;
use crate::ast::util::try_parse_from_string;
use crate::ast::Stmt;

// member bin ops in particular must be valid as statements because they can represent
// a no-args call to a method or a UFCS free function call 
#[test]
pub fn member_bin_op_is_valid_call_stmt() {
    let result = try_parse_from_string::<Stmt>("Test", "a.B");
    
    match result {
        Ok(Stmt::Call(call)) => {
            match call.as_ref() {
                ast::Call::FunctionNoArgs(no_args_call) => {
                    assert_eq!(no_args_call.target.to_string(), "a.B");
                    assert_eq!(no_args_call.self_arg, None, "parser should not set the self arg");
                }
                
                other => panic!("expected no-args function call, got: {}", other),
            }
        }

        Ok(other) => panic!("expected call statement, got: {}", other),

        Err(err) => panic!("should parse as a valid statement, got: {}", err.err),
    }
}
