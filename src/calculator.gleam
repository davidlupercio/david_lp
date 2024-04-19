//// This module defines the `calculate` and `main` functions (part 4 of the assignment).

import error.{type CalculatorError}
import tokenize
import parse
import evaluate
import gleam/erlang
import gleam/io
import gleam/int

/// Takes a mathematical expression as a string, and returns the result of the calculation.
/// 
/// If any error occurs, the appropriate CalculatorError is returned.
pub fn calculate(input: String) -> Result(Int, CalculatorError) {
  let a = tokenize.tokenize(input)

  case a {
    Ok(tokens) -> {
      let tokens = parse.identify_negations(tokens)
      let rpn = parse.shunting_yard(tokens)

      case rpn {
        Ok(tokens) -> {
          let expr = parse.parse_rpn([], tokens)
          case expr {
            Ok(expression) -> {
              let result = evaluate.evaluate(expression)
            }
            Error(err) -> Error(err)
          }
        }
        Error(_) -> Error(error.DivideByZeroError)
      }
    }
    Error(_) -> Error(error.DivideByZeroError)
  }
}

/// The main function of the application.
/// 
/// This function prompts the user for a mathematical expression,
/// and then either outputs the result, or an error. It will
/// then prompt the user for another mathematical expression, etc.
/// 
/// If an input error occurs, or the user enters "q", then the
/// main function returns.
/// 
/// Example session:
/// 
/// ```console
/// > 5 + 4
/// Result: 9
/// > 2 / 0
/// Error: Divide by zero
/// > 2 -
/// Error: Parse error
/// > hi
/// Error: Unknown character: 'h'
/// > q
/// ```
pub fn main() {
  let input = erlang.get_line("> ")
  case input {
    Ok(expression) ->
      case calculate(expression) {
        Ok(result) -> Ok(io.println("Resultado: " <> int.to_string(result)))
        Error(err) -> Ok(io.println("Error: " <> error.to_string(err)))
      }
    Ok("q") -> Ok(io.println("```"))
    Error(_) -> Ok(io.println("abort"))
  }
}
