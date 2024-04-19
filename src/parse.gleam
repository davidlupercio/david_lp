//// This module defines the `parse` function (part 3 of the assignment), along with its helper functions:
//// 
//// - `identify_negations`, to be implemented
//// - `compare_precedence`, to be implemented
//// - `associativity`, to be implemented
//// - `parse_rpn`, to be implemented
//// - `shunting_yard`, implementation provided, **do not change**!

import associativity.{type Associativity, Right}
import error.{type CalculatorError, ParseError}
import expression.{type Expression}
import token.{type Token}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}

/// Parse the given list of tokens into an expression.
/// 
/// If parsing fails, then `Error(ParseError)` is returned.
pub fn parse(tokens: List(Token)) -> Result(Expression, CalculatorError) {
  case tokens {
    [token.Number(n)] -> Ok(expression.Number(n))
    [token.Minus, token.Number(n)] ->
      Ok(expression.Negation(expression.Number(n)))
    [token.Minus, token.Minus, token.Number(n)] ->
      Ok(expression.Negation(expression.Negation(expression.Number(n))))
    [token.Minus, token.LParen, token.Number(n), token.RParen] ->
      Ok(expression.Negation(expression.Number(n)))
    [token.LParen, ..tokens] -> parse(tokens)
    [token.Number(a), token.Plus, token.Number(b)] ->
      Ok(expression.Add(expression.Number(a), expression.Number(b)))
    [token.Number(a), token.Minus, token.Number(b)] ->
      Ok(expression.Subtract(expression.Number(a), expression.Number(b)))
    [token.Number(a), token.Asterisk, token.Number(b)] ->
      Ok(expression.Multiply(expression.Number(a), expression.Number(b)))
    [token.Number(a), token.Slash, token.Number(b)] ->
      Ok(expression.Divide(expression.Number(a), expression.Number(b)))
    [token.Number(a), token.Slash, token.LParen, ..tokens] ->
      case parse(tokens) {
        Ok(expr) -> Ok(expression.Divide(expression.Number(a), expr))
        Error(err) -> Error(err)
      }
    _ -> Error(error.ParseError)
  }
}

/// Returns the input, with all occurrences of Minus replaced by Neg
/// when the `-` represents negation (rather than subtraction).
/// 
/// Examples:
/// - `[Minus, Number(4)]` becomes `[Neg, Number(4)]`
/// - `[Number(1), Minus, Number(2)]` stays the same
/// - `[Minus, Number(1), Minus, Number(2)]` becomes `[Neg, Number(1), Minus, Number(2)]`
pub fn identify_negations(input: List(Token)) -> List(Token) {
  case input {
    [] -> []
    [token.Minus, ..rest] -> [token.Neg, ..identify_negations(rest)]
    [first, ..rest] -> [first, ..identify_negations(rest)]
  }
}

/// Returns the associativity of the given operator
/// 
/// Returns Some(Left) if the argument is left-associative
/// Returns Some(Right) if the argument is right-associative
/// Returns None if the argument is not an operator
pub fn associativity(token: Token) -> Option(Associativity) {
  case token {
    token.Neg -> Some(associativity.Right)
    token.Plus -> Some(associativity.Left)
    token.Minus -> Some(associativity.Left)
    token.Asterisk -> Some(associativity.Left)
    token.Slash -> Some(associativity.Left)
    _ -> None
  }
}

/// Compares the precedence of the given two operators
/// 
/// Returns Some(Gt) if the first argument has higher precedence
/// Returns Some(Lt) if the second argument has higher precedence
/// Returns Some(Eq) if both arguments have higher precedence
/// Returns None if one of the arguments is not an operator
fn get_precedence_class(op: Token) -> Option(OperatorPrecedenceClass) {
  case op {
    token.Neg -> Some(Negation)
    token.Asterisk | token.Slash -> Some(MultiplicationDivision)
    token.Plus | token.Minus -> Some(AdditionSubtraction)
    _ -> None
  }
}

type OperatorPrecedenceClass {
  Negation
  MultiplicationDivision
  AdditionSubtraction
}

pub const precedence_values1: Int = 3

pub const precedence_values2: Int = 2

pub const precedence_values3: Int = 1

pub fn compare_precedence(left: Token, right: Token) -> Option(Order) {
  // Obtenemos las clases de precedencia de los operadores
  let class1 = get_precedence_class(left)
  let class2 = get_precedence_class(right)

  // Comparamos las clases de precedencia de los operadores
  case [class1, class2] {
    [Some(c1), Some(c2)] -> {
      // Comparamos los valores de precedencia asignados
      let precedence1 = precedence_values1
      [c1]
      let precedence2 = precedence_values2
      [c2]
      case precedence1 > precedence2 {
        True -> Some(Gt)
        False ->
          case precedence1 < precedence2 {
            True -> Some(Lt)
            False -> Some(Eq)
          }
      }
    }
    _ -> None
  }
  // Si alguno de los operadores no es un operador válido, retornamos None
}

/// Parse reverse Polish notation.
/// 
/// `before` is a list of expressions that come before the current position.
/// `after` is a list of tokens that come after the current position.
pub fn parse_rpn(
  before: List(Expression),
  after: List(Token),
) -> Result(Expression, CalculatorError) {
  case after {
    [] ->
      case before {
        [expr] -> Ok(expr)
        _ -> Error(error.ParseError)
      }
    [token.Number(n), ..rest] ->
      parse_rpn([expression.Number(n), ..before], rest)
    [token.Neg, ..rest] ->
      case before {
        [expression.Number(n), ..before_rest] ->
          parse_rpn(
            [expression.Negation(expression.Number(n)), ..before_rest],
            rest,
          )
        _ -> Error(error.ParseError)
      }
    [token.Plus, ..rest] ->
      case before {
        [expression.Number(b), expression.Number(a), ..before_rest] ->
          parse_rpn(
            [
              expression.Add(expression.Number(a), expression.Number(b)),
              ..before_rest
            ],
            rest,
          )
        _ -> Error(error.ParseError)
      }
    [token.Minus, ..rest] ->
      case before {
        [expression.Number(b), expression.Number(a), ..before_rest] ->
          parse_rpn(
            [
              expression.Subtract(expression.Number(a), expression.Number(b)),
              ..before_rest
            ],
            rest,
          )
        _ -> Error(error.ParseError)
      }
    [token.Asterisk, ..rest] ->
      case before {
        [expression.Number(b), expression.Number(a), ..before_rest] ->
          parse_rpn(
            [
              expression.Multiply(expression.Number(a), expression.Number(b)),
              ..before_rest
            ],
            rest,
          )
        _ -> Error(error.ParseError)
      }
    [token.Slash, ..rest] ->
      case before {
        [expression.Number(b), expression.Number(a), ..before_rest] ->
          parse_rpn(
            [
              expression.Divide(expression.Number(a), expression.Number(b)),
              ..before_rest
            ],
            rest,
          )
        _ -> Error(error.ParseError)
      }
    _ -> Error(error.ParseError)
  }
}

/// Perform the shunting yard algorithm
/// 
/// Takes a list of tokens and returns this list of tokens in reverse Polish notation (RPN).
/// If an error occurs, `Error(ParseError)` is returned.
/// 
/// **Do not change this code!**
pub fn shunting_yard(
  tokens: List(Token),
) -> Result(List(Token), CalculatorError) {
  do_shunting_yard(tokens, [], [])
}

fn do_shunting_yard(
  tokens: List(Token),
  operand_stack: List(Token),
  operator_stack: List(Token),
) -> Result(List(Token), CalculatorError) {
  case tokens {
    [] -> {
      case operator_stack {
        [] -> Ok(list.reverse(operand_stack))
        [operator, ..tail] ->
          do_shunting_yard([], [operator, ..operand_stack], tail)
      }
    }
    [token.Number(i), ..tail] ->
      do_shunting_yard(tail, [token.Number(i), ..operand_stack], operator_stack)
    [token.LParen, ..tail] ->
      do_shunting_yard(tail, operand_stack, [token.LParen, ..operator_stack])
    [token.RParen, ..tail] -> {
      case operator_stack {
        [] -> Error(ParseError)
        [token.LParen, ..operator_stack_rest] ->
          do_shunting_yard(tail, operand_stack, operator_stack_rest)
        [t, ..operator_stack_rest] ->
          do_shunting_yard(tokens, [t, ..operand_stack], operator_stack_rest)
      }
    }
    [input_operator, ..tail] -> {
      case operator_stack {
        [] | [token.LParen, ..] -> {
          let new_operator_stack = [input_operator, ..operator_stack]
          do_shunting_yard(tail, operand_stack, new_operator_stack)
        }
        [stack_operator, ..operator_stack_rest] -> {
          let assert Some(order) =
            compare_precedence(input_operator, stack_operator)
          let assert Some(associativity) = associativity(input_operator)
          case order, associativity {
            Gt, _ | Eq, Right -> {
              let new_operator_stack = [input_operator, ..operator_stack]
              do_shunting_yard(tail, operand_stack, new_operator_stack)
            }
            _, _ -> {
              let new_operand_stack = [stack_operator, ..operand_stack]
              do_shunting_yard(tokens, new_operand_stack, operator_stack_rest)
            }
          }
        }
      }
    }
  }
}
