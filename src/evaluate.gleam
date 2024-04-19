//// This module defines the `evaluate` function (part 1 of the assignment).
//////

import expression.{type Expression}
import error.{type CalculatorError}
import gleam/int
import gleam/float

/// Evaluates the given expression, either returns an integer or an error
pub fn evaluate(expression: Expression) -> Result(Int, CalculatorError) {
  case expression {
    expression.Number(n) -> Ok(n)

    expression.Negation(n) -> {
      let result = evaluate(n)
      case result {
        Ok(n) -> Ok(-n)
        Error(_) -> Error(error.DivideByZeroError)
      }
    }

    expression.Add(l, r) -> {
      let left_result = evaluate(l)
      let right_result = evaluate(r)

      case [left_result, right_result] {
        [Ok(n1), Ok(n2)] -> Ok(n1 + n2)
        _ -> Error(error.DivideByZeroError)
      }
    }

    expression.Subtract(l, r) -> {
      let left_result = evaluate(l)
      let right_result = evaluate(r)

      case [left_result, right_result] {
        [Ok(n1), Ok(n2)] -> Ok(n1 - n2)
        _ -> Error(error.DivideByZeroError)
      }
    }

    expression.Multiply(l, r) -> {
      let left_result = evaluate(l)
      let right_result = evaluate(r)

      case [left_result, right_result] {
        [Ok(n1), Ok(n2)] -> Ok(n1 * n2)
        _ -> Error(error.DivideByZeroError)
      }
    }

    expression.Divide(l, r) -> {
      let left_result = evaluate(l)
      let right_result = evaluate(r)

      case [left_result, right_result] {
        [Ok(n1), Ok(n2)] -> {
          case n2 {
            0 -> Error(error.DivideByZeroError)
            _ -> remainder(n1, n2)
          }
        }
        _ -> Error(error.DivideByZeroError)
      }
    }
  }
}

fn remainder(n1, n2) {
  let result = n1 / n2
  let h = convert_float(n1, n2)

  let rounded_result = case h {
    0.0 -> result
    _ -> {
      let rounded_decimal = case n1 {
        x if x >= 0 -> float.floor(h +. 0.5)
        _ -> float.ceiling(h -. 0.5)
      }
      float.truncate(rounded_decimal)
    }
  }
  Ok(rounded_result)
}

fn convert_float(n1, n2) -> Float {
  let x = int.to_float(n1)
  let y = int.to_float(n2)

  x /. y
}
