//// This module defines the `tokenize` function (part 2 of the assignment).

import error.{type CalculatorError}
import token.{type Token}
import gleam/string
import gleam/list

/// Takes the input string, and turns it into a list of tokens.
/// 
/// If an unrecognized character is encountered, `Error(UnknownCharacterError)` is returned.
pub fn tokenize(input: String) -> Result(List(Token), CalculatorError) {
  let u1 = string.slice(from: input, at_index: 0, length: 1)
  let u2 = string.slice(from: input, at_index: 1, length: 1)
  let count = string.to_graphemes(input)

  case count {
    [] -> Ok([])
    [_] ->
      case input {
        "" -> Ok([])
        _ ->
          case is_number(u1) {
            True ->
              case input {
                _ -> {
                  let tokens =
                    input
                    |> string.split(" ")
                    |> list.map(parse_token)
                  Ok(tokens)
                }
              }
            False -> Error(error.UnknownCharacterError(u1))
          }
      }
    [_, _] ->
      case input {
        "" -> Ok([])
        _ ->
          case is_number(u1) {
            True ->
              case is_number(u2) {
                True ->
                  case input {
                    _ -> {
                      let tokens =
                        input
                        |> string.split(" ")
                        |> list.map(parse_token)
                      Ok(tokens)
                    }
                  }
                False -> Error(error.UnknownCharacterError(u2))
              }
            False -> Error(error.UnknownCharacterError(u1))
          }
      }
    _ ->
      case input {
        "" -> Ok([])
        _ ->
          case is_number(u1) {
            True ->
              case input {
                _ -> {
                  let tokens =
                    input
                    |> string.split(" ")
                    |> list.map(parse_token)
                  Ok(tokens)
                }
              }
            False -> Error(error.UnknownCharacterError(u1))
          }
      }
  }
}

fn parse_token(token: String) -> Token {
  let l = string.length(token)

  case numbers(token) {
    Ok(number) -> token.Number(number)
    Error(_) -> {
      case l {
        2 -> {
          let digit1 = string.slice(from: token, at_index: 0, length: 1)
          let digit2 = string.slice(from: token, at_index: 1, length: 1)
          case [numbers(digit1), numbers(digit2)] {
            [Ok(num1), Ok(num2)] -> token.Number(num1 * 10 + num2)
            _ -> token.Number(0)
          }
        }
        _ -> token.Number(0)
      }
    }
  }
}

pub fn all_tokens(input: String) -> Token {
  case input {
    "(" -> token.LParen
    ")" -> token.RParen
    " " -> token.Space
    "+" -> token.Plus
    "-" -> token.Minus
    "*" -> token.Asterisk
    "/" -> token.Slash
    _ -> token.Number(0)
  }
}

pub fn is_number(input: String) -> Bool {
  case input {
    "0" -> True
    "1" -> True
    "2" -> True
    "3" -> True
    "4" -> True
    "5" -> True
    "6" -> True
    "7" -> True
    "8" -> True
    "9" -> True
    _ -> False
  }
}

pub fn numbers(input: String) -> Result(Int, CalculatorError) {
  case input {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    _ -> Error(error.UnknownCharacterError(input))
  }
}
