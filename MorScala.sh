#!/bin/sh
exec scala "$0" "$@"
!#

import java.text.Normalizer

object Main extends App {
    args.map(Morse.encode)
        .foreach(println)
}

object Morse {
    sealed trait MorseCode {
        val symbol: String
    }
    case object Short extends MorseCode {
        val symbol = "."
    }
    case object Long extends MorseCode {
        val symbol = "–"
    }

    type S = Short
    type L = Long

    val symbolSpaceSymbol = " "
    val letterSpaceSymbol = " " * 3
    val wordSpaceSymbol = " " * 4

    val morseMapping: Map[Char, List[MorseCode]] = Map(
        'A' -> List(Short, Long),
        'B' -> List(Long, Short, Short, Short),
        'C' -> List(Long, Short, Long, Short),
        'D' -> List(Long, Short, Short),
        'E' -> List(Short),
        'F' -> List(Short, Short, Long, Short),
        'G' -> List(Long, Long, Short),
        'H' -> List(Short, Short, Short, Short),
        'I' -> List(Short, Short),
        'J' -> List(Short, Long, Long, Long),
        'K' -> List(Long, Short, Long),
        'L' -> List(Short, Long, Short, Short),
        'M' -> List(Long, Long),
        'N' -> List(Long, Short),
        'O' -> List(Long, Long, Long),
        'P' -> List(Short, Long, Long, Short),
        'Q' -> List(Long, Long, Short, Long),
        'R' -> List(Short, Long, Short),
        'S' -> List(Short, Short, Short),
        'T' -> List(Long),
        'U' -> List(Short, Short, Long),
        'V' -> List(Short, Short, Short, Long),
        'W' -> List(Short, Long, Long),
        'X' -> List(Long, Short, Short, Long),
        'Y' -> List(Long, Short, Long, Long),
        'Z' -> List(Long, Long, Short, Short),

        '1' -> List(Short, Long, Long, Long, Long),
        '2' -> List(Short, Short, Long, Long, Long),
        '3' -> List(Short, Short, Short, Long, Long),
        '4' -> List(Short, Short, Short, Short, Long),
        '5' -> List(Short, Short, Short, Short, Short),
        '6' -> List(Long, Short, Short, Short, Short),
        '7' -> List(Long, Long, Short, Short, Short),
        '8' -> List(Long, Long, Long, Short, Short),
        '9' -> List(Long, Long, Long, Long, Short),
        '0' -> List(Long, Long, Long, Long, Long)
    )

    def sanitizeStringForEncryption(str: String): String = {
        val noAccent = "\\p{InCombiningDiacriticalMarks}+".r.replaceAllIn(Normalizer.normalize(str, Normalizer.Form.NFD), "")
        val noPunctuation = "[^\\w\\s]".r.replaceAllIn(noAccent, "")
        noPunctuation.toUpperCase
    }

    def mosify(c: Char): String = c match {
        case ' ' => wordSpaceSymbol
        case other => {
            val morseMapped = morseMapping.get(other)
            morseMapped match {
                case None => ""
                case Some(value) => value.map(_.symbol).mkString(start="", sep=symbolSpaceSymbol, end=letterSpaceSymbol)
            }
        }
    }

    def mosifySanitized(str: String): String = {
        str.map(mosify).mkString
    }

    def encode(str: String): String = {
        val sanitized = sanitizeStringForEncryption(str)
        mosifySanitized(sanitized)
    }
}
