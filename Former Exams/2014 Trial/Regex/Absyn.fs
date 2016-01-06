module Absyn

type re = 
   | Char of char
   | Eps
   | Seq of re * re
   | Star of re
   | Choice of re * re


// NFA 
type state = int

type sym = SEps | SChar of char

type nfa = 
    { 
        start : state; 
        accept : state; 
        trans : (state * sym * state) list 
    }