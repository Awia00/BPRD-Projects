module Misc

let print = printfn "%s"

let debug_p = ref true
let debug s = if !debug_p then print s else ()