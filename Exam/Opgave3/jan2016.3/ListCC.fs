(* File ListC/ListCC.fs *)
module ListCC



[<EntryPoint>]
let main argv = 
    let args = System.Environment.GetCommandLineArgs()

    let _ = printf "List-C compiler v 1.0.0.0 of 2012-02-13\n"

    let _ = if args.Length > 0 then
              let source = @"E:\User (D)\Programmering\BPRD-Projects\Exam\Opgave3\jan2016.3\Opgave3Tests.lc"
              let stem = if source.EndsWith(".lc") then source.Substring(0,source.Length-3) 
                         else if source.EndsWith(".c") then source.Substring(0,source.Length-2) 
                              else source
              let target = stem + ".out"
              printf "Compiling %s to %s\n" source target;
              try ignore (Comp.compileToFile (Parse.fromFile source) target)
              with Failure msg -> printf "ERROR: %s\n" msg
              else
              printf "Usage: listcc <source file>\n"
    0 // return an integer exit code
