using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExercisesCSharp
{
    //
    //Exercise 1.4
    //
    public class TestClass
    {
        public TestClass()
        {
            Expr e = new Add(new CstI(17), new Var("z"));
            Console.WriteLine(e);
            
            // (ii)
            Expr e1 = new Mul(new CstI(100), e);
            Console.WriteLine(e1);
            Expr e2 = new Sub(e1, e);
            Console.WriteLine(e2);
            Expr e3 = new Add(e1, e2);
            Console.WriteLine(e3);
        }
    }

    #region (i + iii + iiii)
    public abstract class Expr
    {
        public abstract int Eval(Dictionary<string, int> env);
        public abstract Expr Simplify();
    }

    public class CstI : Expr
    {
        public CstI(int i)
        {
            I = i;
        }
        public int I { get; set; }
        public override string ToString()
        {
            return I.ToString();
        }

        public override int Eval(Dictionary<string, int> env)
        {
            return I;
        }

        public override Expr Simplify()
        {
            return this;
        }
    }

    public class Var : Expr
    {
        public Var(string variableName)
        {
            VariableName = variableName;
        }
        public string VariableName { get; set; }
        public override string ToString()
        {
            return VariableName;
        }

        public override int Eval(Dictionary<string, int> env)
        {
            return env[VariableName]; // this will crash if it does not exist, which is fine.
        }

        public override Expr Simplify()
        {
            return this;
        }
    }

    public abstract class Binop : Expr
    {
        protected Binop(Expr expr1, Expr expr2)
        {
            Expr1 = expr1;
            Expr2 = expr2;
        }
        public Expr Expr1 { get; set; }
        public Expr Expr2 { get; set; }
    }

    public class Add : Binop
    {
        public Add(Expr expr1, Expr expr2) : base(expr1, expr2)
        {
        }

        public override string ToString()
        {
            return "(" + Expr1 + " + " + Expr2 + ")"; // tostring not neccesary.
        }

        public override int Eval(Dictionary<string, int> env)
        {
            return Expr1.Eval(env) + Expr2.Eval(env);
        }

        public override Expr Simplify()
        {
            if (Expr1 == new CstI(0) && Expr2 == new CstI(0))
            {
                return new CstI(0);
            }
            if (Expr1 == new CstI(0))
            {
                return Expr2;
            }
            if (Expr2 == new CstI(0))
            {
                return Expr1;
            }
            return this;
        }
    }

    public class Mul : Binop
    {
        public Mul(Expr expr1, Expr expr2) : base(expr1, expr2)
        {
        }

        public override string ToString()
        {
            return "(" + Expr1 + " * " + Expr2 + ")"; // tostring not neccesary.
        }

        public override int Eval(Dictionary<string, int> env)
        {
            return Expr1.Eval(env) * Expr2.Eval(env);
        }
        public override Expr Simplify()
        {
            if (Expr1 == new CstI(0) || Expr2 == new CstI(0))
            {
                return new CstI(0);
            }
            if (Expr1 == new CstI(1))
            {
                return Expr2;
            }
            if (Expr2 == new CstI(1))
            {
                return Expr1;
            }
            return this;
        }
    }

    public class Sub : Binop
    {
        public Sub(Expr expr1, Expr expr2) : base(expr1, expr2)
        {
        }

        public override string ToString()
        {
            return "(" + Expr1 + " - " + Expr2 + ")"; // tostring not neccesary.
        }
        public override int Eval(Dictionary<string, int> env)
        {
            return Expr1.Eval(env) - Expr2.Eval(env);
        }

        public override Expr Simplify()
        {
            if (Expr1 == new CstI(0) && Expr2 == new CstI(0))
            {
                return new CstI(0);
            }
            if (Expr1 == Expr2)
            {
                return new CstI(0);
            }
            if (Expr2 == new CstI(0))
            {
                return Expr1;
            }
            return this;
        }
    }
    #endregion
}
