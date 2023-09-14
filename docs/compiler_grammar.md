
# Compiler Grammar

## Current Grammar

$$
\begin{align*}
\text{program} & \to \text{statement}* \\
\text{statement} & \to \text{declaration} \\
& | \text{function\_call} \\
& | \text{if\_statement} \\
& | \text{while\_loop} \\
& | \text{expression} \, \text{EoL} \\
\text{declaration} & \to \text{"let"} \, \text{Identifier} \, ( "=" \, \text{expression} )? \\
\text{function\_call} & \to \text{BuiltInFunction} "(" \text{arguments}? ")" \\
& | \text{Identifier} "(" \text{arguments}? ")" \\
\text{if\_statement} & \to \text{"if"} \, \text{expression} "\{" \, \text{statement}* "\}" \\
& \left( \text{"else"} "\{" \, \text{statement}* "\}" \right)? \\
\text{while\_loop} & \to \text{"while"} \, \text{expression} "\{" \, \text{statement}* "\}" \\
\text{expression} & \to \text{term} \, (("+" | "-" | "<" | "<=" | ">" | ">=") \, \text{term})* \\
\text{term} & \to \text{factor} \left( ("*" | "/" | "\%") \, \text{factor} \right)* \\
\text{factor} & \to "(" \, \text{expression} ")" \\
& | "-" \, \text{factor} \\
& | \text{Number} \\
& | \text{StringLiteral} \\
& | \text{function\_call} \\
& | \text{Identifier} \\
\text{arguments} & \to \text{expression} \left( "," \, \text{expression} \right)*
\end{align*}
$$

*If the LaTeX fails to load, please copy the LaTeX code above and paste it in this [Website](http://www.texrendr.com/)*

---

## Goal for the Future

$$
\begin{align*}
\text{program} & \to \text{statement}* \\
\text{statement} & \to \text{declaration} \\
& | \text{function\_definition} \\
& | \text{function\_call} \\
& | \text{if\_statement} \\
& | \text{while\_loop} \\
& | \text{for\_loop} \\
& | \text{return\_statement} \\
& | \text{expression} \, \text{EoL} \\
\text{declaration} & \to \text{"let"} \, \text{Identifier} \, ( "=" \, \text{expression} )? \\
\text{function\_definition} & \to \text{"fn"} \, \text{Identifier} "(" \text{parameter\_list}? ")" \, "\{" \, \text{statement}* "\}" \\
\text{parameter\_list} & \to \text{Identifier} \, ("," \, \text{parameter\_list})? \\
\text{function\_call} & \to \text{BuiltInFunction} "(" \text{arguments}? ")" \\
& | \text{Identifier} "(" \text{arguments}? ")" \\
\text{if\_statement} & \to \text{"if"} \, \text{expression} "\{" \, \text{statement}* "\}" \\
& \left( \text{"else"} "\{" \, \text{statement}* "\}" \right)? \\
\text{while\_loop} & \to \text{"while"} \, \text{expression} "\{" \, \text{statement}* "\}" \\
\text{for\_loop} & \to \text{"for"} \, \text{Identifier} \, \text{"in"} \, \text{expression} "\{" \, \text{statement}* "\}" \\
\text{return\_statement} & \to \text{"return"} \, \text{expression} \\
\text{expression} & \to \text{term} \, (("+" | "-" | "\&\&" | "\|\|" | "<" | "<=" | ">" | ">=") \, \text{term})* \\
\text{term} & \to \text{factor} \, (("*" | "/") \, \text{factor})* \\
\text{factor} & \to "(" \, \text{expression} ")" \\
& | "-" \, \text{factor} \\
& | "!" \, \text{factor} \\
& | \text{Number} \\
& | \text{StringLiteral} \\
& | \text{function\_call} \\
& | \text{Identifier} \\
\text{arguments} & \to \text{expression} \, ("," \, \text{expression})*
\end{align*}
$$

*If the LaTeX fails to load, please copy the LaTeX code above and paste it in this [Website](http://www.texrendr.com/)*

---
