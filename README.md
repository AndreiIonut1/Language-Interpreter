# Language-Interpreter
Language interpreter for a simplified OOP language:

Specificatii de limbaj:
class <simbol_clasă> [extends <simbol_clasă_părinte>]
Se definește o nouă clasa cu numele <simbol_clasă> care poate să extindă
<simbol_clasă_părinte> (în cazul în care apare keyword-ul extends)

newvar <simbol_variabilă> = <simbol_clasă> instanțiază o variabilă de tipul
<simbol_clasă> - variabilele se țin doar în clasa Global

<tip_returnat> <simbol_clasă>::<simbol_funcție> ([tip_param_1, tip_param_2 … ,tip_param_n])
Se definește o nouă funcție care aparține clasei <simbol_clasă>.
Pot exista funcții cu aceeași denumire (într-o clasă) care primesc parametrii cu
tipuri diferite sau primesc parametrii cu tipuri diferite și întorc un alt tip. Funcțiile cu aceeași
denumire nu diferă doar prin tipul întors. De asemenea, numărul de argumente poate varia.

Exemple limbaj:
Int f::A (Double, Double)
Int f::A (Float, Double)
Float f::A (Double, Double, Double)

Functiile esentiale:
parse::String -> [Instruction]
primeste un string si intoarce o lista de instructiuni

interpret::Instruction -> Program -> Program
primeste o instructiune si o adauga in program.
Se poate sa lipseasca tipul unei variabile(se foloseste keywordul infer). Atunci interpret
va incerca sa deduca tipul acesteia inainte de a o adauga in program.

infer :: Expr -> Program -> Maybe String
Se primeste o expresie(Expr) sunt forma unui datatype(arbore de expresie), un program si se cere determinarea tipului
expresiei, daca acest lucru este posibil(intoarce Maybe String)

Exemplu:
Expr: FCall "a” "func" [Va "mydouble", Va "myfloat"] (a.func(mydouble, myfloat))
Program:
class Double
class A
newvar a = A
newvar mydouble = Double
newvar myfloat = Float
Double A::func (Double, Float)
Double A::func (Float, Float)

Sinteza de tip reuseste: exista functia "func" in clasa A care primeste un Double si un Float. Tipul o sa fie Double.
