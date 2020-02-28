# Language-Interpreter
Language interpreter for a simplified OOP language:

Specificatii de limbaj:<br />

class <simbol_clasă> [extends <simbol_clasă_părinte>]<br />
Se definește o nouă clasa cu numele <simbol_clasă> care poate să extindă<br />
<simbol_clasă_părinte> (în cazul în care apare keyword-ul extends)<br />

newvar <simbol_variabilă> = <simbol_clasă> instanțiază o variabilă de tipul<br />
<simbol_clasă> - variabilele se țin doar în clasa Global<br />

<tip_returnat> <simbol_clasă>::<simbol_funcție> ([tip_param_1, tip_param_2 … ,tip_param_n])<br />
Se definește o nouă funcție care aparține clasei <simbol_clasă>.<br />
Pot exista funcții cu aceeași denumire (într-o clasă) care primesc parametrii cu<br />
tipuri diferite sau primesc parametrii cu tipuri diferite și întorc un alt tip. Funcțiile cu aceeași<br />
denumire nu diferă doar prin tipul întors. De asemenea, numărul de argumente poate varia.<br />

Exemple limbaj:
Int f::A (Double, Double)<br />
Int f::A (Float, Double)<br />
Float f::A (Double, Double, Double)<br />

Functiile esentiale:<br />
parse::String -> [Instruction]<br />
primeste un string si intoarce o lista de instructiuni<br />

interpret::Instruction -> Program -> Program<br />
primeste o instructiune si o adauga in program.<br />
Se poate sa lipseasca tipul unei variabile(se foloseste keywordul infer). Atunci interpret
va incerca sa deduca tipul acesteia inainte de a o adauga in program.<br />

infer :: Expr -> Program -> Maybe String<br />
Se primeste o expresie(Expr) sunt forma unui datatype(arbore de expresie), un program si se cere determinarea tipului<br />
expresiei, daca acest lucru este posibil(intoarce Maybe String)<br />

Exemplu:
Expr: FCall "a” "func" [Va "mydouble", Va "myfloat"] (a.func(mydouble, myfloat))

Program:
class Double

class A <br />
newvar a = A <br />
newvar mydouble = Double<br />
newvar myfloat = Float <br />
Double A::func (Double, Float) <br />
Double A::func (Float, Float) <br />

Sinteza de tip reuseste: exista functia "func" in clasa A care primeste un Double si un Float. Tipul o sa fie Double.<br />
