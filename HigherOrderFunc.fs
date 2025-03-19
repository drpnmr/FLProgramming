
//Задание 1. Вывести Hello World

printfn "Hello World"

//Задание 2. Написать функцию решения квадратного уравнения

let solve (a, b, c) = 
    let D = b*b-4.*a*c in
        (-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a)
        // [(-b+sqrt(D))/(2.0*a); (-b-sqrt(D))/(2.0*a)]

//System.Console.WriteLine(solve (1.0, 2.0, -3.0))


let solve_carry a b c = 
    let D = b*b-4.*a*c
    if D<0 then None
    else Some ((-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a))


type SolveResult =
    None
    | Linear of float
    | Quadratic of float*float

let solve_types a b c  =
    let D = b * b - 4.0 * a * c
    if a = 0.0 then
        if b = 0.0 then None 
        else Linear(-c / b)
    else
        if D < 0.0 then None
        else Quadratic(((-b + sqrt D) / (2.0 * a)), ((-b - sqrt D) / (2.0 * a)))


let res = solve_types 1.0 2.0 -3.0

match res with
| None -> printfn "Нет решений"
| Linear (x) -> printfn "Линейное уравнение, корни : %f" x
| Quadratic (x1, x2) -> printfn "Квадратное уравнение, корни : (%f, %f)" x1 x2


