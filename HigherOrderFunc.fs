
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

// System.Console.WriteLine(solve 1.0 2.0)

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


//Задание 3. Площадь круга, площадь цилиндра

let S r =
    let pi = 3.14 in
        pi*r*r;

let mult_h = fun circle h ->
    circle*h;

let superpositione = S >> mult_h // сначала применяется S, а затем результат передается в mult_h

let carry r h =
    mult_h (S r) h

//Задание 4-5. Рекурсия

let sumDigitsUp n =
    let rec sumDigitsUp1 n curSum =
        if n = 0 then curSum
        else
            let n1 = n / 10
            let cifr = n % 10
            let newSum = curSum + cifr
            sumDigitsUp1 n1 newSum
    sumDigitsUp1 n 0

let rec sumDigitsDown n =
    if n = 0 then 0
    else (n%10) + (sumDigitsDown (n / 10))


let main () =

    let radius = 3
    let height = 5

    let res_superposition = superpositione radius height
    printfn "Объем цилиндра (суперпозиция): %f" res_superposition

    let res_carry = carry radius height
    printfn "Объем цилиндра (каррирование): %f" res_carry

    let value = 12345

    System.Console.Write("Сумма цифр числа (рекурсия вверх): ")
    System.Console.WriteLine(sumDigitsUp(value))
    System.Console.Write("Сумма цифр числа (рекурсия вниз): ")
    System.Console.WriteLine(sumDigitsDown(value))

main()