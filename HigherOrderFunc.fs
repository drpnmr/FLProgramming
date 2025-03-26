
//Задание 1. Вывести Hello World

printfn "Hello World"

//Задание 2. Написать функцию решения квадратного уравнения

let solve (a, b, c) = 
    let D = b*b-4.*a*c in
        (-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a)
        // [(-b+sqrt(D))/(2.0*a); (-b-sqrt(D))/(2.0*a)]


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

let sumDigitsDown n =
    let rec sumDigitsUp1 n curSum =
        if n = 0 then curSum
        else
            let n1 = n / 10
            let cifr = n % 10
            let newSum = curSum + cifr
            sumDigitsUp1 n1 newSum
    sumDigitsUp1 n 0

let rec sumDigitsUp n =
    if n = 0 then 0
    else (n%10) + (sumDigitsDown (n / 10))

let rec fibUp n =
    if n = 0 then 0
    elif n = 1 then 1
    else fibUp (n - 1) + fibUp (n - 2)

//Лабораторная работа

let rec fibUpMatch n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fibUpMatch (n - 1) + fibUpMatch (n - 2)

let fibDownMatch n =
    let rec Fib a res b = 
        match b with
        | 0 -> res
        | n -> Fib res (res + a) (b - 1)
    Fib 0 1 n

let get ( res : bool) =
    match res with 
    | true -> 
        fun n -> sumDigitsUp n

    | false ->
        fun n -> fibDownMatch n
            
let task7 n (operation: int -> int -> int) value =
    let rec func n acc =
        match n with
        |0 -> acc
        |n ->
            let new_acc = operation acc (n % 10)
            func (n/10) new_acc
    func n value
     
     
//Задание 11. Любимый язык

let favoriteLang (lang: string) =
    match lang with
    | "F#" | "prolog" -> "Подлиза!"
    | "Python" -> "Неплохой выбор, но F# лучше"
    | "Java" -> "Круто, но попробуй F#"
    | _ -> "Что? Переходи на F#!"



let main () =

    // Задание 2

    //System.Console.WriteLine(solve (1.0, 2.0, -3.0))
    // System.Console.WriteLine(solve 1.0 2.0)

    // Задание 3

    let radius = 3
    let height = 5

    let res_superposition = superpositione radius height
    System.Console.Write("Объем цилиндра (суперпозиция): ")
    System.Console.WriteLine(res_superposition)

    let res_carry = carry radius height
    System.Console.Write("Объем цилиндра (каррирование): ")
    System.Console.WriteLine(res_carry)

    // Задания 4-5

    let value = 12345

    System.Console.Write("Сумма цифр числа (рекурсия вверх): ")
    System.Console.WriteLine(sumDigitsUp(value))
    System.Console.Write("Сумма цифр числа (рекурсия вниз): ")
    System.Console.WriteLine(sumDigitsDown(value))


    // Лабораторная работа

    let value_fib = 19
    System.Console.Write("Фиббоначи 19 элемент: ")
    System.Console.WriteLine(fibUp(value_fib))

    let result = fibUpMatch 19 
    let result2 = fibDownMatch 19 
    System.Console.Write("Фиббоначи 19 элемент: ")
    System.Console.WriteLine(result2)   

    let res3 = get false
    System.Console.WriteLine(res3 19)
    
    System.Console.Write("test: ")
    let test n = task7 n (fun x y -> if (x<y) then x else y) 10
    System.Console.WriteLine(test 123)

    // Задание 11
    System.Console.Write("Напиши любимый язык программирования: ")
    let text = System.Console.ReadLine()
    let result11 = favoriteLang text
    System.Console.WriteLine(result11)
    
main()