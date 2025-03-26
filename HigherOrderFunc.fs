
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


//Задание 13, 14. Операции над взаимно простыми

let rec NOD a b =
    match b with
    |0 -> a
    |_ -> NOD b (a % b)

let coprime n (operation: int -> int -> int) initial =
    
    let isCoprime x = NOD x n = 1
    
    let rec traverse current acc =
        match current with
        | 0 -> acc
        | _ ->
            let newAcc = if isCoprime current then operation acc current else acc
            traverse (current - 1) newAcc
    
    traverse n initial

let eulerFunc number =
    coprime number (fun acc _ -> acc + 1) 0

// Задание 15. Взаимно простые с условием

let coprime_with_condition n (operation: int -> int -> int) initial (condition: int -> bool) =
    
    let isCoprime x = NOD x n = 1
    
    let rec traverse current acc =
        match current with
        | 0 -> acc
        | _ ->
            let newAcc = 
                if isCoprime current && condition current then operation acc current else acc
            traverse (current - 1) newAcc
    
    traverse n initial

// Задание 16, метод 1. Сумма простых делителей

let isPrime n =
    if n <= 1 then false
    else
        let rec isNotDivisible i =
            if i = n then true
            elif n % i = 0 then false
            else isNotDivisible (i + 1)
        isNotDivisible 2

let sumPrimeDivisors n =
    let rec loop divisor acc =
        if divisor > n then acc
        elif n % divisor = 0 && isPrime divisor then loop (divisor + 1) (acc + divisor)
        else loop (divisor + 1) acc
    loop 2 0

// Задание 16, метод 2. Количество нечетных цифр > 3

let digitsGreater3 number =
    let rec loop num acc =
        match num with
        |0 -> acc  
        |_ -> 
            let digit = num % 10 
            let newAcc = 
                if digit % 2 <> 0 && digit > 3 then acc + 1 else acc 
            loop (num / 10) newAcc
    loop number 0 

// Задание 16, метод 3. Произведение делителей, сумма цифр которых < суммы цифр исходного числа

let sumOfDigits n =
    let rec loop num acc =
        match num with
        |0 -> acc
        |_-> loop (num / 10) (acc + num % 10)
    loop n 0

let multyDivisors n condition =
    let rec loop d acc =
        if d > n then acc
        elif n % d = 0 && condition d then loop (d + 1) (acc * d) 
        else loop (d + 1) acc
    loop 1 1 

let multyCondition n =
    let mainSum = sumOfDigits n
    multyDivisors n (fun x -> sumOfDigits x < mainSum)



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

    // Задание 12

    let superpos_lang = (fun () -> System.Console.ReadLine()) >> favoriteLang >> System.Console.WriteLine  // Суперпозиция
    //System.Console.WriteLine("Напиши любимый язык программирования: ")
    //superpos_lang()
 

    let carry_lang input output =  // Каррирование
        output (favoriteLang input)

    //System.Console.WriteLine("Напиши любимый язык программирования: ")
    //carry_lang (System.Console.ReadLine()) System.Console.WriteLine


    // Задания 13, 14
    let max13 = coprime 10 (fun x y -> if (x>y) then x else y) 0
    let sum13 = coprime 10 (+) 0
    System.Console.WriteLine(sum13)

    let euler = eulerFunc 10
    System.Console.WriteLine(euler)

    // Задание 15
    let sum15 = coprime_with_condition 10 (+) 0 (fun x -> x % 2 = 0)
    System.Console.WriteLine(sum15)

    // Задание 16, метод 1
    let sum16_1 = sumPrimeDivisors 10
    System.Console.WriteLine(sum16_1)

    // Задание 16, метод 2
    let sum16_2 = digitsGreater3 12345
    System.Console.WriteLine(sum16_2)

    // Задание 16, метод 3
    let mult16_3 = multyCondition 18
    System.Console.WriteLine(mult16_3)
    
main()