
//Задание 1. Вывести Hello World

printfn "Hello World"

//Задание 2. Написать функцию решения квадратного уравнения

let solve (a, b, c) = 
    let D = b*b-4.*a*c in
        (-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a)

System.Console.WriteLine(solve (1.0, 2.0, -3.0))
