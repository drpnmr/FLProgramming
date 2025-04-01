
let readList n =
    let rec readListRec n acc =
        match n with
            |0 ->acc
            |k ->
                let el = System.Console.ReadLine() |> int //преобразовали в число
                let newAcc = acc@[el] //el добавляется в конец acc, причем оператор @ создает новый список
                readListRec (k-1) newAcc
    readListRec n []
 
let rec printList list = 
     match list with
         | [] -> ignore
         | head::tail -> //первый + остальные
             System.Console.WriteLine(head.ToString())
             printList tail
 
let rec reduce list (f:(int->int->int)) (predicate:(int->bool)) acc =
     match list with
         | [] -> acc
         | head::tail ->
             let newAcc = if predicate head then f acc head else acc
             reduce tail f predicate newAcc
 
let minElement list =
     match list with
         | [] -> 0
         | h::t -> reduce t min (fun a -> true) h 
 
let sumEven list = 
     reduce list (+) (fun a -> a % 2 = 0) 0
 
let oddCount list = 
     reduce list (fun a b -> a + 1) (fun a -> a % 2 = 1) 0

let main () =
     //let arr = readList 5

     let arr = [1; 2; 3; 4; 5]
     printList arr
     let result = sumEven arr
     System.Console.WriteLine(result)
 
     let minEl = minElement arr
     System.Console.WriteLine(minEl)
 
     let odds = oddCount arr
     System.Console.WriteLine(odds)

main()