
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

let maxElement list =
     match list with
         | [] -> 0
         | h::t -> reduce t max (fun a -> true) h 
 

let sumEven list = 
     reduce list (+) (fun a -> a % 2 = 0) 0
 
let oddCount list = 
     reduce list (fun a b -> a + 1) (fun a -> a % 2 = 1) 0

let rec frequency list n k =
    match list with
    | [] -> k
    | head::tail -> 
        let new_k = if head = n then k+1 else k
        frequency tail n new_k

//Задание 5. Найти наиболее часто встречающийся элемент списка

let rec frequency_list list main_list cur_list = 
    match list with
    | [] -> cur_list
    | head::tail -> 
        let freq_elem = frequency main_list head 0
        let new_list = cur_list@[freq_elem]
        frequency_list tail main_list new_list

let pos list el = 
    let rec pos_inner list el p = 
        match list with
            |[] -> 0
            | head::tail -> 
                if (head = el) then p
                else 
                    let p_next = p + 1
                    pos_inner tail el p_next
    pos_inner list el 1

let get_from_list list pos = 
    let rec get list p cur_p = 
        match list with 
            |[] -> 0
            |head::tail -> 
                if p = cur_p then head
                else 
                    let p_next = cur_p + 1
                    get tail p p_next
    get list pos 1

let find_most_frequent list = 
    let fL = frequency_list list list []
    (maxElement fL) |> (pos fL) |> (get_from_list list)   


let main () =
     //let arr = readList 5

     let arr = [1; 2; 3; 4; 5]
     //printList arr
     System.Console.Write("Сумма четных от 1 до 5: ")
     let result = sumEven arr
     System.Console.WriteLine(result)
     
     System.Console.Write("Минимальный от 1 до 5: ")
     let minEl = minElement arr
     System.Console.WriteLine(minEl)
 
     System.Console.Write("Количество нечетных от 1 до 5: ")
     let odds = oddCount arr
     System.Console.WriteLine(odds)

     let arr_5 = [1; 2; 3; 4; 5; 2; 3; 3;]
     System.Console.Write("Элементы списка для задания 5: ")
     printList arr_5
     System.Console.Write("Самый частый из них: ")
     System.Console.WriteLine(find_most_frequent arr_5)

main()