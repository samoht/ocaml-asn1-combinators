(* IntList ::= CHOICE { end NULL, cons SEQUENCE { head INTEGER, tail IntList } },
   and [1; 2] in DER. *)
let der = "\x30\x0a\x02\x01\x01\x30\x05\x02\x01\x02\x05\x00"
let codec = ref None
let spawned = ref None

(* Runs when the fix is unrolled. Before the fix that was inside the first
   decode, in the Lazy it forced, and a decode started then from another domain
   forced the same Lazy and got Lazy.Undefined. *)
let int_list =
  Asn.S.fix (fun self ->
      (match (!codec, !spawned) with
      | Some c, None -> spawned := Some (Domain.spawn (fun () -> Asn.decode c der))
      | _ -> ());
      Asn.S.map
        (function `C1 () -> [] | `C2 (x, xs) -> x :: xs)
        (function [] -> `C1 () | x :: xs -> `C2 (x, xs))
        Asn.S.(choice2 null (sequence2 (required int) (required self))))

let ok = function Ok ([ 1; 2 ], "") -> () | _ -> exit 1

let () =
  let c = Asn.codec Asn.der int_list in
  codec := Some c;
  ok (Asn.decode c der);
  (match !spawned with Some d -> ok (Domain.join d) | None -> ());
  let c = Asn.codec Asn.der int_list in
  List.init 8 (fun _ -> Domain.spawn (fun () -> Asn.decode c der))
  |> List.iter (fun d -> ok (Domain.join d))
