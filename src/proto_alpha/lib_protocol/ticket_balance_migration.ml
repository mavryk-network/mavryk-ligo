open Alpha_context

let add_ticket_balance contract ctxt ticket =
  Ticket_balance_key.ticket_balance_key_and_amount ctxt ~owner:contract ticket
  >>=? fun (hash, delta, ctxt) ->
  (* let delta : Z.t = Script_int.to_zint amount in *)
  Ticket_balance.adjust_balance ctxt hash ~delta >>=? fun (_, ctxt) ->
  return ctxt

let update_contract_tickets ctxt contract =
  Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
  match script with
  | None -> return ctxt
  | Some script ->
      Script_ir_translator.parse_script
        ctxt
        ~legacy:true
        ~allow_forged_in_storage:true
        script
      >>=? fun (ex_script, _ctxt) ->
      let (Ex_script {storage_type; storage; _}) = ex_script in
      Ticket_scanner.(
        tickets_of_value ctxt ~include_lazy:true storage_type storage)
      >>=? fun (tickets, ctxt) ->
      List.fold_left_es (add_ticket_balance contract) ctxt tickets

let is_originated contract =
  match Contract.is_implicit contract with Some _ -> false | _ -> true

let init ctxt =
  Contract.list ctxt >>= fun contracts ->
  let contracts = List.filter is_originated contracts in
  List.fold_left_es update_contract_tickets ctxt contracts
