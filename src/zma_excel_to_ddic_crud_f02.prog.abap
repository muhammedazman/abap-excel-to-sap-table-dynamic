*----------------------------------------------------------------------*
***INCLUDE ZMA_EXCEL_TO_DDIC_CRUD_F02.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form check_key_fields_match
*&---------------------------------------------------------------------*
form check_key_fields_match .
  loop at gt_key_fields assigning field-symbol(<ls_key_field>).
    try.
        if <ls_key_field>-rollname ne co_mandt.
          data(l_field) = gt_excel[ row = 1 value = <ls_key_field>-fieldname ] .
        endif.

      catch cx_root into data(lx_err).
        lv_msg_err = 'Schlüsselfeld in excel nicht gefunden'.
        perform add_new_exception using lv_msg_err <ls_key_field>.
    endtry.
  endloop.
endform.

*&---------------------------------------------------------------------*
*& Form check_all
*&---------------------------------------------------------------------*
form check_all using lv_conv_status type abap_bool.
  clear gv_row_count.

  data: lv_tabname   type tabname,
        lv_fieldname type fieldname.
  data: lv_status_clear type abap_boolean.

* Exceldaten
  loop at <lt_data> assigning <ls_data>.
    assign component co_light of structure <ls_data> to <lv_light>.
    assign component co_status of structure <ls_data> to <lv_status>.
    assign component co_tabcol of structure <ls_data> to <lt_tabcol>.

    if gv_initialized = abap_true.
      clear <lt_tabcol>.
    endif.

    lv_status_clear = cond #( when gv_save_clicked = abap_true then abap_false
                              when gv_initialized = abap_true  then abap_true
                              else abap_false ).

    if lv_status_clear = abap_true.
      <lv_light>  = co_light_succ.
      <lv_status> = ''.
    endif.

*   schlüsselfeld
    perform check_to_key_fields.

*   dublicate rows
    perform check_itab_dublicate_row.

*   style table
    perform set_lvc_t_styl using <ls_data> .

*--------------------------------------------------------------------*
*   loop at -- check field
*--------------------------------------------------------------------*
    loop at gt_table_field assigning <ls_table_field> where fieldname <> co_mandt.

*     feld-data
      assign component <ls_table_field>-fieldname of structure <ls_data> to <lv_field_data>.

      if lv_conv_status = abap_true.
        if <lv_field_data> is assigned and <lv_field_data> is not initial.
          try.
              " convertion exit routine
              perform conversion_exit_routine_lsdata using  <ls_table_field>  changing <lv_field_data>.
            catch cx_root into data(lx_err).
              lv_msg_row = |{ <ls_table_field>-scrtext_m }: Conversion Exit Error |.
              perform set_status  using lv_msg_row co_light_erro <ls_table_field>-fieldname
                                  changing <ls_data>.
          endtry.
        endif.
      endif.

*     domäne
      if <ls_table_field>-domname is not initial and <lv_field_data> is not initial.
        perform check_to_domain using <ls_table_field>.
      endif.

*     prüftabelle
      if <ls_table_field>-checktable is not initial and <lv_field_data> is not initial.
        lv_tabname   = <ls_table_field>-checktable.
        lv_fieldname = <ls_table_field>-fieldname.

        perform check_to_wertetabelle using <ls_table_field>.
      endif.
    endloop. " end of gt_table_field

    <lv_light>  = cond #( when <lv_light> is initial and <lv_status> is initial then co_light_succ
                          else <lv_light> ).

    <lv_status> = cond #( when <lv_light> = co_light_succ then co_ok
                          else <lv_status> ).

*   datenbank
    perform check_data_in_sap_table changing <ls_data>.

    perform count_transaction_in_excel using <ls_data>.

    unassign: <ls_table_field>, <lv_field_data>.
  endloop. " end of lt_data

  if go_alv is bound.
    perform refresh_top_of_page.
  endif.

  gv_save_clicked = abap_false.
endform.


*&---------------------------------------------------------------------*
*& Form check_to_key_fields
*&---------------------------------------------------------------------*
form check_to_key_fields.
  loop at gt_key_fields assigning field-symbol(<ls_key>).
    if <ls_key>-fieldname ne co_mandt.
      assign component <ls_key>-fieldname of structure <ls_data> to field-symbol(<fs_v>).

      if <fs_v> is not assigned.
        lv_msg_row = |Keyflag nicht gefunden { <ls_key>-fieldname }|.
        perform set_status using lv_msg_row co_light_erro <ls_key>-fieldname
                           changing <ls_data>.
      elseif <fs_v> is initial.
        perform set_status using 'Warnung: Einige Schlüsselfelder sind leer'
                                  co_light_warn
                                  <ls_key>-fieldname
                        changing <ls_data> .
      endif.
    endif.
  endloop. " end of gt_key_fields
endform.


*&---------------------------------------------------------------------*
*& Form check_status
*&---------------------------------------------------------------------*
form set_status using lv_msg       type char50
                      lv_light     type c
                      lv_fieldname type fieldname
                changing ls_data   type any.

  field-symbols <lt_tabcol> type lvc_t_scol.
  assign component co_tabcol of structure ls_data to <lt_tabcol>.

* Zuordnung zu LIGHT und STATUS
  assign component co_light of structure ls_data to field-symbol(<lv_light>).
  assign component co_status of structure ls_data to field-symbol(<lv_status>).


  if <lv_light> = co_light_succ.
    <lv_status> = ''.
  endif.

  split lv_msg at space  into table data(itab).
  if <lv_status> ns itab[ 1 ]. " contains not string
    <lv_status> = |{ <lv_status> }{ lv_msg }. |.
  endif.

  if <lv_light> <> co_light_erro or lv_light = co_light_dubl_err.
    <lv_light> = lv_light.
  endif.

*== set color
  data ls_color_field type lvc_s_scol.
  data ls_color       type lvc_s_colo.

*    int=1, inv=0, col=6, bright red
  if <lv_light> = co_light_erro or <lv_light> = co_light_warn or <lv_light> = co_light_dubl_err.
    clear: ls_color_field, ls_color.
    ls_color_field-fname = lv_fieldname.

    if <lv_light> = co_light_erro.
      ls_color-int = 0.
      ls_color-inv = 0.
      ls_color-col = 6.
    else.
      ls_color-int = 0.
      ls_color-inv = 0.
      ls_color-col = 7.
    endif.

    ls_color_field-color = ls_color.
    insert ls_color_field into table <lt_tabcol>.
  endif.
endform.


*&---------------------------------------------------------------------*
*& Form check_to_domain
*&---------------------------------------------------------------------*
form check_to_domain using ls_table_field type dfies.

  field-symbols: <lv_value> type any.

  assign component ls_table_field-fieldname of structure <ls_data> to <lv_value>.

  if <lv_value> is assigned.
* Domäne über Name
    data(lt_domain_value_by_name) = lcl_domvalues=>get_by_name( ls_table_field-rollname ).

    data(lv_len) = lines( lt_domain_value_by_name ).

    if lv_len > 0.


      read table lt_domain_value_by_name assigning field-symbol(<ls_dom>) index 1.

      if sy-subrc = 0.
        if <ls_dom>-option = 'BT'.
          if <lv_value> between <ls_dom>-low and <ls_dom>-high.
            sy-subrc = 0.
          endif.
        elseif <ls_dom>-option = 'EQ'.
          read table lt_domain_value_by_name transporting no fields with key low = <lv_value>.
        else.
          sy-subrc = 1.
        endif.
      endif.

      if sy-subrc <> 0.
        lv_msg_row = |{ ls_table_field-scrtext_m }: keine Domänendaten eingegeben|.
        if <ls_dom>-option = 'BT'.
          lv_msg_row = |{ ls_table_field-scrtext_m }: Daten außerhalb des Bereichs|.
        endif.

        perform set_status using lv_msg_row co_light_erro ls_table_field-fieldname
        changing <ls_data>.

      endif.
    endif.

    unassign <lv_value>.

  endif.
endform.


*&---------------------------------------------------------------------*
*& Form check_to_wertetabelle
*&---------------------------------------------------------------------*
form check_to_wertetabelle using ls_table_field type dfies.
  data: lt_checktable      type standard table of ty_checktables,
        lt_checktable_data type standard table of ty_checktable_data,
        ls_checktable_data type ty_checktable_data,
        lv_istrue          type abap_bool.

  field-symbols: <lv_value>      type any.

  if ls_table_field-fieldname <> co_mandt.
    assign component ls_table_field-fieldname of structure <ls_data> to <lv_value>.

    if <lv_value> is assigned.
      if gt_checktables is not initial.
        lt_checktable = filter #( gt_checktables using key tab_fieldname
                                                     where tabname   = ls_table_field-tabname
                                                       and fieldname = ls_table_field-fieldname ).
      endif.

      if lt_checktable is initial.
        select  * into  table @lt_checktable
          from dd05q
          where dd05q~tabname = @ls_table_field-tabname
          and dd05q~fieldname = @ls_table_field-fieldname.

        append lines of lt_checktable to gt_checktables.
      endif.

      if sy-subrc = 0.
        loop at lt_checktable into data(ls_checktable) where fieldname = ls_table_field-fieldname
                                                          and tabname = ls_table_field-tabname.
          data: lv_where type string,
                lv_value type string,
                lv_fname type fieldname,
                lv_tname type tabname.

          lv_value = <lv_value>.
          lv_tname = ls_checktable-checktable.
          lv_fname = ls_checktable-checkfield.
          lv_where = |{ lv_fname } = '{ lv_value }'|.

          try.
              lv_istrue = abap_true.

              if gt_checktable_data is not initial.
                lt_checktable_data = filter #( gt_checktable_data  where tabname   = ls_checktable-checktable
                                                                     and fieldname = ls_checktable-checkfield
                                                                     and value     = lv_value ).
              endif.

              if lt_checktable_data is initial.
                lv_istrue = abap_false.

                field-symbols: <ft_table> type standard table.
                data: table_ref type ref to data.

                perform create_table_with_field using ls_table_field changing table_ref.

                assign table_ref->* to <ft_table>.

                select (lv_fname)
                   from (lv_tname)
                   where (lv_where)
                   group by (lv_fname)
                   order by (lv_fname)
                   into table @<ft_table>.

                loop at <ft_table> assigning field-symbol(<lv_check_data>).
                  ls_checktable_data-fieldname = lv_fname.
                  ls_checktable_data-tabname   = lv_tname.
                  ls_checktable_data-value     = <lv_check_data>.
                  insert ls_checktable_data into table gt_checktable_data.
                endloop.

                read table gt_checktable_data transporting no fields with key tabname   = lv_tname
                                                                              fieldname = lv_fname
                                                                              value     = lv_value.
                lv_istrue = cond #( when sy-subrc = 0 then abap_true else abap_false ).
              endif.

              if lv_istrue = abap_true.
                exit.
              endif.

            catch cx_root.
          endtry.
        endloop.

        if lv_istrue = abap_false.
          lv_msg_row = |{ ls_table_field-scrtext_m }: in der Wertetabelle nicht gefunden|.
          perform set_status using lv_msg_row co_light_erro ls_table_field-fieldname
          changing <ls_data>.
        endif.

      endif.
    endif.
  endif.
endform.


*&---------------------------------------------------------------------*
*& Form check_data_in_sap_table
*&---------------------------------------------------------------------*
form check_data_in_sap_table changing ls_data type any .

  data ls_table_field type dfies.
  data lv_status      type c.
  data lv_where_con   type string.
  data lv_cnt         type i.


* Where-Clause
  loop at gt_table_field into ls_table_field where keyflag = 'X' and fieldname <> 'MANDT'.

    assign component ls_table_field-fieldname of structure ls_data to field-symbol(<lv_data>).

    if <lv_data> is assigned.
      if lv_where_con is not initial.
        lv_where_con = |{ lv_where_con } AND |.
      endif.

      lv_where_con = |{ lv_where_con }{ ls_table_field-fieldname } = '{ <lv_data> }'|.
    endif.
  endloop. " en of where clause

* Traffic Light and status
  assign component co_light of structure ls_data to <lv_data>.

  if p_tname is not initial and lv_where_con is not initial.
    select single 1 into @lv_cnt from (p_tname) where (lv_where_con).
  endif.

  lv_status = cond #( when sy-subrc eq 4 and ( <lv_data> = co_light_crud_succ_del or
                                               <lv_data> = co_light_succ          or
                                               <lv_data> = co_light_warn            )   then co_status_insert

                      when sy-subrc eq 0 and ( p_delete = 'X' and p_aktive = 'X' )
                                         and ( <lv_data> = co_light_succ  or <lv_data> = co_light_warn ) then co_status_delete

                      when sy-subrc eq 0  and ( <lv_data> = co_light_crud_succ_upt or
                                                <lv_data> = co_light_crud_succ_ins or
                                                <lv_data> = co_light_succ          or
                                                <lv_data> = co_light_warn             ) then co_status_update

                      when <lv_data> eq co_light_erro or <lv_data> eq co_light_crud_err then co_status_error

                      else co_status_error ).

  <lv_data> = cond #( when <lv_data> = co_light_crud_succ_ins or
                           <lv_data> = co_light_crud_succ_del or
                           <lv_data> = co_light_crud_succ_upt then co_light_succ
                      when <lv_data> = co_light_crud_err or
                           <lv_data> = co_light_dubl_err      then co_light_erro
                      else <lv_data> ).

* Status(CRUD)
  assign component co_crud of structure ls_data to <lv_data>.
  <lv_data> = cond #( when <lv_data> = co_status_delete and lv_status = co_status_insert then co_status_insert
                      when <lv_data> = co_status_delete and lv_status <> co_status_insert then co_status_delete
                      else lv_status ).
endform.


*&---------------------------------------------------------------------*
*& Form insert_update_delete
*&---------------------------------------------------------------------*
form insert_update_delete using lbl_admin type abap_boolean .

* Zusätliche Spalte
  data: st_extr_columns type table of selopt.
  perform add_not_used_columns using co_light   changing st_extr_columns.
  perform add_not_used_columns using co_status  changing st_extr_columns.
  perform add_not_used_columns using co_crud    changing st_extr_columns.
  perform add_not_used_columns using co_cell    changing st_extr_columns.
  perform add_not_used_columns using co_tabcol  changing st_extr_columns.
*  perform add_not_used_columns using co_mandt   changing st_extr_columns.

*--------------------------------------------------------------------*
* data definition
*--------------------------------------------------------------------*.
  field-symbols: <lt_styl> type lvc_t_styl,
                 <ls_styl> type lvc_s_styl.

  data lv_where type string.
  data lv_set_statement type string.
  field-symbols: <fs_data> type any,
                 <fv_data> type data,
                 <lv_data> type data.

  data: d_line type ref to data.

  field-symbols: <ft_tab> type any table.
  create data d_line type table of (p_tname).
  assign d_line->* to <ft_tab>.

  create data d_line like line of <ft_tab>.
  assign d_line->* to <fs_data>.

  assign component 1 of structure <fs_data> to <fv_data>.
  <fv_data> = ''.

*--------------------------------------------------------------------*
* set to value and where
*--------------------------------------------------------------------*
  try.
      loop at <lt_data> assigning <ls_data>.
        assign component co_crud of structure <ls_data> to field-symbol(<fv_status_crud>).
        assign component co_status of structure <ls_data> to field-symbol(<fv_status>).
        assign component co_cell of structure <ls_data> to <lt_styl>.


        if <fv_status_crud> eq co_status_error.
          continue.
        endif.

        if lbl_admin = abap_false or
        ( lbl_admin = abap_true and <fv_status> = 'selected' ).

          loop at gt_comp_details into data(ls_comp_field) where name not in st_extr_columns.

            assign component ls_comp_field-name of structure <fs_data> to <fv_data>.
            assign component ls_comp_field-name of structure <ls_data> to <lv_data>.

            <fv_data> = <lv_data>.

            assign component ls_comp_field-name of structure <ls_data> to <lv_field_data>.

            if <lv_field_data> is assigned.

*== set where clause
              read table gt_key_fields transporting no fields with key fieldname = ls_comp_field-name. " if key-field is
              if sy-subrc = 0.
                if lv_where is not initial.
                  lv_where = |{ lv_where } and |.
                endif.

                lv_where = |{ lv_where }{ ls_comp_field-name } = `{ <lv_field_data> }`|.
              endif.

*== set update fields
              if ls_comp_field-name <> co_mandt.
                lv_set_statement = |{ lv_set_statement }{ ls_comp_field-name } = `{ <lv_field_data> }` |.
              endif.
            endif.
          endloop. " end of gt_comp_details

*== kein Fehler
          if <fv_status_crud> ne co_status_error.
            case <fv_status_crud>.
              when co_status_insert.

                if lbl_admin = abap_true or ( p_insert = abap_true and lbl_admin = abap_false ).
                  perform insert using p_tname <fs_data>.
                endif.

              when co_status_update.

                if lbl_admin = abap_true or ( p_update = abap_true and lbl_admin = abap_false ).
                  perform update using p_tname lv_set_statement lv_where.
                endif.

              when co_status_delete.

                if lbl_admin = abap_true or ( p_delete = abap_true and lbl_admin = abap_false ).
                  perform delete using p_tname lv_where.
                endif.
            endcase.

            clear: lv_where, lv_set_statement.
          endif.
        endif.
      endloop. " end of lt_data

      commit work and wait.
    catch cx_root.
      rollback work.
  endtry.

endform.


*&---------------------------------------------------------------------*
*& Form insert
*&---------------------------------------------------------------------*
form insert using lv_tabname  type dd03l-tabname
      ls_tab      type any.
* (4.1) Wenn Insert, muss man keinen sein,

  insert (lv_tabname)  from ls_tab.

  assign component co_status of structure <ls_data> to field-symbol(<lv_status_result>).
  assign component co_light of structure <ls_data> to field-symbol(<lv_light_result>).

  if sy-subrc = 0.
    <lv_status_result> = 'OK: Insert ist erfolgreich'.
    <lv_light_result> = co_light_crud_succ_ins.

    gv_count_insert_sap  = gv_count_insert_sap + 1.
    gv_transaktion_count = gv_transaktion_count + 1.
  else.
    rollback work.
    <lv_status_result> = 'Fehler: Insert ist nicht erfolgreich'.
    <lv_light_result> = co_light_crud_err.

    gv_count_error_sap = gv_count_error_sap + 1.
  endif.

endform.


*&---------------------------------------------------------------------*
*& Form update
*&---------------------------------------------------------------------*
form update using lv_tabname   type dd03l-tabname
      lv_set_statement type string
      lv_where         type string.

* (4.2) Wenn Update, muss man vorhanden sein,
  update (lv_tabname) set (lv_set_statement) where (lv_where).

  assign component co_status of structure <ls_data> to field-symbol(<lv_status_result>).
  assign component co_light of structure <ls_data> to field-symbol(<lv_light_result>).

  if sy-subrc = 0.
    <lv_status_result> = 'OK: Update ist erfolgreich'.
    <lv_light_result>  = co_light_crud_succ_upt.

    gv_count_update_sap  = gv_count_update_sap + 1.
    gv_transaktion_count = gv_transaktion_count + 1.
  else.
    rollback work.
    <lv_status_result> = 'Fehler: Update ist nicht erfolgreich'.
    <lv_light_result> = co_light_crud_err.

    gv_count_error_sap = gv_count_error_sap + 1.
  endif.

endform.

*&---------------------------------------------------------------------*
*& Form delete
*&---------------------------------------------------------------------*
form delete using lv_tabname type dd03l-tabname
      lv_where   type string.

  delete from (lv_tabname)  where (lv_where).

  assign component co_status of structure <ls_data> to field-symbol(<lv_status_result>).
  assign component co_light of structure <ls_data> to field-symbol(<lv_light_result>).

  if sy-subrc = 0.
    <lv_status_result> = 'OK: Delete ist erfolgreich'.
    <lv_light_result>  = co_light_crud_succ_del.

    gv_count_delete_sap  = gv_count_delete_sap + 1.
    gv_transaktion_count = gv_transaktion_count + 1.
  else.
    rollback work.
    <lv_status_result> = 'Fehler: Delete ist nicht erfolgreich'.
    <lv_light_result> = co_light_crud_err.

    gv_count_error_sap = gv_count_error_sap + 1.
  endif.

endform.

*&---------------------------------------------------------------------*
*& Form add_not_used_columns
*&---------------------------------------------------------------------*
form add_not_used_columns  using    lv_column_name type fieldname
changing lt_extr_columns type standard table.
  data: ls_extr_columns type selopt.

  ls_extr_columns-sign   = 'I'.
  ls_extr_columns-option = 'EQ'.
  ls_extr_columns-low    = lv_column_name.

  append ls_extr_columns to lt_extr_columns.
  clear ls_extr_columns.
endform.

*&---------------------------------------------------------------------*
*& Form check_tabelle_in_sap
*&---------------------------------------------------------------------*
form check_tabelle_in_sap  using lv_tabname   type tabname
changing lv_count_tab type i.

* Überprüft, ob es Tabelle gibt.

  select single 1 into @lv_count_tab
  from dd03l
  where tabname = @lv_tabname.

  if sy-subrc is not initial.
    perform add_new_exception
    using
          'Tabelle ist nicht verfügbar'(e02)
          lv_tabname.
  endif.

endform.

*&---------------------------------------------------------------------*
*& Form check_key_fields_in_tabelle
*&---------------------------------------------------------------------*
form check_key_fields_in_tabelle  using    lv_tabname type tabname.

* get key fields
  select * into table gt_key_fields  from dd03l
    where tabname   = lv_tabname
      and keyflag   = 'X'
      and comptype  = 'E'
      and as4local  = 'A'.

  if sy-subrc <> 0.
    perform add_new_exception
    using
          'keine Rollname gefunden'(e01)
          lv_tabname.
  endif.
endform.

*&---------------------------------------------------------------------*
*& Form check_itab_dublicate_row
*&---------------------------------------------------------------------*
form check_itab_dublicate_row.
  data lv_where type string.
  field-symbols <fs_wa> type any.

  loop at gt_key_fields assigning field-symbol(<ls_key>) where fieldname ne co_mandt.
    assign component <ls_key>-fieldname of structure <ls_data> to field-symbol(<fs_v>).

    if <fs_v> is assigned.
      if lv_where is not initial.
        lv_where = |{ lv_where } AND |.
      endif.

      lv_where = |{ lv_where }{ <ls_key>-fieldname } = '{ <fs_v> }'|.
    endif.
  endloop.

  data i_counter type i.
  loop at <lt_data> transporting no fields where (lv_where).
    i_counter = i_counter + 1.
  endloop.

  if i_counter > 1.
    lv_msg_row = 'Dublicate Schlüsselkey'.
    perform set_status using lv_msg_row co_light_dubl_err <ls_key>-fieldname
    changing <ls_data>.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form set_lvc_t_styl
*&---------------------------------------------------------------------*
form set_lvc_t_styl using ls_data type any.

  data ls_stylerow type lvc_s_styl.
  data lv_style    type raw4.

  field-symbols <lt_style> type lvc_t_styl.

  assign component co_cell of structure <ls_data> to <lt_style>.

  clear <lt_style>.

  loop at gt_table_field assigning <ls_table_field>.
    clear ls_stylerow.

    assign component <ls_table_field>-fieldname of structure ls_data to <lv_field_data>.
    assign component co_light of structure ls_data to <lv_light>.

    if <lv_field_data> is assigned.
      lv_style =
            cond #( when gv_edit_bool = abap_true   then cl_gui_alv_grid=>mc_style_enabled
                                                    else cl_gui_alv_grid=>mc_style_disabled ).


      ls_stylerow-style =
            cond #( when <lv_field_data> <> ''
                     and <ls_table_field>-keyflag = abap_true
                     and <lv_light> <> co_light_dubl_err then cl_gui_alv_grid=>mc_style_disabled
                                                         else lv_style
                   ).


*Color Field
*-----------------------------------------------------------------*
      assign component co_tabcol of structure ls_data to <lt_tabcol>.
      if <lt_tabcol> is assigned and gv_edit_bool = abap_true. "wenn eine Zelle farbig ist, aktivieren Sie sie.
        read table <lt_tabcol> transporting no fields with key fname = <ls_table_field>-fieldname.

        if sy-subrc = 0.
          ls_stylerow-style = cl_gui_alv_grid=>mc_style_enabled.
        endif.
      endif.
    endif.

    ls_stylerow-fieldname = <ls_table_field>-fieldname.

    insert ls_stylerow into table <lt_style>.
  endloop.
endform.

*&---------------------------------------------------------------------*
*& Form count_transaction_in_excel
*&---------------------------------------------------------------------*
form count_transaction_in_excel  using  ls_data type any.
  field-symbols <lv_data> type char01.

  assign component co_crud of structure ls_data to <lv_data>.

  case <lv_data>.
    when co_status_insert.
      gv_count_insert_excel = gv_count_insert_excel + 1.
    when co_status_delete.
      gv_count_delete_excel = gv_count_delete_excel + 1.
    when co_status_update.
      gv_count_update_excel = gv_count_update_excel + 1.
    when co_status_error.
      gv_count_error_excel = gv_count_error_excel + 1.
  endcase.

  gv_row_count = gv_row_count + 1.

endform.

form clear_all_counter.

  gv_count_insert_excel = 0.
  gv_count_update_excel = 0.
  gv_count_delete_excel = 0.
  gv_count_error_excel  = 0.
  gv_count_insert_sap   = 0.
  gv_count_update_sap   = 0.
  gv_count_delete_sap   = 0.
  gv_count_error_sap    = 0.

  gv_transaktion_count  = 0.
  gv_row_count = 0.

endform.

*&---------------------------------------------------------------------*
*& Form create_table_with_field
*&---------------------------------------------------------------------*
form create_table_with_field  using    ls_table_field type dfies
                              changing lt_check_data  type ref to data.

  data: lr_element type ref to cl_abap_elemdescr,
        lr_struc   type ref to cl_abap_structdescr,
        lr_table   type ref to cl_abap_tabledescr,
        lt_comp    type cl_abap_structdescr=>component_table,
        ls_comp    like line of lt_comp,
        lr_data    type ref to data.
  field-symbols <table> type any table.

*== Element Description for field
  lr_element ?= cl_abap_elemdescr=>describe_by_name( ls_table_field-rollname ).
*== Field name
  ls_comp-name = ls_table_field-fieldname.
*== Field type (element)
  ls_comp-type = lr_element.
*== add element to components table
  append ls_comp to lt_comp.

*== Create structure/ work area
  lr_struc = cl_abap_structdescr=>create( lt_comp ).

*== create table by structure reference
  lr_table = cl_abap_tabledescr=>create(
                  p_line_type  = lr_struc
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).

*== create data handle for table
  create data lr_data type handle lr_table.

  lt_check_data = lr_data.

*== assign data to table-pointer
*  ASSIGN lr_data->* TO <table>.

endform.
