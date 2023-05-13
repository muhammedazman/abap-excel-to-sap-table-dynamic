*&---------------------------------------------------------------------*
*& Include          ZMA_EXCEL_TO_DDIC_CRUD_C01
*&---------------------------------------------------------------------*

class lcl_file implementation.

  method create.
    ro_result = new lcl_file( ).
  endmethod.

  method get_filename_open_pc.

    cl_gui_frontend_services=>file_open_dialog( exporting
                                                 window_title            = 'Select XLS/XLSX file...'
                                                 file_filter             =  cl_gui_frontend_services=>filetype_excel
                                                 default_extension       =  mv_filter
                                               changing
                                                 file_table              =  mt_files
                                                 rc                      =  mf_rc
                                               exceptions
                                                 file_open_dialog_failed =  1
                                                 cntl_error              =  2
                                                 error_no_gui            =  3
                                                 not_supported_by_gui    =  4
                                                 others                  =  5
                                               ).
    if mf_rc = 1.
      read table mt_files into ms_file index 1.
      rv_filename = ms_file-filename.
    endif.


    if sy-subrc <> 0.
      lv_msg_err = |{ sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.

      perform add_new_exception using lv_msg_err
            'FILE_OPEN_DIALOG'.
    endif.
  endmethod.


  method excel_to_internal_table.

    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = mv_file
        i_begin_col             = 1
        i_begin_row             = 1
        i_end_col               = 1000
        i_end_row               = 10000
      tables
        intern                  = rt_excel
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.

    if sy-subrc <> 0.
      lv_msg_err = cond #( when sy-subrc = 1 then 'INCONSISTENT_PARAMETERS'
                           when sy-subrc = 2 then 'UPLOAD_OLE'
                           else 'OTHERS' ).

      perform add_new_exception using lv_msg_err 'ALSM_EXCEL_TO_INTERNAL_TABLE'.
    endif.
  endmethod.

endclass.


class lcl_datenelements implementation.
  method create.
    ro_result = new lcl_datenelements( ).
  endmethod.


  method get_key_fields.
    data lv_count_tabelle type i.

    perform check_tabelle_in_sap  using iv_tabname  changing lv_count_tabelle.

    if lv_count_tabelle = 1.
      perform check_key_fields_in_tabelle using iv_tabname.
    endif.
  endmethod.


  method get_columns_of_sap_tabelle.
    perform get_ddic_field_list using p_tname changing rt_tab.
  endmethod.


  method serial_comp.
    r_type_struct ?= cl_abap_typedescr=>describe_by_data( struct ).
    check r_type_struct->type_kind = cl_abap_typedescr=>typekind_struct1  "flat
    or r_type_struct->type_kind = cl_abap_typedescr=>typekind_struct2. "deep

    do.
      try.
          data(comp_name) = segment( val   = comp_names
                                     index = sy-index
                                     sep   = `,` ).
          check line_exists( r_type_struct->components[ name = comp_name ] ).
          assign component line_index( r_type_struct->components[ name = comp_name ] ) of structure struct to field-symbol(<comp_val>).
          return = |{ return }{ <comp_val> }|.
        catch cx_sy_strg_par_val.
          exit.
      endtry.
    enddo.
  endmethod.
endclass.


class lcl_alv implementation.
  method create.
    ro_result = new lcl_alv( ).
  endmethod.


  method create_container .
    perform set_gui_splitter_container using 2 " row
          1 " column
    changing go_splitter
      go_container_top
      go_container_bottom
      go_alv.
  endmethod.


  method set_handler .
* Eventhandler registrieren
    set handler lcl_events=>on_toolbar for go_alv.
    set handler lcl_events=>on_f4help for go_alv.
    set handler lcl_events=>on_menu_bt_handle for go_alv.
    set handler lcl_events=>on_user_command for go_alv.
    set handler lcl_events=>on_top_of_page for go_alv.
    set handler lcl_events=>on_data_changed for go_alv.
  endmethod.

  method set_fcat.
    perform set_fcat_merge.
    perform add_extra_fcat.
  endmethod.


  method set_layout .
    data: lv_title type lvc_title.
    lv_title = 'Simulationsprogramm (nur Kontrolle)'(i01).

    if p_aktive = abap_true.
      lv_title = cond #( when p_insert = 'X' then 'Aktive Mode-Stapelverarbeitung (Insert)'(i02)
      when p_update = abap_true then 'Aktive Mode-Stapelverarbeitung (Update)'(i03)
      when p_delete = abap_true then 'Aktive Mode-Stapelverarbeitung (Delete)'(i04) ).
    elseif p_admin = abap_true.
      lv_title = ' Admin Mode - Bearbeitung & alle Transaktionen (nur ausgewählte Zeilen)'(i05).
    endif.

    gs_layout = value lvc_s_layo( zebra      = abap_true
                                  cwidth_opt = 'A'
                                  excp_fname = co_light
                                  stylefname = co_cell
                                  ctab_fname = co_tabcol
                                  detailinit = space
                                  grid_title = lv_title ).
  endmethod.

  method set_fcat_with_ddic .

    if is_table_field-datatype = 'CURR'.
      read table gt_table_field into data(ls_table_cuky_field) with key datatype = 'CUKY'.
    elseif is_table_field-datatype = 'QUAN'.
      read table gt_table_field into data(ls_table_unit_field) with key datatype = 'UNIT'.
    endif.

    append value lvc_s_fcat(
      fieldname  = is_table_field-fieldname
      ref_table  = is_table_field-tabname
      ref_field  = is_table_field-fieldname
      rollname   = is_table_field-rollname
      domname    = is_table_field-domname
      datatype   = is_table_field-datatype
      inttype    = is_table_field-inttype
      intlen     = is_table_field-intlen
      decimals   = is_table_field-decimals
      key        = is_table_field-keyflag
      col_pos    = iv_colpos
      no_out     = space
      scrtext_l  = is_table_field-scrtext_l
      scrtext_m  = is_table_field-scrtext_m
      scrtext_s  = is_table_field-scrtext_s
      reptext    = is_table_field-scrtext_m
      seltext    = is_table_field-scrtext_m
      checktable = cond #( when is_table_field-checktable is not initial then is_table_field-checktable )
      emphasize  = cond #( when is_table_field-keyflag = 'X' then 'C510')
      cfieldname = cond #( when is_table_field-datatype = 'CURR' and ls_table_cuky_field is initial then ls_table_cuky_field-fieldname )
      qfieldname = cond #( when is_table_field-datatype = 'QUAN' and ls_table_cuky_field is initial then ls_table_unit_field-fieldname )
    )
    to gt_fcat.
  endmethod.


  method set_fcat_custom.
    data ls_fcat type lvc_s_fcat.

    if is_fieldcatalog is not initial.
      ls_fcat = is_fieldcatalog.
      append ls_fcat to gt_fcat.
    endif.
    clear ls_fcat.
  endmethod.


  method set_alv.

    data: lt_f4fields type lvc_t_f4.

    lt_f4fields = value #( ( fieldname = co_crud register  = abap_true ) ).

    go_alv->register_f4_for_fields( it_f4 = lt_f4fields ).

    go_alv->list_processing_events( exporting i_event_name   = 'TOP_OF_PAGE'
                                              i_dyndoc_id    = go_docu ).

    go_alv->set_table_for_first_display( exporting
                                            i_bypassing_buffer = abap_false  " Puffer ausschalten
                                            is_layout          = gs_layout   " Layout
*                                            i_structure_name   = p_tname
                                          changing
                                            it_fieldcatalog    = gt_fcat     " Feldkatalog
                                            it_outtab          = <lt_data> ). " Ausgabetabelle

    cl_gui_alv_grid=>set_focus( control = go_alv ).
    go_alv->refresh_table_display( ).
    write: space.
  endmethod.

  method display_alv.
    me->create_container( ).
    me->set_handler( ).
    me->set_layout( ).
    me->set_fcat( ).
    me->set_alv( ).
  endmethod.
endclass.

class lcl_events implementation.

* Toolbar-Buttons hinzufügen:
* butn_type   Bezeichung
* 0           Button (normal)
* 1           Menü + Defaultbutton
* 2           Menü
* 3           Separator
* 4           Radiobutton
* 5           Auswahlknopf (Checkbox)
* 6           Menüeintrag
  method on_toolbar.
    data toolbar type ttb_button.
    toolbar = value ttb_button(
      base toolbar ( function = co_ucomm_toolbar
                         icon = cond #( when show_toolbar is initial then icon_column_right else icon_column_left )
                    quickinfo = cond #( when show_toolbar is initial then 'ALV-Standardfunktion anzeigen'(003)  else 'ALV-Standardfunktion ausblenden'(004) ) )
                  ( butn_type = 3 ) ).
    "SHOW/HIDE TOOLBAR
    if show_toolbar is initial.
      e_object->mt_toolbar =  toolbar.
    else.
      e_object->mt_toolbar =  toolbar = value ttb_button( base toolbar ( lines of e_object->mt_toolbar ) ).
    endif.

    append value #(  butn_type = 2
                     icon      = icon_list
                     quickinfo = 'Optionen für leere Spalten'
                     function  = co_ucomm_menu ) to e_object->mt_toolbar.

    if p_simula = abap_true.
      perform delete_toolbar_buttons changing e_object.

    elseif p_aktive = abap_true.
      perform delete_toolbar_buttons changing e_object.

* Separator hinzufügen
      append value #( butn_type = 3 ) to e_object->mt_toolbar.

      data(lv_text) = cond #( when p_insert = abap_true then co_insert_all_text
                              when p_update = abap_true then co_update_all_text
                              when p_delete = abap_true then co_delete_all_text ).

      data(lv_icon) = cond #( when p_insert = abap_true then icon_system_save
                              when p_update = abap_true then icon_system_save
                              when p_delete = abap_true then icon_delete_row ).

* Speichern-Button hinzufügen
      append value #( butn_type = 5
                      text      = lv_text
                      icon      = lv_icon
                      function  = co_ucomm_save_aktiv
                      quickinfo = lv_text
                      disabled  = gv_edit_bool  ) to e_object->mt_toolbar.


* Separator hinzufügen
      append value #( butn_type = 3 ) to e_object->mt_toolbar.

* SE16n
      append value #( butn_type = 0
                      text      = co_se16n_toolbar_text
                      icon      = icon_table_settings
                      function  = co_ucomm_se16n
                      quickinfo = text-i06
                      disabled  = gv_edit_bool  ) to e_object->mt_toolbar.

    elseif p_admin = abap_true.
* Separator hinzufügen
      append value #( butn_type = 3 ) to e_object->mt_toolbar.

* Prüfen-Button hinzufügen
      append value #( butn_type = 5
                      text      = cond #( when gv_edit_bool = abap_true then co_pruefen_toolbar_text else co_edit_toolbar_text )
                      icon      = icon_change_text
                      function  = co_ucomm_edit
                      quickinfo = text-i07
                      disabled  = abap_false ) to e_object->mt_toolbar.

* Delete-Button hinzufügen
      append value #( butn_type = 5
                      text      = co_delete_toolbar_text
                      icon      = icon_delete_row
                      function  = co_ucomm_delete
                      quickinfo = text-i09
                      disabled  = gv_edit_bool  ) to e_object->mt_toolbar.

* Speichern-Button hinzufügen
      append value #( butn_type = 5
                      text      = co_save_toolbar_text
                      icon      = icon_system_save
                      function  = co_ucomm_save_all
                      quickinfo = text-i08
                      disabled  = gv_edit_bool  ) to e_object->mt_toolbar.

* Separator hinzufügen
      append value #( butn_type = 3 ) to e_object->mt_toolbar.

* SE16n
      append value #( butn_type = 5
                      text      = co_se16n_toolbar_text
                      icon      = icon_table_settings
                      function  = co_ucomm_se16n
                      quickinfo = text-i06
                      disabled  = gv_edit_bool  ) to e_object->mt_toolbar.

    endif.

    loop at e_object->mt_toolbar assigning field-symbol(<fs_button>) where ( function = cl_gui_alv_grid=>mc_fc_refresh ).
      <fs_button>-function = co_ucomm_refresh.
    endloop.

    loop at e_object->mt_toolbar assigning <fs_button> where  ( function = cl_gui_alv_grid=>mc_fc_loc_append_row ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_insert_row ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_copy_row   ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_copy       ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_delete_row ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_move_row   ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_cut        ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_undo       ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_paste      ) or
                                                              ( function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
      <fs_button>-disabled = cond #( when gv_edit_bool = abap_true then abap_false else abap_true ).
    endloop.

  endmethod.
  method on_user_command. "===> BEGIN OF USER_COMMEND
    case e_ucomm.
*-----------------------------------*
*      REFRESH
*-----------------------------------*
      when co_ucomm_refresh.
        perform clear_all_counter.
        perform check_all using abap_true. " conversion false
*        sender->refresh_table_display( ).
*-----------------------------------*
*      DELETE
*-----------------------------------*
      when co_ucomm_delete.
        data: it_row_no type lvc_t_roid.
        sender->get_selected_rows( importing et_row_no = it_row_no ).
        if lines( it_row_no ) > 0.
          sort: it_row_no by row_id.
          loop at it_row_no assigning field-symbol(<fs_row>).
            assign component co_light of structure <lt_data>[ <fs_row>-row_id ] to field-symbol(<l_light>).

            if <l_light> = co_light_succ or <l_light> = co_light_warn.
              assign component co_crud of structure <lt_data>[ <fs_row>-row_id ] to field-symbol(<l_crud>).
              case <l_crud>.
                when co_status_update.
                  <l_crud> = co_status_delete.
                when co_status_delete.
                  <l_crud> = co_status_update.
                when others.
                  message text-w02 type 'S' display like 'W'.
              endcase.
            else.
              message text-w01 type 'S' display like 'W'.
            endif.
          endloop.

          perform clear_all_counter.
*          sender->refresh_table_display( ).
        endif.
*-----------------------------------*
*      SAVE_ALL
*-----------------------------------*
      when co_ucomm_save_all.
        sender->get_selected_rows( importing et_row_no = it_row_no ).
        if lines( it_row_no ) > 0.
          sort: it_row_no by row_id.
          loop at it_row_no assigning <fs_row>.
            assign component co_light of structure <lt_data>[ <fs_row>-row_id ] to <l_light>.
            if <l_light> = co_light_succ or <l_light> = co_light_warn.
              assign component co_status of structure <lt_data>[ <fs_row>-row_id ] to field-symbol(<l_status>).
              <l_status> = 'selected'.
            else.
              message text-w01 type 'S' display like 'W'.
            endif.
          endloop.

          gv_save_clicked  = abap_true.

          perform clear_all_counter.
          perform insert_update_delete using abap_true. " admin
          perform check_all            using abap_false.

*          sender->refresh_table_display( ).
        else.
          message 'Bitte wählen Sie die zu bearbeitenden Zeilen aus.' type 'I'.
        endif.
*-----------------------------------*
*      SAVE_AKTIV
*-----------------------------------*
      when co_ucomm_save_aktiv.
        gv_save_clicked = abap_true.
        perform clear_all_counter.
        perform insert_update_delete using abap_false. " nicht admin
        perform check_all            using abap_false.
*-----------------------------------*
*      EDIT
*-----------------------------------*
      when co_ucomm_edit.
        data: it_fcat type lvc_t_fcat.
*       Feldkatalog holen
        sender->get_frontend_fieldcatalog( importing et_fieldcatalog = it_fcat ).
        sender->get_frontend_layout( importing es_layout =  data(ls_layout)  ).

        case gv_edit_bool.
          when abap_true.
            gv_edit_bool         = abap_false.
            ls_layout-grid_title = text-h01.
            perform clear_all_counter.
            perform check_all using abap_true. " do not conversion exit routine
          when others.
            field-symbols <color_fields> type lvc_t_scol.
            gv_edit_bool         = abap_true.
            ls_layout-grid_title = text-h02.

            loop at <lt_data> assigning <ls_data>.
              perform check_itab_dublicate_row.       "dublicate rows
              perform set_lvc_t_styl using <ls_data>. "style_table

              assign component co_light of structure <ls_data> to <lv_light>.
              assign component co_crud  of structure <ls_data> to <lv_crud_stat>.
              <lv_light>     = cond #( when <lv_light> <> co_light_succ and <lv_light> <> co_light_warn  then co_light_erro  else <lv_light> ).
              <lv_crud_stat> = cond #( when <lv_light> eq co_light_erro then co_status_error else <lv_crud_stat> ).
            endloop.
        endcase.

*       Feldkatalog zurückgeben
        sender->set_table_for_first_display( exporting is_layout       = ls_layout
                                              changing it_fieldcatalog = it_fcat
                                                       it_outtab       = <lt_data> ).
*        sender->refresh_table_display( ).
*-----------------------------------*
*      HIDE/SHOW
*-----------------------------------*
      when co_ucomm_hide or co_ucomm_show.
        data:
          it_fields     type lvc_t_fcat,
          col_pos       type i,
          lv_clause(45).

        go_alv->get_frontend_fieldcatalog( importing et_fieldcatalog = it_fields[] ).

        loop at it_fields assigning field-symbol(<fields>)
          where domname ne co_mandt and fieldname <> co_status and fieldname <> co_crud.
          col_pos = sy-tabix.
          <fields>-col_pos = col_pos.

          case e_ucomm.
            when co_ucomm_hide. "hide select options
              if lines( <lt_data> ) > 0.
                clear m_show_empty.
                lv_clause = |{ <fields>-fieldname } IS NOT INITIAL|.
                loop at <lt_data> assigning <ls_data>  where (lv_clause).
                  exit.
                endloop.
                if sy-subrc ne 0.
                  <fields>-no_out = abap_true.
                endif.
              endif.
            when co_ucomm_show.
              m_show_empty = abap_true.
              <fields>-no_out = abap_false.
          endcase.
        endloop.

        loop at it_fields assigning <fields>
          where fieldname = co_crud or fieldname = co_status.
          <fields>-col_pos = col_pos + 1.
           col_pos += 1.
        endloop.

        call method go_alv->set_frontend_fieldcatalog exporting it_fieldcatalog = it_fields[].

        data l_stable type lvc_s_stbl.
        l_stable = 'XX'.
        go_alv->set_frontend_layout( gs_layout ) .
*        go_alv->refresh_table_display( exporting is_stable = l_stable i_soft_refresh = abap_true  ).
*-----------------------------------*
*      SE16N
*-----------------------------------*
      when co_ucomm_se16n.
        data: lt_output_fields type standard table of se16n_output with default key.
        data: ls_selfields type se16n_seltab.
*        data: lt_selfields type table of se16n_seltab.
        data: lt_selfields type se16n_or_seltab_t.
        data: ls_or_seltab type se16n_or_seltab.
        data: lt_or_seltab type se16n_or_t.

        sender->get_selected_rows( importing et_row_no = it_row_no ).

        if lines( it_row_no ) > 0.
          sort: it_row_no by row_id.

          loop at it_row_no assigning <fs_row>.
            try.
                assign component co_light of structure <lt_data>[ <fs_row>-row_id ] to <l_light>.

                if <l_light> = co_light_succ or <l_light> = co_light_warn.
                  loop at gt_key_fields assigning field-symbol(<fs_key>) where fieldname <> co_mandt .
                    assign component <fs_key>-fieldname of structure <lt_data>[ <fs_row>-row_id ]
                      to field-symbol(<l_key_value>). " assign

                    lt_selfields = value se16n_or_seltab_t(
                      base lt_selfields ( field = <fs_key>-fieldname  sign = 'I'  option = 'EQ'  low = <l_key_value> ) ).
                  endloop.

                  append lines of lt_selfields to ls_or_seltab-seltab.
                  append ls_or_seltab to lt_or_seltab.
                  refresh lt_selfields.
                else.
                  message text-w01 type 'S' display like 'W'.
                endif.
              catch cx_sy_itab_line_not_found.
            endtry.
          endloop.
        else.
          message 'Bitte wählen Sie die entsprechenden Zeilen aus.' type 'I'.
          exit.
        endif.

        data lt_fcat type lvc_t_fcat.
        go_alv->get_frontend_fieldcatalog( importing et_fieldcatalog = lt_fcat[] ).

        loop at lt_fcat into data(ls_fcat)
          where fieldname <> co_light
            and fieldname <> co_status
            and fieldname <> co_crud
            and fieldname <> co_mandt
            and no_out = abap_false.

          append ls_fcat-fieldname  to lt_output_fields.
        endloop.

        export lt_output_fields from lt_output_fields to memory id 'ITAB_OUT'.
        export lt_or_seltab     from lt_or_seltab     to memory id 'ITAB_SEL'.
        submit zma_se16n with p_tname = p_tname and return.
*-----------------------------------*
*      TOOLBAR
*-----------------------------------*
      when co_ucomm_toolbar.
        show_toolbar = bit-not show_toolbar.
      when others.
    endcase.

    sender->refresh_table_display(
        is_stable      = value #( row = abap_true col = abap_true )
        i_soft_refresh = abap_true ).
  endmethod. "END OF USER_COMMEND <===

  method on_menu_bt_handle.
    if e_ucomm = co_ucomm_menu.
      call method e_object->add_function
        exporting
          fcode = co_ucomm_hide
          text  = 'Leere Spalten ausblenden'.
      call method e_object->add_function
        exporting
          fcode = co_ucomm_show
          text  = 'Leere Spalten anzeigen'.
    endif.
  endmethod.

  method on_top_of_page.
    perform top_of_page_split.
    call method go_docu->merge_document.
    call method go_docu->display_document
      exporting
        reuse_control      = 'X'
        reuse_registration = 'X'
        parent             = go_container_top.
  endmethod.

  method on_f4help.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = co_crud
        value_org       = 'S'
      tables
        value_tab       = gt_crud_status
        field_tab       = lt_field_tab
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  endmethod.

  method on_data_changed.
    field-symbols <fls_data> type any.

    loop at er_data_changed->mt_mod_cells  assigning field-symbol(<c>)
    where fieldname eq 'GROUP'
    and error     ne space.

      if <c> is assigned.
        clear <c>-error.
        modify er_data_changed->mt_mod_cells
        from <c> transporting error.

        append <c> to er_data_changed->mt_good_cells.

        delete er_data_changed->mt_protocol
        where fieldname eq 'GROUP'
        and row_id    eq <c>-row_id.

        read table <lt_data> transporting no fields index <c>-row_id.

        if sy-subrc <> 0.
          field-symbols <fs_new_line> type any.
          data d_line type ref to data.
          create data d_line like line of <lt_data>.
          assign d_line->* to <fs_new_line>.

          insert <fs_new_line> into <lt_data> index  <c>-row_id.
        endif.

        <fls_data> = <lt_data>[ <c>-row_id ].

        perform set_lvc_t_styl using <fls_data>.

        " Zeile x aus der iTab it_mara rausholen und daraus die Zelle anhand des Spaltennamens (Feldnamens) holen
        assign component <c>-fieldname of structure <fls_data> to field-symbol(<f>).
        if <f> is assigned.
          " Änderungswert in die Zelle der iTab rückschreiben
          <f> = <c>-value.
        endif.
      endif.
    endloop.
  endmethod.

endclass.

class lcl_domvalues implementation.

  method get_by_type.
    try.
        rt_ddfixvalues = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( iv_data ) )->get_ddic_fixed_values( ).

      catch cx_root into data(lx_err_dom).
        lv_msg_err = lx_err_dom->get_text( ).

        perform add_new_exception
        using
              lv_msg_err
              'LCL_DOMVALUES: GET_BY_TYPE'
              .
    endtry.
  endmethod.

  method get_by_name.
    try.
        data: ls_domvalues type ty_domvalues.

        read table gt_domvalues into ls_domvalues with key rollname = iv_name.

        if sy-subrc = 0.
          rt_ddfixvalues = ls_domvalues-ddfixvalue.
        else.
          rt_ddfixvalues = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( iv_name ) )->get_ddic_fixed_values( ).

          ls_domvalues-rollname   = iv_name.
          ls_domvalues-ddfixvalue = rt_ddfixvalues.
          append ls_domvalues to gt_domvalues.
        endif.


      catch cx_root into data(lx_err_dom).
        lv_msg_err = lx_err_dom->get_text( ).

        perform add_new_exception
        using
              lv_msg_err
              'LCL_DOMVALUES: GET_BY_TYPE'
              .
    endtry.
  endmethod.
endclass.
