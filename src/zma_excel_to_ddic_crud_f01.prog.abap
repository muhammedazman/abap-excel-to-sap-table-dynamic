*&---------------------------------------------------------------------*
*& Include          ZMA_EXCEL_TO_DDIC_CRUD_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form generate_to_datenelements
*&---------------------------------------------------------------------*
form generate_to_datenelements .
  try.

*--------------------------------------------------------------------*
* Data Definition
*--------------------------------------------------------------------*
      data: lv_rollname   type dd03l-rollname,
            i_cnt         type i,
            i_max_col_num type i,
            i_max_row     type i.


*--------------------------------------------------------------------*
* LT_COMP : Excel first row = Header
*--------------------------------------------------------------------*
      perform set_lt_comp changing lv_rollname i_cnt i_max_col_num.

*--------------------------------------------------------------------*
* SET gt_excel zusätliche Spalte = | LIGHT | STATUS | CRUD |
*--------------------------------------------------------------------*
      perform set_ext_columns using i_cnt i_max_col_num
                              changing ls_comp lt_comp.

*--------------------------------------------------------------------*
* CREATE LS_DATA LT_DATA
*--------------------------------------------------------------------*
*      perform create_dynamic_table.
*
      lr_struct_dscr = cl_abap_structdescr=>create( lt_comp ).
      create data lr_struct_data type handle lr_struct_dscr.
      assign lr_struct_data->* to <ls_data>.

      lr_table_dscr = cl_abap_tabledescr=>create( lr_struct_dscr ).
      create data lr_table_data type handle lr_table_dscr.
      assign lr_table_data->* to <lt_data>.

*--------------------------------------------------------------------*
* SET <ls_data> <lt_data>
*--------------------------------------------------------------------*
      perform set_lt_data using i_max_col_num.

*--------------------------------------------------------------------*
* SET gt_comp_details
*--------------------------------------------------------------------*
      perform set_to_comp_details.


    catch cx_root into data(lx_err).
      lv_msg_err =  lx_err->get_text( ).

      perform add_new_exception using lv_msg_err 'PERFORM GENERATE_TO_DATENELEMENTS'.
  endtry.
endform.



*&---------------------------------------------------------------------*
*& Form set_lt_comp
*&---------------------------------------------------------------------*
form set_lt_comp  changing lv_rollname    type rollname
                           lv_tabix       type i
                           lv_max_col_num type i.

  data lv_data_type type datatype_d.
  data lv_fieldname type fieldname.
  data lv_colpos    type i value 1.

  read table gt_table_field into data(ls_curr) with key datatype = 'CUKY'.

  sort gt_excel by row col ascending.
  loop at gt_excel assigning field-symbol(<ls_excel>) where row = '1' .
    lv_tabix = sy-tabix.

    " von Fieldname
    select  single rollname datatype fieldname into
                  ( lv_rollname, lv_data_type, lv_fieldname )
    from dd03l
    where tabname   = p_tname          and
          fieldname = <ls_excel>-value and
          comptype  = 'E'              and
          as4local  = 'A'.

    if sy-subrc <> 0.
      continue.
    endif.

    cl_abap_datadescr=>describe_by_name( exporting p_name           = lv_rollname
                                         receiving p_descr_ref      = data(lv_descr_ref)
                                         exceptions type_not_found  = 1
                                                    others          = 2 ).

    if sy-subrc = 0.
      read table gt_table_field assigning field-symbol(<ls_itab>) with key rollname = lv_rollname.

      if sy-subrc = 0.
        lv_max_col_num = lv_max_col_num + 1.

        ls_comp-type ?= lv_descr_ref.
        ls_comp-name = lv_fieldname.

        append ls_comp to lt_comp.

*        lv_colpos = lv_colpos + 1.

*        lcl_alv=>set_fcat_with_ddic(
*          exporting
*            is_table_field = <ls_itab>
*            iv_colpos      = lv_colpos
*        ).
      else.
        lv_msg_err =  |Datenelement { <ls_excel>-value } konnte in der Tabelle { p_tname } gefunden werden. |.
        perform add_new_exception using lv_msg_err <ls_excel>-value.
      endif.

    elseif sy-subrc = 1 .
      lv_msg_err =  'kein Typ gefunden.'.

      perform add_new_exception using lv_msg_err <ls_excel>-value.
    else.
      lv_msg_err =  'unbekannter Fehler.'.

      perform add_new_exception using lv_msg_err <ls_excel>-value.
    endif.
  endloop.
endform.


*&---------------------------------------------------------------------*
*& Form set_ext_columns
*&---------------------------------------------------------------------*
form set_ext_columns  using    lv_cnt          type  i
                               lv_max_col_num  type  i
                      changing ls_comp         type  cl_abap_structdescr=>component
                               lt_comp         type  cl_abap_structdescr=>component_table.


  field-symbols <ls_excel> type alsmex_tabline.

  data: lv_previous_row type i,
        lv_actuall_row  type i,
        lv_previous_col type i,
        lv_actuall_col  type i,
        lv_counter_col  type i.

  loop at gt_excel assigning <ls_excel>.
    lv_actuall_row = <ls_excel>-row.
    lv_actuall_col = <ls_excel>-col.

    if lv_previous_row is initial.
      lv_previous_row = lv_actuall_row.
    endif.

    if lv_previous_row ne lv_actuall_row.
      gt_index_row_col = value #( base gt_index_row_col
                                                ( row   = lv_previous_row
                                                  col   = lv_previous_col + gv_col_light_idx
                                                  value = cond #( when lv_previous_row = '1' then co_light ) )

                                                ( row   = lv_previous_row
                                                  col   = lv_previous_col + gv_col_status_idx
                                                  value = cond #( when lv_previous_row = '1' then co_status ) )

                                                ( row   = lv_previous_row
                                                  col   = lv_previous_col + gv_col_crud_idx
                                                  value = cond #( when lv_previous_row = '1' then co_crud ) ) ).
      lv_previous_row = <ls_excel>-row.

      if lv_counter_col < lv_previous_col.
        lv_counter_col = lv_previous_col.
      endif.
    endif.

    lv_previous_col = lv_actuall_col.
  endloop.

* add to extra colums in gt_excel
  append lines of gt_index_row_col to gt_excel.

  sort gt_excel.

  perform add_new_column using co_datatype_char01     co_light  changing ls_comp lt_comp.
  perform add_new_column using co_datatype_char0256   co_status changing ls_comp lt_comp.
  perform add_new_column using co_datatype_char01     co_crud   changing ls_comp lt_comp.
  perform add_new_column using co_datatype_lvc_t_styl co_cell   changing ls_comp lt_comp.
  perform add_new_column using co_datatype_lvc_t_scol co_tabcol changing ls_comp lt_comp.

*  Gesamtzahl der Spalten
  gv_col_num = lv_max_col_num + co_cnt_ext_feld.

endform.



*&---------------------------------------------------------------------*
*& Form set_lt_data
*&---------------------------------------------------------------------*
form set_lt_data using lv_max_col_num   type i.


  if gt_error_messages is initial.
    data: lv_previous_row type i,
          lv_actuell_row  type i.

    loop at gt_excel assigning field-symbol(<ls_excel>) where row > '1'.
      try.
          lv_actuell_row = <ls_excel>-row.

          if lv_actuell_row ne lv_previous_row.
            append initial line to <lt_data> assigning <ls_data>.
            lv_previous_row = lv_actuell_row.
          endif.

          assign gt_excel[ row = '1' col = <ls_excel>-col ] to field-symbol(<lfs_excel>).
          assign gt_table_field[ fieldname = <lfs_excel>-value ] to <ls_table_field>.
          assign component <ls_table_field>-fieldname of structure <ls_data> to field-symbol(<lv_col_data>).

          if <ls_excel>-col le lv_max_col_num.
            try.
                if <ls_excel>-value is not initial.
                  perform conversion_exit_routine using <ls_table_field> changing <ls_excel> <ls_data> <lv_col_data>.

                  if <ls_table_field>-datatype ns 'RAW'.
                    <lv_col_data> = <ls_excel>-value.
                  endif.
                endif.
              catch cx_root into data(lx_value).
                lv_msg_err = |VALUE:{ <ls_excel>-value } Row:{ <ls_excel>-row } Col:{ <ls_excel>-col }|.
                perform add_new_exception using lv_msg_err  'SET_LT_DATA'.
            endtry.
          endif.

          gv_row_count = gv_row_count + 1.
        catch cx_root into data(lx_conv).
          lv_msg_err = lx_conv->get_text( ).

          lv_msg_err = |VALUE:{ <ls_excel>-value } Row:{ <ls_excel>-row } Col:{ <ls_excel>-col }|.
          perform add_new_exception using lv_msg_err  'CONVERTION_EXIT_ROUTINE'.
      endtry.
    endloop.
  endif.
endform.

*&---------------------------------------------------------------------*
*& Form convention_exit_roution
*&---------------------------------------------------------------------*
form add_new_column using lv_name      type rollname
                          lv_col_name  type fieldname
                    changing ls_comp type  cl_abap_structdescr=>component
                             lt_comp type  cl_abap_structdescr=>component_table.

  data: lr_data type ref to data.
  create data lr_data type (lv_name).
  ls_comp-type ?= cl_abap_datadescr=>describe_by_data_ref( lr_data ).
  ls_comp-name = lv_col_name.
  append ls_comp to lt_comp.
endform.


*&---------------------------------------------------------------------*
*& Form convension_exit_roution
*&---------------------------------------------------------------------*
form conversion_exit_routine
  using ls_table_field type dfies
        changing ls_excel type alsmex_tabline
                 ls_data  type any
                 lv_col_data type any.


  assign component co_status of structure ls_data to field-symbol(<lv_status>).
  assign component co_light of structure ls_data to field-symbol(<lv_light>).


  if <ls_table_field>-datatype eq 'TIMS'.
    replace all occurrences of ':' in ls_excel-value with ''.

  elseif <ls_table_field>-datatype cs 'INT'.
    replace all occurrences of '.' in ls_excel-value with ''.

  elseif <ls_table_field>-datatype eq 'DATS' or <ls_table_field>-domname eq 'TZNTSTMPS'.

    try.
        if <ls_table_field>-domname eq 'TZNTSTMPS'.
          data: lv_utc type timestamp.
          data: lv_date type d.
          data: lv_time type t.

          split ls_excel-value at space into data(v1) data(v2).

          condense v1.

          cl_abap_datfm=>conv_date_ext_to_int( exporting im_datext  =  v1
                                               importing ex_datint  = lv_date ).
          condense v2.
          replace all occurrences of ':' in v2 with ''.
          lv_time = v2.

          cl_abap_tstmp=>systemtstmp_syst2utc( exporting
                                                       syst_date = lv_date
                                                       syst_time = lv_time
                                                     importing
                                                       utc_tstmp = lv_utc ).

          ls_excel-value = lv_utc.
        else.

          cl_abap_datfm=>conv_date_ext_to_int( exporting im_datext  =  ls_excel-value
                                               importing ex_datint  =  data(lv_datint) ).

          ls_excel-value = lv_datint.

          call method cl_isu_date_check=>date_check_plausibility
            exporting
              x_date                    = conv #( ls_excel-value )
            exceptions
              plausibility_check_failed = 1
              others                    = 2.

          if sy-subrc <> 0.
            perform set_status
             using
              'Datum ist nicht plausibel. '
               co_light_erro
               <ls_table_field>-fieldname
             changing
               ls_data
             .
          endif.
        endif.

      catch cx_abap_datfm_no_date into data(lx_no_date).
        perform set_status
          using
            'Kein Datum. '
            co_light_erro
            <ls_table_field>-fieldname
          changing
            ls_data
          .

      catch cx_abap_datfm_invalid_date into data(lx_invalid_date).
        perform set_status
          using
            'Ungültiges Datumsformat in Excel. '
            co_light_warn
            <ls_table_field>-fieldname
          changing
            ls_data
          .

      catch cx_abap_datfm_format_unknown into data(lx_format_unknown).
        perform set_status
         using
           'Datumsformat unbekannt. '
           co_light_erro
           <ls_table_field>-fieldname
         changing
           ls_data
         .
      catch cx_abap_datfm_ambiguous into data(ambiguous).
        perform set_status
          using
           'zweideutiges Datum. '
            co_light_erro
            <ls_table_field>-fieldname
          changing
            ls_data
          .
    endtry.



  elseif <ls_table_field>-convexit is not initial.
    perform conversion_alpha_and_another using <ls_table_field> changing ls_excel-value.

  elseif <ls_table_field>-datatype = 'QUAN' or <ls_table_field>-datatype = 'CURR'.
    data lv_cuky_col_idx type i.
    data lv_cuky type char50.

    read table gt_table_field into data(ls_curr_field)  with key datatype = 'CUKY'.
    if sy-subrc = 0.
      lv_cuky_col_idx = gt_excel[ row = '1' value = ls_curr_field-fieldname ]-col.
      lv_cuky = gt_excel[ row = ls_excel-row  col = lv_cuky_col_idx ]-value.
    endif.

    perform conversion_quan_and_curr using <ls_table_field> lv_cuky changing ls_excel-value.

  elseif <ls_table_field>-datatype cs 'DEC'.
    replace all occurrences of ',' in ls_excel-value with '.'.
  elseif <ls_table_field>-datatype cs 'RAW'.
    perform conversion_raw_data using <ls_table_field> ls_excel-value changing lv_col_data.
*  else.
*    perform conversion_dynamic using <ls_table_field> changing ls_excel-value.
  endif.

endform.


*&---------------------------------------------------------------------*
*& Add exception
*&---------------------------------------------------------------------*
form add_new_exception using l_msg   type char50
      l_value type any.
  append value ty_message( message = l_msg value = l_value ) to gt_error_messages.
endform.


*&---------------------------------------------------------------------*
*& Form display_error_messages
*&---------------------------------------------------------------------*
form display_error_messages .
  write: 'VALUE', 66 'FEHLER'.
  uline.
  loop at gt_error_messages into data(ls_error_message).
    write: / |{ ls_error_message-value width = abap_max_class_comp_name_ln } \|  { ls_error_message-message width = 50 }|.
  endloop.
endform.

form conversion_exit_routine_lsdata using ls_table_field type dfies
                                 changing lv_field_data  type any.

  if ls_table_field-datatype eq 'DATS'  or ls_table_field-domname eq 'TZNTSTMPS'.

    try.
        if ls_table_field-domname eq 'TZNTSTMPS'.
          data lv_utc type timestamp.
          data lv_date type d.
          data lv_time type t.

          cl_abap_tstmp=>systemtstmp_utc2syst(  exporting  utc_tstmp = lv_field_data
                                                importing  syst_date = lv_date    " System Date
                                                           syst_time = lv_time    " System Time
                                                ).

          cl_abap_tstmp=>systemtstmp_syst2utc( exporting syst_date = lv_date
                                                         syst_time = lv_time
                                               importing utc_tstmp = lv_utc
                                               ).

          lv_field_data = lv_utc.
        else.
          if strlen( lv_field_data ) = 8.                   "20191231

            data lv_datext type string.
            cl_abap_datfm=>conv_date_int_to_ext( exporting im_datint = lv_field_data
                                                 importing ex_datext = lv_datext
                                                   ).

            cl_abap_datfm=>conv_date_ext_to_int( exporting im_datext = lv_datext
                                                 importing ex_datint = data(lv_datint) ).

            lv_field_data = lv_datint.

          elseif strlen( lv_field_data ) = 10.  "31.12.2019
            cl_abap_datfm=>conv_date_ext_to_int( exporting im_datext = lv_field_data
                                                 importing ex_datint = lv_datint ).

            lv_field_data = lv_datint.
          else.
            raise exception type cx_abap_datfm_format_unknown.
          endif.

          call method cl_isu_date_check=>date_check_plausibility
            exporting
              x_date                    = conv #( lv_field_data )
            exceptions
              plausibility_check_failed = 1
              others                    = 2.

          if sy-subrc <> 0.
            lv_msg_row = |{ ls_table_field-scrtext_m }: Datum ist nicht plausibel. |.
            perform set_status using lv_msg_row co_light_erro ls_table_field-fieldname
            changing <ls_data>.
          endif.
        endif.

      catch cx_abap_datfm_no_date into data(lx_no_date).
        lv_msg_row = |{ ls_table_field-scrtext_m }: Kein Datum. |.
        perform set_status using lv_msg_row co_light_erro ls_table_field-fieldname
        changing <ls_data>.

      catch cx_abap_datfm_invalid_date into data(lx_invalid_date).
        lv_msg_row = |{ ls_table_field-scrtext_m }: Ungültigesformat in Excel. |.
        perform set_status using lv_msg_row co_light_warn ls_table_field-fieldname
        changing <ls_data>.

      catch cx_abap_datfm_format_unknown into data(lx_format_unknown).
        lv_msg_row = |{ ls_table_field-scrtext_m }: Datumsformat unbekannt. |.
        perform set_status using lv_msg_row co_light_erro ls_table_field-fieldname
        changing <ls_data>.

      catch cx_abap_datfm_ambiguous into data(ambiguous).
        lv_msg_row = |{ ls_table_field-scrtext_m }: zweideutiges Datum. |.
        perform set_status using lv_msg_row co_light_erro ls_table_field-fieldname
        changing <ls_data>.
    endtry.

  elseif ls_table_field-convexit is not initial.
    perform conversion_alpha_and_another using ls_table_field
    changing lv_field_data.

  elseif gv_initialized = abap_false and ( ls_table_field-datatype = 'QUAN' or ls_table_field-datatype = 'CURR' ).
    data lv_cuky type char50.

    if ls_table_field-datatype = 'CURR'.
      read table gt_table_field into data(ls_curr_field) with key datatype = 'CUKY'.

      if sy-subrc = 0.
        assign component ls_curr_field-fieldname of structure <ls_data> to field-symbol(<lv_cuky>).
        lv_cuky = <lv_cuky>.
      endif.
    endif.

    perform conversion_quan_and_curr using ls_table_field lv_cuky
                                     changing lv_field_data.

  elseif <ls_table_field>-datatype cs 'RAW'.
    perform conversion_raw_data using ls_table_field ''
                                changing lv_field_data.
*  else.
*    perform conversion_dynamic using ls_table_field
*    changing lv_field_data.
  endif.
endform.

*&---------------------------------------------------------------------*
*& Form get_ddic_field_list
*&---------------------------------------------------------------------*
form get_ddic_field_list
using lv_tname  type tabname
changing lt_return_value  type standard table.


  try.
      data: lo_desc type ref to cl_abap_structdescr.
      lo_desc ?= cl_abap_structdescr=>describe_by_name( p_tname ).
      lt_return_value = lo_desc->get_ddic_field_list( ).

    catch cx_root into data(lx_err).
      lv_msg_err = lx_err->get_text( ).

      perform add_new_exception
      using
            lv_msg_err
            'DDIF_FIELDINFO_GET'
            .
  endtry.
endform.
*&---------------------------------------------------------------------*
*& Form set_gui_splitter_container
*&---------------------------------------------------------------------*
form set_gui_splitter_container  using    lv_row    type i
      lv_column type i
changing lo_splitter            type ref to cl_gui_splitter_container
  lo_container_top       type ref to cl_gui_container
  lo_container_bottom    type ref to cl_gui_container
  lo_alv                 type ref to cl_gui_alv_grid.

  lo_splitter = new cl_gui_splitter_container( parent                     = cl_gui_container=>screen0
  no_autodef_progid_dynnr    = abap_true   " wichtig
  rows                       = lv_row
  columns                    = lv_column ).

  lo_container_top    =  go_splitter->get_container( row = 1 column = 1 ).
  lo_container_bottom =  go_splitter->get_container( row = 2 column = 1 ).

  lo_alv  =  new cl_gui_alv_grid( i_parent = go_container_bottom ).

  lo_splitter->set_row_height( id = 1 height = 25 ).

* Top of Page event
  create object go_docu
    exporting
      style = 'ALV_GRID'.

  lo_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

* ALV-Grid selektionsbereit setzen
  lo_alv->set_ready_for_input( i_ready_for_input = 1 ).
endform.


*&---------------------------------------------------------------------*
*& Form add_extra_fcat
*&---------------------------------------------------------------------*
form add_extra_fcat .
  data ls_fcat type lvc_s_fcat.
  data lv_cols_num type i.

  data(lt_fcat) = gt_fcat.
  sort lt_fcat descending by col_pos.

  read table lt_fcat into ls_fcat index 1.
  lv_cols_num = ls_fcat-col_pos + 1.

  clear ls_fcat.
  ls_fcat = value lvc_s_fcat(
                     fieldname = co_light
                     rollname  = co_datatype_char01
                     emphasize = 'C401'
                     coltext   = co_coltext_light
                     col_pos   = 1
                     ).
  lcl_alv=>set_fcat_custom( is_fieldcatalog = ls_fcat ).

  clear ls_fcat.
  ls_fcat = value lvc_s_fcat(
                     fieldname = co_crud
                     rollname  = co_datatype_char01
                     emphasize = 'C110'
                     coltext   = co_coltext_crud
                     col_pos   = lv_cols_num + 1
                     just      = 'C' " centered
                     outputlen = 1
                     f4availabl = abap_true
                     ).
  lcl_alv=>set_fcat_custom( is_fieldcatalog = ls_fcat ).

  clear ls_fcat.
  ls_fcat = value lvc_s_fcat(
                     fieldname = co_status
                     rollname  = co_datatype_char0256
                     emphasize = 'C401'
                     coltext   = co_coltext_status
                     col_pos   = lv_cols_num + 2
                     outputlen = 256
                     ).
  lcl_alv=>set_fcat_custom( is_fieldcatalog = ls_fcat ).

*  clear ls_fcat.
*  ls_fcat = value lvc_s_fcat(
*                     fieldname = 'CELL_STYLE'
*                     ref_field = 'CELSTAT'
*                     ref_table = 'BCSS_IBOT'
*                     no_out    = 'X'
*                     no_init_ch = space
*                     col_pos   = gv_col_num + 3
*                     ).
*  lcl_alv=>set_fcat_custom( is_fieldcatalog = ls_fcat ).
endform.

*&---------------------------------------------------------------------*
*& Form create_dynamic_table
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
form create_dynamic_table.
  lcl_alv=>set_fcat( ).

*Calling the method to create Dynamic Internal Table
  call method cl_alv_table_create=>create_dynamic_table
    exporting
      it_fieldcatalog = gt_fcat
    importing
      ep_table        = data(d_ref).

  assign d_ref->* to <lt_data>.

  data: dy_line type ref to data.
  create data dy_line like line of <lt_data>.
  assign dy_line->* to <ls_data>.


endform.

*&---------------------------------------------------------------------*
*& Form set_to_comp_details
*&---------------------------------------------------------------------*
form set_to_comp_details .
  data : ref_table_des type ref to cl_abap_structdescr.

  ref_table_des ?= cl_abap_typedescr=>describe_by_data( <ls_data> ).

  gt_comp_details = ref_table_des->components[]. " filling to global data for check all

endform.

*&---------------------------------------------------------------------*
*& Form conversion_dynamic
*&---------------------------------------------------------------------*
form conversion_dynamic  using    ls_table_field type dfies
changing lv_value type any.

  data: xdata type ref to data.
  create data xdata type (ls_table_field-rollname).
  assign xdata->* to field-symbol(<fs_xdata>).
  <fs_xdata> = lv_value.
  lv_value = |{ <fs_xdata> width = ls_table_field-leng alpha = in }|.
endform.

*&---------------------------------------------------------------------*
*& Form conversion_alpha_and_another
*&---------------------------------------------------------------------*
form conversion_alpha_and_another  using    ls_table_field type dfies
                                   changing lv_field_data type any.

  case ls_table_field-convexit.
    when 'ALPHA'.
      lv_field_data = |{ lv_field_data width = ls_table_field-leng alpha = in }|.
    when others.
      if gv_initialized = abap_false.
        data(lv_fm_convname) = |CONVERSION_EXIT_{ ls_table_field-convexit }_INPUT|.

        call function lv_fm_convname
          exporting
            input         = lv_field_data
          importing
            output        = lv_field_data
          exceptions
            error_message = 1
            others        = 2.

        if sy-subrc <> 0.
          call function lv_fm_convname
            exporting
              input         = lv_field_data
              language      = 'E'
            importing
              output        = lv_field_data
            exceptions
              error_message = 1
              others        = 2.
        endif.
      endif.
  endcase.
endform.

*&---------------------------------------------------------------------*
*& Form conversion_quan
*&---------------------------------------------------------------------*
form conversion_quan_and_curr using     ls_table_field type dfies
      lv_currency    type char50
changing  lv_field_data type any.

  data lv_value_in type string.

  data: xdata type ref to data.
  create data xdata type (ls_table_field-rollname).
  assign xdata->* to field-symbol(<lv_value_out>).

  data(ls_tabfield) = value tabfield( tabname   = p_tname
        fieldname = ls_table_field-fieldname ).

  lv_value_in = lv_field_data.


  call function 'RS_CONV_EX_2_IN'
    exporting
      input_external               = conv char30( condense( val = lv_value_in ) )
      table_field                  = ls_tabfield
      currency                     = cond #( when ls_table_field-datatype = 'CURR' then lv_currency )
    importing
      output_internal              = <lv_value_out>
    exceptions
      input_not_numerical          = 1
      too_many_decimals            = 2
      more_than_one_sign           = 3
      ill_thousand_separator_dist  = 4
      too_many_digits              = 5
      sign_for_unsigned            = 6
      too_large                    = 7
      too_small                    = 8
      invalid_date_format          = 9
      invalid_date                 = 10
      invalid_time_format          = 11
      invalid_time                 = 12
      invalid_hex_digit            = 13
      unexpected_error             = 14
      invalid_fieldname            = 15
      field_and_descr_incompatible = 16
      input_too_long               = 17
      no_decimals                  = 18
      invalid_float                = 19
      conversion_exit_error        = 20
      others                       = 21.

  lv_field_data = <lv_value_out>.

endform.
*&---------------------------------------------------------------------*
*& Form conversion_raw_data
*&---------------------------------------------------------------------*
form conversion_raw_data using ls_table_field type dfies
                                lv_value      type char50
                       changing lv_result type any.

  data: xdata type ref to data.
  create data xdata type (ls_table_field-rollname).
  assign xdata->* to field-symbol(<lv_value_out>).

  field-symbols:  <l_buf> type any.

  assign <lv_value_out> to <l_buf> casting type (ls_table_field-rollname).

  if lv_value is not initial.
    move lv_value to <l_buf>.
  else.
    move lv_result to <l_buf>.
  endif.

  lv_result = <l_buf>.

endform.


form top_of_page_split .
  data: s_tab         type sdydo_text_table,
        c_middle_area type ref to cl_dd_area,
        c_right_area  type ref to cl_dd_area,
        lv_text       type sdydo_text_element.

  types: begin of tab_text,
           text type sdydo_text_element,
         end of tab_text.

  data: i_text type table of tab_text.
  data: w_text type tab_text.

  data: lv_iconname type iconname.
  data: lv_value    type i.


  go_docu->vertical_split( exporting split_area  = go_docu
                                     split_width = '50%'
                           importing right_area  = c_right_area ).

  go_docu->vertical_split( exporting split_area  = go_docu
                                     split_width = '50%'
                           importing right_area  = c_middle_area ).

*== Excel Liste
  lv_text = |Excel Liste Status (Datensätze: { gv_row_count })|.
  go_docu->add_text( exporting  text = lv_text
                        sap_fontsize = cl_dd_document=>medium
                        sap_emphasis = cl_dd_document=>strong ).
  call method go_docu->underline.


  lv_text = 'Einfügen'.
  go_docu->add_icon( exporting sap_icon     = 'ICON_INSERT_ROW'
                               sap_size     = cl_dd_document=>medium ).
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).
  go_docu->add_gap(  exporting width        = 20 ).

  lv_text = |: { gv_count_insert_excel }|.
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = 'Normal' ).
  call method go_docu->new_line.

  lv_text = 'Änderung'.
  go_docu->add_icon( exporting sap_icon     = 'ICON_CHANGE'
                               sap_size     = cl_dd_document=>medium ).
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).
  go_docu->add_gap(  exporting width        = 19 ).

  lv_text = |: { gv_count_update_excel }|.
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = 'Normal' ).
  call method go_docu->new_line.

  lv_text = 'Löschen'.
  go_docu->add_icon( exporting sap_icon     = 'ICON_DELETE'
                               sap_size     = cl_dd_document=>medium ).
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).
  go_docu->add_gap(  exporting width        = 21 ).

  lv_text = |: { gv_count_delete_excel }|.
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = 'Normal' ).
  call method go_docu->new_line.

  lv_text = 'Fehler'.
  go_docu->add_icon( exporting sap_icon     = 'ICON_MESSAGE_ERROR'
                               sap_size     = cl_dd_document=>medium ).
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).
  go_docu->add_gap(  exporting width        = 24 ).

  lv_text = |: { gv_count_error_excel }|.
  go_docu->add_text( exporting text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = 'Normal' ).

*== Die Werte von SAP Tabelle
  lv_text = |SAP Tabelle Status (Datensätze: { gv_transaktion_count })|.
  call method c_middle_area->add_text exporting
                                        text         = lv_text
                                        sap_fontsize = cl_dd_document=>medium
                                        sap_emphasis = cl_dd_document=>strong.
  call method c_middle_area->underline.

  lv_text = 'Eingefügte Zeilen'.
  c_middle_area->add_icon( exporting sap_icon     = 'ICON_INSERT_ROW'
                                     sap_size     = cl_dd_document=>medium ).
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = cl_dd_document=>strong ).
  c_middle_area->add_gap(  exporting width        = 15 ).

  lv_text = |: { gv_count_insert_sap }|.
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = 'Normal' ).
  call method c_middle_area->new_line.

  lv_text = 'Geänderte Zeilen'.
  c_middle_area->add_icon( exporting sap_icon     = 'ICON_CHANGE'
                                     sap_size     = cl_dd_document=>medium ).
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = cl_dd_document=>strong ).
  c_middle_area->add_gap(  exporting width        = 16 ).

  lv_text = |: { gv_count_update_sap }|.
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = 'Normal' ).
  call method c_middle_area->new_line.

  lv_text = 'Gelöschte Zeilen'.
  c_middle_area->add_icon( exporting sap_icon     = 'ICON_DELETE'
                                     sap_size     = cl_dd_document=>medium ).
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = cl_dd_document=>strong ).
  c_middle_area->add_gap(  exporting width        = 17 ).

  lv_text = |: { gv_count_delete_sap }|.
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = 'Normal' ).
  call method c_middle_area->new_line.

  lv_text = 'Fehler'.
  c_middle_area->add_icon( exporting sap_icon     = 'ICON_MESSAGE_ERROR'
                                     sap_size     = cl_dd_document=>medium ).
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = cl_dd_document=>strong ).
  c_middle_area->add_gap(  exporting width        = 33 ).

  lv_text = |: { gv_count_error_sap }|.
  c_middle_area->add_text( exporting text         = lv_text
                                     sap_fontsize = cl_dd_document=>medium
                                     sap_emphasis = 'Normal' ).
  call method c_middle_area->new_line.
  call method c_middle_area->new_line.
  call method c_middle_area->new_line.

*== Beschreibung
  call method c_right_area->new_line
    exporting
      repeat = 2
    .

  c_right_area->add_icon( exporting sap_icon = 'ICON_GREEN_LIGHT' ).
  c_right_area->add_text( exporting text     = 'erlaubt Transaktion' ).
  call method c_right_area->new_line.

  c_right_area->add_icon( exporting sap_icon = 'ICON_YELLOW_LIGHT' ).
  c_right_area->add_text( exporting text     = 'erlaubt Transaktion' ).
  call method c_right_area->new_line.

  c_right_area->add_icon( exporting sap_icon = 'ICON_RED_LIGHT' ).
  c_right_area->add_text( exporting text     = 'nicht erlaubt Transaktion' ).

endform. "TOP_OF_PAGE_SPLIT

*&---------------------------------------------------------------------*
*& Form screen_control
*&---------------------------------------------------------------------*
form screen_control .
  loop at screen.
    if screen-group1 = 'M1'.
      if p_aktive = 'X'.
        screen-active = 1.
        screen-invisible = 0.
      else.
        screen-invisible = 1.
        screen-active = 0.
      endif.
      modify screen.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form set_fcat_merge
*&---------------------------------------------------------------------*
form set_fcat_merge .
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = p_tname
      i_bypassing_buffer     = abap_true
    changing
      ct_fieldcat            = gt_fcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc = 0.
    data ls_table_field type dfies.

    loop at gt_fcat assigning field-symbol(<ls_fcat>).

      read table gt_excel transporting no fields with key row = 1 value = <ls_fcat>-fieldname.

      if sy-subrc <> 0.
        delete gt_fcat where fieldname = <ls_fcat>-fieldname.
        continue.
      endif.

      <ls_fcat>-edit   = abap_true.
      <ls_fcat>-no_out = abap_false.

      read table gt_table_field into ls_table_field with key fieldname = <ls_fcat>-fieldname.

      if ls_table_field-checktable is not initial.
        if <ls_fcat>-checktable is initial.
          <ls_fcat>-checktable = ls_table_field-checktable.
        endif.
        <ls_fcat>-emphasize = 'C500'.
      endif.

      if ls_table_field-domname is not initial.
        if <ls_fcat>-domname is initial.
          <ls_fcat>-domname = ls_table_field-domname.
        endif.

        if ls_table_field-domname = 'TZNTSTMPS'.
          <ls_fcat>-edit_mask = '______________'.
        endif.
      endif.
    endloop.
  endif.
endform.

*&---------------------------------------------------------------------*
*& Form event_top_of_page
*&---------------------------------------------------------------------*
form refresh_top_of_page .
  call method go_docu->initialize_document.

  perform top_of_page_split.

  call method go_docu->merge_document.
  call method go_docu->display_document
    exporting
      reuse_control      = 'X'
      reuse_registration = 'X'
      parent             = go_container_top.
endform.
*&---------------------------------------------------------------------*
*& Form delete_toolbar_buttons
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- E_OBJECT
*&---------------------------------------------------------------------*
form delete_toolbar_buttons  changing le_object type ref to cl_alv_event_toolbar_set.

  delete le_object->mt_toolbar where function eq cl_gui_alv_grid=>mc_fc_loc_cut
                               or function eq cl_gui_alv_grid=>mc_fc_loc_copy
                               or function eq cl_gui_alv_grid=>mc_fc_loc_copy_row
                               or function eq cl_gui_alv_grid=>mc_fc_loc_paste
                               or function eq cl_gui_alv_grid=>mc_fc_loc_append_row
                               or function eq cl_gui_alv_grid=>mc_fc_loc_insert_row
                               or function eq cl_gui_alv_grid=>mc_fc_loc_delete_row
                               or function eq cl_gui_alv_grid=>mc_fc_check
                               or function eq cl_gui_alv_grid=>mc_fc_loc_undo.

endform.
*&---------------------------------------------------------------------*
*& Form set_status_table
*&---------------------------------------------------------------------*
form set_crud_status_data .
  data: field_tab      type table of dfies,
        lr_tabdescr    type ref to cl_abap_structdescr,
        l_offset       type i,
        ls_crud_status type  ty_crud_status.

  gt_crud_status = value #(
                  ( crud_type = 'I' status = 'Insert' )
                  ( crud_type = 'U' status = 'Update' )
                  ( crud_type = 'D' status = 'Delete' )
                  ( crud_type = 'E' status = 'Error' )
                ) .

  lr_tabdescr ?= cl_abap_structdescr=>describe_by_data( ls_crud_status ).
  field_tab[] = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

  loop at field_tab assigning field-symbol(<field>).
    <field>-lfieldname = <field>-fieldname.
    <field>-offset = l_offset.
    clear <field>-reptext.
    case <field>-fieldname.
      when 'CRUD_TYPE'.
        <field>-scrtext_s = 'Ty.'.
        <field>-scrtext_m = 'Type'.
        <field>-scrtext_l = 'Crud Type'.
      when 'STATUS'.
        <field>-scrtext_s = 'St.'.
        <field>-scrtext_m = 'Status'.
        <field>-scrtext_l = 'Status'.
      when others.
    endcase.
    l_offset = l_offset + <field>-intlen.
  endloop.

  lt_field_tab = field_tab.
endform.
