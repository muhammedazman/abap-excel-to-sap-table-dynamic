*&---------------------------------------------------------------------*
*& Include          ZMA_EXCEL_TO_DDIC_CRUD_TOP
*&---------------------------------------------------------------------*

report zma_excel_to_ddic_crud.

* Local Class for Excel
class lcl_file definition create private.
  public section.

    class-methods:
      create
        returning value(ro_result) type ref to lcl_file.

    methods:
      get_filename_open_pc
        importing mv_filter          type string
        returning value(rv_filename) type rlgrap-filename,

      excel_to_internal_table
        importing mv_file         type rlgrap-filename
        returning value(rt_excel) type issr_alsmex_tabline.


    data: mt_files type filetable,
          ms_file  type line of filetable,
          mf_rc    type i.
endclass.


class lcl_datenelements definition create private.
  public section.

    class-methods:
      create
        returning value(ro_result) type ref to lcl_datenelements,

      get_key_fields
        importing iv_tabname type tabname,

      get_columns_of_sap_tabelle
        importing iv_tabname    type tabname
        returning value(rt_tab) type dfies_tab,

      serial_comp importing comp_names    type string
                            struct        type data
                  returning value(return) type string.

    class-data r_type_struct type ref to cl_abap_structdescr.
endclass.

class lcx_alv_display_error definition
inheriting from cx_static_check.
endclass.

class lcl_alv definition create private.
  public section.

    class-methods:
      create
        returning value(ro_result) type ref to lcl_alv,

      set_fcat,

      set_fcat_with_ddic
        importing is_table_field type dfies
                  iv_colpos      type i,

      set_fcat_custom
        importing is_fieldcatalog type lvc_s_fcat.

    methods:
      create_container,
      set_handler,
      set_layout,
      set_alv,
      display_alv
        raising lcx_alv_display_error.
endclass.

class lcl_events definition.
  public section.
    class-methods: on_toolbar for event toolbar of cl_gui_alv_grid
      importing
        e_object
        e_interactive.

    class-methods: on_menu_bt_handle for event menu_button  of cl_gui_alv_grid
      importing e_object
                e_ucomm.

    class-methods: on_user_command for event user_command of cl_gui_alv_grid
      importing
        e_ucomm
        sender.

    class-methods: on_data_changed for event data_changed of cl_gui_alv_grid
      importing
        er_data_changed
        sender.

    class-methods: on_top_of_page for event top_of_page of cl_gui_alv_grid
      importing
        e_dyndoc_id
        table_index.

    class-methods: on_f4help for event onf4 of cl_gui_alv_grid
      importing
        es_row_no.

    class-data m_show_empty type abap_bool.
endclass.

class lcl_domvalues definition.
  public section.
    class-methods: get_by_type
      importing
                iv_data               type any
      returning value(rt_ddfixvalues) type ddfixvalues.

    class-methods: get_by_name
      importing
                iv_name               type domname
      returning value(rt_ddfixvalues) type ddfixvalues.
endclass.

*** --- Selection screen designing
selection-screen begin of block b2 with frame title text-002.
  selection-screen begin of line.
    selection-screen comment 1(79) text-t01.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(79) text-t02.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(79) text-t03.
  selection-screen end of line.
selection-screen end of block b2.

selection-screen begin of block b1 with frame title text-001.

  parameters:p_file  type rlgrap-filename modif id par obligatory,
             p_tname type dd02l-tabname   modif id par obligatory.

  selection-screen skip.

  selection-screen begin of line.
    selection-screen comment 1(60) text-sim for field p_simula.
    parameters: p_simula radiobutton group grp0 default 'X' user-command rbtn_mode.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(60) text-akt for field p_aktive.
    parameters: p_aktive radiobutton group grp0.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(60) text-adm for field p_admin.
    parameters: p_admin radiobutton group grp0.
  selection-screen end of line.


  selection-screen begin of block b3 no intervals.

    selection-screen uline modif id m1.


    selection-screen begin of line .
      selection-screen comment (6) text-ins for field p_insert modif id m1.
      parameters: p_insert radiobutton group grp1 default 'X'  modif id m1.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment (6) text-upt for field p_update modif id m1.
      parameters: p_update radiobutton group grp1 modif id m1.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment (6) text-del for field p_delete modif id m1.
      parameters: p_delete radiobutton group grp1 modif id m1.
    selection-screen end of line.

  selection-screen end of block b3.

selection-screen end of block b1.


* local class
data: go_file     type ref to lcl_file,
      go_datenelm type ref to lcl_datenelements,
      go_alv_cl   type ref to lcl_alv.


types: begin of ty_message,
         message type char50,
         value   type abap_typename,
       end of ty_message.

data: gt_error_messages type standard table of ty_message with empty key.

data: lv_msg_err type char50.

data: gt_excel         type table of alsmex_tabline,
      gt_index_row_col type sorted table of alsmex_tabline with unique key row col,
      lt_dref          type ref to data,
      ls_dref          type ref to data,
      lv_col           type i,
      gv_col_num       type i.

*** --- Field Symbols
field-symbols : <fs_table> type any .
field-symbols : <ft_table> type standard table.
field-symbols : <dyn_field> .

*--------------------------------------------------------------------*
* Tabelle Info
*--------------------------------------------------------------------*
data: gt_key_fields   type table of dd03l,
      gt_table_field  type table of dfies,
      gt_comp_details type standard table of abap_compdescr
                        with key name.

field-symbols: <lv_light>      type any,
               <lv_status>     type any,
               <lv_crud_stat>  type any,
               <lv_field_data> type any,
               <lt_tabcol>     type lvc_t_scol.

field-symbols <t_cellstyle>  type lvc_t_styl.

field-symbols: <ls_table_field> type dfies.

data: lv_msg_row type char50.

*--------------------------------------------------------------------*
* Components of Structure
*--------------------------------------------------------------------*
data: ls_comp type        cl_abap_structdescr=>component,
      lt_comp type        cl_abap_structdescr=>component_table.

*--------------------------------------------------------------------*
* Data references for dynamic data/structure/table description
*--------------------------------------------------------------------*
data: lr_struct_data type ref to data,
      lr_table_data  type ref to data.

*--------------------------------------------------------------------*
* Data/Structur/Table description references
*--------------------------------------------------------------------*
data: lr_struct_dscr type ref to cl_abap_structdescr,
      lr_table_dscr  type ref to cl_abap_tabledescr.

*--------------------------------------------------------------------*
* Field symbols for data/structure/table
*--------------------------------------------------------------------*
field-symbols: <ls_data> type any,
               <lt_data> type standard table.


*--------------------------------------------------------------------*
* MAP DATA
*--------------------------------------------------------------------*

types: begin of ty_domvalues,
         rollname   type rollname,
         ddfixvalue type ddfixvalues,
       end of ty_domvalues.

types: begin of ty_checktables,
         tabname    type tabname,
         fieldname  type fieldname,
         primpos    type primpos,
         fortable   type fortable,
         forkey     type forkey,
         checktable type checktable,
         checkfield type fieldname,
       end of ty_checktables.
types: ty_it_checktables type standard table of ty_checktables
                              with unique sorted key tab_fieldname
                              components tabname fieldname fortable forkey.

types: begin of ty_checktable_data,
         tabname   type tabname,
         fieldname type fieldname,
         value     type string,
       end of ty_checktable_data.
types: ty_it_checktable_data type sorted table of ty_checktable_data
                              with unique key tabname fieldname value.

types: begin of ty_crud_status,
         crud_type type char01,
         status    type char8,
       end of ty_crud_status.

data: gt_domvalues       type standard table of ty_domvalues.
data: gt_checktables     type ty_it_checktables.
data: gt_checktable_data type ty_it_checktable_data.
data: gt_crud_status     type standard table of ty_crud_status.
data: lt_field_tab       type table of dfies.



*--------------------------------------------------------------------*
* ALV
*--------------------------------------------------------------------*
data: go_splitter         type ref to cl_gui_splitter_container,
      go_container_top    type ref to cl_gui_container,
      go_container_bottom type ref to cl_gui_container.

data: go_alv type ref to cl_gui_alv_grid.

data: go_docu type ref to cl_dd_document.
data: go_dd_right type ref to cl_dd_area.

data: gs_layout type lvc_s_layo.

data: gt_fcat type lvc_t_fcat,
      gs_fcat type lvc_s_fcat.

data: gv_col_light_idx  type i value 1,
      gv_col_status_idx type i value 2,
      gv_col_crud_idx   type i value 3,
      gv_col_cell_idx   type i.

data: gv_row_count type i.
data: gv_transaktion_count type i.

data: gv_count_insert_excel type i,
      gv_count_update_excel type i,
      gv_count_delete_excel type i,
      gv_count_error_excel  type i,
      gv_count_insert_sap   type i,
      gv_count_update_sap   type i,
      gv_count_delete_sap   type i,
      gv_count_error_sap    type i.

data: gv_edit_bool type abap_bool value abap_false.
data: gv_initialized type abap_bool value abap_false.
data: gv_save_clicked type abap_bool value abap_false.
data: show_toolbar type x.

*--------------------------------------------------------------------*
* CONSTANTS
*--------------------------------------------------------------------*
constants: co_light               type fieldname value 'LIGHT',
           co_status              type fieldname value 'STATUS',
           co_crud                type fieldname value 'CRUD',
           co_cell                type fieldname value 'CELL_STYLE',
           co_tabcol              type fieldname value 'TABCOL',
           co_mandt               type fieldname value 'MANDT',

           co_cnt_ext_feld        type i value 3, " Gesamtzahl der zusätzlichen Spalten

           co_coltext_status      type lvc_txtcol value 'Status',
           co_coltext_crud        type lvc_txtcol value 'Transaction',
           co_coltext_light       type lvc_txtcol value 'Exception',
           co_coltext_cell        type lvc_txtcol value 'Cell Style',

           co_datatype_char01     type rollname value 'CHAR_01',
           co_datatype_char0256   type rollname value 'CHAR0256',
*           co_datatype_s_data     type rollname value 'STRING_DATA',
           co_datatype_lvc_t_styl type rollname value 'LVC_T_STYL',
           co_datatype_lvc_t_scol type rollname value 'LVC_T_SCOL',

           co_light_erro          type c value '1',
           co_light_warn          type c value '2',
           co_light_succ          type c value '3',
           co_light_crud_succ_ins type c value '4',
           co_light_crud_succ_upt type c value '5',
           co_light_crud_succ_del type c value '6',
           co_light_crud_err      type c value '7',
           co_light_dubl_err      type c value '8', " dublicate error

           co_ok(2)               type c value 'OK',

           co_status_insert       type c value 'I',
           co_status_update       type c value 'U',
           co_status_delete       type c value 'D',
           co_status_error        type c value 'E',

           co_insert_all_text(10) type c value 'Insert All',
           co_delete_all_text(10) type c value 'Delete All',
           co_update_all_text(10) type c value 'Update All'.

constants: co_ucomm_save_all   type ui_func value '&SAVE_ALL',
           co_ucomm_save_aktiv type ui_func value '&SAVE_AKTIV',
           co_ucomm_edit       type ui_func value '&EDIT',
           co_ucomm_delete     type ui_func value '&DELETE',
           co_ucomm_pruefen    type ui_func value '&PRUEFEN',
           co_ucomm_refresh    type ui_func value '&BTN_REFRESH',
           co_ucomm_se16n      type ui_func value '&SE16N',
           co_ucomm_menu       type ui_func value '&BTN_MENU',
           co_ucomm_hide       type ui_func value '&HIDE',
           co_ucomm_show       type ui_func value '&SHOW',
           co_ucomm_toolbar    type ui_func value '&TOOLBAR_SHOW'.

constants: co_se16n_toolbar_text(5)    type c value 'SE16N',
           co_edit_toolbar_text(10)    type c value 'Bearbeiten',
           co_pruefen_toolbar_text(10) type c value 'Prüfen',
           co_delete_toolbar_text(10)  type c value 'Löschen',
           co_save_toolbar_text(10)    type c value 'Speichern'.
