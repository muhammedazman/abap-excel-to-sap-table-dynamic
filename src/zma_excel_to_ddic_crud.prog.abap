*&---------------------------------------------------------------------*
*& Report ZMA_EXCEL_TO_DDIC_CRUD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


include zma_excel_to_ddic_crud_top. " data definitions

include zma_excel_to_ddic_crud_c01. " class implementation

include zma_excel_to_ddic_crud_f01. " performs

include zma_excel_to_ddic_crud_f02. " check all


initialization.
  go_file     = lcl_file=>create( ).
  go_datenelm = lcl_datenelements=>create( ).
  go_alv_cl   = lcl_alv=>create( ).
  perform set_crud_status_data.


at selection-screen on value-request for p_file.
  p_file = go_file->get_filename_open_pc( mv_filter = cl_gui_frontend_services=>filetype_excel ).


at selection-screen output.
  perform screen_control.


start-of-selection.

*--------------------------------------------------------------------*
* GET KEY FIELDS
*--------------------------------------------------------------------*
  lcl_datenelements=>get_key_fields( p_tname ).

*--------------------------------------------------------------------*
* GET EXCEL
*--------------------------------------------------------------------*
  gt_excel = go_file->excel_to_internal_table( mv_file = p_file ).

*--------------------------------------------------------------------*
* GET SAP TABELLE TECHNICAL DETAIL
*--------------------------------------------------------------------*
  gt_table_field = lcl_datenelements=>get_columns_of_sap_tabelle( p_tname ).

  loop at gt_table_field into data(ls_table_field).
    read table gt_excel transporting no fields with key row = 1 value = ls_table_field-fieldname.

    if sy-subrc <> 0.
      delete gt_table_field where fieldname = ls_table_field-fieldname.
    endif.
  endloop.

*--------------------------------------------------------------------*
* DATENELEMENTS
*--------------------------------------------------------------------*
  perform generate_to_datenelements.

*--------------------------------------------------------------------*
* CHECK
*--------------------------------------------------------------------*
  if <ls_data> is assigned and <lt_data> is assigned.
* excel key fields = sap table key field
    perform check_key_fields_match.

    if gt_error_messages is initial.
      perform check_all using abap_false. " do not convertion exit routine
    endif.
  endif.

*--------------------------------------------------------------------*
* DISPLAY ALV
*--------------------------------------------------------------------*
  if gt_error_messages is initial.
    go_alv_cl->display_alv( ).
    gv_initialized = abap_true.
  else.
    perform display_error_messages.
  endif.
