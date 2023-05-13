*&---------------------------------------------------------------------*
*& Report ZMA_SE16N
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zma_se16n.

parameters: p_tname type tabname.

data i_tab_output type standard table of se16n_output with default key.
data i_tab_or_seltab type se16n_or_t.

initialization.
  import lt_output_fields = i_tab_output    from memory id 'ITAB_OUT'.
  import lt_or_seltab     = i_tab_or_seltab from memory id 'ITAB_SEL'.


start-of-selection.
  call function 'SE16N_INTERFACE'
    exporting
      i_tab            = p_tname
      i_edit           = 'X'
      i_sapedit        = 'X'
      i_max_lines      = 500
      i_display        = 'X'
    tables
      it_output_fields = i_tab_output
      it_or_selfields  = i_tab_or_seltab
    exceptions
      no_values        = 1
      others           = 2.
  if sy-subrc = 0.
    clear: i_tab_or_seltab, i_tab_output.

    delete from memory id 'ITAB_OUT'.
    delete from memory id 'ITAB_SEL'.
  endif.
