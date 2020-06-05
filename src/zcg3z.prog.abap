*&---------------------------------------------------------------------*
*& Report ZCG3Z
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcg3z.

DATA: ig_okcode TYPE syucomm.
TABLES rcgfiletr.

START-OF-SELECTION.
  rcgfiletr-ftftype = 'ASC'.
  CALL SCREEN 1020 STARTING AT 10 4.

MODULE d1020_init OUTPUT.
*  data(fcodes) = value #( ( |EEXO| ) ).
  SET TITLEBAR 'PUL'.
  SET PF-STATUS 'POPUPIMP'." excluding fcodes.
ENDMODULE.

MODULE d1020_exit INPUT.
  CASE ig_okcode.
    WHEN 'EIMP'.
      " UPLOAD
      CASE rcgfiletr-ftftype.
        WHEN 'ASC'.
          DATA lt_line TYPE string_table.
          PERFORM read_char_file USING rcgfiletr-ftfront CHANGING lt_line.
          PERFORM write_text_file USING rcgfiletr-ftappl lt_line.
        WHEN 'BIN'.
          DATA l_xstring TYPE xstring.
          PERFORM read_bin_file USING rcgfiletr-ftfront CHANGING l_xstring.
          PERFORM write_bin_file USING rcgfiletr-ftappl l_xstring.
      ENDCASE.
      MESSAGE i159(c$) WITH rcgfiletr-ftfront rcgfiletr-ftappl.
    WHEN 'ECAN'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE d1020_valreq_ftfront INPUT.
  DATA lt_filetable    TYPE filetable.
  DATA l_rc           TYPE i.
  DATA l_action       TYPE i.
  FIELD-SYMBOLS <ls_file> TYPE file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*      EXPORTING
*        multiselection = abap_false "false par défaut
    CHANGING
      file_table              = lt_filetable
      rc                      = l_rc
      user_action             = l_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc NE 0.
    " TODO
  ELSEIF l_action NE cl_gui_frontend_services=>action_ok.
    " Télé-chargement annulé par l'utilisateur
  ELSE.
    " 1 ou plusieurs fichiers sélectionnés
    READ TABLE lt_filetable INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc = 0.
      rcgfiletr-ftfront = <ls_file>-filename.
    ENDIF.
  ENDIF.
ENDMODULE.

FORM read_char_file
      USING
        i_filename TYPE csequence
      CHANGING
        et_line    TYPE string_table.
  DATA(i_filename2) = CONV string( i_filename ).
  REFRESH et_line.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename = i_filename2
      filetype = 'ASC'
    CHANGING
      data_tab = et_line
    EXCEPTIONS
      OTHERS   = 17.

ENDFORM.

FORM read_bin_file
      USING i_filename TYPE clike
      CHANGING e_file_xstring TYPE xstring.
  DATA l_filename TYPE string.
  DATA l_length TYPE i.
  DATA lt_x255 TYPE TABLE OF x255.

  l_filename = i_filename.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename   = l_filename
      filetype   = 'BIN'
    IMPORTING
      filelength = l_length
    CHANGING
      data_tab   = lt_x255
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0.

    CALL METHOD cl_swf_utl_convert_xstring=>table_to_xstring
      EXPORTING
        i_table  = lt_x255
        i_size   = l_length
      RECEIVING
        r_stream = e_file_xstring
      EXCEPTIONS
        OTHERS   = 3.

  ENDIF.

ENDFORM.

FORM write_text_file
      USING
        i_filename  TYPE csequence
        it_string   TYPE string_table.
  FIELD-SYMBOLS <l_string> TYPE string.

  DATA(i_filename2) = CONV string( i_filename ).
  OPEN DATASET i_filename2 IN TEXT MODE FOR OUTPUT ENCODING DEFAULT. "crlf, encoding, bom, ...
  IF sy-subrc = 0.
    LOOP AT it_string ASSIGNING <l_string>.
      TRANSFER <l_string> TO i_filename2.
    ENDLOOP.
    CLOSE DATASET i_filename2.
  ENDIF.
ENDFORM.

FORM write_bin_file
      USING
        i_filename      TYPE csequence
        i_file_xstring  TYPE xstring.

  DATA lt_xstring TYPE TABLE OF x255.
  DATA l_length TYPE i.
  DATA l_filename TYPE string.

  l_filename = i_filename.

  CALL METHOD cl_swf_utl_convert_xstring=>xstring_to_table
    EXPORTING
      i_stream = i_file_xstring
    IMPORTING
      e_table  = lt_xstring
    EXCEPTIONS
      OTHERS   = 3.

  l_length = xstrlen( i_file_xstring ).

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize = l_length
      filename     = l_filename
      filetype     = 'BIN'
    CHANGING
      data_tab     = lt_xstring
    EXCEPTIONS
      OTHERS       = 3.

ENDFORM.
