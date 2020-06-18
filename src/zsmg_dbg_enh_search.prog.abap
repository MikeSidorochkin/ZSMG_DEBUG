*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZSB_ENHANCMENT_SEARCH</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Поиск расширений</SCRIPT_COMMENT>
*<BP_REACHED>X</BP_REACHED>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION,
      get_abap_src_info RETURNING VALUE(rs_info) TYPE tpda_scr_prg_info RAISING cx_tpda_src_info,
      check_stack,
      check_breakpoints,
      check_bp IMPORTING iv_bp_type TYPE string
                         it_stack   TYPE tpda_stack_view_it.

    METHODS: check_events_in_stack IMPORTING is_stack_view TYPE tpda_stack_view.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_script_params,
        trace          TYPE flag,
        debug          TYPE flag,
        customer_exits TYPE flag,
        new_badi       TYPE flag,
        old_badi       TYPE flag,
        enhancements   TYPE flag,
        bte            TYPE flag,
        user_exits     TYPE flag,
      END OF ts_script_params.

    DATA: mt_functions    TYPE RANGE OF tpda_event.
    DATA: mt_forms        TYPE RANGE OF tpda_event.
    DATA: mt_progs_cache  TYPE HASHED TABLE OF tpda_scr_prg_info WITH UNIQUE KEY program include line.

    DATA:
      ms_script_params TYPE ts_script_params.
ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD init.
    ms_script_params-trace = ms_script_params-customer_exits = ms_script_params-old_badi =
    ms_script_params-new_badi = ms_script_params-enhancements = ms_script_params-bte = abap_true.

    cl_ci_query_attributes=>generic(
      EXPORTING
        p_name       = CONV #( sy-repid )
        p_title      = 'Script options'
        p_attributes = VALUE #(

                                ( kind  = 'G'
                                  text  = 'Script options'
                                  ref   = REF #( sy-subrc ) )

                                ( ref   = REF #( ms_script_params-trace )
                                  kind  = 'R'
                                  text  = 'Create trace file' )

                                ( ref   = REF #( ms_script_params-debug )
                                  kind  = 'R'
                                  text  = 'Stop with debugger' )

                                ( kind  = 'G'
                                  text  = 'Enhancements types'
                                  ref   = REF #( sy-subrc ) )

                                ( ref   = REF #( ms_script_params-customer_exits )
                                  kind  = 'C'
                                  text  = 'Customer Exits' )

                                ( ref   = REF #( ms_script_params-new_badi )
                                  kind  = 'C'
                                  text  = 'New BAdI' )

                                ( ref   = REF #( ms_script_params-old_badi )
                                  kind  = 'C'
                                  text  = 'Old BAdI' )

                                ( ref   = REF #( ms_script_params-enhancements )
                                  kind  = 'C'
                                  text  = 'Enhancements' )

                                ( ref   = REF #( ms_script_params-bte )
                                  kind  = 'C'
                                  text  = 'BTE (OpenFI)' )

                                ( ref   = REF #( ms_script_params-user_exits )
                                  kind  = 'C'
                                  text  = 'User Exits' )
        )

        p_display    = '' ).

    TRY.
        IF ms_script_params-old_badi = abap_true.
          cl_tpda_script_bp_services=>set_bp_method_global_class(
            p_methodname = 'GET_INSTANCE'
            p_classname  = 'CL_EXITHANDLER' ).
        ENDIF.

        IF ms_script_params-customer_exits = abap_true.
          cl_tpda_script_bp_services=>set_bp_statement( p_statement = 'CALL CUSTOMER-FUNCTION' ).
        ENDIF.

        IF ms_script_params-enhancements = abap_true.
          cl_tpda_script_bp_services=>set_bp_statement( p_statement = 'ENHANCEMENT' ).
        ENDIF.

        IF ms_script_params-new_badi = abap_true.
          cl_tpda_script_bp_services=>set_bp_statement( p_statement = 'CALL BADI' ).
          cl_tpda_script_bp_services=>set_bp_statement( p_statement = 'GET BADI' ).
        ENDIF.

        cl_tpda_script_bp_services=>set_bp_stack_change( cl_tpda_script_bp_services=>c_script_bp ).
      CATCH cx_tpda_scr_bp.
    ENDTRY.

    IF ms_script_params-bte = abap_true.
      mt_functions = VALUE #( ( sign = 'I' option = 'CP' low = 'OPEN_FI_PERFORM_*' )
                              ( sign = 'I' option = 'CP' low = 'OUTBOUND_CALL_*'   )
                              ( sign = 'I' option = 'CP' low = 'EXIT*' ) ).
    ENDIF.

    IF ms_script_params-user_exits = abap_true.
      mt_forms = VALUE #( ( sign = 'I' option = 'CP' low = 'USEREXIT*' ) ).
    ENDIF.
  ENDMETHOD.                    "init

  METHOD check_bp.
    CASE iv_bp_type.
      WHEN 'CALL CUSTOMER-FUNCTION'.
        CHECK ms_script_params-customer_exits = abap_true.
      WHEN 'ENHANCEMENT'.
        CHECK ms_script_params-enhancements = abap_true.
      WHEN 'CALL BADI' OR 'GET BADI'.
        CHECK ms_script_params-new_badi = abap_true.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF ms_script_params-debug = abap_true.
      me->break( ).
    ENDIF.

    IF ms_script_params-trace = abap_true.
      TRY.
          DATA(ls_program_info) = get_abap_src_info( ).
        CATCH cx_tpda_src_info.
          RETURN.
      ENDTRY.

      trace->add_custom_info(
        EXPORTING
          p_trace_entry = VALUE #( value = |{ iv_bp_type } AT: { it_stack[ 1 ]-eventtype } { it_stack[ 1 ]-eventname }, line: { it_stack[ 1 ]-line }, program:{ it_stack[ 1 ]-program }|
                                   type = iv_bp_type ) ).

      trace->add_src_info( ).
      trace->add_event_info(
        EXPORTING
          p_abap_only = abap_true ).
    ENDIF.
  ENDMETHOD.

  METHOD get_abap_src_info.
    cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = rs_info ).
    READ TABLE mt_progs_cache WITH TABLE KEY program = rs_info-program
                                             include = rs_info-include
                                             line    = rs_info-line
      TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE cx_tpda_src_info.
    ELSE.
      INSERT rs_info INTO TABLE mt_progs_cache.
    ENDIF.
  ENDMETHOD.

  METHOD check_stack.
    TRY.
        DATA(lt_stack) = cl_tpda_script_abapdescr=>get_abap_dynp_stack( ).
      CATCH cx_tpda_stack_error.
    ENDTRY.

    check_events_in_stack( is_stack_view = lt_stack[ 1 ] ).
  ENDMETHOD.

  METHOD check_breakpoints.
    TRY.
        DATA(lt_stack) = cl_tpda_script_abapdescr=>get_abap_dynp_stack( ).
      CATCH cx_tpda_stack_error.
        RETURN.
    ENDTRY.

    TRY.
        DATA(lt_breakpoints) = cl_tpda_script_bp_services=>get_reached_script_bps( ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    LOOP AT lt_breakpoints ASSIGNING FIELD-SYMBOL(<ls_breakpoint>) WHERE statementsta IS NOT INITIAL.
      check_bp( it_stack = lt_stack
                iv_bp_type = CONV #( <ls_breakpoint>-statementsta ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD script.
    check_stack( ).
    check_breakpoints( ).
  ENDMETHOD.                    "script

  METHOD end.
  ENDMETHOD.                    "end

  METHOD check_events_in_stack.
    DATA:
      ls_program_info TYPE tpda_scr_prg_info.

    CASE is_stack_view-eventtype.
      WHEN 'FUNCTION'.
        CHECK is_stack_view-eventname IN mt_functions AND ms_script_params-bte = abap_true.

        IF ms_script_params-trace = abap_true.
          TRY.
              ls_program_info = get_abap_src_info( ).
            CATCH cx_tpda_src_info.
              RETURN.
          ENDTRY.

          trace->add_custom_info(
            EXPORTING
              p_trace_entry = VALUE #( value = |FM { is_stack_view-eventname }|
                                       type = 'BTE(FM)' ) ).
        ENDIF.

      WHEN 'FORM'.
        CHECK is_stack_view-eventname IN mt_forms AND ms_script_params-user_exits = abap_true.

        IF ms_script_params-trace = abap_true.
          TRY.
              ls_program_info = get_abap_src_info( ).
            CATCH cx_tpda_src_info.
              RETURN.
          ENDTRY.

          trace->add_custom_info(
            EXPORTING
              p_trace_entry = VALUE #( value = |PROGRAM: { ls_program_info-program }, INCLUDE: { ls_program_info-include }, FORM { is_stack_view-eventname }|
                                       type = 'USER-EXIT' ) ).
        ENDIF.

      WHEN 'METHOD'.
        IF is_stack_view-eventname = 'GET_INSTANCE' AND is_stack_view-program CP 'CL_EXITHANDLER*' AND ms_script_params-old_badi = abap_true.
          TRY.
              DATA(lv_exit_name) = cl_tpda_script_data_descr=>get_simple_value( 'EXIT_NAME' ).
            CATCH cx_tpda_varname cx_tpda_script_no_simple_type.
          ENDTRY.

          IF ms_script_params-trace = abap_true.
            TRY.
                ls_program_info = get_abap_src_info( ).
              CATCH cx_tpda_src_info.
                RETURN.
            ENDTRY.
          ENDIF.

          IF NOT lv_exit_name IS INITIAL.
            IF ms_script_params-trace = abap_true.
              trace->add_custom_info(
                EXPORTING
                  p_trace_entry = VALUE #( value = | OLD BADI: { lv_exit_name }|
                                           type = 'OLD_BADI' ) ).
            ENDIF.
          ELSE.
            TRY.
                IF ms_script_params-trace = abap_true.
                  DATA(ls_instance) = cl_tpda_script_data_descr=>get_variable_info( p_var_name  = 'INSTANCE' ).
                  trace->add_custom_info(
                    EXPORTING
                      p_trace_entry = VALUE #( value = |OLD BADI : { ls_instance-vartype } |
                                               type = 'OLD_BADI' ) ).
                ENDIF.
              CATCH cx_tpda_varname .
                RETURN.
            ENDTRY.
          ENDIF.
        ELSEIF ( is_stack_view-eventname CP  'IPR_*' OR
                 is_stack_view-eventname CP  'IPO_*' OR
                 is_stack_view-eventname CP  'IWO_*' ) AND ms_script_params-enhancements = abap_true.

          IF ms_script_params-trace = abap_true.
            DATA(lv_type) =  SWITCH string( is_stack_view-eventname+0(4)
                                            WHEN 'IPR_' THEN 'PreExit Method'
                                            WHEN 'IPO_' THEN 'PostExit Method'
                                            WHEN 'IWO_' THEN 'OverWrite-Exit Method'
                                            ELSE '' ).

            TRY.
                ls_program_info = get_abap_src_info( ).
              CATCH cx_tpda_src_info.
                RETURN.
            ENDTRY.

            SPLIT ls_program_info-program AT '=' INTO sy-msgv1 sy-msgv2 IN CHARACTER MODE.
            SPLIT ls_program_info-eventname AT '~' INTO sy-msgv2 sy-msgv3 IN CHARACTER MODE.

            trace->add_custom_info(
              EXPORTING
                p_trace_entry = VALUE #( value = |Class: { sy-msgv1 }, Method: { sy-msgv3  } |
                                             type = lv_type ) ).
          ENDIF.
        ELSE.
          RETURN.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF ms_script_params-trace = abap_true.
      trace->add_src_info( ).
      trace->add_event_info(
        EXPORTING
          p_abap_only = abap_true ).
    ENDIF.

    IF ms_script_params-debug = abap_true.
      me->break( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
