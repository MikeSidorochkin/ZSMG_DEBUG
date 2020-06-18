*<script:persistent>

REPORT  rstpda_script_template.

*<script:header>
*<scriptname>ZSMG_DBG_SKIP_AUTH</scriptname>
*<script_class>lcl_debugger_script</script_class>
*<script_comment>Skip authority checks</script_comment>
*<bp_reached>x</bp_reached>

*</script:header>

*<script:presettings>
*<bp>
*<flagactive>x</flagactive>
*<kind>1 </kind>
*<statementsta>authority-check</statementsta>
*</bp>

*</script:presettings>

*<script:script_class>
*---------------------------------------------------------------------*
*       class lcl_debugger_script definition
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
 CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

   PUBLIC SECTION.
     METHODS: prologue  REDEFINITION,
       init    REDEFINITION,
       script  REDEFINITION,
       end     REDEFINITION.

 ENDCLASS.


 CLASS lcl_debugger_script IMPLEMENTATION.
   METHOD prologue.
     super->prologue( ).
   ENDMETHOD.

   METHOD init.
   ENDMETHOD.

   METHOD script.
     TRY.
         debugger_controller->debug_step(
           EXPORTING
             p_command = cl_tpda_script_debugger_ctrl=>debug_step_over ).
       CATCH cx_tpda_scr_rtctrl_status .
       CATCH cx_tpda_scr_rtctrl .
     ENDTRY.

     TRY.
         cl_tpda_script_data_descr=>change_value(
           EXPORTING
             p_new_value = '0'
             p_varname   = 'sy-subrc' ).
       CATCH cx_tpda_varname .
       CATCH cx_tpda_scr_auth .
     ENDTRY.
   ENDMETHOD.

   METHOD end.
   ENDMETHOD.
 ENDCLASS.

*</script:script_class>

*</SCRIPT:PERSISTENT>
