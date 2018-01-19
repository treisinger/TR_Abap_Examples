*&---------------------------------------------------------------------*
*& Report  Z_DP_PROTOTYPE_01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_DP_PROTOTYPE_01.


*----------------------------------------------------------------------*
* -> CLASS cl_employee DEFINITION
*----------------------------------------------------------------------*
CLASS cl_employee DEFINITION ABSTRACT.

  PUBLIC SECTION.

    DATA : name TYPE string,
           role TYPE string.

    METHODS : clone    ABSTRACT EXPORTING clone_instance TYPE REF TO cl_employee,
              tostring ABSTRACT.

ENDCLASS.                    "cl_employee DEFINITION

*----------------------------------------------------------------------*
* -> CLASS cl_Typist DEFINITION
*----------------------------------------------------------------------*
CLASS cl_typist DEFINITION INHERITING FROM cl_employee.

  PUBLIC SECTION.

    DATA : wordsperminute TYPE n.

    METHODS : clone    REDEFINITION,
              tostring REDEFINITION.

ENDCLASS.                    "cl_Typist DEFINITION

*----------------------------------------------------------------------*
* -> CLASS cl_typist IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_typist IMPLEMENTATION.

  METHOD clone.

    DATA : o_cl_typist TYPE REF TO cl_typist.

    CREATE OBJECT clone_instance TYPE cl_typist.

    TRY.

        o_cl_typist ?= clone_instance.

        MOVE : me->name           TO o_cl_typist->name,
               me->role           TO o_cl_typist->role,
               me->wordsperminute TO o_cl_typist->wordsperminute.

      CATCH cx_sy_move_cast_error.

        WRITE: / 'Up Cast Failed'.

    ENDTRY.

  ENDMETHOD.                    "clone

  METHOD tostring.

    DATA : v_msg TYPE string.

    CONCATENATE : `Name = ` me->name
                  `, Role = ` me->role
                  `, Words Perminute = ` me->wordsperminute
                  INTO v_msg.

    WRITE / v_msg.

  ENDMETHOD.                    "tostring

ENDCLASS.                    "cl_typist IMPLEMENTATION

*----------------------------------------------------------------------*
* -> CLASS cl_Developer DEFINITION
*----------------------------------------------------------------------*
CLASS cl_developer DEFINITION INHERITING FROM cl_employee.

  PUBLIC SECTION.

    DATA : preferredlanguage TYPE string.

    METHODS : clone    REDEFINITION,
              tostring REDEFINITION.

ENDCLASS.                    "cl_Developer DEFINITION

*----------------------------------------------------------------------*
* -> CLASS cl_Developer IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_developer IMPLEMENTATION.

  METHOD clone.

    DATA : o_cl_developer TYPE REF TO cl_developer.

    CREATE OBJECT clone_instance TYPE cl_developer.

    TRY.

        o_cl_developer ?= clone_instance.

        MOVE : me->name              TO o_cl_developer->name,
               me->role              TO o_cl_developer->role,
               me->preferredlanguage TO o_cl_developer->preferredlanguage.

      CATCH cx_sy_move_cast_error.

        WRITE: / 'Up Cast Failed'.

    ENDTRY.

  ENDMETHOD.                    "clone

  METHOD tostring.

    DATA : v_msg TYPE string.

    CONCATENATE : `Name = ` me->name
                  `, Role = ` me->role
                  `, Preferred Language = ` me->preferredlanguage
                  INTO v_msg.

    WRITE / v_msg.

  ENDMETHOD.                    "tostring

ENDCLASS.                    "cl_Developer IMPLEMENTATION

************************************************************************
************************************************************************
*                                                                      *
*                           PROGRAM FLOW LOGIC                         *
*                                                                      *
************************************************************************

START-OF-SELECTION.

  DATA : o_base      TYPE REF TO cl_employee,
         o_dev       TYPE REF TO cl_developer,
         o_devcopy   TYPE REF TO cl_developer,
         o_casterror TYPE REF TO cx_sy_move_cast_error.

  CREATE OBJECT o_dev.

  IF o_dev IS BOUND.

    o_dev->name = 'Rafael Riso'.
    o_dev->role = 'Team Leader'.
    o_dev->preferredlanguage = 'ABAP OO'.

  ENDIF.

  TRY.

      o_dev->clone( IMPORTING clone_instance = o_base ).

      o_devcopy ?= o_base.

    CATCH cx_sy_move_cast_error INTO o_casterror.

      WRITE: / 'Up Cast Failed'.

  ENDTRY.

  IF o_devcopy IS BOUND.

    o_devcopy->name = 'Johnnie Walker'.

  ENDIF.

  IF o_dev IS BOUND AND o_devcopy IS BOUND.

    o_dev->tostring( ).

    ULINE.

    o_devcopy->tostring( ).

  ENDIF.
