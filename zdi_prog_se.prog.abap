*&---------------------------------------------------------------------*
*& Report  ZDI_PROG_SE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report zdi_prog_se.

load-of-program.

  data di_container type ref to zcl_di_container_sw.

  create object di_container.

  di_container->bind( service = 'ZIF_BP_SERVICE' target = 'ZCL_BP_SERVICE' ).

*   di_container->bind( service = 'ZCL_BP_SERVICE' ).

  di_container->bind( service = 'ZIF_BP_ACCOUNT' target = 'ZCL_BP_ACCOUNT' ).

*   di_container->bind( service = 'ZCL_BP_ACCOUNT' ).

start-of-selection.

  data account type ref to zif_bp_account.

  account ?= di_container->get( service = 'ZIF_BP_ACCOUNT' ).

  check 1 = 1.
