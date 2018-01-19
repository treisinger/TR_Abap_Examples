class ZCL_BP_ACCOUNT definition
  public
  final
  create public .

public section.

  interfaces ZIF_BP_ACCOUNT .

  methods CONSTRUCTOR
    importing
      !I_BP_SERVICE type ref to ZIF_BP_SERVICE .
protected section.
private section.

  data SERVICE type ref to ZIF_BP_SERVICE .
ENDCLASS.



CLASS ZCL_BP_ACCOUNT IMPLEMENTATION.


method CONSTRUCTOR.

    service = i_bp_service.

  endmethod.
ENDCLASS.
