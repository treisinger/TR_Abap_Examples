report zdi_prog.

data:

  lvs_msg type string.

define inject.
*> &1 ref to interface or class
  zdi=>create_inst(
    changing c_ref = &1 ).
  assert &1 is not initial.
end-of-definition.

define test_mode.
  zdi=>test_mode( ).
end-of-definition.

define pure_mode.
  zdi=>pure_mode( ).
end-of-definition.

define prn.
*> &1 obj to test
  lvs_msg = &1->test( ).
  write lvs_msg.
  new-line.
end-of-definition.

start-of-selection.

* we have the business logic interface:
*   ZDI_IF
* we have business logic classes implementing ZDI_IF:
*   default (standard, from us) ZDI_DEFAULT
*   alternative (customer)      ZDI_ALTERNATIVE
*   test (for units)            ZDI_TEST

  data:
    lcl_di type ref to zdi_if.
*    lif_di2 type ref to zdi_alternative,
*    lcl_di3 type ref to zdi_alternative,
*    lif_di4 type ref to zdi_default,
*    lif_di5 type ref to zdi_default.
*    lcl_di1 type ref to zdi_default,
*    lif_di2 type ref to zdi_if,
*    lcl_di3 type ref to zdi_default,
*    lif_di4 type ref to zdi_if,
*    lif_di5 type ref to zdi_if.

* the business logic class that is instantiated varies depending on the
* mode we are in. Within Unit tests, the test mode could be activated
* and any configuration and persistence classes could be switched out.

check 1 = 1.

test_mode.

inject:
  lcl_di.


*  inject:
*    lcl_di1, " instantiates alternative class ZDI_ALTERNATIVE
*    lif_di2. " instantiates alternative class ZDI_ALTERNATIVE
*
*  test_mode. " indicates we want a class tagged with test tag
*
*  inject:
*    lcl_di3, " instantiates alternative class ZDI_ALTERNATIVE
*    lif_di4. " instantiates test class ZDI_TEST
*
*  pure_mode. " indicates we want a class tagged with default tag
*
*  inject:
*    lif_di5. " instantiates default class ZDI_DEFAULT

*  prn:
**    lcl_di1.
*
*  prn:
*    lcl_di1, " alternative - alternative is a descendant of default
*    lif_di2, " alternative - alternative implements interface
*    lcl_di3, " alternative - test class is not related to default
*    lif_di4, " test - test implements interface
*    lif_di5. " default - default implements interface and is tagged def

  prn:
    lcl_di.
