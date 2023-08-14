%%%===================================================================
%%%
%%%===================================================================

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-define(      ATOM_CACHE_REF,  82).
-define(            ATOM_EXT, 100).
-define(       ATOM_UTF8_EXT, 118).
-define(          BINARY_EXT, 109).
-define(      BIT_BINARY_EXT,  77).
-define(          EXPORT_EXT, 113).
-define(           FLOAT_EXT,  99).
-define(             FUN_EXT, 117).
-define(         INTEGER_EXT,  98).
-define(       LARGE_BIG_EXT, 111).
-define(     LARGE_TUPLE_EXT, 105).
-define(            LIST_EXT, 108).
-define(           LOCAL_EXT, 121).
-define(             MAP_EXT, 116).
-define( NEWER_REFERENCE_EXT,  90).
-define(       NEW_FLOAT_EXT,  70).
-define(         NEW_FUN_EXT, 112).
-define(         NEW_PID_EXT,  88).
-define(        NEW_PORT_EXT,  89).
-define(   NEW_REFERENCE_EXT, 114).
-define(             NIL_EXT, 106).
-define(             PID_EXT, 103).
-define(            PORT_EXT, 102).
-define(       REFERENCE_EXT, 101).
-define(      SMALL_ATOM_EXT, 115).
-define( SMALL_ATOM_UTF8_EXT, 119).
-define(       SMALL_BIG_EXT, 110).
-define(   SMALL_INTEGER_EXT,  97).
-define(     SMALL_TUPLE_EXT, 104).
-define(          STRING_EXT, 107).
-define(         V4_PORT_EXT, 120).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-define(CODE(Code,Name), code(Code) -> Name; code(Name) -> Code).
