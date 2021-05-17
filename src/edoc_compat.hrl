-ifndef(__EDOC_COMPAT_HRL__).
-define(__EDOC_COMPAT_HRL__, true).

-ifndef(NO_APP).
%% OTP 24+ EDoc
-define(NO_APP, no_app).
-endif.

-if(?OTP_RELEASE >= 24).
-define(context, doclet_context).
-else.
%% Pre OTP 24 EDoc
-define(context, context).
-endif. %% NO_APP

-endif. %% __EDOC_COMPAT_HRL__
