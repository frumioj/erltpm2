-define(TPM_ST_NO_SESSIONS, 16#8001).
-define(TPM_ST_SESSIONS, 16#8002).

% Command codes
-define(TPM_CC_CREATE, 16#00000153).
-define(TPM_CC_CREATE_PRIMARY, 16#00000131).
-define(TPM_CC_LOAD, 16#00000157).
-define(TPM_CC_FLUSH_CONTEXT, 16#00000165).
-define(TPM_CC_START_AUTH_SESSION, 16#00000176).
-define(TPM_CC_POLICY_SECRET, 16#00000151).
% ... add all other command codes

% Handle types
-define(TPM_RH_OWNER, 16#40000001).
-define(TPM_RH_NULL, 16#40000007).
-define(TPM_RS_PW, 16#40000009).

% Algorithm IDs
-define(TPM_ALG_RSA, 16#0001).
-define(TPM_ALG_SHA256, 16#000B).
-define(TPM_ALG_AES, 16#0006).
% ... add other algorithm definitions