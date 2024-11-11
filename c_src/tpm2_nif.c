#include <erl_nif.h>
#include <tss2/tss2_sys.h>
#include <tss2/tss2_tcti_device.h>
#include <string.h>

typedef struct {
    TSS2_SYS_CONTEXT *sys_context;
    TSS2_TCTI_CONTEXT *tcti_context;
} tpm_context_t;

static ErlNifResourceType *tpm_context_resource_type = NULL;

static void tpm_context_dtor(ErlNifEnv *env, void *obj) {
    tpm_context_t *ctx = (tpm_context_t *)obj;
    if (ctx->sys_context) {
        Tss2_Sys_Finalize(ctx->sys_context);
        free(ctx->sys_context);
    }
    if (ctx->tcti_context) {
        Tss2_Tcti_Finalize(ctx->tcti_context);
        free(ctx->tcti_context);
    }
}

static ERL_NIF_TERM init_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    tpm_context_t *ctx;
    ERL_NIF_TERM result;
    size_t size;
    TSS2_RC rc;

    ctx = enif_alloc_resource(tpm_context_resource_type, sizeof(tpm_context_t));
    if (!ctx) {
        return enif_make_badarg(env);
    }

    size_t tcti_size;
    rc = Tss2_Tcti_Device_Init(NULL, &tcti_size, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        enif_release_resource(ctx);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, rc));
    }

    ctx->tcti_context = (TSS2_TCTI_CONTEXT *)malloc(tcti_size);
    if (!ctx->tcti_context) {
        enif_release_resource(ctx);
        return enif_make_badarg(env);
    }

    rc = Tss2_Tcti_Device_Init(ctx->tcti_context, &tcti_size, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        free(ctx->tcti_context);
        enif_release_resource(ctx);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, rc));
    }

    size = Tss2_Sys_GetContextSize(0);
    ctx->sys_context = (TSS2_SYS_CONTEXT *)malloc(size);
    if (!ctx->sys_context) {
        Tss2_Tcti_Finalize(ctx->tcti_context);
        free(ctx->tcti_context);
        enif_release_resource(ctx);
        return enif_make_badarg(env);
    }

    rc = Tss2_Sys_Initialize(ctx->sys_context, size, ctx->tcti_context, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        free(ctx->sys_context);
        Tss2_Tcti_Finalize(ctx->tcti_context);
        free(ctx->tcti_context);
        enif_release_resource(ctx);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, rc));
    }

    result = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM transmit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    tpm_context_t *ctx;
    ErlNifBinary command_bin, response_bin;
    TSS2_RC rc;
    size_t response_size = 4096; // Adjust as needed

    if (!enif_get_resource(env, argv[0], tpm_context_resource_type, (void **)&ctx) ||
        !enif_inspect_binary(env, argv[1], &command_bin)) {
        return enif_make_badarg(env);
    }

    if (!enif_alloc_binary(response_size, &response_bin)) {
        return enif_make_badarg(env);
    }

    // Example: Use TPM2_Sys_Execute to send a command
    rc = Tss2_Sys_Execute(ctx->sys_context);
    if (rc != TSS2_RC_SUCCESS) {
        enif_release_binary(&response_bin);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, rc));
    }

    // Example: Complete the command and get the response
    // This part will depend on the specific command you are executing
    // rc = Tss2_Sys_<Command>_Complete(ctx->sys_context, &response_bin.data);

    if (!enif_realloc_binary(&response_bin, response_size)) {
        enif_release_binary(&response_bin);
        return enif_make_badarg(env);
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &response_bin));
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    const char *mod = "tpm2_nif";
    const char *name = "tpm_context";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    tpm_context_resource_type = enif_open_resource_type(env, mod, name, tpm_context_dtor, flags, NULL);
    if (!tpm_context_resource_type) {
        return -1;
    }

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"init_context", 0, init_context},
    {"transmit", 2, transmit}
};

ERL_NIF_INIT(tpm2_nif, nif_funcs, load, NULL, NULL, NULL)
